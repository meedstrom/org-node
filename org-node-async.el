;;; org-node-async.el --- The beating heart -*- lexical-binding: t; -*-

(require 'bytecomp)
(require 'org-node-lib)

(defvar org-node-async-reset-hook nil)
(defvar org-node-async-rescan-file-hook nil)
(defvar org-node-async--start-time nil)

(defun org-node-async-reset ()
  "Wipe and rebuild the cache.
For an user-facing command, see \\[org-node-reset]."
  (clrhash org-nodes)
  (clrhash org-node-collection)
  (clrhash org-node--refs-table)
  (clrhash org-node--reflinks-table)
  (clrhash org-node--links-table)
  (org-node--init-org-id-locations-or-die)
  (org-node-async--collect (-uniq (hash-table-values org-id-locations)))
  (message nil))



;; I feel like this could be easier to read...
(defun org-node-async--add-node-to-tables (node-as-plist)
  "Add a node to `org-nodes' and maybe `org-node-collection'."
  (let ((node (apply #'make-org-node-data node-as-plist)))
    ;; Record the node even if it has no ID
    (puthash (or (org-node-get-id node) (format-time-string "%N"))
             node
             org-nodes)
    (when (or (not org-node-only-show-subtrees-with-id) (org-node-get-id node))
      ;; Populate `org-node--refs-table'
      (dolist (ref (org-node-get-refs node))
        (puthash ref (org-node-get-id node) org-node--refs-table))
      (when (funcall org-node-filter-fn node)
        ;; Populate `org-node-collection'
        (dolist (title (cons (org-node-get-title node)
                             (org-node-get-aliases node)))
          (puthash (funcall org-node-format-candidate-fn node title)
                   node
                   org-node-collection))
        ;; Let refs work as aliases
        (dolist (ref (org-node-get-refs node))
          (puthash ref node org-node-collection))))))

(defun org-node-async--forget-id-location (file)
  (org-node--init-org-id-locations-or-die)
  (cl-loop for id being the hash-keys of org-id-locations
           using (hash-values file-on-record)
           when (file-equal-p file file-on-record)
           do (remhash id org-id-locations)))

(defun org-node-async--split-into-n-sublists (big-list n)
  (let ((len (/ (length big-list) n))
        res)
    (dotimes (i n)
      (push
       (if (= i (- n 1))
           ;; The last iteration should just take what's left, not use `take'
           big-list
         (prog1 (take len big-list)
           (setq big-list (nthcdr len big-list))))
       res))
    res))

;; (org-node-async--split-into-n-sublists
;;       '(a v e e  q l fk k k ki i o r r  r r r r r r r r g g g  g g gg)
;;       4)

;; FIXME: I suspect each process is slow at the start, spinning up.  Must be
;; that it's parsing the long quoted lambda, as it contains a big list of files
;; literally.  Try reading that list in from a .eld file.  Or somehow just
;; write the entire lambda to a file and run emacs with the command args that
;; say "execute that file kthx".
(defun org-node-async--collect (files)
  (setq org-node-async--n-cores
        (max 1 (1- (string-to-number
                    (pcase system-type
                      ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
                       (if (executable-find "nproc")
                           (shell-command-to-string
                            "nproc --all")
                         (shell-command-to-string
                          "lscpu -p | egrep -v '^#' | wc -l")))
                      ((or 'darwin)
                       (shell-command-to-string
                        "sysctl -n hw.logicalcpu_max"))
                      ((or 'cygwin 'windows-nt 'ms-dos)
                       (user-error "org-node does not support Windows")))))))
  (let* ((native (and (featurep 'native-compile)
                      (native-comp-available-p)))
         (lib (find-library-name "org-node-worker"))
         (lib.eln (comp-el-to-eln-filename lib))
         (lib.elc (byte-compile-dest-file lib))
         (variables
          `(($bungle-file-name-handler . ,org-node-perf-bungle-file-name-handler)
            ($not-a-full-reset . ,(not (hash-table-empty-p org-nodes)))
            (coding-system-for-write . ,org-node-perf-assume-coding-system)
            (coding-system-for-read . ,org-node-perf-assume-coding-system)
            (gc-cons-threshold
             . ,(or org-node-perf-gc-cons-threshold gc-cons-threshold))
            ;; REVIEW Maybe it makes no difference
            (inhibit-eol-conversion
             . ,(member org-node-perf-assume-coding-system
                        '(utf-8-unix utf-8-dos utf-8-mac utf-16-le-dos utf-16-be-dos)))
            ($backlink-drawer-re
             . ,(concat "^[[:space:]]*:"
                        (or (and (boundp 'org-super-links-backlink-into-drawer)
                                 (stringp org-super-links-backlink-into-drawer)
                                 (downcase org-super-links-backlink-into-drawer))
                            "backlinks")
                        ":"))
            ($global-todo-re
             . ,(org-node-worker--make-todo-regexp
                 (mapconcat #'identity
                            (mapcan #'cdr (default-toplevel-value
                                           'org-todo-keywords))
                            " ")))
            ($file-todo-option-re
             . ,(rx bol (or "#+todo: " "#+seq_todo: " "#+typ_todo: "))))))
    ;; Pre-compile code for the external Emacs processes to use,
    ;; in case the user's package manager didn't.
    (if native
        (unless (and (file-exists-p lib.eln)
                     (file-newer-than-file-p lib.eln lib))
          (native-compile lib))
      (unless (and (file-exists-p lib.elc)
                   (file-newer-than-file-p lib.elc lib))
        (byte-compile-file lib)))
    (setq org-node-async--start-time (current-time))
    (setq org-node-async--done-ctr 0)
    (with-temp-file (file-name-concat (temporary-file-directory)
                                      "org-node-worker-variables.eld")
      (insert (prin1-to-string variables)))
    (if org-node-perf-multicore
        ;; Split the work over many Emacs processes
        (let ((file-lists (org-node-async--split-into-n-sublists
                           files org-node-async--n-cores))
              ;; Perf attempts
              ;; (write-region-inhibit-fsync t)
              ;; (coding-system-for-write org-node-perf-assume-coding-system)
              ;; (write-file-hooks nil)
              ;; (file-name-handler-alist nil)
              )
          (dolist (old-process org-node-async--processes)
            (when (process-live-p old-process)
              (kill-process old-process)))
          (dotimes (i org-node-async--n-cores)
            (with-temp-file (format "/tmp/org-node-file-list-%d.eld" i)
              (insert (prin1-to-string (pop file-lists))))
            (push (make-process
                   :name (format "org-node-%d" i)
                   :noquery t
                   :stderr (get-buffer-create " *org-node-worker stderr*")
                   :command
                   (list (file-truename (expand-file-name invocation-name
                                                          invocation-directory))
                         "--quick"
                         "--no-init-file"
                         "--no-site-lisp"
                         "--batch"
                         "--insert"
                         (format "/tmp/org-node-file-list-%d.eld" i)
                         "--eval"
                         (format
                          "(setq files (cons %d (car (read-from-string (buffer-string)))))"
                          i)
                         "--load"
                         (if native
                             lib.eln
                           lib.elc)
                         "--funcall"
                         "org-node-worker-function")
                   :sentinel #'org-node-async-finish)
                  org-node-async--processes)))
      ;; TODO: execute in the current emacs process
      )))

(defvar org-node-async--processes (list))
(defcustom org-node-perf-multicore t
  ""
  :group 'org-node
  :type 'boolean)

;; (async-start (lambda () (sleep-for 1) (message "hi ho"))
;;              (lambda (res) (message "done %s" (random 100))))

(defvar org-node-async--done-ctr 0)
(defvar org-node-async--n-cores nil)

(defun org-node-async-finish (process _)
  (with-temp-buffer
    (insert-file-contents
     (format "/tmp/org-node-result-%s.eld"
             (substring (process-name process) -1)))
    (dolist (instruction (car (read-from-string (buffer-string))))
      (apply (car instruction) (cdr instruction))))
  (when (eq (cl-incf org-node-async--done-ctr)
            org-node-async--n-cores)
    ;; (org-id-update-id-locations)
    ;; (org-id-locations-save)
    ;; (org-node-async-reset)
    (message "Finished in %.2f s"
             (float-time (time-since org-node-async--start-time)))))

(provide 'org-node-async)

;;; org-node-async.el ends here
