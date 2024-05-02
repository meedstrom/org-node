;;; org-node-async.el --- The beating heart -*- lexical-binding: t; -*-

(require 'async)
(require 'org-node-lib)

(defvar org-node-async-reset-hook nil)
(defvar org-node-async-rescan-file-hook nil)
(defvar org-node-async-start-time nil)

(defun org-node-async-reset ()
  "Wipe and rebuild the cache.
For an user-facing command, see \\[org-node-reset]."
  (setq org-node-async-start-time (current-time))
  (clrhash org-nodes)
  (clrhash org-node-collection)
  (clrhash org-node--refs-table)
  (clrhash org-node--reflinks-table)
  (clrhash org-node--links-table)
  (org-node--init-org-id-locations-or-die)
  (org-node-async--collect (-uniq (hash-table-values org-id-locations)))
  (message nil))



;; I feel like this could be easier to read...
(defun org-node-async--add-node-to-tables (node)
  "Add NODE to `org-nodes' and maybe `org-node-collection'."
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
        (puthash ref node org-node-collection)))))

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
           big-list
         (prog1 (take len big-list)
           (setq big-list (nthcdr len big-list))))
       res))
    res))

;; (org-node-async--split-into-n-sublists
;;       '(a v e e  q l fk k k ki i o r r  r r r r r r r r g g g  g g gg)
;;       4)

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
                       ""))))))
  (let* ((native (and (featurep 'native-compile)
                      (native-comp-available-p)))
         (lib (find-library-name "org-node-worker"))
         (lib.eln (comp-el-to-eln-filename lib))
         (file-lists (org-node-async--split-into-n-sublists
                      files org-node-async--n-cores)))
    ;; Ensure the async processes can run a precompiled artifact
    (if native 
        (unless (and (file-exists-p lib.eln)
                     (file-newer-than-file-p lib.eln lib))
          (native-compile lib))
      (byte-recompile-file lib))
    (setq org-node-async--done-ctr 0)
    (dotimes (_ org-node-async--n-cores)
      (async-start
       `(lambda ()
          (condition-case _
              (if ,native
                  (native-elisp-load ,lib.eln)
                (require 'bytecomp) ;; not sure the require is needed
                (load ,(byte-compile-dest-file lib)))
            ;; Fail gracefully even if it means uncompiled lisp
            (( error )
             (load ,lib)))
          (setq org-node-worker--input
                '((backlink-drawer-re
                   . ,(concat "^[[:space:]]*:"
                              (or (and (boundp 'org-super-links-backlink-into-drawer)
                                       (stringp org-super-links-backlink-into-drawer)
                                       (downcase org-super-links-backlink-into-drawer))
                                  "backlinks")
                              ":"))
                  (global-todo-keywords
                   . ,(default-toplevel-value 'org-todo-keywords))
                  (bungle-file-name-handler . ,org-node-perf-bungle-file-name-handler)
                  (assume-coding-system . ,org-node-perf-assume-coding-system)
                  (gc-cons-threshold . ,org-node-perf-gc-cons-threshold)
                  (not-a-full-reset . ,(not (hash-table-empty-p org-nodes)))
                  (link-bracket-re . ,org-link-bracket-re)
                  (link-plain-re . ,org-link-plain-re)
                  (files . ,(pop file-lists))))
          (org-node-worker-function))
       #'org-node-async--callback))))

;; (async-start (lambda () (sleep-for 1) (message "hi ho"))
;;              (lambda (res) (message "done %s" (random 100))))


(defvar org-node-async--done-ctr 0)
(defvar org-node-async--n-cores nil)

(defun org-node-async--callback (result)
  (when (eq 'done result)
    (message "Received data, setting variables in main Emacs process"))
  (let ((plz-update-id-locations nil))
    (when-let ((add (plist-get result :add)))
      (org-id-add-location (car add) (cdr add)))

    (when-let ((forget (plist-get result :add)))
      (org-node-async--forget-id-location forget)
      (setq plz-update-id-locations t))

    (when-let ((node (plist-get result :node)))
      (org-node-async--add-node-to-tables
       (apply #'make-org-node-data node)))

    (when-let ((link (plist-get result :link))
               (path (plist-get result :path))
               (type (plist-get result :type)))
      (push link (gethash path (if (equal type "id")
                                   org-node--links-table
                                 org-node--reflinks-table))))
    (when (eq 'done result)
      ;; All done
      (when (eq (cl-incf org-node-async--done-ctr)
                org-node-async--n-cores)
        (when plz-update-id-locations
          (org-id-update-id-locations)
          (org-id-locations-save)
          ;; (org-node-async-reset)
          (message "Finished by T+%.2f s"
                   (float-time (time-since org-node-async-start-time))))))))

(provide 'org-node-async)

;;; org-node-async.el ends here
