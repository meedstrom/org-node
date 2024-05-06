;;; org-node-async.el -*- lexical-binding: t; -*-

;; TODO: Balance the split file-lists a bit depending on file sizes?  Consult
;; the filesystem for the size: (nth 7 (file-attributes FILE))

(require 'bytecomp)
(require 'org-node-common)
(require 'org-node-worker)

(defvar org-node-async--start-time nil
  "Timestamp used to measure time to rebuild the cache.")

(defun org-node-async--split-into-n-sublists (big-list n)
  "Split BIG-LIST into a list of N sublists.

In the special case where BIG-LIST contains fewer than N
elements, the return value is still N items, where some are nil."
  (let ((len (/ (length big-list) n))
        res)
    (dotimes (i n)
      (push
       (if (= i (- n 1))
           ;; Let the last iteration just take what's left
           big-list
         (prog1 (take len big-list)
           (setq big-list (nthcdr len big-list))))
       res))
    res))

;; (org-node-async--split-into-n-sublists
;;       '(a v e e)
;;       7)

;; (org-node-async--split-into-n-sublists
;;       '(a v e e  q l fk k k ki i o r r  r r r r r r r r g g g  g g gg)
;;       4)

(defun org-node-async--collect (&optional files)
  (mkdir (org-node-worker--tmpfile) t)
  (with-current-buffer (get-buffer-create " *org-node*")
    (erase-buffer))
  (setq org-node-async--jobs
        (max 1 (1- (string-to-number
                    (pcase system-type
                      ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
                       (if (executable-find "nproc")
                           (shell-command-to-string "nproc --all")
                         (shell-command-to-string
                          "lscpu -p | egrep -v '^#' | wc -l")))
                      ((or 'darwin)
                       (shell-command-to-string "sysctl -n hw.logicalcpu_max"))
                      ;; I have no idea if this works
                      ((or 'cygwin 'windows-nt 'ms-dos)
                       (ignore-errors
                         (with-temp-buffer
                           (call-process "echo" nil t nil
                                         "%NUMBER_OF_PROCESSORS%")
                           (buffer-string)))))))))
  (let* ((lib (find-library-name "org-node-worker"))
         (native (when (and (featurep 'native-compile)
                            (native-comp-available-p))
                   (comp-el-to-eln-filename lib)))
         (native-comp-speed 3)
         (elc (org-node-worker--tmpfile "worker.elc")))
    ;; Pre-compile the worker, if the user's package manager didn't compile it
    ;; already, or if development is happening in org-node-worker.el.
    (if native
        (unless (and (file-exists-p native)
                     (file-newer-than-file-p native lib))
          (native-compile lib))
      (unless (and (file-exists-p elc)
                   (file-newer-than-file-p elc lib))
        ;; If we obey `byte-compile-dest-file-function', it's hard to predict
        ;; that the .elc won't end up cluttering some source directory, so just
        ;; force it into /tmp.
        (let ((byte-compile-dest-file-function
               `(lambda (&rest _) ,elc)))
          (byte-compile-file lib))))


    (setq org-node-async--start-time (current-time))
    (setq org-node-async--done-ctr 0)
    (with-temp-file (org-node-worker--tmpfile "work-variables.eld")
      (insert (prin1-to-string
               (append (org-node--work-variables)
                       '(($not-a-full-reset . ,(not (null files))))
                       org-node-async-inject-variables))))
    (setq files (or files (org-node-files)))

    ;; NB: I considered keeping the processes alive to skip the spin-up
    ;; time, but the subprocesses report `emacs-init-time' as 0.001s.
    ;; There could be an unmeasured OS component though.  And maybe when
    ;; loading the library?  Worth a test in the future.  Now we just spin
    ;; up new ones every time.  Btw, how do we know these child processes
    ;; are loading the .eln of all their lisp?
    (while-let ((old-process (pop org-node-async--processes)))
      (when (process-live-p old-process)
        (delete-process old-process)))

    ;; Split the work over many Emacs processes
    (let ((file-lists (org-node-async--split-into-n-sublists
                       files org-node-async--jobs))
          (print-length nil))
      ;; If user has e.g. 8 cores but only 5 files, spin up only 5 jobs
      (delq nil file-lists)
      (setq org-node-async--jobs (min org-node-async--jobs (length file-lists)))
      (dotimes (i org-node-async--jobs)
        (delete-file (org-node-worker--tmpfile "result-%d.eld" 2))
        (with-temp-file (org-node-worker--tmpfile "file-list-%d.eld" i)
          (insert (prin1-to-string (pop file-lists))))
        (push (make-process
               :name (format "org-node-%d" i)
               :noquery t
               :stderr (get-buffer-create " *org-node*")
               :command (list (file-truename
                               (expand-file-name invocation-name
                                                 invocation-directory))
                              "--quick"
                              "--no-init-file"
                              "--no-site-lisp"
                              "--batch"
                              "--insert"
                              (org-node-worker--tmpfile "file-list-%d.eld" i)
                              "--eval"
                              (format
                               "(setq $files (cons %d (car (read-from-string (buffer-string)))))"
                               i)
                              "--load"
                              (or native elc)
                              "--funcall"
                              "org-node-worker--collect")
               :sentinel (lambda (process event)
                           (org-node-async--handle-finished-job
                            process event i)))
              org-node-async--processes)))))

(defvar org-node-async--processes (list))
(defvar org-node-async--done-ctr 0)
(defvar org-node-async--jobs nil)

(defun org-node-async--handle-finished-job (process _ i)
  (when (= 0 org-node-async--done-ctr)
    ;; This used to be in `org-node-cache-reset', wiping tables before
    ;; launching the processes, but that leads to a larger time window when
    ;; completions are unavailable.  Instead, wait until the first process
    ;; starts returning, then wipe the tables.
    (clrhash org-nodes)
    (clrhash org-node-collection)
    (clrhash org-node--refs-table)
    (clrhash org-node--reflinks-table)
    (clrhash org-node--links-table))
  (with-temp-buffer
    ;; Paste what the worker output
    (let ((file (org-node-worker--tmpfile "result-%d.eld" i)))
      (if (not (file-exists-p file))
          (progn
            (message "An org-node worker failed to scan files, not producing %s.  See buffer *org-node errors*."
                     file)
            (when (get-buffer "*org-node errors*")
              (kill-buffer "*org-node errors*"))
            (with-current-buffer (get-buffer-create " *org-node*")
              (rename-buffer "*org-node errors*")))
        (insert-file-contents file)
        ;; Execute the demands that the worker wrote
        (let ((please-rescan nil))
          (dolist (demand (car (read-from-string (buffer-string))))
            (apply (car demand) (cdr demand))
            (when (eq 'org-node--forget-id-location (car demand))
              (setq please-rescan t)))
          ;; The last process has completed
          (when (eq (cl-incf org-node-async--done-ctr) org-node-async--jobs)
            (when please-rescan
              (org-node-async--collect (org-node-files)))
            ;; Print time elapsed.  Don't do it if cache mode is off because
            ;; that'd be spammy as it resets all the time.
            (when (bound-and-true-p org-node-cache-mode)
              (let ((n-subtrees
                     (cl-loop for node being the hash-values of org-nodes
                              count (org-node-get-is-subtree node))))
                (message
                 "org-node: found %d files, %d subtrees and %d links in %.2fs"
                 (- (hash-table-count org-nodes) n-subtrees)
                 n-subtrees
                 (+ (length (apply #'append
                                   (hash-table-values org-node--links-table)))
                    (length (apply #'append
                                   (hash-table-values org-node--reflinks-table))))
                 (float-time (time-since org-node-async--start-time)))))))))))

(provide 'org-node-async)

;;; org-node-async.el ends here
