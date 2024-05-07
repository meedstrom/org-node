;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

;; TODO: Balance the split file-lists a bit depending on file sizes?  Consult
;; the filesystem for the size: (nth 7 (file-attributes FILE))

(require 'bytecomp)
(require 'org-node-common)
(require 'org-node-worker)

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Instruct on-save hooks and such things to update the cache.
While the mode is active, commands such as `org-node-find' and
`org-node-insert-link' do not need to update the cache every
time."
  :global t
  :group 'org-node
  (remove-hook 'org-mode-hook #'org-node-cache-mode)
  ;; Take this opportunity to check for deprecated usage
  (when (or (string-search "plist-get" (prin1-to-string org-node-filter-fn))
            (string-search "plist-get" (prin1-to-string org-node-format-candidate-fn)))
    (display-warning 'org-node (string-fill "\n2024-04-30 breaking change: node metadata comes in objects now, not plists!  This change was made because plist-get fails silently, which makes debugging more difficult.  See updated examples for org-node-filter-fn etc." 79)))
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node-cache-rescan-file)
        (advice-add #'rename-file :after #'org-node-cache-rescan-file)
        (advice-add #'rename-file :before #'org-node-cache--handle-delete)
        (advice-add #'delete-file :before #'org-node-cache--handle-delete)
        (org-node-cache-ensure nil t t))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache--handle-delete)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

(defun org-node-cache-ensure (&optional synchronous force _polite-init)
  "Ensure that `org-id-locations' and `org-nodes' are fresh.

Initialize `org-id-locations' if it is not already, and ensure it
is a hash table \\(not an alist as it sometimes decides to be).
Make sure `org-node-cache-mode' is on.

Optional argument SYNCHRONOUS t means that if a reset is needed
or ongoing, to block Emacs until it is done.  When SYNCHRONOUS
nil, immediately return with a non-nil value if a reset is
underway, a nil value otherwise.

Optional argument FORCE means force a reset to happen (but let it
happen asynchronously unless SYNCHRONOUS is also set).  Will not
interrupt an already ongoing reset.

Usually when FORCE is nil, this function effectively no-ops, so
it is safe to put at the start of any command that uses org-node
stuff.

On a first-time run, force a synchronous reset no matter the
arguments.

\(fn &optional SYNCHRONOUS FORCE)"
  ;; First just make sure we've loaded org-id's stuff from disk.
  (when (null org-id-locations)
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load)))
  (when (listp org-id-locations)
    (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-id-locations-load)
    (when (hash-table-empty-p org-id-locations)
      (org-node-die "org-id-locations empty%s" org-node--standard-tip)))
  ;; Phew!  Hell of an API, org-id.  Now for the org-node stuff.
  (when (hash-table-empty-p org-nodes)
    ;; When the user runs an autoloaded command and that's the first-time init,
    ;; block Emacs until we've built a completion table.  Otherwise if we are
    ;; here because activating `org-node-cache-mode' got us here, do not block
    ;; Emacs.
    (setq synchronous (not _polite-init))
    (setq force t))
  ;; Mandate cache-mode.  Note if this was called from cache-mode itself, the
  ;; mode variable is already t, so this cannot result in an infinite loop.
  (when (not org-node-cache-mode)
    (org-node-cache-mode)
    (setq force t))
  (let ((live (-any-p #'process-live-p org-node-cache--processes)))
    (when force
      (unless live (org-node-cache--collect)))
    (when synchronous
      (message "org-node caching...")
      (mapc #'accept-process-output org-node-cache--processes))
    ;; Return non-nil if a sync is ongoing, nil if "ready"
    (when (or live force)
      org-node-cache--processes)))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-rescan-file (&optional _arg1 arg2 &rest _)
  "Seek nodes and links in a single file.
Either operate on ARG2 if it seems to be a file name, else the
current buffer file."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let ((file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                  arg2
                (if (and (stringp buffer-file-name)
                         (file-exists-p buffer-file-name))
                    buffer-file-name
                  nil))))
    (when file
      (when (equal (file-name-extension file) "org")
        (org-node-cache--collect (list file))
        (when (boundp 'org-node-cache-scan-file-hook)
          (lwarn 'org-node :warning
                 "Hook renamed: org-node-cache-scan-file-hook to org-node-cache-rescan-file-hook"))
        (run-hooks 'org-node-cache-rescan-file-hook)))))

(let ((timer (timer-create)))
  (defun org-node-cache--handle-delete (&optional arg1 &rest _)
    "Update org-id and org-node after an Org file is deleted.

Expected to run prior to deletion, and operate on ARG1 if it
seems to be a filename, else the current buffer file.

First remove any references to the file in `org-id-locations'.
Then schedule a cache reset for after a few idle seconds.  The
delay minimizes the risk of bothering the user who may be trying
to delete several files in a row."
    (let ((file-being-deleted
           (if (and arg1 (stringp arg1) (file-exists-p arg1))
               arg1
             (if (and (stringp buffer-file-name)
                      (file-exists-p buffer-file-name))
                 buffer-file-name
               nil))))
      (when file-being-deleted
        (when (member (file-name-extension file-being-deleted)
                      '("org" "org_archive" "gpg"))
          (org-node--forget-id-location file-being-deleted)
          (cancel-timer timer)
          (setq timer (run-with-idle-timer 6 nil #'org-node-cache-ensure nil t)))))))

(defun org-node-cache-peek ()
  "Print some random members of `org-nodes' that have IDs.
See also the type `org-node-data'."
  (interactive)
  (let ((id-nodes (hash-table-values org-nodes)))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length id-nodes)) id-nodes)))))

;; I feel like this could be easier to read...
(defun org-node-cache--add-node-to-tables (node-as-plist)
  "Add a node to `org-nodes' and other tables."
  (let* ((node (apply #'make-org-node-data node-as-plist))
         (id (org-node-get-id node)))
    ;; Add to `org-id-locations' too
    (puthash id (org-node-get-file-path node) org-id-locations)
    (puthash id node org-nodes)
    ;; Populate `org-node--refs-table'
    (dolist (ref (org-node-get-refs node))
      (puthash ref id org-node--refs-table))
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

(defun org-node-cache--add-link-to-tables (link-plist path type)
  "Record a link or reflink in the tables for those."
  (push link-plist (gethash path (if (equal type "id")
                                     org-node--links-table
                                   org-node--reflinks-table))))

(defvar org-node-cache--start-time nil
  "Timestamp used to measure time to rebuild the cache.")

(defun org-node-cache--collect (&optional files)
  (mkdir (org-node-worker--tmpfile) t)
  (with-current-buffer (get-buffer-create " *org-node*")
    (erase-buffer))
  (setq org-node-cache--jobs
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
         (elc (org-node-worker--tmpfile "worker.elc"))
         (targeted (not (null files))))
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

    (setq org-node-cache--start-time (current-time))
    (setq org-node-cache--done-ctr 0)
    (with-temp-file (org-node-worker--tmpfile "work-variables.eld")
      (insert
       (prin1-to-string
        ;; The purpose of $sigils is just visual, to distinguish these
        ;; variables in the body of `org-node-worker--collect'.
        (append
         org-node-inject-variables
         `(($targeted . ,targeted)
           ($keep-file-name-handlers . ,org-node-perf-keep-file-name-handlers)
           ($assume-coding-system . ,org-node-perf-assume-coding-system)
           ($link-re . ,org-link-plain-re)
           ($gc-cons-threshold
            . ,(or org-node-perf-gc-cons-threshold gc-cons-threshold))
           ($file-todo-option-re
            . ,(rx bol (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
           ($file-name-handler-alist
            . ,(--keep (rassoc it org-node-perf-keep-file-name-handlers)
                       file-name-handler-alist))
           ($global-todo-re
            . ,(org-node-worker--make-todo-regexp
                (string-join (mapcan #'cdr (default-value 'org-todo-keywords))
                             " ")))
           ($backlink-drawer-re
            . ,(concat "^[[:space:]]*:"
                       (or (and (fboundp 'org-super-links-link)
                                (require 'org-super-links)
                                (stringp org-super-links-backlink-into-drawer)
                                (downcase org-super-links-backlink-into-drawer))
                           "backlinks")
                       ":")))))))

    ;; NB: I considered keeping the processes alive to skip the spin-up
    ;; time, but the subprocesses report `emacs-init-time' as 0.001s.
    ;; There could be an unmeasured OS component though.  And maybe when
    ;; loading the library?  Worth a test in the future.  Now we just spin
    ;; up new ones every time.  Btw, how do we know these child processes
    ;; are loading the .eln of all their lisp?
    (while-let ((old-process (pop org-node-cache--processes)))
      (when (process-live-p old-process)
        (delete-process old-process)))

    ;; For debugging
    ;; (if single-threaded
    ;;     (with-temp-buffer
    ;;       (insert (or files (org-node-files)))
    ;;       (defvar $files (cons 0 (car (read-from-string (buffer-string)))))
    ;;       (org-node-worker--collect))
    ;;   )

    ;; Split the work over many Emacs processes
    (let ((print-length nil)
          (file-lists (org-node--split-into-n-sublists
                       (or files (org-node-files t))
                       org-node-cache--jobs)))
      ;; If user has e.g. 8 cores but only 5 files, spin up only 5 jobs
      (delq nil file-lists)
      (setq org-node-cache--jobs (min org-node-cache--jobs (length file-lists)))
      (dotimes (i org-node-cache--jobs)
        (delete-file (org-node-worker--tmpfile "demands-%d.eld" 2))
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
               :sentinel (lambda (process _event)
                           (org-node-cache--handle-finished-job
                            process i targeted)))
              org-node-cache--processes)))))

(defvar org-node-cache--processes (list))
(defvar org-node-cache--done-ctr 0)
(defvar org-node-cache--jobs nil)

(defun org-node-cache--handle-finished-job (process i targeted)
  (unless targeted
    (when (= 0 org-node-cache--done-ctr)
      ;; This used to be in `org-node-cache-ensure', wiping tables before
      ;; launching the processes, but that leads to a larger time window when
      ;; completions are unavailable.  Instead, we've waited until the first
      ;; process starts returning, and only now wipe the tables.
      (clrhash org-nodes)
      (clrhash org-node-collection)
      (clrhash org-node--refs-table)
      (clrhash org-node--reflinks-table)
      (clrhash org-node--links-table)))
  (with-temp-buffer
    ;; Paste what the worker output
    (let ((file (org-node-worker--tmpfile "demands-%d.eld" i)))
      (if (not (file-exists-p file))
          (progn
            (message "An org-node worker failed to scan files, not producing %s.  See buffer *org-node errors*."
                     file)
            (when (get-buffer "*org-node errors*")
              (kill-buffer "*org-node errors*"))
            (with-current-buffer (get-buffer-create " *org-node*")
              (rename-buffer "*org-node errors*")
              (insert (format-time-string "\nThis output printed at %T"))))
        (insert-file-contents file)
        ;; Execute the demands that the worker wrote
        (dolist (demand (car (read-from-string (buffer-string))))
          (apply (car demand) (cdr demand)))
        ;; Check if this was the last process to return, then wrap-up
        (when (eq (cl-incf org-node-cache--done-ctr) org-node-cache--jobs)
          ;; Print time elapsed.  Don't do it if this was a
          ;; $targeted run (operated on single file after a rename) as the
          ;; sums would be misleading.
          (when (not targeted)
            (let ((n-subtrees (cl-loop
                               for node being the hash-values of org-nodes
                               count (org-node-get-is-subtree node)))
                  (n-backlinks (length (apply #'append
                                              (hash-table-values
                                               org-node--links-table))))
                  (n-reflinks (length (apply #'append
                                             (hash-table-values
                                              org-node--reflinks-table)))))
              ;; (2024-05-06) NOTE In a few days, remove "w ID" (transitional)
              (message "org-node saw %d files, %d subtrees w ID, %d ID-links, %d potential reflinks in %.1fs"
                       (- (hash-table-count org-nodes) n-subtrees)
                       n-subtrees
                       n-backlinks
                       n-reflinks
                       (float-time (time-since org-node-cache--start-time)))))
          (org-id-locations-save))))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
