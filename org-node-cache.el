;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

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
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node-cache-rescan-file)
        (advice-add #'rename-file :after #'org-node-cache-rescan-file)
        (advice-add #'delete-file :before #'org-node-cache--handle-delete)
        (org-node-cache-ensure nil t t))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
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
  ;; Mandate `org-node-cache-mode'.  Note if this was called from the
  ;; initialization of `org-node-cache-mode' itself, the variable is already t,
  ;; so this cannot result in an infinite loop.
  (when (not org-node-cache-mode)
    (org-node-cache-mode)
    (setq force t))
  (let ((live (-any-p #'process-live-p org-node-cache--processes)))
    (when force
      ;; Launch the async processes
      (unless live (org-node-cache--collect)))
    (when synchronous
      ;; Block until all processes finish
      (message "org-node caching...")
      (mapc #'accept-process-output org-node-cache--processes))
    ;; Return non-nil if a sync is ongoing, nil if "ready"
    (when (or live force)
      org-node-cache--processes)))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-rescan-file (&optional arg1 arg2 &rest _)
  "Scan nodes and links in a single file.
Either operate on ARG2 if it seems to be a file name, else the
current buffer file.

Meant as an advice for `rename-file'.  To manually scan a file
from Lisp, use `org-node-cache--collect' instead."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let* ((file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                   arg2
                 (if (and (stringp buffer-file-name)
                          (file-exists-p buffer-file-name))
                     buffer-file-name
                   nil)))
         (oldname (if arg2 arg1)))
    (when file
      (when (equal (file-name-extension file) "org")
        (org-node-cache--collect (list file))
        (when oldname
          (org-node-cache--handle-delete oldname))
        (when (boundp 'org-node-cache-scan-file-hook)
          (lwarn 'org-node :warning
                 "Hook renamed: org-node-cache-scan-file-hook to org-node-cache-rescan-file-hook"))
        (run-hooks 'org-node-cache-rescan-file-hook)))))

(let ((timer (timer-create)))
  (defun org-node-cache--handle-delete (&optional file-being-deleted &rest _)
    "Update org-id and org-node when an Org file is deleted.

Expected to run prior to deletion, as a :before advice on
`delete-file'.

First remove any references to the file in `org-id-locations'.
Then schedule the cache to reset after a few idle seconds.  The
delay minimizes the risk of bothering the user who may be in the
proess of deleting several files in a row."
    ;; (when (eq (current-buffer) (window-buffer (selected-window)))
    (when file-being-deleted
      (when (--any-p (string-suffix-p it file-being-deleted)
                     '(".org" ".org_archive" ".org.gpg"))
        (message "Forgetting the deleted file... (%s)" file-being-deleted)
        (org-node--forget-id-location file-being-deleted)
        (cancel-timer timer)
        (setq timer (run-with-idle-timer 6 nil #'org-node-cache-ensure nil t))))))

(defun org-node-cache-peek ()
  "Print some random members of `org-nodes' that have IDs.
For reference, see type `org-node-data'."
  (interactive)
  (let ((id-nodes (hash-table-values org-nodes)))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length id-nodes)) id-nodes)))))

;; I feel like this could be easier to read...
(defun org-node-cache--add-node-to-tables (node-as-plist)
  "Add a node to `org-nodes' and other tables.
The input NODE-AS-PLIST is simply a list of arguments to give to
`make-org-node-data'.

If you wish to scan a file for nodes, use the higher-level
function `org-node-cache--collect'."
  (let* ((node (apply #'make-org-node-data node-as-plist))
         (id (org-node-get-id node)))
    ;; Tell org-id about the node too
    (org-id-add-location id (org-node-get-file-path node)) ;; REVIEW perf
    ;; (puthash id (org-node-get-file-path node) org-id-locations)
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
  "Record a link or reflink into the appropriate table.
For demonstration of LINK-PLIST, PATH and TYPE, read the source
of `org-node-worker--collect-links-until'."
  (push link-plist (gethash path
                            (if (equal type "id")
                                org-node--links-table
                              org-node--reflinks-table))))

(defvar org-node-cache--start-time nil
  "Timestamp used to measure time to rebuild the cache.")

(let (queue
      (timer (timer-create)))
  (defun org-node-cache--collect (&optional files)
    "Thin wrapper for `org-node-cache--collect*'.
Like `org-node-cache--collect*', but try to queue multiple calls
that occur in a very short time, like when multiple files are
being renamed at once."
    (setq queue (append files queue))
    ;; Let's say there are 10 calls about to happen simultaneously, but we
    ;; don't know the future.  The first call will execute normally, and the
    ;; next 9 calls will see that it is busy and wait, so the second run will
    ;; scan 9 files.  Weird and could definitely be polished, but good enough.
    ;; The important thing is that nothing is dropped.
    (if (-any-p #'process-live-p org-node-cache--processes)
        (progn
          (cancel-timer timer)
          (setq timer (run-with-timer 1 nil #'org-node-cache--collect queue)))
      (let ((queue* (-clone queue)))
        (setq queue nil)
        (org-node-cache--collect* queue*)))))

(defun org-node-cache--collect* (&optional files)
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
    ;; already, or if local changes have been made to org-node-worker.el
    (if native
        (unless (file-newer-than-file-p native lib)
          (native-compile lib))
      (unless (file-newer-than-file-p elc lib)
        ;; Don't obey `byte-compile-dest-file-function', because it's hard to
        ;; predict that the .elc won't end up cluttering some source directory
        (let ((byte-compile-dest-file-function `(lambda (&rest _) ,elc)))
          (byte-compile-file lib))))

    (setq org-node-cache--start-time (current-time))
    (setq org-node-cache--done-ctr 0)
    (with-temp-file (org-node-worker--tmpfile "work-variables.eld")
      (insert
       (prin1-to-string
        ;; The purpose of $sigils is just visual, to distinguish these
        ;; "external" variables in the body of `org-node-worker--collect'.
        (append
         org-node-inject-variables
         `(($targeted . ,targeted)
           ($keep-file-name-handlers . ,org-node-perf-keep-file-name-handlers)
           ($assume-coding-system . ,org-node-perf-assume-coding-system)
           ($link-re . ,org-link-plain-re)
           ($gc-cons-threshold
            . ,(or org-node-perf-gc-cons-threshold gc-cons-threshold))
           ($file-option-todo-re
            . ,(rx bol (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
           ($file-name-handler-alist
            . ,(--keep (rassoc it org-node-perf-keep-file-name-handlers)
                       file-name-handler-alist))
           ($global-todo-re
            . ,(org-node-worker--make-todo-regexp
                (string-join (-mapcat #'cdr (default-value 'org-todo-keywords))
                             " ")))
           ($backlink-drawer-re
            . ,(concat "^[[:space:]]*:"
                       (or (and (fboundp 'org-super-links-link)
                                (require 'org-super-links)
                                (stringp org-super-links-backlink-into-drawer)
                                org-super-links-backlink-into-drawer)
                           "backlinks")
                       ":")))))))

    ;; NB: I considered keeping the processes alive to skip the spin-up time,
    ;; but the subprocesses report `emacs-init-time' as 0.001s.  There could be
    ;; an unmeasured OS component though.  And maybe there's delay loading the
    ;; worker.eln? https://nullprogram.com/blog/2018/02/22/
    ;;
    ;; Worth a test in the future.  Now we just start new processes every time.
    ;; REVIEW Can we ensure these child processes are loading the .eln variant
    ;;        of all the emacs core lisp?
    (while-let ((old-process (pop org-node-cache--processes)))
      (when (process-live-p old-process)
        (delete-process old-process)))

    ;; If debugging, run single-threaded so we can step through
    ;; org-node-worker.el with edebug
    (if org-node--dbg
        (progn
          (with-temp-file (org-node-worker--tmpfile "file-list-0.eld")
            (insert (prin1-to-string (org-node-files))))
          (delete-file (org-node-worker--tmpfile "demands-0.eld"))
          (setq org-node-worker--demands nil)
          (setq i 0)
          (when editorconfig-mode
            (message "Maybe disable editorconfig-mode while debugging"))
          (org-node-worker--collect)
          (setq org-node-cache--jobs 1)
          (org-node-cache--handle-finished-job 0 nil))

      ;; If not debugging, split the work over many Emacs processes
      ;; TODO: Balance the split file-lists a bit depending on file sizes?  To
      ;;       get the size: (nth 7 (file-attributes FILE)). It's not uncommon
      ;;       for one process to take noticeably longer because there's a
      ;;       mega-file among them.
      (let ((print-length nil)
            (file-lists (org-node--split-into-n-sublists
                         (or files (org-node-files t))
                         org-node-cache--jobs)))
        (delq nil file-lists)
        ;; If user has e.g. 8 cores but only 5 files, run only 5 jobs
        (setq org-node-cache--jobs
              (min org-node-cache--jobs (length file-lists)))
        (dotimes (i org-node-cache--jobs)
          (delete-file (org-node-worker--tmpfile "demands-%d.eld" i))
          (with-temp-file (org-node-worker--tmpfile "file-list-%d.eld" i)
            (insert (prin1-to-string (pop file-lists))))
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create " *org-node*")
                 :command (list (file-truename
                                 ;; True path to Emacs binary
                                 (expand-file-name invocation-name
                                                   invocation-directory))
                                "--quick"
                                "--no-init-file"
                                "--no-site-lisp"
                                "--batch"
                                "--eval" (format "(setq i %d)" i)
                                "--load" (or native elc)
                                "--funcall" "org-node-worker--collect")
                 :sentinel (lambda (_process _event)
                             (org-node-cache--handle-finished-job i targeted)))
                org-node-cache--processes))))))

(defvar org-node-cache--processes (list))
(defvar org-node-cache--done-ctr 0)
(defvar org-node-cache--jobs nil)

(defun org-node-cache--handle-finished-job (i targeted)
  (unless targeted
    (when (= 0 org-node-cache--done-ctr)
      ;; This used to be in `org-node-cache-ensure', wiping tables before
      ;; launching the processes, but that leads to a larger time window when
      ;; completions are unavailable.  Instead, we've waited until the first
      ;; process starts returning, and only now wipe the tables.
      ;;
      ;; TODO: Should wait until the last process finishes (refactor to collect
      ;;       their return values all in one go)
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
          (when (eq 'org-node--forget-id-location (car demand))
            (message "Forgetting nonexistent file... %s" (cdr demand)))
          (apply (car demand) (cdr demand)))
        ;; Check if this was the last process to return, then wrap-up
        (when (eq (cl-incf org-node-cache--done-ctr) org-node-cache--jobs)
          ;; Print time elapsed.  Don't do it if this was a $targeted run
          ;; (i.e. operated on single file after a rename) as the sums would be
          ;; misleading.
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
              (message "org-node saw %d files, %d subtrees, %d ID-links, %d potential reflinks in %.2fs"
                       (- (hash-table-count org-nodes) n-subtrees)
                       n-subtrees
                       n-backlinks
                       n-reflinks
                       (float-time (time-since org-node-cache--start-time)))))
          (org-id-locations-save))))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
