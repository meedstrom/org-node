;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

;; TODO Test perf of keeping alive the child processes to skip spin-up
;; TODO Maybe set gc-cons-threshold in child processes to most-positive-fixnum
;; REVIEW Ensure that the child processes are loading the .eln variant of all
;;        the emacs core lisp

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
        (org-node-cache-ensure 'async t))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that `org-id-locations' and `org-nodes' are fresh.

Initialize `org-id-locations' if it is not already, and ensure it
is a hash table (not an alist, as it sometimes decides to be).
Make sure `org-node-cache-mode' is enabled.

Optional argument SYNCHRONOUS t means that if a reset is needed
or ongoing, to block Emacs until it is done.  When the argument
is nil or the symbol `async', return immediately.

Optional argument FORCE t means force a reset to happen (but let
it happen asynchronously unless SYNCHRONOUS is also set).  Will
not interrupt an already ongoing reset.

Usually when FORCE is nil, this function effectively no-ops, so
it is safe to put at the start of any user command.  It only
really interferes with user experience if SYNCHRONOUS t and it
takes your computer long to finish \\[org-node-reset]."
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
    (setq synchronous (if (eq synchronous 'async) nil t))
    (setq force t))
  (when (eq synchronous 'async)
    (setq synchronous nil))
  ;; Mandate `org-node-cache-mode'.  Note if this was called from the
  ;; initialization of `org-node-cache-mode' itself, the variable is already t,
  ;; so this cannot result in an infinite loop.
  (when (not org-node-cache-mode)
    (org-node-cache-mode)
    (setq force t))
  (let ((live (-any-p #'process-live-p org-node-cache--processes)))
    (when force
      ;; Launch the async processes
      (unless live (org-node-cache--scan)))
    (when synchronous
      ;; Block until all processes finish
      (message "org-node caching...")
      (mapc #'accept-process-output org-node-cache--processes))))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-rescan-file (&optional arg1 arg2 &rest _)
  "Scan nodes and links in a single file.
Either operate on ARG2 if it seems to be a file name, else the
current buffer file.

Meant for `after-save-hook' or `rename-file' advice.  To manually
scan a file from Lisp, use `org-node-cache--scan' instead."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let ((file (if (and (stringp arg2) (file-exists-p arg2))
                  arg2
                (if (and (stringp buffer-file-name)
                         (file-exists-p buffer-file-name))
                    buffer-file-name
                  nil)))
        (oldname (if arg2 arg1)))
    (when file
      (when (equal (file-name-extension file) "org")
        (org-node-cache--scan (list file))
        (when oldname
          (org-node-cache--handle-delete oldname))
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
        (setq timer (run-with-idle-timer 4 nil #'org-node-cache-ensure nil t))))))

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
function `org-node-cache--scan'."
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
  (push link-plist (gethash path (if (equal type "id")
                                     org-node--links-table
                                   org-node--reflinks-table))))


(defvar org-node-cache--processes nil
  "List of subprocesses.")
(defvar org-node-cache--done-ctr 0
  "Count of finished subprocesses.")
(defvar org-node-cache--stderr " *org-node*"
  "Name of buffer for the subprocesses stderr.")
(defvar org-node-cache--start-time nil
  "Timestamp used to measure time it took to rebuild cache.")

(let ((queue nil)
      (timer (timer-create)))
  (defun org-node-cache--scan (&optional files)
    "Thin wrapper for `org-node-cache--scan*'.
Like `org-node-cache--scan*', but try to bundle multiple calls
that occur in a short time, like when multiple files are
being renamed at once."
    ;; Weird algorithm, but the important thing is that no calls are dropped.
    (setq queue (append files queue))
    (if (-any-p #'process-live-p org-node-cache--processes)
        ;; Retry soon
        (progn
          (cancel-timer timer)
          (setq timer (run-with-timer 1 nil #'org-node-cache--scan queue)))
      ;; Scan now
      (let ((queue* (-clone queue)))
        (setq queue nil)
        (org-node-cache--scan* queue*)))))

(defun org-node-cache--scan* (&optional files)
  (mkdir (org-node-worker--tmpfile) t)
  (let* ((lib (find-library-name "org-node-worker"))
         (eln-path (and (featurep 'native-compile)
                        (native-comp-available-p)
                        (comp-el-to-eln-filename lib)))
         (elc-path (org-node-worker--tmpfile "worker.elc"))
         (targeted (not (null files))))
    ;; Pre-compile org-node-worker.el, in case the user's package manager
    ;; didn't compile it already, or if local changes have been made
    (if eln-path
        (unless (file-newer-than-file-p eln-path lib)
          (native-compile lib))
      ;; No native-comp, make an .elc
      (unless (file-newer-than-file-p elc-path lib)
        ;; Hardcode the location so it won't clutter some source directory
        (let ((byte-compile-dest-file-function `(lambda (&rest _) ,elc-path)))
          (byte-compile-file lib))))

    ;; Prep for new scan
    (setq org-node-cache--start-time (current-time))
    (setq org-node-cache--done-ctr 0)
    (with-current-buffer (get-buffer-create org-node-cache--stderr)
      (erase-buffer))
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
    ;; but the subprocesses report `emacs-init-time' as 0.001s.
    (while-let ((old-process (pop org-node-cache--processes)))
      (when (process-live-p old-process)
        (delete-process old-process)))

    (if org-node--dbg
        ;; Special case: if debugging, run single-threaded so we can step
        ;; through the org-node-worker.el functions with edebug
        (progn
          (with-temp-file (org-node-worker--tmpfile "file-list-0.eld")
            (insert (prin1-to-string (org-node-files))))
          (delete-file (org-node-worker--tmpfile "demands-0.eld"))
          (delete-file (org-node-worker--tmpfile "errors-0.eld"))
          (setq org-node-worker--demands nil)
          (setq i 0)
          (when editorconfig-mode
            (message "Maybe disable editorconfig-mode while debugging"))
          (pop-to-buffer (get-buffer-create "*org-node debug*"))
          (erase-buffer)
          (org-node-worker--collect)
          (org-node-cache--handle-finished-job targeted 1))

      ;; If not debugging, split the work over many Emacs processes
      (let* ((print-length nil)
             (print-level nil)
             (file-lists (org-node-cache--split-file-list
                          (or files (org-node-files))
                          (org-node-cache--count-logical-cores)))
             (n-jobs (length file-lists)))
        (dotimes (i n-jobs)
          (delete-file (org-node-worker--tmpfile "demands-%d.eld" i))
          (delete-file (org-node-worker--tmpfile "errors-%d.eld" i))
          (with-temp-file (org-node-worker--tmpfile "file-list-%d.eld" i)
            (insert (prin1-to-string (pop file-lists))))
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create org-node-cache--stderr)
                 :command (list (file-truename
                                 ;; True path to Emacs binary
                                 (expand-file-name invocation-name
                                                   invocation-directory))
                                "--quick"
                                "--batch"
                                "--eval" (format "(setq temporary-file-directory \"%s\")"
                                                 temporary-file-directory)
                                "--eval" (format "(setq i %d)" i)
                                "--load" (or eln-path elc-path)
                                "--funcall" "org-node-worker--collect")
                 :sentinel (lambda (_process _event)
                             (org-node-cache--handle-finished-job
                              targeted n-jobs)))
                org-node-cache--processes))))))

(defun org-node-cache--handle-finished-job (targeted n-jobs)
  "Check if this was the last process to return, then wrap-up."
  (with-current-buffer (get-buffer-create org-node-cache--stderr)
    (goto-char (point-min))
    (insert (format-time-string "\n%T.%3N")))
  (when (eq (cl-incf org-node-cache--done-ctr) n-jobs)
    (unless targeted
      (clrhash org-nodes)
      (clrhash org-node-collection)
      (clrhash org-node--refs-table)
      (clrhash org-node--reflinks-table)
      (clrhash org-node--links-table))
    (dotimes (i n-jobs)
      (let ((demands-file (org-node-worker--tmpfile "demands-%d.eld" i))
            (err-file (org-node-worker--tmpfile "errors-%d.eld" i)))
        (when (file-exists-p err-file)
          (message "org-node: problems scanning some files, see %s" err-file))
        (if (not (file-exists-p demands-file))
            (progn
              ;; Had 1+ errors, so unhide the stderr buffer from now on
              (let ((buf (get-buffer " *org-node*")))
                (when buf
                  (setq org-node-cache--stderr "*org-node errors*")
                  (with-current-buffer buf
                    (rename-buffer org-node-cache--stderr))))
              (message "An org-node worker failed to scan files, not producing %s.  See buffer %s"
                       demands-file org-node-cache--stderr))
          ;; Execute the demands that the worker wrote
          (with-temp-buffer
            (insert-file-contents demands-file)
            (dolist (demand (car (read-from-string (buffer-string))))
              (when (eq 'org-node--forget-id-location (car demand))
                ;; Special case
                (message "Forgetting nonexistent file... %s" (cdr demand)))
              (apply (car demand) (cdr demand)))))))
    (unless targeted
      (org-node-cache--print-elapsed))
    (org-id-locations-save)))

(defun org-node-cache--print-elapsed ()
  "Print time elapsed since `org-node-cache--start-time'.
Also report statistics about the content of `org-nodes'."
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

(defun org-node-cache--split-file-list (files n)
  "Split FILES into a list of N lists of files.

Take their file-sizes into account, so that a third of those N
lists are short lists of big files, while the other two-thirds
are long lists of small files.  The single biggest file will tend
to be isolated into a list of one member.

Since each sublist is meant to be given to one child process,
this balancing reduces the risk that one process takes noticably
longer due to being saddled with a mega-file in addition to the
average workload."
  (let ((max-reserved-cores (/ n 3))
        (biggest 0)
        big-files small-files lists-of-big-files lists-of-small-files)
    (setq n (- n max-reserved-cores))
    (if (= 0 max-reserved-cores)
        (setq small-files files)
      ;; Construct a little list BIG-FILES which will definitely have the
      ;; biggest files as members on top, and some stragglers at the bottom
      (dolist (file files)
        (let ((size (file-attribute-size (file-attributes file))))
          (if (> biggest size)
              (push file small-files)
            (setq biggest size)
            (push (cons size file) big-files))))
      ;; Reserve some cores for scanning the biggest files
      (while (and big-files (> max-reserved-cores (length lists-of-big-files)))
        (let (sublist)
          (while (and big-files
                      (>= biggest
                          (apply #'+ (caar big-files) (mapcar #'car sublist))))
            (push (pop big-files) sublist))
          (push (mapcar #'cdr sublist) lists-of-big-files))))
    ;; Make lists of the rest
    (let ((len (/ (length small-files) n)))
      (dotimes (i n)
        (let ((sublist (if (= i (- n 1))
                           ;; Let the last iteration just take what's left
                           small-files
                         (prog1 (take len small-files)
                           (setq small-files (-drop len small-files))))))
          (when sublist (push sublist lists-of-small-files))))
      (append lists-of-big-files
              lists-of-small-files))))

(defun org-node-cache--count-logical-cores ()
  "Count the available processor cores, minus 1."
  (max 1 (1- (string-to-number
              (pcase system-type
                ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
                 (if (executable-find "nproc")
                     (shell-command-to-string "nproc --all")
                   (shell-command-to-string
                    "lscpu -p | egrep -v '^#' | wc -l")))
                ((or 'darwin)
                 (shell-command-to-string "sysctl -n hw.logicalcpu_max"))
                ;; No idea if this works
                ((or 'cygwin 'windows-nt 'ms-dos)
                 (ignore-errors
                   (with-temp-buffer
                     (call-process "echo" nil t nil
                                   "%NUMBER_OF_PROCESSORS%")
                     (buffer-string)))))))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
