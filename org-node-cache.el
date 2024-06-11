;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

;; TODO Test perf of keeping alive the child processes to skip spin-up
;;
;; REVIEW Ensure that the child processes are loading the .eln variant of all
;;        the emacs core lisp

(require 'bytecomp)
(require 'org-node-common)
(require 'org-node-worker)

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Instruct on-save hooks and such things to update the cache.

Without this mode active, commands such as `org-node-find' may
present out-of-date completions, and `org-node-backlink-mode' may
delete too many backlinks on cleanup."
  :global t
  :group 'org-node
  (remove-hook 'org-mode-hook #'org-node-cache-mode)
  (cancel-timer org-node-cache--timer)
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node-cache--rescan-file-a)
        (advice-add #'rename-file :after #'org-node-cache--rescan-file-a)
        (advice-add #'delete-file :after #'org-node-cache--handle-delete-a)
        (org-node-cache-ensure 'must-async t)
        (setq org-node-cache--timer
              (run-with-idle-timer 60 t #'org-node-cache--scan-new-or-modified))
        (when (boundp 'org-node-cache-reset-hook)
          (display-warning 'org-node "Deprecated 2024-06-09: org-node-cache-reset-hook removed, update your initfiles")))
    (remove-hook 'after-save-hook #'org-node-cache--rescan-file-a)
    (advice-remove #'rename-file #'org-node-cache--rescan-file-a)
    (advice-remove #'delete-file #'org-node-cache--handle-delete-a)))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that `org-nodes' and other tables are ready for use.
Specifically, do the following:

- Initialize `org-id-locations' if it is not already.
- Ensure `org-id-locations' is a hash table and not an alist.
- (Re-)build the cache if it is empty or if FORCE t.

Optional argument SYNCHRONOUS t means that if a cache build is
requested or already ongoing, block Emacs until it is done.

When SYNCHRONOUS is nil, return immediately and let the caching
proceed in the background.  As that may take a few seconds, that
would mean that the `org-nodes' table is probably still outdated
by the time you query it, but that is acceptable in many
situations such as in an user command - the next invocation of
that command will be up to date.

If the `org-nodes' table is currently empty, behave as if
SYNCHRONOUS t, unless SYNCHRONOUS is the symbol `must-async'.

Usually when both arguments are nil, this function effectively
no-ops, so it is cheap to put at the start of any user command.
It can only interfere with user experience if SYNCHRONOUS t and
it takes your computer long to finish \\[org-node-reset]."
  (org-node-cache--init-ids)
  (when (hash-table-empty-p org-nodes)
    (setq synchronous (if (eq synchronous 'must-async) nil t))
    (setq force t))
  (when force
    ;; Launch the async processes
    (org-node-cache--scan-all))
  (when (eq t synchronous)
    ;; Block until all processes finish
    (if org-node-cache-mode
        (message "org-node caching...")
      (message "org-node caching... (Hint: Turn on org-node-cache-mode)"))
    (mapc #'accept-process-output org-node-cache--processes)))

(defun org-node-cache--init-ids ()
  (when (not org-id-track-globally)
    (user-error "Org-node requites `org-id-track-globally'"))
  (when (null org-id-locations)
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load)))
  (when (listp org-id-locations)
    (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-id-locations-load)
    (when (hash-table-empty-p org-id-locations)
      (org-node--die "org-id-locations empty, try `org-id-update-id-locations' or `org-roam-update-org-id-locations'"))))

(defvar org-node-cache--mtime-table
  (make-hash-table :test #'equal :size 1000))

(defvar org-node-cache--timer (timer-create)
  "Timer for intermittently checking `org-node-extra-id-dirs'
for new, changed or deleted files.

This is redundant behavior to detect changes made by something
other than the current instance of Emacs.")

;; 200 ms
(defun org-node-cache--scan-new-or-modified ()
  "If `org-node-files' finds any new files, arrange to scan them.

It does not hurt to call this often, like at the end of most
org-node related commands, but it should not be called where the
user would experience a delay as input latency, like at the
beginning of a command, since it's likely to take a good bit
longer than the human ~20ms perception threshold.

This function will detect files that were created or modified
outside this Emacs instance, so it is a good choice for
reliability, but when you know a specific file to check, use
`org-node-cache--rescan-file-a' instead, since it is faster by not
querying the filesystem."
  (let ((known (hash-table-keys org-node-cache--mtime-table))
        (discovered (org-node-files)))
    (if-let ((deleted (-difference known discovered)))
        (progn
          (org-node--forget-id-locations deleted)
          (dolist (file deleted)
            (remhash file org-node-cache--mtime-table))
          (org-node-cache--scan-all))
      (cl-loop for file in discovered
               as saved-mtime = (gethash file org-node-cache--mtime-table)
               as mtime = (file-attribute-modification-time
                           (file-attributes file))
               when (or (not saved-mtime)
                        (not (time-equal-p saved-mtime mtime)))
               do
               (puthash file mtime org-node-cache--mtime-table)
               (org-node-cache--scan-targeted (list file))))))

(defun org-node-cache--rescan-file-a (&optional arg1 arg2 &rest _)
  "Scan nodes and links in a single file.
Either operate on ARG2 if it seems to be a file name, else the
current buffer file.

Meant for `after-save-hook' or as advice on `rename-file'.  To
manually scan a file from Lisp, see
`org-node-cache--scan-targeted', or more conveniently,
`org-node-cache--scan-new-or-modified'."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  The file being renamed may not be the current buffer; it may be
  ;; happening in a Dired buffer, for example.
  (let ((file (if (and (stringp arg2) (file-exists-p arg2))
                  arg2
                (if (and (stringp buffer-file-name)
                         (file-exists-p buffer-file-name))
                    buffer-file-name
                  (message "org-node-cache--rescan-file: Couldn't figure out file to rescan")
                  nil)))
        (oldname (if arg2 arg1)))
    (when (and file
               (member (file-name-extension file)
                       '("org" "org_archive" "org.gpg")))
      (let ((afile (abbreviate-file-name (file-truename file))))
        (when oldname
          (let ((old-afile (abbreviate-file-name (file-truename oldname))))
            (org-node--forget-id-locations (list old-afile))
            (org-node-cache--dirty-delete-file old-afile)))
        (org-node-cache--scan-targeted (list afile))))))

;; TODO: actually do this on handle-finished-job?
(defun org-node-cache--handle-delete-a (file-being-deleted &rest _)
  "Make org-id and org-node forget about FILE-BEING-DELETED."
  (when file-being-deleted
    (let ((afile (abbreviate-file-name (file-truename file-being-deleted))))
      (when (--any-p (string-suffix-p it afile)
                     '(".org" ".org_archive" ".org.gpg"))
        (org-node--forget-id-locations (list afile))
        (org-node-cache--dirty-delete-file afile)))))

(defun org-node-cache-peek ()
  "Print some random members of `org-nodes' that have IDs.
For reference, see type `org-node-data'."
  (interactive)
  (let ((id-nodes (hash-table-values org-nodes)))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length id-nodes)) id-nodes)))))

(defun org-node-cache--nodes-in-file (file)
  "List all known ID nodes in FILE."
  (cl-loop for node being the hash-values of org-nodes
           when (equal file (org-node-get-file-path node))
           collect node))

(defun org-node-cache--ids-in-file (file)
  "List all known Org IDs in FILE."
  (cl-loop for node being the hash-values of org-nodes
           when (equal file (org-node-get-file-path node))
           collect (org-node-get-id node)))

(defun org-node-cache--dirty-delete-file (file)
  "Quick and dirty."
  (dolist (id (org-node-cache--ids-in-file file))
    (org-node-cache--dirty-delete-node-by-id id)))

(defun org-node-cache--dirty-delete-completions-by-id (id)
  (cl-loop for formatted-title being the hash-keys of org-node-collection
           using (hash-values node)
           when (equal id (org-node-get-id node))
           do (remhash formatted-title org-node-collection)))

;; ;; Maybe faster
;; (defun org-node-cache--dirty-delete-completions* (ids)
;;   (if (listp ids)
;;       (setq org-node-collection
;;             (ht<-alist
;;              (cl-loop
;;               for cell in (ht->alist org-node-collection)
;;               unless (member (org-node-get-id (cdr cell)) ids)
;;               collect cell)))
;;     (org-node-cache--dirty-delete-completions-by-id ids)))

;; Maybe faster
(defun org-node-cache--dirty-delete-completions (ids)
  (if (listp ids)
      (cl-loop for formatted-title being the hash-keys of org-node-collection
               using (hash-values node)
               when (member (org-node-get-id node) ids)
               do (remhash formatted-title org-node-collection))
    (org-node-cache--dirty-delete-completions-by-id ids)))

;; NOTE Does not succeed at deleting links from the node to other IDs
(defun org-node-cache--dirty-delete-node-by-id (id)
  "Quick and dirty."
  (let ((node (gethash id org-nodes)))
    (when node
      (org-node-cache--dirty-delete-completions id)
      (remhash id org-nodes)
      (remhash id org-node--links-table)
      (dolist (ref (org-node-get-refs node))
        (remhash ref org-node--refs-table)))))

;; TODO also update links tables so backlinks buffer uses the right title
(defun org-node-cache--dirty-rescan-heading-at-point ()
  "Quick and dirty, works in unsaved buffers.

Not meant to be perfect, but good enough to ensure the heading
shows up among completion candidates."
  (let* ((props (org-entry-properties))
         (id (cdr (assoc "ID" props)))
         (heading (org-get-heading t t t t))
         (fpath (abbreviate-file-name (file-truename buffer-file-name)))
         (ftitle (cadar (org-collect-keywords '("TITLE"))))
         (old (copy-sequence (gethash id org-nodes))))
    (when (and id (not (string-blank-p id))
               heading (not (string-blank-p heading)))
      (org-node-cache--dirty-delete-completions id)
      (org-node-cache--add-node-to-tables
       (list :title heading
             :id id
             :is-subtree t
             :file-path fpath
             :file-title ftitle
             :file-title-or-basename (or ftitle (file-name-nondirectory fpath))
             :aliases (split-string-and-unquote
                       (or (cdr (assoc "ROAM_ALIASES" props)) ""))
             :refs (split-string-and-unquote
                    (or (cdr (assoc "ROAM_REFS" props)) ""))
             :pos (org-entry-beginning-position)
             :level (org-current-level)
             :olp (org-get-outline-path)
             ;; Less important
             :tags      (and old (org-node-get-tags old))
             :todo      (and old (org-node-get-todo old))
             :deadline  (and old (org-node-get-deadline old))
             :scheduled (and old (org-node-get-scheduled old)))))))

;; I feel like this could be easier to read...
(defun org-node-cache--add-node-to-tables (node-as-plist)
  "Add a node to `org-nodes' and other tables.
The input NODE-AS-PLIST is a list of arguments to pass to
`make-org-node-data'."
  (let* ((node (apply #'make-org-node-data node-as-plist))
         (id (org-node-get-id node))
         (path (org-node-get-file-path node)))
    ;; Tell org-id about the node too, but don't use `org-id-add-location' as
    ;; it does unnecessary CPU cycles
    (puthash id path org-id-locations)
    (unless (member path org-id-files)
      (push path org-id-files))
    ;; Tell org-node
    (puthash id node org-nodes)
    (dolist (ref (org-node-get-refs node))
      (puthash ref id org-node--refs-table))
    (when (funcall org-node-filter-fn node)
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
For demonstration of LINK-PLIST, read the source of
`org-node-worker--collect-links-until'.

PATH is the actual link, which is an org-id if TYPE is \"id\".
Otherwise, PATH may be something like \"//www.gnu.org\"."
  (push link-plist (gethash path (if (equal type "id")
                                     org-node--links-table
                                   org-node--reflinks-table))))

(let ((timer (timer-create)))
  (defun org-node-cache--scan-all ()
    (if (-any-p #'process-live-p org-node-cache--processes)
        ;; Retry soon
        (progn
          ;; TODO: implement a timeout, 20s?
          ;; (while-let ((old-process (pop org-node-cache--processes)))
          ;; (delete-process old-process))
          (cancel-timer timer)
          (setq timer (run-with-timer 1 nil #'org-node-cache--scan-all)))
      ;; Scan now
      (org-node-cache--scan))))

(let ((waiting nil)
      (timer (timer-create)))
  (defun org-node-cache--scan-targeted (files)
    "Thin wrapper for `org-node-cache--scan'.
Ensure that multiple calls occurring in a short time (like when
multiple files are being renamed) will be handled
eventually and not dropped."
    (setq waiting (-union waiting (mapcar #'abbreviate-file-name files)))
    (if (-any-p #'process-live-p org-node-cache--processes)
        ;; Retry soon
        (progn
          (cancel-timer timer)
          (setq timer (run-with-timer
                       1 nil #'org-node-cache--scan-targeted waiting)))
      ;; Scan now
      (let ((to-scan (copy-sequence waiting)))
        (setq waiting nil)
        (dolist (file to-scan)
          (unless (file-exists-p file)
            (setq to-scan (remove file to-scan))
            (org-node--forget-id-locations (list file))))
        (when to-scan
          (org-node-cache--scan to-scan))))))

(defvar org-node-cache--processes nil
  "List of subprocesses.")
(defvar org-node-cache--done-ctr 0
  "Count of finished subprocesses.")
(defvar org-node-cache--stderr " *org-node*"
  "Name of buffer for the subprocesses stderr.")
(defvar org-node-cache--start-time nil
  "Timestamp used to measure time it took to rebuild cache.")

(defun org-node-cache--scan (&optional files)
  (while-let ((old-process (pop org-node-cache--processes)))
    ;; REVIEW Remove this after no reports for a while (2024-06-07)
    (when (process-live-p old-process)
      (lwarn 'org-node :warning "Process still alive when killed"))
    (delete-process old-process))
  (mkdir (org-node-worker--tmpfile) t)
  (let* ((lib (find-library-name "org-node-worker"))
         (native-path (and (featurep 'native-compile)
                           (native-comp-available-p)
                           (require 'comp)
                           (comp-el-to-eln-filename lib)))
         (elc-path (org-node-worker--tmpfile "worker.elc"))
         (targeted (not (null files))))
    ;; Pre-compile org-node-worker.el, in case the user's package manager
    ;; didn't compile it already, or if local changes have been made
    (if native-path
        (unless (file-newer-than-file-p native-path lib)
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
           ($link-re . ,org-link-plain-re)
           ($assume-coding-system . ,org-node-perf-assume-coding-system)
           ($file-option-todo-re
            . ,(rx bol (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
           ($file-name-handler-alist
            . ,(--keep (rassoc it org-node-perf-keep-file-name-handlers)
                       file-name-handler-alist))
           ($global-todo-re
            . ,(if (stringp (car org-todo-keywords))
                   (org-node--die "Quit because `org-todo-keywords' is configured with obsolete syntax, please fix")
                 (org-node-worker--make-todo-regexp
                  (string-join (-mapcat #'cdr (default-value 'org-todo-keywords))
                               " "))))
           ($backlink-drawer-re
            . ,(concat "^[[:space:]]*:"
                       (or (and (fboundp 'org-super-links-link)
                                (require 'org-super-links)
                                (stringp org-super-links-backlink-into-drawer)
                                org-super-links-backlink-into-drawer)
                           "backlinks")
                       ":")))))))

    (if org-node--dbg
        ;; Special case: if debugging, run single-threaded so we can step
        ;; through the org-node-worker.el functions with edebug
        (progn
          (with-temp-file (org-node-worker--tmpfile "file-list-0.eld")
            (insert (prin1-to-string (or files (org-node-files)))))
          (delete-file (org-node-worker--tmpfile "demands-0.eld"))
          (delete-file (org-node-worker--tmpfile "errors-0.txt"))
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
          (delete-file (org-node-worker--tmpfile "errors-%d.txt" i))
          (with-temp-file (org-node-worker--tmpfile "file-list-%d.eld" i)
            (insert (prin1-to-string (pop file-lists))))
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create org-node-cache--stderr)
                 :command (list (file-truename
                                 ;; Let the children use the same binary
                                 ;; executable used by this Emacs, so the
                                 ;; compiled bytecode will be correct
                                 (expand-file-name invocation-name
                                                   invocation-directory))
                                "--quick"
                                "--batch"
                                "--eval" "(setq gc-cons-threshold most-positive-fixnum)"
                                "--eval" (format "(setq temporary-file-directory \"%s\")"
                                                 temporary-file-directory)
                                "--eval" (format "(setq i %d)" i)
                                "--load" (or native-path elc-path)
                                "--funcall" "org-node-worker--collect")
                 ;; TODO: implement a timeout
                 :sentinel (lambda (_process _event)
                             (org-node-cache--handle-finished-job
                              targeted n-jobs)))
                org-node-cache--processes))))))

(defun org-node-cache--handle-finished-job (targeted n-jobs)
  "Check if this was the last process to return, then wrap-up."
  (when (eq (cl-incf org-node-cache--done-ctr) n-jobs)
    (let (demands)
      (unless targeted
        (clrhash org-nodes)
        (clrhash org-node-collection)
        (clrhash org-node--refs-table)
        (clrhash org-node--reflinks-table)
        (clrhash org-node--links-table))
      (with-temp-buffer
        (dotimes (i n-jobs)
          (let ((demands-file (org-node-worker--tmpfile "demands-%d.eld" i))
                (err-file (org-node-worker--tmpfile "errors-%d.txt" i)))
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
              (erase-buffer)
              (insert-file-contents demands-file)
              (if targeted
                  (org-node-cache--apply-results-from-targeted-scan
                   (car (read-from-string (buffer-string)))
                   )
                (org-node-cache--apply-results-from-full-scan
                 (car (read-from-string (buffer-string)))))))))
      (unless targeted
        (org-id-locations-save)
        (org-node-cache--print-elapsed)))))

;; TODO let the worker actually pre-collect all the found ids, the missing
;;      files etc into lists.
(defun org-node-cache--apply-results-from-targeted-scan (demands)
  (let ((missing (alist-get 'missing-files demands)))
    (org-node--forget-id-locations missing)
    (mapc #'org-node-cache--dirty-delete-file missing))

  (org-node-cache--dirty-delete-completions (alist-get 'found-ids demands))

  (dolist (node-recipe (alist-get 'found-nodes demands))
    (org-node-cache--add-node-to-tables node-recipe))

  (dolist (link-recipe (alist-get 'found-links demands))
    (apply #'org-node-cache--add-link-to-tables link-recipe))

  (let ((files (alist-get 'found-files demands)))
    (when files
      (run-hook-with-args 'org-node-rescan-hook files))))

(defun org-node-cache--apply-results-from-full-scan (demands)
  (org-node--forget-id-locations (alist-get 'missing-files demands))
  (dolist (node-recipe (alist-get 'found-nodes demands))
    (org-node-cache--add-node-to-tables node-recipe))
  (dolist (link-recipe (alist-get 'found-links demands))
    (apply #'org-node-cache--add-link-to-tables link-recipe))
  )

(defun org-node-cache--print-elapsed ()
  "Print time elapsed since `org-node-cache--start-time'.
Also report statistics about the content of `org-nodes'.

Currently, the printed message implies that all of `org-nodes'
was collected within the time elapsed, so you should not run this
function after only a partial scan, as the message would be
misleading."
  (if (not org-node-cache-mode)
      (message "Scanned (Hint: Turn on org-node-cache-mode to do this less)")
    (let ((n-subtrees (cl-loop
                       for node being the hash-values of org-nodes
                       count (org-node-get-is-subtree node)))
          (n-backlinks (length (apply #'append
                                      (hash-table-values
                                       org-node--links-table))))
          (n-reflinks
           (cl-loop
            for ref being the hash-keys of org-node--refs-table
            as ref-sans-type = (replace-regexp-in-string "^.*?:" "" ref)
            sum (length (gethash ref-sans-type  org-node--reflinks-table)))))
      (message "org-node saw %d files, %d subtrees, %d ID-links, %d reflinks in %.2fs"
               (- (hash-table-count org-nodes) n-subtrees)
               n-subtrees
               n-backlinks
               n-reflinks
               (float-time (time-since org-node-cache--start-time))))))

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
          (if size
              (if (> biggest size)
                  (push file small-files)
                (setq biggest size)
                (push (cons size file) big-files))
            ;; Couldn't access the file attributes, so the file was probably
            ;; deleted before we got here
            (org-node--forget-id-locations (list file)))))
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
