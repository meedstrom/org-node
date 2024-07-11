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
        (add-hook 'after-save-hook #'org-node-cache--handle-save)
        (add-hook 'org-node-creation-hook #'org-node-cache--dirty-ensure-node-known)
        (add-hook 'org-node-insert-link-hook #'org-node--record-link-at-point)
        (add-hook 'org-roam-post-node-insert-hook #'org-node--record-link-at-point)
        (add-hook 'completion-at-point-functions #'org-node-complete-at-point)
        (advice-add 'org-insert-link :after #'org-node--record-link-at-point)
        (advice-add 'rename-file :after #'org-node-cache--handle-save)
        (advice-add 'delete-file :after #'org-node-cache--handle-save)
        (org-node-cache-ensure 'must-async t)
        (setq org-node-cache--timer
              (run-with-idle-timer 60 t #'org-node-cache--scan-new-or-modified))
        (when (boundp 'org-node-cache-reset-hook)
          (display-warning 'org-node "Deprecated 2024-07-11: org-node-cache-reset-hook removed, update your initfiles")))
    (remove-hook 'after-save-hook #'org-node-cache--handle-save)
    (remove-hook 'org-node-creation-hook #'org-node-cache--dirty-ensure-node-known)
    (remove-hook 'org-node-insert-link-hook #'org-node--record-link-at-point)
    (remove-hook 'org-roam-post-node-insert-hook #'org-node--record-link-at-point)
    (remove-hook 'completion-at-point-functions #'org-node-complete-at-point)
    (advice-remove 'org-insert-link #'org-node--record-link-at-point)
    (advice-remove 'rename-file #'org-node-cache--handle-save)
    (advice-remove 'delete-file #'org-node-cache--handle-save)))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that `org-node--node-by-id' and other tables are ready for use.
Specifically, do the following:

- Initialize `org-id-locations' if it is not already.
- Ensure `org-id-locations' is a hash table and not an alist.
- (Re-)build the cache if it is empty, or if FORCE t.

Optional argument SYNCHRONOUS t means that if a cache build is
needed or already ongoing, block Emacs until it is done.

When SYNCHRONOUS is nil, return immediately and let the caching
proceed in the background.  As that may take a few seconds, that
would mean that the `org-node--node-by-id' table is probably still outdated
by the time you query it, but that is acceptable in many
situations such as in an user command since the table is mostly
correct - and fully correct by the time of the next invocation.

If the `org-node--node-by-id' table is currently empty, behave as if
SYNCHRONOUS t, unless SYNCHRONOUS is the symbol `must-async'.

Usually when both arguments are nil, this function no-ops, so it
is cheap to put at the start of any user command.  It can only
really interfere with user experience if SYNCHRONOUS t and it
takes your computer long to finish \\[org-node-reset]."
  (org-node-cache--init-ids)
  (when (hash-table-empty-p org-node--node-by-id)
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
    (mapc #'accept-process-output org-node-cache--processes)
    ;; Just in case... see docstring of `org-node--create'.
    ;; Not super happy about this edge case
    (while (member org-node-cache--retry-timer timer-list)
      (cancel-timer org-node-cache--retry-timer)
      (funcall (timer--function org-node-cache--retry-timer))
      (mapc #'accept-process-output org-node-cache--processes))))

(defun org-node-cache--init-ids ()
  (require 'org-id)
  (when (not org-id-track-globally)
    (user-error "Org-node requires `org-id-track-globally'"))
  (when (null org-id-locations)
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load)))
  (when (listp org-id-locations)
    (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-id-locations-load)
    (when (hash-table-empty-p org-id-locations)
      (org-node--die "org-id-locations empty, try `org-id-update-id-locations' or `org-roam-update-org-id-locations'"))))

(defvar org-node-cache--timer (timer-create)
  "Timer for intermittently checking `org-node-extra-id-dirs'
for new, changed or deleted files.

This redundant behavior helps detect changes made by something
other than the current instance of Emacs, such as an user typing
rm on the command line instead of using \\[delete-file].")

;; 200 ms
(defun org-node-cache--scan-new-or-modified ()
  "If `org-node-files' finds any new files, arrange to scan them.
If a file is gone, reset the cache.

It does not hurt to call this often, like at the end of most
org-node related commands, but it should not be called where the
user would experience a delay as input latency, like at the
beginning of a command, since it's likely to take a good bit
longer than the human ~20ms perception threshold.

This function will detect files that were created or modified
outside this Emacs instance, so it is a good choice for
reliability, but when you know a specific file to check, use
`org-node-cache--scan-targeted' instead, since it is faster by
not needing to compare all file-modification times."
  (let ((known-earlier (hash-table-keys org-node--mtime-by-file))
        (found-now (org-node-files)))
    (let ((deleted (-difference known-earlier found-now))
          (new-or-modified
           (cl-loop
            for file in found-now
            as saved-mtime = (gethash file org-node--mtime-by-file)
            as mtime = (file-attribute-modification-time
                        (file-attributes file))
            when (or (not saved-mtime)
                     (not (time-equal-p saved-mtime mtime)))
            do (puthash file mtime org-node--mtime-by-file)
            and collect file)))
      (if (null known-earlier)
          (org-node-cache--scan-all)
        (if deleted
            (progn
              (org-node--forget-id-locations deleted)
              (dolist (file deleted)
                (remhash file org-node--mtime-by-file))
              (org-node-cache--scan-all))
          (when new-or-modified
            (org-node-cache--scan-targeted new-or-modified)))))))

(defun org-node-cache--handle-save (&optional arg1 arg2 &rest _)
  "Scan nodes and links in a single file, or forget them if the
file is gone.

Either operate on ARG2 if it seems to be a file name, else ARG1,
else the current buffer file.  Meant for `after-save-hook' or as
advice on `rename-file' or `delete-file'."
  (let ((file (cond
               ((and (stringp arg2) (file-exists-p arg2)) ;; rename-file
                arg2)
               ((stringp arg1) ;; delete-file
                arg1)
               ((stringp buffer-file-name) ;; after-save-hook
                buffer-file-name))))
    (when (--any-p (string-suffix-p it file)
                   '(".org" ".org_archive" ".org.gpg"))
      (org-node-cache--scan-targeted (list file)))))

(defun org-node-cache-peek ()
  "Print some random members of `org-node--node-by-id'.
For reference, see type `org-node-get'."
  (interactive)
  (let ((id-nodes (hash-table-values org-node--node-by-id))
        (print-length nil))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length id-nodes)) id-nodes)))))


;;; Scan

(defun org-node-cache--scan-all ()
  (org-node-cache--try-launch-scan t))

(defun org-node-cache--scan-targeted (files)
  (org-node-cache--try-launch-scan files))

(defvar org-node-cache--retry-timer (timer-create))
;; (defvar org-node-cache--queue nil)
;; (defvar org-node-cache--wait-start nil)
;; (defvar org-node-cache--full-scan-requested nil)

;; On the one hand, elegant, but somehow it's long for such a simple concept
(let (file-queue wait-start full-scan-requested)
  (defun org-node-cache--try-launch-scan (&optional files)
    "Ensure that multiple calls occurring in a short time (like when
multiple files are being renamed) will be handled
eventually and not dropped."
    (if (eq t files)
        (setq full-scan-requested t)
      (setq file-queue (-union (mapcar #'abbreviate-file-name files) file-queue)))
    (if (-any-p #'process-live-p org-node-cache--processes)
        (progn
          (unless wait-start
            (setq wait-start (current-time)))
          (if (> (float-time (time-since wait-start)) 30)
              ;; Timeout subprocess stuck in some infinite loop (eats battery)
              (progn
                (setq wait-start nil)
                (message "org-node: some processes worked longer than 30 sec, killed")
                (while-let ((old-process (pop org-node-cache--processes)))
                  (delete-process old-process)))
            ;; Retry soon
            (cancel-timer org-node-cache--retry-timer)
            (setq org-node-cache--retry-timer
                  (run-with-timer 1 nil #'org-node-cache--try-launch-scan))))
      ;; Scan now
      (setq wait-start nil)
      (if full-scan-requested
          (progn
            (setq full-scan-requested nil)
            (org-node-cache--scan (org-node-files)
                                  #'org-node-cache--finish-full))
        ;; Targeted scan of specific files
        (let (new modified)
          (cl-loop for file in file-queue
                   if (gethash file org-node--mtime-by-file)
                   do (push file modified)
                   else do (push file new))
          (cond (new
                 (setq file-queue modified)
                 (org-node-cache--scan new
                                       #'org-node-cache--finish-new))
                (modified
                 (setq file-queue nil)
                 (org-node-cache--scan modified
                                       #'org-node-cache--finish-modified)))))
      (when file-queue
        (cancel-timer org-node-cache--retry-timer)
        (setq org-node-cache--retry-timer
              (run-with-timer 1 nil #'org-node-cache--try-launch-scan))))))

(defvar org-node-cache--processes nil
  "List of subprocesses.")
(defvar org-node-cache--done-ctr 0
  "Count of finished subprocesses.")
(defvar org-node-cache--stderr-name " *org-node*"
  "Name of buffer for the subprocesses stderr.")
(defvar org-node-cache--start-time nil
  "Timestamp used to measure time it took to rebuild cache.")

(defvar org-node-cache-max-jobs nil
  "Number of subprocesses to run.
If left at nil, will be set at runtime to the result of
`org-node-cache--count-logical-cores'.")

(defun org-node-cache--init-lib ()
  "Do first-time setup, maybe re-do if world has changed.

Return file path to either .eln or the .elc variant of compiled
org-node-worker library."
  (unless org-node-cache-max-jobs
    (setq org-node-cache-max-jobs (org-node-cache--count-logical-cores)))
  ;; Compile the user-provided lambdas. Would use native-comp here. but
  ;; it wrecks customize, and any elegant workaround grows to ~30 LoC.
  (unless (compiled-function-p org-node-filter-fn)
    (setq org-node-filter-fn (byte-compile org-node-filter-fn)))
  (unless (compiled-function-p org-node-format-candidate-fn)
    (setq org-node-format-candidate-fn (byte-compile org-node-format-candidate-fn)))
  ;; Compile org-node-worker.el, in case the user's package manager
  ;; didn't do so already, or local changes have been made.
  (let* ((file-name-handler-alist nil)
         (lib (find-library-name "org-node-worker"))
         (native-path (and (featurep 'native-compile)
                           (native-comp-available-p)
                           (require 'comp)
                           (comp-el-to-eln-filename lib)))
         (elc-path (org-node-worker--tmpfile "worker.elc")))
    (mkdir (org-node-worker--tmpfile) t)
    (if native-path
        (unless (file-newer-than-file-p native-path lib)
          (native-compile lib))
      ;; No native-comp facility, so make an .elc
      (unless (file-newer-than-file-p elc-path lib)
        ;; Ensure the .elc won't clutter some source directory
        (let ((byte-compile-dest-file-function `(lambda (&rest _) ,elc-path)))
          (byte-compile-file lib))))
    (or native-path elc-path)))

(defun org-node-cache--scan (files finalizer)
  "Begin async scanning FILES for id-nodes and links.

When finished, run the FINALIZER function to update current
tables."
  (let ((compiled-lib (org-node-cache--init-lib))
        (file-name-handler-alist nil))
    ;; Prep for scan
    (garbage-collect)
    (setq org-node-cache--start-time (current-time))
    (setq org-node-cache--done-ctr 0)
    (when (-any-p #'process-live-p org-node-cache--processes)
      ;; This shouldn't happen, but just in case
      (mapc #'delete-process org-node-cache--processes)
      (message "org-node processes alive, bug report would be appreciated"))
    (setq org-node-cache--processes nil)
    (with-current-buffer (get-buffer-create org-node-cache--stderr-name)
      (erase-buffer))
    (with-temp-file (org-node-worker--tmpfile "work-variables.eld")
      (let ((standard-output (current-buffer))
            (print-length nil)
            (print-level nil))
        (prin1
         ;; NOTE The purpose of $sigils is just visual, to distinguish these
         ;; "external" variables in the body of
         ;; `org-node-worker--collect-dangerously'.
         (append
          org-node-inject-variables
          `(($link-re . ,org-link-plain-re)
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

    (if org-node--debug
        ;; Special case for debugging; run single-threaded so we can step
        ;; through the org-node-worker.el functions with edebug
        (progn
          (delete-file (org-node-worker--tmpfile "results-0.eld"))
          (delete-file (org-node-worker--tmpfile "errors-0.txt"))
          (with-temp-file (org-node-worker--tmpfile "file-list-0.eld")
            (let ((standard-output (current-buffer))
                  (print-length nil))
              (prin1 files)))
          (setq i 0)
          (when (bound-and-true-p editorconfig-mode)
            (message "Maybe disable editorconfig-mode while debugging"))
          (pop-to-buffer (get-buffer-create "*org-node debug*"))
          (erase-buffer)
          (org-node-worker--collect-dangerously)
          (org-node-cache--handle-finished-job 1 finalizer))

      ;; If not debugging, split the work over many child processes
      (let* ((file-lists
              (org-node--split-into-n-sublists files org-node-cache-max-jobs))
             (n-jobs (length file-lists)))
        (dotimes (i n-jobs)
          (delete-file (org-node-worker--tmpfile "results-%d.eld" i))
          (delete-file (org-node-worker--tmpfile "errors-%d.txt" i))
          ;; TODO maybe better perf if the file-lists are all inside work-variables
          (with-temp-file (org-node-worker--tmpfile "file-list-%d.eld" i)
            (let ((standard-output (current-buffer))
                  (print-length nil)
                  (print-level nil))
              (prin1 (pop file-lists))))
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create org-node-cache--stderr-name)
                 :command
                 ;; REVIEW does taskset ever make a diff?  If not, remove it.
                 ;; It assumes `org-node-cache-max-jobs' is no more than the
                 ;; number of actual cores.
                 (append (when (executable-find "taskset")
                           (list "taskset" "-c" (number-to-string i)))
                         (list
                          (file-truename
                           ;; Ensure the children run the same binary
                           ;; executable as used by this Emacs, so the
                           ;; compiled lib is correct for it
                           (expand-file-name invocation-name
                                             invocation-directory))
                          "--quick"
                          "--batch"
                          "--eval" (format "(setq gc-cons-threshold %d)"
                                           (or org-node-perf-gc-cons-threshold
                                               gc-cons-threshold))
                          "--eval" (format "(setq temporary-file-directory \"%s\")"
                                           temporary-file-directory)
                          "--eval" (format "(setq i %d)" i)
                          "--load" compiled-lib
                          "--funcall" "org-node-worker--collect-dangerously"))
                 :sentinel (lambda (_process _event)
                             (org-node-cache--handle-finished-job
                              n-jobs finalizer)))
                org-node-cache--processes))))))

(defun org-node-cache--handle-finished-job (n-jobs finalizer)
  "Check if this was the last process to return (by counting up
to N-JOBS), then if so, wrap-up and call FINALIZER."
  (when (eq n-jobs (cl-incf org-node-cache--done-ctr))
    (let* ((file-name-handler-alist nil)
           ;; (coding-system-for-read 'utf-8-unix)
           (result-sets
            (with-temp-buffer
              (cl-loop
               for i below n-jobs
               collect
               (let ((results-file (org-node-worker--tmpfile "results-%d.eld" i))
                     (err-file (org-node-worker--tmpfile "errors-%d.txt" i)))
                 (when (file-exists-p err-file)
                   (message "org-node: problems scanning some files, see %s" err-file))
                 (if (file-exists-p results-file)
                     (progn
                       (erase-buffer)
                       (insert-file-contents results-file)
                       (read (buffer-string)))
                   ;; Had 1+ errors, so unhide the stderr buffer from now on
                   (let ((buf (get-buffer " *org-node*")))
                     (when buf
                       (setq org-node-cache--stderr-name "*org-node errors*")
                       (with-current-buffer buf
                         (rename-buffer org-node-cache--stderr-name)))
                     (message "An org-node worker failed to scan files, not producing %s.  See buffer %s"
                              results-file org-node-cache--stderr-name)
                     nil)))))))
      ;; TODO just build the list via the cl-loop above
      (funcall finalizer (--reduce (-zip-with #'nconc it acc)
                                   (-non-nil result-sets))))))


;;; Finalizers

(defun org-node-cache--finish-full (results)
  (clrhash org-node--node-by-id)
  (clrhash org-node--node-by-candidate)
  (clrhash org-node--id-by-ref)
  (clrhash org-node--id-by-title)
  (clrhash org-node--reflinks-by-ref)
  (clrhash org-node--backlinks-by-id)
  (-let (((missing-files mtimes nodes id-links reflinks cites) results))
    (org-node--forget-id-locations missing-files)
    (dolist (file missing-files)
      (remhash file org-node--mtime-by-file))
    (dolist (found mtimes)
      (puthash (car found) (cdr found) org-node--mtime-by-file))
    (org-node-cache--record-nodes nodes)
    (org-node-cache--record-id-links id-links)
    (org-node-cache--record-reflinks reflinks)
    (org-node-cache--record-cites cites))
  (org-id-locations-save)
  (org-node-cache--print-elapsed))

(defun org-node-cache--finish-new (results)
  (-let (((missing-files mtimes nodes id-links reflinks) results))
    (org-node--forget-id-locations missing-files)
    (dolist (file missing-files)
      (remhash file org-node--mtime-by-file))
    (dolist (found mtimes)
      (puthash (car found) (cdr found) org-node--mtime-by-file))
    (org-node-cache--record-nodes nodes)
    (org-node-cache--record-id-links id-links)
    (org-node-cache--record-reflinks reflinks)
    (run-hook-with-args 'org-node-rescan-hook (mapcar #'car mtimes))))

;; NOTE For performance, we do not bother to update the links tables on file
;; modification.  Doing so would not be a simple puthash operation, but need
;; looping to find all matches of :dest as well as :origin, and that gets slow
;; when saving a big file containing 5000 links -- slow enough to annoy users
;; of `auto-save-visited-mode', at minimum.
;;
;; Fortunately, our insert-link advices will already record links added during
;; normal usage!  What's left is mainly that /deleted/ links will remain in the
;; table until removed at the next full scan.  That is OK, as I think the
;; nicest code relies on frequent full scans anyway.
(defun org-node-cache--finish-modified (results)
  (-let (((missing-files mtimes nodes) results))
    (org-node--forget-id-locations missing-files)
    (org-node-cache--dirty-forget-files missing-files)
    (dolist (file missing-files)
      (remhash file org-node--mtime-by-file))
    (dolist (found mtimes)
      (puthash (car found) (cdr found) org-node--mtime-by-file))
    ;; In case a title was edited
    (org-node-cache--dirty-forget-completions-in (mapcar #'car mtimes))
    (org-node-cache--record-nodes nodes)
    (run-hook-with-args 'org-node-rescan-hook (mapcar #'car mtimes))))


;;; Record

(defun org-node-cache--record-id-links (links)
  "Save LINKS to table `org-node--backlinks-by-id'.
LINKS plists are demonstrated in source of
`org-node-worker--collect-links-until'."
  (dolist (link links)
    (push link (gethash (plist-get link :dest) org-node--backlinks-by-id))))

(defun org-node-cache--record-reflinks (links)
  "Save LINKS to table `org-node--reflinks-by-ref'.
LINKS plists are demonstrated in source of
`org-node-worker--collect-links-until'."
  (dolist (link links)
    (push link
          (gethash (concat (plist-get link :type) ":" (plist-get link :dest))
                   org-node--reflinks-by-ref))))

(defun org-node-cache--record-cites (cites)
  (dolist (cite cites)
    (push cite (gethash (plist-get cite :key) org-node--cites-by-citekey))))

(defun org-node-cache--record-nodes (node-recipes)
  (mapc #'org-node-cache--record-node node-recipes))

(defun org-node-cache--record-node (node-recipe)
  "Add a node to `org-node--node-by-id' and other tables.

The input NODE-RECIPE is a list of arguments to pass to
`org-node--make-obj'."
  (let* ((node (apply #'org-node--make-obj node-recipe))
         (id (org-node-get-id node))
         (path (org-node-get-file-path node)))
    ;; Share the id location with org-id & do so with a manual `puthash'
    ;; because `org-id-add-location' would run heavy logic we've already done.
    (puthash id path org-id-locations)
    (unless (member path org-id-files)
      (push path org-id-files))
    ;; Register the node
    (puthash id node org-node--node-by-id)
    (dolist (ref (org-node-get-refs node))
      (puthash ref id org-node--id-by-ref))
    (when (funcall org-node-filter-fn node)
      (dolist (title (cons (org-node-get-title node)
                           (org-node-get-aliases node)))
        (let ((collision (gethash title org-node--id-by-title))
              (cand (funcall org-node-format-candidate-fn node title)))
          (puthash cand node org-node--node-by-candidate)
          (puthash title id org-node--id-by-title)
          (when (and collision org-node-warn-title-collisions)
            (unless (equal id collision)
              (message "Two nodes have same name: %s, %s (%s)"
                       id collision title)))))
      ;; (yolo) Let ROAM_REFS work as aliases too
      (dolist (ref (org-node-get-refs node))
        (puthash ref node org-node--node-by-candidate)))))


;;; "Dirty" functions
;; Help keep the cache reasonably in sync without having to do a full reset

(defun org-node-cache--dirty-forget-files (files)
  "Remove from cache all ROAM_REFS and IDs that existed within
FILES, and remove the corresponding completion candidates."
  (when files
    (org-node-cache--dirty-forget-completions-in files)
    (cl-loop
     for node being the hash-values of org-node--node-by-id
     when (member (org-node-get-file-path node) files)
     collect (org-node-get-id node) into ids
     and append (org-node-get-refs node) into refs
     finally do
     (dolist (ref refs)
       (remhash ref org-node--id-by-ref))
     (dolist (id ids)
       (remhash id org-node--node-by-id)))))

(defun org-node-cache--dirty-forget-completions-in (files)
  "Remove the completion candidates for all nodes in FILES."
  (when files
    (cl-loop
     for candidate being the hash-keys of org-node--node-by-candidate
     using (hash-values node)
     when (member (org-node-get-file-path node) files)
     do (remhash candidate org-node--node-by-candidate))))

(defun org-node-cache--dirty-ensure-node-known ()
  "Quick and dirty, works in unsaved buffers.

Not meant to be perfect, but good enough to ensure that the node
at point will show up among completion candidates and that
`org-node-backlink-mode' won't autoclean backlinks to here on
account of it \"not existing yet\"."
  (let ((id (org-entry-get nil "ID" t))
        (case-fold-search t))
    (unless (gethash id org-node--node-by-id)
      (save-excursion
        (without-restriction
          (goto-char (point-min))
          (re-search-forward (concat "^[[:space:]]*:id: +" id))
          (let ((props (org-entry-properties))
                (heading (org-get-heading t t t t))
                (fpath (abbreviate-file-name (file-truename buffer-file-name)))
                (ftitle (cadar (org-collect-keywords '("TITLE")))))
            (org-node-cache--record-node
             (list
              :title (or heading ftitle)
              :id id
              :is-subtree (if heading t)
              :file-path fpath
              :file-title ftitle
              :file-title-or-basename (or ftitle (file-name-nondirectory fpath))
              :aliases (split-string-and-unquote
                        (or (cdr (assoc "ROAM_ALIASES" props)) ""))
              :refs (split-string-and-unquote
                     (or (cdr (assoc "ROAM_REFS" props)) ""))
              :pos (if heading (org-entry-beginning-position) 1)
              :level (or (org-current-level) 0)
              :olp (org-get-outline-path)
              ;; Less important
              :properties props
              :tags (org-get-tags-at)
              :todo (if heading (org-get-todo-state))
              :deadline (cdr (assoc "DEADLINE" props))
              :scheduled (cdr (assoc "SCHEDULED" props))))))))))


;;; Etc

(defun org-node-cache--print-elapsed ()
  "Print time elapsed since `org-node-cache--start-time'.
Also report statistics about the content of `org-node--node-by-id'.

Currently, the printed message implies that all of `org-node--node-by-id'
was collected within the time elapsed, so you should not run this
function after only a partial scan, as the message would be
misleading."
  (if (not org-node-cache-mode)
      (message "Scan complete (Hint: Turn on org-node-cache-mode)")
    (let ((n-subtrees (cl-loop
                       for node being the hash-values of org-node--node-by-id
                       count (org-node-get-is-subtree node)))
          (n-backlinks (length (apply #'append
                                      (hash-table-values
                                       org-node--backlinks-by-id))))
          (n-reflinks (cl-loop
                       for ref being the hash-keys of org-node--id-by-ref
                       sum (length (gethash ref org-node--reflinks-by-ref)))))
      (message "org-node saw %d files, %d subtrees, %d ID-links, %d reflinks in %.2fs"
               (- (hash-table-count org-nodes) n-subtrees)
               n-subtrees
               n-backlinks
               n-reflinks
               (float-time (time-since org-node-cache--start-time))))))

(defun org-node-cache--count-logical-cores ()
  "Return sum of available processor cores, minus 1."
  (max 1 (1- (string-to-number
              (pcase system-type
                ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
                 (if (executable-find "nproc")
                     (shell-command-to-string "nproc --all")
                   (shell-command-to-string "lscpu -p | egrep -v '^#' | wc -l")))
                ((or 'darwin)
                 (shell-command-to-string "sysctl -n hw.logicalcpu_max"))
                ;; No idea if this works
                ((or 'cygwin 'windows-nt 'ms-dos)
                 (ignore-errors
                   (with-temp-buffer
                     (call-process "echo" nil t nil "%NUMBER_OF_PROCESSORS%")
                     (buffer-string)))))))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
