;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'org-node-async)
(require 'org-node-worker)

(defvar org-node-cache--modeless-timer (timer-create)
  "Recurring timer used while `org-node-cache-mode' is off.")

(defun org-node-cache-reset ()
  "Wipe and rebuild the cache.
For an user-facing command, see \\[org-node-reset]."
  (org-node-cache-ensure nil t))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that `org-id-locations' and `org-nodes' are fresh.

Optional argument SYNCHRONOUS means that if a reset is needed, do
it synchronously (i.e. block Emacs until done).  Usually while
`org-node-cache-mode' is active, no reset will be needed, but
when that mode is off, they may happen a lot, so consider whether
setting this to t would make sense in that context (hint: no).

Optional argument FORCE means demand a reset, but let it happen
asynchronously unless SYNCHRONOUS is also set.  When
`org-node-cache-mode' is off, a reset is always demanded even
when this argument is nil."
  ;; First just make sure we've loaded org-id's stuff from disk.
  (when (null org-id-locations)
    (message "org-id-locations empty%s" org-node--standard-tip)
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load)))
  (when (listp org-id-locations)
    (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-id-locations-load)
    (when (hash-table-empty-p org-id-locations)
      (org-node-die "org-id-locations empty%s" org-node--standard-tip)))
  ;; Phew!  Hell of an API, org-id.  Now for the org-node stuff.
  (when (not org-node-cache-mode)
    (setq force t)
    (cancel-timer org-node-cache--modeless-timer)
    (setq org-node-cache--modeless-timer
          (run-with-idle-timer 45 t #'org-node-cache-reset)))
  (when (hash-table-empty-p org-nodes)
    (setq synchronous t force t))
  (let ((live (-any-p #'process-live-p org-node-async--processes)))
    (cond
     ((and synchronous force) ;; Blocking scan requested
      (or live (org-node-async--collect (org-node-files)))
      (message "org-node: Caching synchronously...")
      (mapc #'accept-process-output org-node-async--processes))

     ((and (not synchronous) force) ;; Chill scan requested
      (or live (org-node-async--collect (org-node-files))))

     ((and synchronous (not force)) ;; Block only if scan ongoing
      (mapc #'accept-process-output org-node-async--processes)))))

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
        (cancel-timer org-node-cache--modeless-timer)
        (org-node-reset))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache--handle-delete)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-rescan-file (&optional _arg1 arg2 &rest _)
  "Seek nodes and links in a single file."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let ((file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                  arg2
                (buffer-file-name))))
    (when (derived-mode-p 'org-mode)
      (org-node-async--collect (list file))
      (when (boundp 'org-node-cache-scan-file-hook)
        (lwarn 'org-node :warning
               "Hook renamed: org-node-cache-scan-file-hook to org-node-cache-rescan-file-hook"))
      (run-hooks 'org-node-cache-rescan-file-hook))))

(let ((timer (timer-create)))
  (defun org-node-cache--handle-delete (&optional arg1 &rest _)
    "Update org-id and org-node after an Org file is deleted.

First remove any references to the file in
`org-id-locations'. Then schedule an `org-node-cache-reset' for
after a few idle seconds.  The delay minimizes the risk of
bothering the user who may be trying to delete several files in a
row."
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
          (setq timer (run-with-idle-timer 6 nil #'org-node-cache-reset)))))))

(defun org-node-cache-peek ()
  "Print some random members of `org-nodes' that have IDs.
See also the type `org-node-data'."
  (interactive)
  (let ((id-nodes (-filter #'org-node-get-id (hash-table-values org-nodes))))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length id-nodes)) id-nodes)))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
