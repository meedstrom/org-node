;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'org-node-async)
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
        (advice-add #'delete-file :before #'org-node-cache--handle-delete))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache--handle-delete)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

(defun org-node-cache-peek ()
  "For debugging: peek on a random member of `org-nodes'.
See also the type `org-node-data'."
  (interactive)
  (require 'map)
  (require 'seq)
  (let ((fields (--map (intern (concat "org-node-" (symbol-name it)))
                       (map-keys (cdr (cl-struct-slot-info 'org-node-data)))))
        (random-node (seq-random-elt
                      (-filter #'org-node-get-id (hash-table-values org-nodes)))))
    (message "%s"
             (--zip-with
              (format "(%s X) => %s\n" it other)
              fields
              (cl-loop for field in fields
                       collect (funcall field random-node))))))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-reset ()
  "Wipe and rebuild the cache.
For an user-facing command, see \\[org-node-reset]."
  (clrhash org-nodes)
  (clrhash org-node-collection)
  (clrhash org-node--refs-table)
  (clrhash org-node--reflinks-table)
  (clrhash org-node--links-table)
  (org-node--init-org-id-locations-or-die)
  (if org-node-perf-multicore
      (org-node-async--collect (-uniq (hash-table-values org-id-locations)))
    (org-node-worker--collect (-uniq (hash-table-values org-id-locations))
                              (org-node--work-variables))
    (run-hooks 'org-node-cache-reset-hook)))

(defun org-node-cache-rescan-file (&optional _arg1 arg2 &rest _)
  "Seek nodes and links in a single file."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let ((file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                  arg2
                (buffer-file-name))))
    (when (derived-mode-p 'org-mode)
      ;; (org-node--init-org-id-locations-or-die)
      (org-node-worker--collect (list file) (org-node--work-variables))
      (when (boundp 'org-node-cache-scan-file-hook)
        (lwarn 'org-node :warning
               "Hook renamed: org-node-cache-scan-file-hook to org-node-cache-rescan-file-hook"))
      (run-hooks 'org-node-cache-rescan-file-hook))))

(defun org-node-cache-ensure-fresh ()
  (org-node--init-org-id-locations-or-die)
  (let ((org-node-perf-multicore nil)) ;;HACK
    ;; Once-per-session tip
    (when (and (hash-table-empty-p org-node-collection)
               (not (member 'org-node-cache-mode org-mode-hook)))
      (message "To speed up this command, turn on `org-node-cache-mode'"))
    (when (or (not org-node-cache-mode)
              (hash-table-empty-p org-node-collection))
      (org-node-cache-reset))))

(let ((timer (timer-create)))
  (defun org-node-cache--handle-delete (&optional arg1 &rest _)
    "Update org-id and org-node after an Org file is deleted.

First remove any references to the file in `org-id-locations'.

Then schedule a rebuild of org-node caches after a few idle
seconds.  The delay avoids bothering the user who may be trying
to delete several files in a row."
    (let ((file-being-deleted (if (and arg1 (file-exists-p arg1))
                                  arg1
                                (buffer-file-name))))
      (when (member (file-name-extension file-being-deleted)
                    '("org" "org_archive" "gpg"))
        (org-node--forget-id-location file-being-deleted)
        (cancel-timer timer)
        (setq timer (run-with-idle-timer 6 nil #'org-node-cache-reset))))))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
