;;; org-node-backlink.el -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'org-node-cache)

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated."
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        (add-hook 'before-save-hook #'org-node-backlink--update-buffer-safe 0 t)
        (add-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-in-target-a -99 t)
        (add-hook 'org-node-insert-link-hook #'org-node-backlink--add-in-target-a -99 t)
        ;; It seems advices cannot be buffer-local, but it's OK, this function
        ;; will do nothing if this mode isn't active in current buffer.
        (advice-add 'org-insert-link :after #'org-node-backlink--add-in-target-a))
    (remove-hook 'before-save-hook #'org-node-backlink--update-buffer-safe t)
    (remove-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-in-target-a t)
    (remove-hook 'org-node-insert-link-hook #'org-node-backlink--add-in-target-a t)))

(defvar org-node-backlink--fix-ctr 0)
(defvar org-node-backlink--fix-cells nil)

(defun org-node-backlink-fix-all (&optional remove)
  "Add :BACKLINKS: property to all nodes known to `org-id-locations'.
Optional argument REMOVE-THEM means remove them instead, the same
as the user command \\[org-node-backlink-regret]."
  (interactive)
  (when (or (null org-node-backlink--fix-cells) current-prefix-arg)
    ;; Start over
    (org-node-cache-reset)
    (setq org-node-backlink--fix-cells (org-id-hash-to-alist org-id-locations)))
  (when (or (not (= 0 org-node-backlink--fix-ctr)) ;; resume interrupted
            (and
             (y-or-n-p (format "Edit the %d files found in `org-id-locations'?"
                               (length org-node-backlink--fix-cells)))
             (y-or-n-p "You understand that your auto git-commit systems and similar will probably run?")))
    (let ((find-file-hook nil)
          (org-mode-hook nil)
          (after-save-hook nil)
          (before-save-hook nil)
          (org-agenda-files nil)
          (org-inhibit-startup t))
      ;; Do 1000 at a time, because Emacs cries about opening too many file
      ;; buffers in one loop
      (dotimes (_ 1000)
        (when-let ((cell (pop org-node-backlink--fix-cells)))
          (message
           "Adding/updating :BACKLINKS:... (you can stop and resume anytime) (%d) %s"
           (cl-incf org-node-backlink--fix-ctr) (car cell))
          (org-with-file-buffer (car cell)
            (org-with-wide-buffer
             (org-node-backlink--update-buffer (cdr cell) remove))
            (and org-file-buffer-created
                 (buffer-modified-p)
                 (let ((save-silently t)
                       (inhibit-message t))
                   (save-buffer)))))))
    (if org-node-backlink--fix-cells
        ;; Keep going
        (run-with-timer 1 nil #'org-node-backlink-fix-all remove)
      ;; Reset
      (setq org-node-backlink--fix-ctr 0))))

(defun org-node-backlink-regret (dir)
  "Remove Org properties :BACKLINKS: and :CACHED_BACKLINKS: from all
files known to `org-id-locations'."
  (interactive)
  (org-node-backlink-fix-all 'remove))


;;; Single file update

;; TODO Let it update giant files quickly.  Now it just gives up and gets out
;; of the way if there are >100 links.  But I'd like to support truly giant
;; files.  Take a page from the ws-butler concept: only update the parts of the
;; file that changed.
(defun org-node-backlink--update-buffer-safe ()
  "Wrapper for `before-save-hook', and avoids signaling an error."
  (interactive)
  (let ((n-links
         (save-mark-and-excursion
           (goto-char (point-min))
           (cl-loop while (re-search-forward org-link-bracket-re nil t)
                    count t))))
    ;; If there's a ton of links in this file, don't do anything
    (unless (> n-links 100)
      ;; Catch and drop any error
      (condition-case err
          (org-node-backlink--update-buffer)
        ((t error)
         (message "org-node-backlink failed with message %s %s"
                  (car err) (cdr err)))))))

(defun org-node-backlink--update-buffer (&optional ids remove)
  (let ((ids (or ids (cdr (assoc buffer-file-name
                                 (org-id-hash-to-alist org-id-locations))))))
    (save-excursion
      (goto-char (point-min))
      (dolist (id ids)
        ;; Allow it to fail, as it may just mean the buffer is narrowed
        (when-let ((pos (org-find-property "ID" id)))
          (goto-char pos)
          ;; Old name, get rid of it
          (org-entry-delete nil "CACHED_BACKLINKS")
          (if remove
              (org-entry-delete nil "BACKLINKS")
            (let* ((ROAM_REFS (ignore-errors
                                (org-node-get-refs (gethash id org-nodes))))
                   (reflinks (--map (gethash it org-node--reflinks-table)
                                    ROAM_REFS))
                   (backlinks (gethash id org-node--links-table))
                   (combined
                    (->> (append reflinks backlinks)
                         (--map (plist-get it :src))
                         ;; TODO Not sure how there are nils, look into it
                         (remq 'nil)
                         (-uniq)
                         (-sort #'string-lessp)
                         ;; At this point we have a sorted list of ids (sorted
                         ;; only for idempotency)
                         (--map (org-link-make-string
                                 (concat "id:" it)
                                 (org-node-get-title
                                  (gethash it org-nodes))))))
                   (link-string (string-join combined "  ")))
              (if combined
                  (unless (equal link-string (org-entry-get nil "BACKLINKS"))
                    (org-entry-put nil "BACKLINKS" link-string))
                (org-entry-delete nil "BACKLINKS")))))))))


;;; Link-insertion advice

(defvar org-node-backlink--fails nil
  "List of IDs that could not be resolved.")

(defun org-node-backlink--add-in-target-a (&rest _)
  "Meant as advice after any command that inserts a link.
See `org-node-backlink--add-in-target' - this is
merely a wrapper that drops the input."
  (when org-node-backlink-mode
    (org-node-cache-ensure-fresh)
    (org-node-backlink--add-in-target)))

(defun org-node-backlink--add-in-target (&optional part-of-mass-op)
  "For known link at point, leave a backlink in the target node.
Does NOT try to validate the rest of the target's backlinks."
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (let ((elm (org-element-context)))
    (let ((path (org-element-property :path elm))
          (type (org-element-property :type elm))
          id file)
      (when (and path type)
        (if (equal "id" type)
            ;; Classic backlink
            (progn
              (setq id path)
              (setq file (org-id-find-id-file id))
              (unless file
                (push id org-node-backlink--fails)
                (user-error "ID not found \"%s\"%s"
                            id
                            org-node--standard-tip)))
          ;; "Reflink"
          (setq id (gethash (concat type ":" path) org-node--refs-table))
          (setq file (ignore-errors
                       (org-node-get-file-path (gethash id org-nodes)))))
        (when (and id file)
          (org-node-backlink--add-in-target-1 file id part-of-mass-op))))))

(defun org-node-backlink--add-in-target-1 (target-file target-id &optional part-of-mass-op)
  (let ((case-fold-search t)
        (src-id (org-id-get nil nil nil t)))
    (if (not src-id)
        (message "Unable to find ID in file, so it won't get backlinks: %s"
                 (buffer-file-name))
      (let* ((src-title (save-excursion
                          (re-search-backward (concat "^[ \t]*:id: +" src-id))
                          (or (org-get-heading t t t t)
                              (org-get-title)
                              (file-name-nondirectory (buffer-file-name)))))
             (src-link (concat "[[id:" src-id "][" src-title "]]")))
        (org-with-file-buffer target-file
          (org-with-wide-buffer
           (let ((otm (bound-and-true-p org-transclusion-mode)))
             (when otm (org-transclusion-mode 0))
             (goto-char (point-min))
             (if (not (re-search-forward
                       (concat "^[ \t]*:id: +" (regexp-quote target-id))
                       nil t))
                 (push target-id org-node-backlink--fails)
               (let ((backlinks-string (org-entry-get nil "BACKLINKS"))
                     new-value)
                 ;; NOTE: Not sure why, but version 2cff874 dropped some
                 ;; backlinks and it seemed to be fixed by one or both of these
                 ;; changes:
                 ;; - sub `-uniq' for `delete-dups'
                 ;; - sub `remove' for `delete'
                 (if backlinks-string
                     ;; Build a temp list to check we don't add the same link
                     ;; twice. To use the builtin
                     ;; `org-entry-add-to-multivalued-property', the link
                     ;; descriptions would have to be free of spaces.
                     (let ((ls (split-string (replace-regexp-in-string
                                              "]][[:space:]]+\\[\\["
                                              "]]\f[["
                                              (string-trim backlinks-string))
                                             "\f" t)))
                       (dolist (id-dup (--filter (string-search src-id it) ls))
                         (setq ls (remove id-dup ls)))
                       (push src-link ls)
                       (when (-any-p #'null ls)
                         (org-node-die "nulls in %S" ls))
                       ;; Prevent unnecessary work like putting the most recent
                       ;; link in front even if it was already in the list
                       (sort ls #'string-lessp)
                       ;; Two spaces between links help them look distinct
                       (setq new-value (string-join ls "  ")))
                   (setq new-value src-link))
                 (unless (equal backlinks-string new-value)
                   (org-entry-put nil "BACKLINKS" new-value)
                   (unless part-of-mass-op
                     (and org-file-buffer-created
                          (buffer-modified-p)
                          (save-buffer))))
                 (when otm (org-transclusion-mode)))))))))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
