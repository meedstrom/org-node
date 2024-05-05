;;; org-node-backlink.el -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'org-node-cache)

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated."
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        (add-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-in-target-a -99 t)
        (add-hook 'org-node-insert-link-hook #'org-node-backlink--add-in-target-a -99 t)
        ;; It seems advices cannot be buffer-local, but it's OK, these fns
        ;; do nothing if this mode isn't active in the current buffer.
        (advice-add 'org-insert-link :after
                    #'org-node-backlink--add-in-target-a)
        (advice-add 'ws-butler-before-save :before
                    #'org-node-backlink--update-changed-parts-of-buffer))
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
             (y-or-n-p "You understand that this may trigger your auto git-commit systems and similar?")))
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
           "Adding/updating :BACKLINKS:... (you may quit and resume anytime) (%d) %s"
           (cl-incf org-node-backlink--fix-ctr) (car cell))
          (delay-mode-hooks
            (org-with-file-buffer (car cell)
              (org-with-wide-buffer
               (org-node-backlink--update-buffer remove))
              (and org-file-buffer-created
                   (buffer-modified-p)
                   (let ((save-silently t)
                         (inhibit-message t))
                     (save-buffer))))))))
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

(defun org-node-backlink--update-subtree-here (&optional remove)
  "Assumes point is at an :ID: line!
Update the :BACKLINKS: property.  With arg REMOVE, remove it instead."
  (org-entry-delete nil "CACHED_BACKLINKS")  ;; Old name, get rid of it
  (if remove
      (org-entry-delete nil "BACKLINKS")
    (skip-chars-forward "[:space:]")
    (let* ((id (buffer-substring-no-properties
                (point) (+ (point) (skip-chars-forward "^ \n"))))
           (ROAM_REFS
            (org-node-get-refs
             (or (gethash id org-nodes)
                 (if (string-blank-p id)
                     (user-error "Blank ID property in %s" buffer-file-name)
                   (error "ID exists but not scanned by org-node for some reason, bad syntax? %s in file %s"
                          id buffer-file-name)))))
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
                 ;; only to reduce git diffs when the order changes) of
                 ;; every node that links to here
                 (--map (org-link-make-string
                         (concat "id:" it)
                         (org-node-get-title
                          (or (gethash it org-nodes)
                              (error "ID in backlink tables not known to main org-nodes table: %s"
                                     it)))))))
           (link-string (string-join combined "  ")))
      (if combined
          (unless (equal link-string (org-entry-get nil "BACKLINKS"))
            (org-entry-put nil "BACKLINKS" link-string))
        (org-entry-delete nil "BACKLINKS")))))

(defun org-node-backlink--update-buffer (&optional remove markers)
  (save-excursion
    (goto-char (point-min))
    (if markers
        (dolist (marker markers)
          (goto-char (marker-position marker))
          (set-marker marker nil)
          (org-node-backlink--update-subtree-here remove))
      (let ((case-fold-search t))
        (while (re-search-forward "^[[:space:]]*:id: " nil t)
          (org-node-backlink--update-subtree-here remove))))))

(defvar-local org-node-backlink--subtree-markers (list))

(defun org-node-backlink--flag-subtrees-with-changes (_prop start end)
  (save-excursion
    (goto-char start)
    (let ((case-fold-search t))
      ;; We're in a region where ws-butler detected a change, but the region
      ;; will almost never envelop the preceding heading, so search back for it
      (save-excursion
        (when (re-search-backward "^[[:space:]]*:id: " nil t)
          (goto-char (match-end 0))
          (push (point-marker) org-node-backlink--subtree-markers)))
      ;; ...and if the region of detected change is massive, spanning multiple
      ;; subtrees, find each one
      (while (re-search-forward "^[[:space:]]*:id: " end t)
        (push (point-marker) org-node-backlink--subtree-markers)))))

(defun org-node-backlink--update-changed-parts-of-buffer ()
  (when org-node-backlink-mode
    (unless ws-butler-mode
      (message "If saving a big file is slow, see org-node README for a fix based on ws-butler"))
    (while-let ((marker (pop org-node-backlink--subtree-markers)))
      (set-marker marker nil))
    ;; Catch any error because this runs at `before-save-hook' which MUST fail
    ;; gracefully and let the user save anyway.
    (condition-case err
        (progn
          (ws-butler-map-changes #'org-node-backlink--flag-subtrees-with-changes)
          (org-node-backlink--update-buffer
           nil org-node-backlink--subtree-markers))
      (( error user-error debug )
       (lwarn 'org-node :error
              "org-node-backlink--update-changed-parts-of-buffer: %S" err)))))





;;; Link-insertion advice
;; TODO: Simplify

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
