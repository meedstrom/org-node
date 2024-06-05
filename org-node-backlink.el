;;; org-node-backlink.el -*- lexical-binding: t; -*-

;; FIXME I believe that when you remove a link, then
;;       `org-node-cache-rescan-file' is not enough to remove the link from
;;       the link table.  It gets corrected only on reset.

(require 'org-node-common)
(require 'org-node-cache)

(define-globalized-minor-mode org-node-backlink-global-mode
  org-node-backlink-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-node-backlink-mode))))

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated."
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        (add-hook 'org-roam-post-node-insert-hook
                  #'org-node-backlink--add-in-target-a -99 t)
        (add-hook 'org-node-insert-link-hook
                  #'org-node-backlink--add-in-target-a -99 t)
        (add-hook 'after-change-functions
                  #'org-node-backlink--flag-change nil t)
        (add-hook 'before-save-hook
                  #'org-node-backlink--update-changed-parts-of-buffer nil t)
        ;; It seems advices cannot be buffer-local, but it's OK, this fn
        ;; does nothing if this mode isn't active in the current buffer.
        (advice-add 'org-insert-link :after
                    #'org-node-backlink--add-in-target-a))
    (remove-hook 'after-change-functions
                 #'org-node-backlink--flag-change t)
    (remove-hook 'before-save-hook
                 #'org-node-backlink--update-changed-parts-of-buffer t)
    (remove-hook 'org-roam-post-node-insert-hook
                 #'org-node-backlink--add-in-target-a t)
    (remove-hook 'org-node-insert-link-hook
                 #'org-node-backlink--add-in-target-a t)))

(defvar org-node-backlink--fix-ctr 0)
(defvar org-node-backlink--fix-files nil)

(defun org-node-backlink-regret ()
  "Visit all IDs in `org-id-locations' and remove the :BACKLINKS:
property."
  (interactive)
  (org-node-backlink-fix-all 'remove))

(defun org-node-backlink-fix-all (&optional remove)
  "Add :BACKLINKS: property to all nodes known to `org-id-locations'.
Optional argument REMOVE means remove them instead, the same
as the user command \\[org-node-backlink-regret]."
  (interactive)
  (when (or (null org-node-backlink--fix-files) current-prefix-arg)
    ;; Start over
    (org-node-cache-ensure t t)
    (setq org-node-backlink--fix-files
          (-uniq (hash-table-values org-id-locations))))
  (when (or (not (= 0 org-node-backlink--fix-ctr)) ;; resume interrupted
            (and
             (y-or-n-p (format "Edit the %d files found in `org-id-locations'?"
                               (length org-node-backlink--fix-files)))
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
        (when-let ((file (pop org-node-backlink--fix-files)))
          (message
           "Adding/updating :BACKLINKS:... (you may quit and resume anytime) (%d) %s"
           (cl-incf org-node-backlink--fix-ctr) file)
          (delay-mode-hooks
            (org-node--with-file file
              (org-node-backlink--update-whole-buffer remove))))))
    (if org-node-backlink--fix-files
        ;; Keep going
        (run-with-timer 1 nil #'org-node-backlink-fix-all remove)
      ;; Reset
      (setq org-node-backlink--fix-ctr 0))))

(defun org-node-backlink--update-whole-buffer (&optional remove)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[[:space:]]*:id: " nil t)
        (org-node-backlink--update-subtree-here remove)))))

(defun org-node-backlink--update-subtree-here (&optional remove)
  "Assumes point is at an :ID: line!
Update the :BACKLINKS: property.  With arg REMOVE, remove it instead."
  (org-entry-delete nil "CACHED_BACKLINKS")  ;; Old name, get rid of it
  (if remove
      (org-entry-delete nil "BACKLINKS")
    (skip-chars-forward "[:space:]")
    (let* ((id (buffer-substring-no-properties
                (point) (+ (point) (skip-chars-forward "^ \n"))))
           (node (gethash id org-nodes)))
      ;; The node may not yet have been scanned by org-node, because it was
      ;; created just now, in a capture buffer, which triggered a save right
      ;; away.
      (when node
        (let* ((ROAM_REFS (org-node-get-refs node))
               (reflinks (--keep (gethash it org-node--reflinks-table)
                                 ROAM_REFS))
               (backlinks (gethash id org-node--links-table))
               (combined
                (thread-last
                  (append reflinks backlinks)
                  (--map (plist-get it :src))
                  (-uniq)
                  (-sort #'string-lessp)
                  ;; At this point we have a sorted list of ids (sorted
                  ;; only to reduce git diffs when the order changes) of
                  ;; every node that links to here.  Now format them as links.
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
            (org-entry-delete nil "BACKLINKS")))))))

(defun org-node-backlink--flag-change (beg end _)
  "Propertize changed text, for use by `org-node-backlink-mode'.
Designed to run on `after-change-functions'."
  (when (derived-mode-p 'org-mode)
    (with-silent-modifications
      (put-text-property beg end 'org-node-chg t))))

;; TODO: Allow narrowing to have an effect, i.e. don't use
;; `without-restriction'.  Problem is that org-entry-get will get an ID outside
;; the region (good).  And that ID can be inherited, so we cannot just narrow
;; to one subtree at a time.  At best, we could narrow to one top-level heading
;; at a time.  Or not exactly top-level.  Basically, look for first heading
;; with an ID, narrow to it plus its children, then move forward to heading of
;; same or higher level, then repeaat.
(defun org-node-backlink--update-changed-parts-of-buffer ()
  (when org-node-backlink-mode
    ;; Catch any error because this runs at `before-save-hook' which MUST fail
    ;; gracefully and let the user save anyway
    (condition-case err
        (save-excursion
          (without-restriction
            ;; Iterate over each change-region.  Algorithm borrowed from
            ;; `ws-butler-map-changes', odd but elegant.  Worth knowing that if
            ;; you tell Emacs to search for text that has a given text-property
            ;; with a nil value, that's the same as searching for text without
            ;; that property at all.  So if START is in some unmodified
            ;; territory -- "org-node-chg" is valued at nil -- this way of
            ;; calling `text-property-not-all' means search until it is t.
            ;; Then the opposite happens, to search until it is nil again.
            (let ((start (point-min-marker))
                  (end (make-marker))
                  (case-fold-search t)
                  prop)
              (while (and start (< start (point-max)))
                (setq prop (get-text-property start 'org-node-chg))
                (set-marker end (or (text-property-not-all start
                                                           (point-max)
                                                           'org-node-chg
                                                           prop)
                                    (point-max)))
                (when prop
                  (goto-char start)
                  ;; START and END delineate an area where changes were
                  ;; detected, but the area rarely envelops the current
                  ;; subtree's property drawer, likely placed long before
                  ;; START, so search back for it
                  (save-excursion
                    (let ((id-here (org-entry-get nil "ID" t)))
                      (and id-here
                           ;; This search can fail if buffer is narrowed
                           (re-search-backward
                            (concat "^[[:space:]]*:id: *"
                                    (regexp-quote id-here))
                            nil t)
                           (re-search-forward ":id: *" (pos-eol))
                           (org-node-backlink--update-subtree-here))))
                  ;; ...and if the change-area is massive, spanning multiple
                  ;; subtrees, update each one
                  (while (and (< (point) end)
                              (re-search-forward
                               "^[[:space:]]*:id: " end t))
                    (org-node-backlink--update-subtree-here))
                  (remove-text-properties start end 'org-node-chg))
                ;; Move on and seek the next changed area
                (set-marker start (marker-position end)))
              (set-marker start nil)
              (set-marker end nil))))
      (( error debug )
       (if debug-on-error
           (signal (car err) (cdr err))
         (lwarn 'org-node :error
                "org-node-backlink--update-changed-parts-of-buffer: %S" err)))
      (( user-error debug )
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
    (org-node-cache-ensure)
    (org-node-backlink--add-in-target)))

(defun org-node-backlink--add-in-target ()
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
          (org-node-backlink--add-in-target-1 file id))))))

(defun org-node-backlink--add-in-target-1 (target-file target-id)
  (let ((case-fold-search t)
        (src-id (org-entry-get nil "ID" t)))
    (if (not src-id)
        (message "Unable to find ID in file, so it won't get backlinks: %s"
                 (buffer-file-name))
      (unless (equal src-id target-id)
        (let* ((src-title (save-excursion
                            (without-restriction
                              (re-search-backward (concat "^[ \t]*:id: +" src-id))
                              (or (org-get-heading t t t t)
                                  (org-get-title)
                                  (file-name-nondirectory (buffer-file-name))))))
               (src-link (org-link-make-string (concat "id:" src-id) src-title)))
          (org-node--with-file target-file
            (let ((otm (bound-and-true-p org-transclusion-mode)))
              (when otm (org-transclusion-mode 0))
              (goto-char (point-min))
              (if (not (re-search-forward
                        (concat "^[ \t]*:id: +" (regexp-quote target-id))
                        nil t))
                  (push target-id org-node-backlink--fails)
                (org-node-backlink--add-here src-link src-id)
                (when otm (org-transclusion-mode))))))))))

(defun org-node-backlink--add-here (src-link src-id)
  (let ((backlinks-string (org-entry-get nil "BACKLINKS"))
        new-value)
    (if backlinks-string
        ;; Build a temp list to check we don't add the same link twice.  There
        ;; is an Org builtin `org-entry-add-to-multivalued-property', but we
        ;; cannot use it since the link descriptions may contain spaces.
        (let ((links (split-string (replace-regexp-in-string
                                    "]][[:space:]]+\\[\\["
                                    "]]\f[["
                                    (string-trim backlinks-string))
                                   "\f" t)))
          (dolist (dup (--filter (string-search src-id it) links))
            (setq links (remove dup links)))
          (push src-link links)
          (when (-any-p #'null links)
            (org-node-die "nils in %S" links))
          ;; Enforce deterministic order to prevent unnecessarily reordering
          ;; every time a node is linked that already has the backlink
          (sort links #'string-lessp)
          ;; Two spaces between links help them look distinct
          (setq new-value (string-join links "  ")))
      (setq new-value src-link))
    (unless (equal backlinks-string new-value)
      (org-entry-put nil "BACKLINKS" new-value))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
