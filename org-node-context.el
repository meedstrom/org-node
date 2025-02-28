;;; org-node-context.el ---  -*- lexical-binding: t; -*-
;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'org)
(require 'org-node)
(require 'magit-section)

;; Reassure compiler
(defvar org-node-context-mode)

(defgroup org-node-context nil "Preview backlink contexts in separate buffer."
  :group 'org-node)


;;; Persistence

(defcustom org-node-context-persist-on-disk nil
  "Whether to sync cached backlink previews to disk.

This allows the context buffer created by \\[org-node-context-raise] to
show up more instantly, even the first time it renders a given set of
backlinks.

The backlinks are lumped into a single file in `org-node-data-dir'.

Benefit noticeable mainly if you are a connoisseur of low input
latency, have a bad computer, and often re-start Emacs."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defvar org-node-context--previews (make-hash-table :test #'equal)
  "1:N table mapping IDs to seen previews of backlink contexts.
For use by `org-node-fakeroam-fast-render-mode'.

Each preview is a cons cell \(POS-DIFF . TEXT) where POS-DIFF
corresponds to a link\\='s buffer position relative to that of
the heading that has said ID, and TEXT is an output of
`org-roam-preview-get-contents'.")

(defvar org-node-context--persist-timer (timer-create))
(defvar org-node-context--last-tbl-state 0)
(defvar org-node-context--did-init-persist nil)

(defun org-node-context--maybe-init-persistence (&rest _)
  "Try to restore `org-node-context--previews' from disk.
Then start occasionally syncing back to disk.
No-op if user option `org-node-context-persist-on-disk' is nil."
  (when org-node-context-persist-on-disk
    (unless org-node-context--did-init-persist
      (setq org-node-context--did-init-persist t)
      (cancel-timer org-node-context--persist-timer)
      (setq org-node-context--persist-timer
            (run-with-idle-timer 50 t #'org-node-context--persist))
      ;; Load from disk.
      (when (file-readable-p (org-node-context--persist-file))
        (with-temp-buffer
          (insert-file-contents (org-node-context--persist-file))
          (let ((data (read (current-buffer))))
            (when (hash-table-p data)
              (setq org-node-context--last-tbl-state (hash-table-count data))
              (setq org-node-context--previews data))))))))

(defun org-node-context--persist-file ()
  "Return path to file that caches previews between sessions."
  (mkdir org-node-data-dir t)
  (file-name-concat org-node-data-dir "org-node-backlink-previews.eld"))

(defun org-node-context--persist ()
  "Sync all cached previews to disk."
  (if org-node-context-persist-on-disk
      ;; Only proceed if table has gained new entries.
      ;; NOTE: Does not proceed if there are merely new previews in existing
      ;;       entries, but it's good enough this way.
      (when (not (eq org-node-context--last-tbl-state
                     (hash-table-count org-node-context--previews)))
        (org-node-cache-ensure t)
        (org-node-context--clean-stale-previews)
        (setq org-node-context--last-tbl-state
              (hash-table-count org-node-context--previews))
        (with-temp-file (org-node-context--persist-file)
          (let ((print-length nil))
            (prin1 org-node-context--previews (current-buffer)))))
    (cancel-timer org-node-context--persist-timer)
    (setq org-node-context--did-init-persist nil)))

(defun org-node-context--clean-stale-previews ()
  "Clean stale members in table `org-node-context--previews'.

Note that each entry in that table has potentially many previews,
but when this finds one of them stale, it removes that whole entry."
  (let ((valid-positions (make-hash-table :test #'equal)))
    (maphash
     (lambda (_ links)
       (dolist (link links)
         (push (plist-get link :pos)
               (gethash (plist-get link :origin) valid-positions))))
     org-node--dest<>links)

    (maphash
     (lambda (id previews)
       (let ((node (gethash id org-nodes))
             (valid (gethash id valid-positions)))
         (or (and node
                  (cl-loop
                   for (pos-diff . _text) in previews
                   always (memq (+ pos-diff (org-node-get-pos node)) valid)))
             (remhash id org-node-context--previews))))
     org-node-context--previews)))


;;; History navigation

;; TODO
(defvar org-node-context--remembered-state (make-hash-table :test #'equal)
  "Table associating IDs with remembered context states.

A context for a given node ID is the entirety of what would be rendered
in the context buffer when that node is visited.

The context state is information about user-interactable elements the last
time that context was shown in a visible window.  Including:
- Where was point
- Window scroll position
- The array of backlinks shown, and which sections were collapsed")

(defvar-local org-node-context--current nil)
;; FIXME: either all three local or none local
(defvar org-node-context--future nil)
(defvar org-node-context--past nil)

(defun org-node-context-history-go-back ()
  (interactive () org-node-context-mode)
  (when-let ((last (pop org-node-context--past)))
    (push org-node-context--current
          org-node-context--future)
    (org-node-context--refresh nil last)))

(defun org-node-context-history-go-forward ()
  (interactive () org-node-context-mode)
  (when-let ((next (pop org-node-context--future)))
    (push org-node-context--current
          org-node-context--past)
    (org-node-context--refresh nil next)))


;;; Porcelain

(defcustom org-node-context-collapse-more-than 5
  "How many backlinks before they should all start collapsed."
  :type '(choice natnum (const :value nil))
  :package-version '(org-node . "2.0.0"))

;; FIXME: Problem if truncating away a :END: or #+END_... but not begin.
(defcustom org-node-context-truncate-to-lines 18
  "Experimental.
Applies when the hook `org-node-context-postprocess-hook'
contains `org-node-context--truncate-buffer'."
  :type '(choice natnum (const :value nil))
  :package-version '(org-node . "2.0.0"))

(defun org-node-context--strip-meta-data ()
  "Delete any heading and properties drawers."
  (save-excursion
    (delete-region (point-min) (org-node--end-of-meta-data t))))

(defun org-node-context--strip-comments ()
  (save-excursion
    (goto-char (point-min))
    (while (not (eobp))
      (if (org-at-comment-p)
          (delete-line)
        (forward-line)))))

(defun org-node-context--expand-or-collapse ()
  "Expand or collapse sections depending on count of sections."
  ;;  Don't collapse if user has been browsing the buffer
  (unless (= (point) (point-min))
    (if (> (org-node-context--count-sections)
           org-node-context-collapse-more-than)
        (magit-section-show-level-2-all)
      (magit-section-show-level-3-all))))

(defun org-node-context--count-sections ()
  (let ((n-sections 0))
    (magit-map-sections (lambda (_) (cl-incf n-sections)))
    n-sections))

(defun org-node-context--truncate-buffer ()
  (when-let ((cutoff org-node-context-truncate-to-lines))
    (when (> (line-number-at-pos) cutoff)
      (forward-line (- cutoff))
      (delete-region (point-min) (point)))
    (when (> (line-number-at-pos (point-max)) cutoff)
      (goto-char (point-min))
      (forward-line cutoff)
      (delete-region (point) (point-max)))))

(defun org-node-context--restore-context-state ()
  "Should run near the end of `org-node-context-refresh-hook'."
  (when-let* ((id (oref (magit-current-section) dest-id))
              (state (gethash id org-node-context--remembered-state)))
    (seq-let ( num-sections pt win-pt ) state
      (when (= num-sections (org-node-context--count-sections))
        ;; FIXME: first restore collapse states
        (goto-char pt)
        (cl-assert (eq (selected-window) (get-buffer-window)))
        (set-window-point (selected-window) win-pt)
        ))))

(defcustom org-node-context-postprocess-hook
  (list #'org-node-context--strip-meta-data
        #'org-node-context--strip-comments
        ;; #'org-node-context--truncate-buffer
        )
  "Hook run in a temp buffer containing a backlink preview snippet.
This can be used to transform the snippet to a desired appearance.

Point is inside the link for which the preview snippet is being
generated.

Font-locking is NOT in effect at this time, so there are no text
properties."
  :type 'hook
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-context-refresh-hook
  (list #'org-node-context--expand-or-collapse
        ;; #'org-node-context--restore-context-state
        )
  "Hook run in a context buffer after refreshing it."
  :type 'hook
  :package-version '(org-node . "2.0.0"))

(defface org-node-context-origin-title
  '((((type nil)) ;; On a terminal.
     :extend t
     :inherit org-document-title)
    (t ;; On GUI.
     :extend t
     :height 1.5))
  "Face for backlink node titles in the context buffer."
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-context-main-buffer "*Org-Node Context*"
  "Name of the context buffer."
  :type 'string
  :package-version '(org-node . "2.0.0"))

;;;###autoload
(define-minor-mode org-node-context-follow-mode
  "Make the context buffer update when point moves to a different entry.

-----"
  :require 'org-node
  (cond
   ((not org-node-context-follow-mode)
    (remove-hook 'post-command-hook #'org-node-context--try-refresh t))
   ((not (and (derived-mode-p 'org-mode)
              buffer-file-name))
    (org-node-context-follow-mode 0))
   (t
    (add-hook 'post-command-hook #'org-node-context--try-refresh nil t))))

;;;###autoload
(define-globalized-minor-mode org-node-context-follow-global-mode
  org-node-context-follow-mode
  org-node-context-follow-mode)

(defvar-keymap org-node-context-mode-map
  :parent magit-section-mode-map
  "<return>"                #'org-node-context-visit-thing
  "C-m"                     #'org-node-context-visit-thing
  "l"                       #'org-node-context-history-go-back
  "r"                       #'org-node-context-history-go-forward
  "<remap> <revert-buffer>" #'org-node-context-refresh-this-buffer)

(define-derived-mode org-node-context-mode magit-section-mode
  "org-node context"
  "Major mode for the context buffer."
  (when (or (member #'visual-line-mode org-mode-hook)
            (member #'visual-line-mode text-mode-hook))
    (visual-line-mode)))

(defclass org-node-context-section (magit-section)
  ((dest-id :initform nil)
   (file :initform nil)
   (link-pos :initform nil)))

(defun org-node-context-visit-thing ()
  (interactive nil org-node-context-mode)
  (unless (derived-mode-p 'org-node-context-mode)
    (user-error "`org-node-context-visit-thing' called in non org-node-context-mode buffer"))
  (let ((file     (oref (magit-current-section) file))
        (link-pos (oref (magit-current-section) link-pos)))
    (find-file file)
    (when link-pos
      (goto-char link-pos)
      (recenter))))

;;;###autoload
(defun org-node-context-raise ()
  (interactive)
  (let ((buf org-node-context-main-buffer))
    (cond
     ((derived-mode-p 'org-node-context-mode)
      (save-excursion
        (org-node-context--refresh)))

     ((get-buffer-window buf 'visible)
      (if (derived-mode-p 'org-mode)
          (org-node-context--ensure-context-is-for-here)
        (org-node-context--refresh)))

     ((get-buffer buf)
      (let ((display-buffer-overriding-action
             '(( display-buffer-in-previous-window
                 display-buffer-pop-up-window )
               (inhibit-same-window . t))))
        (display-buffer buf)))

     ((derived-mode-p 'org-mode)
      (org-node-context--refresh (get-buffer-create buf)
                                 (org-entry-get-with-inheritance "ID"))
      (display-buffer buf))

     (t
      (message "Found no context buffer, visit an org-mode buffer first")))))

;;;###autoload
(defun org-node-context-toggle ()
  (interactive)
  (if-let ((win (get-buffer-window org-node-context-main-buffer 'visible)))
      (quit-window nil win)
    (let ((buf (get-buffer-create org-node-context-main-buffer)))
      (org-node-context--refresh buf (org-entry-get-with-inheritance "ID"))
      (display-buffer buf))))

;; TODO: Use any buffer that may be rendering the correct context
(defun org-node-context--try-refresh ()
  "For `post-command-hook' in an Org-mode buffer."
  (when (get-buffer-window org-node-context-main-buffer 'visible)
    (org-node-context--ensure-context-is-for-here)))

(defun org-node-context--ensure-context-is-for-here ()
  (let ((id (org-entry-get-with-inheritance "ID")))
    (and id
         (not (org-node-context--displaying-p nil id))
         (org-node-context--refresh org-node-context-main-buffer id))))

;; TODO REVIEW is it important to hypothetically be able to show multiple
;;             contexts in one buffer?
(defun org-node-context--displaying-p (buf id)
  (when-let ((buf (get-buffer (or buf org-node-context-main-buffer))))
    (equal id (buffer-local-value 'org-node-context--current buf))))

(defun org-node-context-refresh-this-buffer ()
  (interactive nil org-node-context-mode)
  (cl-assert org-node-context-mode)
  (org-node-context--refresh (current-buffer)))


;;; Plumbing

(defun org-node-context--refresh (&optional buf id)
  "Refresh buffer BUF to show context for node known by ID.

If argument BUF not supplied, use `org-node-context-main-buffer'.
If argument ID not supplied, just refresh the context already shown in
that buffer."
  (org-node-context--maybe-init-persistence)
  (with-current-buffer (get-buffer-create (or buf org-node-context-main-buffer))
    (let ((inhibit-read-only t))
      (when (not id)
        ;; NOTE: Can still be nil initially.
        (setq id org-node-context--current))
      (when (and id (get-buffer-window))
        (puthash id
                 (list (point)
                       (window-point)
                       (org-node-context--count-sections))
                 org-node-context--remembered-state)
        (when org-node-context--current
          (push org-node-context--current org-node-context--past)))
      (erase-buffer)
      (org-node-context-mode)
      (setq-local org-node-context--current id)
      (let ((node (gethash id org-nodes)))
        (unless node
          (error "org-node-context: ID not known: %s" id))
        (magit-insert-section this
          ( org-node-context-section )
          (oset this file (org-node-get-file node))
          (magit-insert-heading
            (concat "Context for " (org-node-get-title node)))
          (when-let ((links (org-node-get-id-links-to node)))
            (magit-insert-section this
              ( org-node-context-section )
              (oset this file (org-node-get-file node))
              (magit-insert-heading "ID backlinks:")
              (dolist (link (sort links #'org-node-context--origin-title-lessp))
                (org-node-context--insert-backlink link))
              (insert "\n")))
          (when-let ((links (org-node-get-reflinks-to node)))
            (magit-insert-section this
              ( org-node-context-section )
              (oset this file (org-node-get-file node))
              (magit-insert-heading "Ref backlinks:")
              (dolist (link (sort links #'org-node-context--origin-title-lessp))
                (org-node-context--insert-backlink link))
              (insert "\n"))))
        (org-node-context--kill-work-buffers)
        (run-hooks 'org-node-context-refresh-hook)))))

(defun org-node-context--insert-backlink (link)
  "Insert a magit-section displaying a preview of LINK."
  (let* ((node (or (gethash (plist-get link :origin) org-nodes)
                   (error "Origin not found for link: %S" link)))
         (breadcrumbs (if-let ((olp (org-node-get-olp-full node)))
                          (string-join olp " > ")
                        "Top")))
    (magit-insert-section this
      ( org-node-context-section )
      (oset this file (org-node-get-file node))
      (oset this link-pos (plist-get link :pos))
      (magit-insert-heading
        (format "%s (%s)"
                (propertize (org-node-get-title node)
                            'face
                            'org-node-context-origin-title)
                (propertize breadcrumbs 'face 'completions-annotations)))
      (insert (org-node--get-preview-snippet node link))
      (insert "\n"))))

(defvar org-node-context--snippet-link)
(defun org-node--get-preview-snippet (node link)
  "Get a preview snippet out of NODE file, where LINK is.

Actually, if a snippet was previously cached, return the cached version,
else briefly visit the file at LINK-POS and call
`org-node-context--extract-entry-at-point'."
  (let* ((id (org-node-get-id node))
         (link-pos (plist-get link :pos))
         ;; NOTE: `pos-diff' is not necessary in a simple implementation, but
         ;; this level of granularity lets us avoid wiping all cached previews
         ;; in a large file every time it is saved -- doing so would make the
         ;; cache useless, when you are working in a large file with links
         ;; between parts of itself.
         ;;
         ;; Instead, we just don't wipe anything, and trust in a sloppy rule of
         ;; thumb: when the text between a link and its heading get edited,
         ;; that will almost always result in a new unique `pos-diff'.
         (pos-diff (- link-pos (org-node-get-pos node))))
    (or (alist-get pos-diff (gethash id org-node-context--previews))
        (setf
         (alist-get pos-diff (gethash id org-node-context--previews))
         (let (snippet)
           (with-current-buffer (org-node-context--work-buffer-for
                                 (org-node-get-file node))
             (goto-char link-pos)
             (setq snippet (org-node-context--extract-entry-at-point)))
           (with-current-buffer (org-node-context--general-org-work-buffer)
             (erase-buffer)
             (insert snippet)
             (goto-char pos-diff)
             (run-hooks 'org-node-context-postprocess-hook)
             ;; Finally font-lock now that we are in a tiny buffer that
             ;; contains only the snippet that needs to be font-locked, not the
             ;; entire source file.
             ;;
             ;; It would be nice to do this before to the postprocess hook
             ;; instead of after, to offer the possibility for users to
             ;; override colors or something, but we'd have to run
             ;; `font-lock-ensure' again on principle (as the hook could be
             ;; used to add text), and then IIUC all text properties get reset.
             (font-lock-ensure)
             (buffer-string)))))))

(defun org-node-context--extract-entry-at-point ()
  (save-excursion
    (string-trim (buffer-substring-no-properties
                  (org-back-to-heading-or-point-min)
                  (or (outline-next-heading) (point-max))))))

(defun org-node-context--origin-title-lessp (link-1 link-2)
  "Return t if LINK-1 should be sorted before LINK-2.

Decide this by getting the titles of the nodes wherein the links were
found, and checking if the first title would come lexicographically
before the second title."
  (string<
   (org-node-get-title (gethash (plist-get link-1 :origin) org-nodes))
   (org-node-get-title (gethash (plist-get link-2 :origin) org-nodes))))

(defvar org-node-context--work-buffers nil
  "All buffers spawned by `org-node-context--work-buffer-for'.")

(defun org-node-context--kill-work-buffers ()
  "Kill all buffers in `org-node-context--work-buffers'."
  (while-let ((buf (pop org-node-context--work-buffers)))
    (kill-buffer buf)))

;; TODO: Either duplicate the org element cache of FILE,
;;       or just actually visit FILE.
;;       It would permit us to use the org element API without incurring a
;;       massive performance hit,
;;       since the parse tree would typically already be cached (one hopes).
;;       (If we go the route of actually visiting the file, can this be merged
;;       with `org-node--with-quick-file-buffer'?)
(defun org-node-context--work-buffer-for (file)
  "Get or create a hidden buffer, and read FILE contents into it.
Also enable `org-mode', but ignore `org-mode-hook' and startup options.

The buffer persists, so repeated calls with the same FILE skip the
overhead of creation.  To clean up, call
`org-node-context--kill-work-buffers' explicitly."
  (let ((bufname (format " *org-node-context-%d*" (sxhash file)))
        (org-inhibit-startup t)
        (org-element-cache-persistent nil))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname t)
          (push (current-buffer) org-node-context--work-buffers)
          (delay-mode-hooks (org-mode))
          (insert-file-contents file)
          ;; Ensure that attempted edits trip an error, since the buffer may be
          ;; reused any numbmer of times, and should always reflect FILE.
          (setq-local buffer-read-only t)
          (current-buffer)))))

(defun org-node-context--general-org-work-buffer ()
  "Get or create a hidden `org-mode' buffer.

Like a temp buffer, but never dies.  You should probably use
`erase-buffer' in case it already contains text.  Then finish up with
`font-lock-ensure' if you need the contents fontified."
  (let ((bufname " *org-node-work*")
        (org-inhibit-startup t)
        (org-element-cache-persistent nil))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname t)
          (delay-mode-hooks (org-mode))
          (current-buffer)))))

(provide 'org-node-context)

;;; org-node-context.el ends here
