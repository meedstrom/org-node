;;; org-node-backlink.el --- Manage :BACKLINKS: properties or drawers -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; A mode for ensuring that the Org nodes that should have
;; :BACKLINKS: properties (or :BACKLINKS: drawers) have them,
;; and are up to date.

;;; Code:

(require 'org-node)
(require 'org-node-changes)
(require 'cl-lib)
(require 'compat)

(defgroup org-node-backlink nil "In-file backlinks."
  :group 'org-node)

(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer .1 nil #'display-warning 'org-node
                      "Your config may have misspelled `org-node-backlink-mode' as `org-node-backlinks-mode'"))
    (apply #'org-node-backlink-mode args)))

(defcustom org-node-backlink-do-drawers nil
  "Experimental setting."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))


;;; Global minor mode

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated.

See Info node `(org-node)'.

-----"
  :global t
  (when (member #'org-node-backlink-mode org-mode-hook)
    (display-warning
     'org-node "Since 2024-10-22, `org-node-backlink-mode' is a global mode, but your initfiles still add it to `org-mode-hook'")
    (remove-hook 'org-mode-hook #'org-node-backlink-mode))
  (if org-node-backlink-mode
      (progn
        (advice-add 'org-insert-link :after  #'org-node-backlink--add-in-target)
        (add-hook 'org-node-rescan-functions #'org-node-backlink--maybe-fix-aggressively)
        (add-hook 'org-mode-hook             #'org-node-backlink--local-mode)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-node-backlink--local-mode)))))
    (advice-remove 'org-insert-link          #'org-node-backlink--add-in-target)
    (remove-hook 'org-node-rescan-functions  #'org-node-backlink--maybe-fix-aggressively)
    (remove-hook 'org-mode-hook              #'org-node-backlink--local-mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (org-node-backlink--local-mode 0)))))

;; Not sure if it is recommended to add all hooks globally rather than make a
;; local mode like this just to setup buffers locally.  Going by what I'd
;; prefer, which is to not see so many potential no-ops on hooks.
(define-minor-mode org-node-backlink--local-mode
  "Buffer-local part of `org-node-backlink-mode'.

NOT a local equivalent of aforementioned global mode, but adds a set of
buffer-local hooks to the current buffer, in addition to the global
hooks added by the global mode.  Enabling/disabling the global mode will
also enable/disable this mode in relevant buffers.

In short, this mode is not meant to be toggled on its own.

-----"
  :interactive nil
  (if org-node-backlink--local-mode
      (progn
        (add-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-in-target nil t)
        (add-hook 'org-node-insert-link-hook      #'org-node-backlink--add-in-target nil t)
        (add-hook 'after-change-functions         #'org-node-backlink--flag-buffer-modification nil t)
        (add-hook 'before-save-hook               #'org-node-backlink--fix-flagged-parts-of-buffer nil t))

    (remove-hook 'org-roam-post-node-insert-hook  #'org-node-backlink--add-in-target t)
    (remove-hook 'org-node-insert-link-hook       #'org-node-backlink--add-in-target t)
    (remove-hook 'after-change-functions          #'org-node-backlink--flag-buffer-modification t)
    (remove-hook 'before-save-hook                #'org-node-backlink--fix-flagged-parts-of-buffer t)))


;;; Buffer validation

(defvar org-node-backlink--fix-ctr 0)
(defvar org-node-backlink--files-to-fix nil)

;;;###autoload
(defun org-node-backlink-regret ()
  "Visit all `org-id-locations' and remove :BACKLINKS: property."
  (interactive)
  (org-node-backlink-mode 0)
  (org-node-backlink-fix-all-files 'remove))

;;;###autoload
(defun org-node-backlink-fix-all-files (&optional remove)
  "Add :BACKLINKS: property to all known nodes.
Optional argument REMOVE t means remove them instead, the same
as the user command \\[org-node-backlink-regret].

Can be quit midway through and resumed later.  With
\\[universal-argument], start over instead of resuming."
  (interactive)
  (org-node-cache-ensure)

  ;;TODO
  (if (not org-node-backlink-do-drawers)
      (y-or-n-p "Leave drawers as they are or delete?"))
  (if (cl-loop for node being each hash-value of org-nodes
               when (assoc "BACKLINKS" (org-node-get-props node)) do
               (org-node-backlink-mode 0)
               (user-error "Disabling" )))

  (when (or (equal current-prefix-arg '(4))
            (and (null org-node-backlink--files-to-fix)
                 (y-or-n-p (format "Edit %d Org files?"
                                   (length (org-node-list-files t))))
                 (y-or-n-p
                  (string-fill "You understand that this may trigger your auto git-commit systems and similar because many files are about to be edited and saved?"
                               (default-value 'fill-column)))))
    (setq org-node-backlink--files-to-fix (org-node-list-files t)))
  (when org-node-backlink--files-to-fix
    (if remove
        (setq org-node-backlink--files-to-fix
              (org-node--in-files-do
                :files org-node-backlink--files-to-fix
                :msg "Removing :BACKLINKS: (you may quit and resume anytime)"
                :about-to-do "About to remove backlinks"
                :call (lambda () (org-node-backlink-fix-buffer t))
                :too-many-files-hack t))
      (setq org-node-backlink--files-to-fix
            (org-node--in-files-do
              :files org-node-backlink--files-to-fix
              :msg "Adding/updating :BACKLINKS: (you may quit and resume anytime)"
              :about-to-do "About to edit backlinks"
              :call #'org-node-backlink-fix-buffer
              :too-many-files-hack t)))
    (when (null org-node-backlink--files-to-fix)
      (message "Done editing :BACKLINKS: properties!"))))

(defun org-node-backlink-fix-buffer (&optional remove)
  "Update :BACKLINKS: property on all nodes in buffer.
If REMOVE is non-nil, remove the property."
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search t))
    ;; NOTE: If there is an entry that has :BACKLINKS:, but the :ID: has been
    ;;       removed, it will never be touched again, but that's on the user.
    (while (re-search-forward "^[\t\s]*:id: " nil t)
      ;; (if remove
      ;;     (progn (org-node-backlink--fix-nearby-drawer remove)
      ;;            (org-node-backlink--fix-nearby-property remove)))
      (if org-node-backlink-do-drawers
          (org-node-backlink--fix-nearby-drawer remove)
        (org-node-backlink--fix-nearby-property remove)))))

(defun org-node-backlink--fix-nearby-property (&optional remove)
  "Update the :BACKLINKS: property in the current entry.
If REMOVE is non-nil, remove it instead."
  (if remove
      (org-entry-delete nil "BACKLINKS")
    (let* ((id (org-entry-get nil "ID"))
           (node (gethash id org-nodes)))
      (if (not (and id node))
          (org-entry-delete nil "BACKLINKS")
        (let* ((origins (thread-last
                          (append (org-node-get-id-links-to node)
                                  (org-node-get-reflinks-to node))
                          (mapcar (##plist-get % :origin))
                          (delete-dups)
                          ;; Sort for deterministic order for less noisy diffs.
                          (compat-call sort)))
               (links (cl-loop
                       for origin in origins
                       as origin-node = (gethash origin org-nodes)
                       when origin-node
                       collect (org-link-make-string
                                (concat "id:" origin)
                                (org-node-get-title origin-node))))
               (links-string (string-join links "  ")))
          (if links
              (unless (equal links-string (org-entry-get nil "BACKLINKS"))
                (org-entry-put nil "BACKLINKS" links-string))
            (org-entry-delete nil "BACKLINKS")))))))

(defun org-node-backlink--flag-buffer-modification (beg end _n-deleted-chars)
  "Add text property `org-node-flag' to region between BEG and END.

Designed for `after-change-functions', so this effectively flags
all areas where text is added/changed/deleted.  Where text was
purely deleted, this flags the preceding and succeeding char."
  (with-silent-modifications
    (if (= beg end)
        (put-text-property (max (1- beg) (point-min))
                           (min (1+ end) (point-max))
                           'org-node-flag t)
      (put-text-property beg end 'org-node-flag t))))

(defun org-node-backlink--fix-flagged-parts-of-buffer ()
  "Fix backlinks around parts of buffer that have been modified.

Look for areas flagged by
`org-node-backlink--flag-buffer-modification' and run
`org-node-backlink--fix-nearby-property' at each affected heading.
For a huge file, this is much faster than using
`org-node-backlink-fix-buffer' -- imagine a thousand
headings but you have only done work under one of them."
  (when (derived-mode-p 'org-mode)
    ;; Catch any error because this runs at `before-save-hook' which MUST fail
    ;; gracefully and let the user save anyway
    (condition-case err
        (save-excursion
          (without-restriction
            ;; Iterate over each change-region.  Algorithm borrowed from
            ;; `ws-butler-map-changes', odd but elegant.  Worth knowing that if
            ;; you tell Emacs to search for text that has a given text-property
            ;; with a nil value, that's the same as searching for text without
            ;; that property at all.  So if position START is in some
            ;; unmodified area -- "org-node-flag" is effectively valued at nil
            ;; -- this way of calling `text-property-not-all' means search
            ;; forward until it is t.  Then calling it again has the opposite
            ;; effect, searching until it is nil again.
            (let ((start (point-min-marker))
                  (end (make-marker))
                  (case-fold-search t)
                  prop)
              (while (< start (point-max))
                (setq prop (get-text-property start 'org-node-flag))
                (set-marker end (or (text-property-not-all
                                     start (point-max) 'org-node-flag prop)
                                    (point-max)))
                (when prop
                  (goto-char start)
                  ;; START and END delineate an area where changes were
                  ;; detected, but the area rarely envelops the current
                  ;; subtree's property drawer, likely placed long before
                  ;; START, so search back for it
                  (save-excursion
                    (let ((id-here (org-entry-get-with-inheritance "ID")))
                      (and id-here
                           (re-search-backward
                            (concat "^[\t\s]*:id: +"
                                    (regexp-quote id-here))
                            nil t)
                           (org-node-backlink--fix-nearby-property))))
                  ;; ...and if the change-area is massive, spanning multiple
                  ;; subtrees (like after a big yank), update each subtree
                  ;; within
                  (while (and (< (point) end)
                              (re-search-forward
                               "^[\t\s]*:id: +" end t))
                    (org-node-backlink--fix-nearby-property))
                  (remove-text-properties start end 'org-node-flag))
                ;; This change-area dealt with, move on
                (set-marker start (marker-position end)))
              (set-marker start nil)
              (set-marker end nil))))
      (( t error debug )
       (remove-text-properties (point-min) (point-max) 'org-node-flag)
       (message "org-node: Updating backlinks ran into an issue: %S" err)
       ;; Provide backtrace even tho we don't signal an error
       (when debug-on-error
         (backtrace))))))


;;; Link-insertion advice

;; This logic is independent from the per-buffer validation, because that
;; operates on the file being saved -- in other words, making the file
;; navel-gaze its own content to see if it looks correct according to current
;; links tables.  Technically, that would be enough to result in correct
;; backlinks everywhere if you just run it on all files, and that's
;; more-or-less how `org-node-backlink-fix-all-files' works, but we don't want
;; to do that on every save.

;; By contrast, the below code does not look up tables, just reacts to the
;; exact link being inserted, which has two benefits:

;; 1. You can observe backlinks appearing in realtime before a buffer is saved

;; 2. It's actually necessary, because a link being inserted does not mean we
;;    should check the current file but rather visit and edit the target file.
;;    If we didn't have the below code, we'd have save the current buffer (in
;;    order to update tables) and then open the target file and run
;;    `org-node-backlink-fix-buffer', which can easily take a while for
;;    a big target file.

;; TODO: Report when it has members
(defvar org-node-backlink--fails nil
  "List of IDs that could not be resolved.")

(defun org-node-backlink--add-in-target (&rest _)
  "For known link at point, leave a backlink in the target node."
  (require 'org-element)
  (unless (derived-mode-p 'org-mode)
    (error "Backlink function called in non-Org buffer"))
  (org-node-cache-ensure)
  (let* ((elm (org-element-context))
         (path (org-element-property :path elm))
         (type (org-element-property :type elm))
         dest-id dest-file)
    ;; In a link such as [[id:abc1234]], TYPE is "id" and PATH is "abc1234".
    (when (and type path)
      (if (equal "id" type)
          ;; A classic backlink
          (progn
            (setq dest-id path)
            ;; FIXME !!! wtf is this fallback behavior
            (setq dest-file (org-id-find-id-file dest-id)))
        ;; A "reflink"
        (setq dest-id (gethash path org-node--ref<>id))
        (setq dest-file (ignore-errors
                          (org-node-get-file
                           (gethash dest-id org-nodes)))))
      (when (null dest-file)
        (push dest-id org-node-backlink--fails))
      (when (and dest-id dest-file)
        (let ((case-fold-search t)
              (origin-id (org-entry-get-with-inheritance "ID")))
          (when (and origin-id (not (equal origin-id dest-id)))
            (let ((origin-title
                   (save-excursion
                     (without-restriction
                       ;; This search can fail when point is on the heading
                       ;; with the id-property below, but then point is in the
                       ;; right place anyway.
                       (re-search-backward (concat "^[ \t]*:id: +" origin-id)
                                           nil t)
                       (or (org-get-heading t t t t)
                           (cadar (org-collect-keywords '("TITLE")))
                           (file-name-nondirectory buffer-file-name))))))
              ;; Ensure that
              ;; `org-node-backlink--fix-flagged-parts-of-buffer' will not
              ;; later remove the backlink we're adding
              (org-node--dirty-ensure-node-known)
              (org-node--with-quick-file-buffer dest-file
                :about-to-do "Org-node going to add backlink to the target of the link you just inserted"
                (when (and (boundp 'org-transclusion-exclude-elements)
                           (not (memq 'property-drawer
                                      org-transclusion-exclude-elements)))
                  (error "org-node-backlink-mode: List `org-transclusion-exclude-elements' must include `property-drawer'"))
                (goto-char (point-min))
                (let ((case-fold-search t))
                  (if (re-search-forward (concat "^[ \t]*:id: +"
                                                 (regexp-quote dest-id))
                                         nil
                                         t)
                      (cond
                       ((get-text-property (point) 'read-only)
                        ;; If for some reason the search landed us in a
                        ;; transclude region or other read-only area...  Note
                        ;; that `org-entry-put' inhibits read-only, so it
                        ;; wouldn't signal any error, dangerous because we
                        ;; don't know why it's read-only!
                        (error "org-node: Property drawer seems to be read-only at %d in %s"
                               (point) (buffer-name)))
                       (org-node-backlink-do-drawers
                        (org-node-backlink--add-link-to-drawer origin-id
                                                               origin-title))
                       (t
                        (org-node-backlink--add-to-property origin-id
                                                            origin-title)))
                    (push dest-id org-node-backlink--fails)))))))))))

(defun org-node-backlink--add-to-property (id title)
  "Compose a link string out of ID and TITLE and insert
it in the nearby :BACKLINKS: property."
  (let ((current-backlinks-value (org-entry-get nil "BACKLINKS"))
        (new-link (org-link-make-string (concat "id:" id) title))
        new-value)
    (and current-backlinks-value
         (string-search "\f" current-backlinks-value)
         (error "Form-feed character in BACKLINKS property near %d in %s"
                (point) (buffer-name)))
    (if current-backlinks-value
        ;; Build a temp list to check we don't add the same link twice.
        ;; There is an Org builtin `org-entry-add-to-multivalued-property',
        ;; but we cannot use it since the link descriptions may contain
        ;; spaces.  Further, they may contain quotes(!), so we cannot use
        ;; `split-string-and-unquote' even if we had wrapped the links in
        ;; quotes.
        (let ((links (split-string (replace-regexp-in-string
                                    "]][\s\t]+\\[\\["
                                    "]]\f[["
                                    (string-trim current-backlinks-value))
                                   "\f")))
          (cl-loop for link in links
                   when (string-search id link)
                   do (setq links (delete link links)))
          (push new-link links)
          (when (seq-some #'null links)
            (org-node--die "nils in %S" links))
          ;; Enforce deterministic order to prevent unnecessary reordering
          ;; every time a node is linked that already has the backlink
          (sort links #'string-lessp)
          (setq new-value (string-join links "  ")))
      ;; Only one link
      (setq new-value new-link))
    (unless (equal new-value current-backlinks-value)
      (let ((user-is-editing (buffer-modified-p))
            ;; Prevent reacting to this edit (redundant)
            (after-change-functions
             (remq 'org-node-backlink--flag-buffer-modification
                   after-change-functions)))
        (org-entry-put nil "BACKLINKS" new-value)
        (unless user-is-editing
          (let ((before-save-hook nil)
                (after-save-hook nil))
            (save-buffer)))))))


;;;; Aggressive visit-and-fix

(defcustom org-node-backlink-aggressive nil
  "On save, detect added/deleted links and fix backlinks.

Only has an effect as long as `org-node-backlink-mode' is enabled.

Normally, links added via most commands will also insert a backlink in
real time, but stale backlinks are not cleaned until you carry out some
edits under the heading that has the stale backlink, and save that
buffer.

When t, all affected nodes will be visited silently to update the
:BACKLINKS: properties.

To clarify, this is about the textual contents of :BACKLINKS: properties;
the underlying link tables are up to date anyway.  This defaults to
nil to avoid unnecessary file visitations.

If t, `org-node-perf-eagerly-update-link-tables' must be t as well
\(default).

Minor side effect: `org-element-cache-reset' is called in the buffers
where backlinks are fixed."
  :type 'boolean
  :package-version '(org-node . "1.1.0"))

(defun org-node-backlink--maybe-fix-aggressively (_)
  "Designed for `org-node-rescan-functions'."
  (when org-node-backlink-aggressive
    (if (not org-node-perf-eagerly-update-link-tables)
        (message "Option `org-node-backlink-aggressive' has no effect when `org-node-perf-eagerly-update-link-tables' is nil")
      (let (affected-dests)
        (cl-loop
         for (dest . old-links) in org-node--old-link-sets
         when (cl-set-exclusive-or
               (mapcar (##plist-get % :origin)
                       old-links)
               (mapcar (##plist-get % :origin)
                       (gethash dest org-node--dest<>links))
               :test #'equal)
         do (let* ((id dest)
                   (node (or (gethash id org-nodes)
                             (and (setq id (gethash dest org-node--ref<>id))
                                  (gethash id org-nodes)))))
              ;; (#59) Do nothing if this is an empty link like [[id:]]
              (when node
                ;; Add to the dataset `affected-dests', which looks like:
                ;;   ((file1 . (origin1 origin2 origin3 ...))
                ;;    (file2 . (...))
                ;;    (file3 . (...)))
                (push id (alist-get (org-node-get-file node)
                                    affected-dests
                                    nil
                                    nil
                                    #'equal)))))
        (setq org-node--old-link-sets nil)
        (cl-loop
         for (file . ids) in affected-dests
         when (and (file-readable-p file)
                   (file-writable-p file))
         do (org-node--with-quick-file-buffer file
              :about-to-do "About to fix backlinks"
              (let ((user-is-editing (buffer-modified-p))
                    (case-fold-search t))
                (dolist (id (delete-dups ids))
                  (goto-char (point-min))
                  (when (re-search-forward
                         (concat "^[\t\s]*:id: +" (regexp-quote id))
                         nil t)
                    (org-node-backlink--fix-nearby-property)))
                (unless user-is-editing
                  ;; Normally, `org-node--with-quick-file-buffer' only saves
                  ;; buffers it had to open anew.  Let's save even if it was
                  ;; open.
                  (let ((before-save-hook nil)
                        (after-save-hook nil))
                    (save-buffer))))))))))


;;; Drawers

;; (defcustom org-node-backlink-drawer-name "BACKLINKS"
;;   "
;; Note that you will have to carry out renames yourself,
;; with e.g. \\[project-query-replace-regexp]
;; or \\[multi-file-replace-regexp-as-diff]."
;;   :type 'string
;;   :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-sorter
  #'org-node-backlink-timestamp-lessp
  "Function for sorting lines in the backlinks drawer."
  :type '(radio
          (function-item org-node-backlink-timestamp-lessp)
          (function-item org-node-backlink-link-description-lessp)
          (function-item org-node-backlink-id-blind-simple-lessp)
          (function-item string-lessp)
          (function :tag "Custom function"))
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-sort-in-reverse nil
  "Whether to reverse how lines are sorted in the backlinks drawer."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-line-formatter
  #'org-node-backlink-format-like-org-super-links-default
  "Function to format a new line for the backlinks drawer.

It takes three arguments ID, TITLE, and optionally TIME.

The function should beware not to add:
- Initial whitespace.
- More than one ID per line."
  :type '(radio
          (function-item org-node-backlink-format-as-bullet-with-time)
          (function-item org-node-backlink-format-as-bullet-no-time)
          (function-item org-node-backlink-format-like-org-super-links-default)
          (function :tag "Custom function"))
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-put-drawer-near-bottom nil
  "When creating a BACKLINKS drawer, put it below all text in the entry."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defconst org-node-context--timestamp-regexp "[[<]\\([[:digit:]]\\{4\\}-[[:digit:]]\\{2\\}-[[:digit:]]\\{2\\}\\(?: .*?\\)?\\)[]>]\\|\\(?:<[0-9]+-[0-9]+-[0-9]+[^>
]+?\\+[0-9]+[dwmy]>\\)\\|\\(?:<%%\\(?:([^>
]+)\\)\\([^
>]*\\)>\\)"
  "Copy of `org-element--timestamp-regexp'.")

(defun org-node-backlink--extract-timestamp (text)
  (when (string-match org-node-context--timestamp-regexp text)
    (match-string 0 text)))

(defun org-node-backlink--extract-id (text)
  (with-temp-buffer
    (insert text)
    (goto-char 1)
    (when (search-forward "[[id:" nil t)
      (buffer-substring (point) (- (search-forward "]") 1)))))

(defun org-node-backlink--extract-link-desc (text)
  (with-temp-buffer
    (insert text)
    (goto-char 1)
    (when (and (search-forward "[[id:" nil t)
               (search-forward "][" nil t))
      (buffer-substring (point) (- (search-forward "]]") 2)))))

(defun org-node-backlink-timestamp-lessp (line-1 line-2)
  "Sort by Org timestamp, wherever in the line that is."
  (let ((ts-1 (org-node-backlink--extract-timestamp line-1))
        (ts-2 (org-node-backlink--extract-timestamp line-2)))
    (or (and ts-1 (not ts-2))
        (and ts-1 ts-2 (org-time< ts-1 ts-2)))))

(defun org-node-backlink-link-description-lessp (line-1 line-2)
  "Sort by link description, wherever in the line that is."
  (string-lessp (org-node-backlink--extract-link-desc line-1)
                (org-node-backlink--extract-link-desc line-2)))

(defun org-node-backlink-id-blind-simple-lessp (line-1 line-2)
  "Sort lexicographically, but ignoring nonsense inside [[id:...]]."
  (string< (replace-regexp-in-string "\\[\\[id:.*?]" "" line-1)
           (replace-regexp-in-string "\\[\\[id:.*?]" "" line-2)))

(defun org-node-backlink-format-like-org-super-links-default (id desc &optional time)
  "Construct a string out of ID, DESC, and TIME.

The result can look like:
\"[2025-02-21 Fri 14:39] <- [[id:ID][DESC]]\""
  (concat (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " <- "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-with-time (id desc &optional time)
  "Construct a string out of ID, DESC and TIME.

The result can look like:
\"+ [2025-02-21 Fri 14:39] [[id:ID][DESC]]\""
  (concat "+ "
          (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-no-time (id desc &optional _time)
  "Construct a string out of ID and DESC.

The result can look like:
\"+ [[id:ID][DESC]]\""
  (concat "+ " (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink--reformat-line (line)
  (funcall org-node-backlink-drawer-line-formatter
           (org-node-backlink--extract-id line)
           (org-node-backlink--extract-link-desc line)
           (org-node-backlink--extract-timestamp line)))

;; For link-insertion advice
(defun org-node-backlink--add-link-to-drawer (id title)
  (save-restriction
    (org-node--narrow-to-drawer-create
     "BACKLINKS" org-node-backlink-put-drawer-near-bottom)
    (catch 'break
      (let (lines)
        (while (search-forward "[[id:" (pos-eol) t)
          (let ((id-found (buffer-substring-no-properties
                           (point)
                           (1- (search-forward "]")))))
            (if (equal id-found id)
                ;; No need to do anything
                (throw 'break nil)
              (push (buffer-substring-no-properties (pos-bol) (pos-eol))
                    lines)
              (forward-line 1))))
        (insert "\n"
                (funcall org-node-backlink-drawer-line-formatter id title)
                "\n")
        ;; Re-sort so the just-inserted link ends up in the correct place
        (let ((sorted-lines
               (sort (split-string (buffer-string) "\n" t)
                     org-node-backlink-drawer-sorter)))
          (when org-node-backlink-drawer-sort-in-reverse
            (setq sorted-lines (nreverse sorted-lines)))
          (atomic-change-group
            (delete-region (point-min) (point-max))
            (insert (string-join sorted-lines "\n"))))))))

;; For save-hook
(defun org-node-backlink--fix-nearby-drawer (&optional remove)
  "Update nearby backlinks drawer so it reflects current reality.
Designed for use by `org-node-backlink-fix-buffer', which see
for argument REMOVE."
  (if remove
      (org-node--delete-drawer "BACKLINKS")
    (when-let* ((id (org-entry-get nil "ID"))
                (node (gethash id org-nodes))
                (origins (thread-last
                           (append (org-node-get-id-links-to node)
                                   (org-node-get-reflinks-to node))
                           (mapcar (##plist-get % :origin))
                           (delete-dups))))
      (save-restriction
        (org-node--narrow-to-drawer-create
         "BACKLINKS" org-node-backlink-put-drawer-near-bottom)
        (let* ((lines (split-string (buffer-string) "\n" t))
               (already-present-ids
                (seq-keep #'org-node-backlink--extract-id lines))
               (to-add      (seq-difference   origins already-present-ids))
               (to-remove   (seq-difference   already-present-ids origins))
               (to-reformat (seq-intersection already-present-ids origins)))

          (dolist (id to-remove)
            (save-excursion
              (search-forward id)
              (delete-line)))
          (dolist (id to-reformat)
            (save-excursion
              (search-forward id)
              (back-to-indentation)
              (let ((line (buffer-substring (point) (pos-eol))))
                (atomic-change-group
                  (delete-region (point) (pos-eol))
                  (insert (org-node-backlink--reformat-line line))))))
          (dolist (id to-add)
            (when-let ((known-node (gethash id org-nodes)))
              (let ((title (org-node-get-title known-node)))
                (open-line 1)
                (indent-according-to-mode)
                (insert (funcall org-node-backlink-drawer-line-formatter id title)))))

          (let ((sorted-lines
                 (sort (split-string (buffer-string) "\n" t)
                       org-node-backlink-drawer-sorter)))
            (when org-node-backlink-drawer-sort-in-reverse
              (setq sorted-lines (nreverse sorted-lines)))
            (atomic-change-group
              (delete-region (point-min) (point-max))
              (insert (string-join sorted-lines "\n")))))))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
