;;; org-node-backlink.el --- Manage :BACKLINKS: properties -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
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

;; Advices and hooks that efficiently make sure that the Org property drawers
;; that should have :BACKLINKS: properties have them and are up to date.

;;; Code:

;; TODO: Reuse much of this logic to make org-super-links drawers

(require 'org-node)
(require 'org-node-changes)
(require 'cl-lib)
(require 'compat)

(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer .1 nil #'display-warning 'org-node
                      "Your config may have misspelled `org-node-backlink-mode' as `org-node-backlinks-mode'"))
    (apply #'org-node-backlink-mode args)))


;;;; Minor mode

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated.

See Info node `(org-node)'.

-----"
  :global t
  :group 'org-node
  (when (member #'org-node-backlink-mode org-mode-hook)
    ;; 2024-10-22
    (message "Now a global mode: `org-node-backlink-mode'")
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
  "Buffer-local part of `org-node-backlink-mode'."
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

;;;###autoload (autoload 'org-node-backlink-global-mode "org-node-backlink" nil t)
(org-node-changes--def-whiny-alias 'org-node-backlink-global-mode
                                   #'org-node-backlink-mode
                                   "2024-10-22" nil "15 December 2024")


;;;; Buffer validation

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
  "Update :BACKLINKS: property for all nodes in buffer.
If REMOVE is non-nil, remove the property."
  (interactive)
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward "^[\t\s]*:id: " nil t)
      (org-node-backlink--fix-entry-here remove))))

(defun org-node-backlink--fix-entry-here (&optional remove)
  "Update the :BACKLINKS: property in the current entry.
If REMOVE is non-nil, remove it instead."
  (if remove
      (org-entry-delete nil "BACKLINKS")
    (when-let* ((id (org-entry-get nil "ID"))
                (node (gethash id org-node--id<>node)))
      (catch 'break
        (let* ((sorted-uuids (thread-last
                               (append (org-node-get-id-links-to node)
                                       (org-node-get-reflinks-to node))
                               (mapcar #'org-node-link-origin)
                               (seq-uniq)
                               (compat-call sort)))
               (links (cl-loop
                       for origin in sorted-uuids
                       as origin-node = (gethash origin org-node--id<>node)
                       if origin-node
                       collect (org-link-make-string
                                (concat "id:" origin)
                                (org-node-get-title origin-node))
                       else do
                       (message "Backlink tables mention an ID not in `org-nodes' table, resetting...")
                       (push (lambda ()
                               (message "Backlink tables mention an ID not in `org-nodes' table, resetting... done"))
                             org-node--temp-extra-fns)
                       (org-node--scan-all)
                       (throw 'break t)))
               (links-string (string-join links "  ")))
          (if links
              (unless (equal links-string (org-entry-get nil "BACKLINKS"))
                (org-entry-put nil "BACKLINKS" links-string))
            (org-entry-delete nil "BACKLINKS")))))))

(defun org-node-backlink--flag-buffer-modification (beg end _n-deleted-chars)
  "Add text property `org-node-flag' to region between BEG and END.

Designed for `after-change-functions', so this effectively flags
all areas where text is added/changed/deleted.  Where text was
purely deleted, it flags the preceding and succeeding char."
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
`org-node-backlink--fix-entry-here' at each affected heading.
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
                           (org-node-backlink--fix-entry-here))))
                  ;; ...and if the change-area is massive, spanning multiple
                  ;; subtrees (like after a big yank), update each subtree
                  ;; within
                  (while (and (< (point) end)
                              (re-search-forward
                               "^[\t\s]*:id: +" end t))
                    (org-node-backlink--fix-entry-here))
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
         (message "org-node: Printing backtrace")
         (backtrace))))))


;;;; Link-insertion advice

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
  (unless (derived-mode-p 'org-mode)
    (error "Backlink function called in non-Org buffer"))
  (require 'org-element)
  (when org-node-backlink-mode
    (org-node-cache-ensure)
    (let ((elm (org-element-context)))
      (let ((path (org-element-property :path elm))
            (type (org-element-property :type elm))
            id file)
        (when (and path type)
          (if (equal "id" type)
              ;; Classic backlink
              (progn
                (setq id path)
                (setq file (org-id-find-id-file id)))
            ;; "Reflink"
            (setq id (gethash path org-node--ref<>id))
            (setq file (ignore-errors
                         (org-node-get-file-path
                          (gethash id org-node--id<>node)))))
          (when (null file)
            (push id org-node-backlink--fails))
          (when (and id file)
            (let ((case-fold-search t)
                  (src-id (org-entry-get-with-inheritance "ID")))
              (when (and src-id (not (equal src-id id)))
                (let ((src-title
                       (save-excursion
                         (without-restriction
                           (re-search-backward (concat "^[ \t]*:id: +" src-id))
                           (or (org-get-heading t t t t)
                               (cadar (org-collect-keywords '("TITLE")))
                               (file-name-nondirectory buffer-file-name))))))
                  ;; Ensure that
                  ;; `org-node-backlink--fix-flagged-parts-of-buffer' will not
                  ;; later remove the backlink we're adding
                  (org-node--dirty-ensure-node-known)
                  (org-node--with-quick-file-buffer file
                    :about-to-do "Org-node going to add backlink to the target of the link you just inserted"
                    (org-node-backlink--add-at id src-title src-id)))))))))))

;; REVIEW: rename target/src to dest/origin for consistency?
(defun org-node-backlink--add-at (target-id src-title src-id)
  "Add a backlink at TARGET-ID.
Seek the :ID: property in current buffer that matches TARGET-ID,
then compose a link string out of SRC-ID and SRC-TITLE and insert
it in the nearby :BACKLINKS: property."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (if (not (re-search-forward
              (concat "^[ \t]*:id: +" (regexp-quote target-id))
              nil t))
        (push target-id org-node-backlink--fails)
      (when (get-text-property (point) 'read-only)
        ;; If for some reason the search landed us in a transclude region or
        ;; other read-only area...  Note that `org-entry-put' inhibits
        ;; read-only, so it wouldn't signal any error.
        (error "org-node: Property drawer seems to be read-only at %d in %s"
               (point) (buffer-name)))
      (let ((current-backlinks-value (org-entry-get nil "BACKLINKS"))
            (src-link (org-link-make-string (concat "id:" src-id) src-title))
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
                       when (string-search src-id link)
                       do (setq links (delete link links)))
              (push src-link links)
              (when (seq-some #'null links)
                (org-node--die "nils in %S" links))
              ;; Enforce deterministic order to prevent unnecessarily reordering
              ;; every time a node is linked that already has the backlink
              (sort links #'string-lessp)
              (setq new-value (string-join links "  ")))
          (setq new-value src-link))
        (unless (equal current-backlinks-value new-value)
          (let ((user-is-editing (buffer-modified-p))
                (after-change-functions
                 (remq 'org-node-backlink--flag-buffer-modification
                       after-change-functions)))
            (org-entry-put nil "BACKLINKS" new-value)
            (unless user-is-editing
              (let ((before-save-hook nil)
                    (after-save-hook nil))
                (save-buffer)))))))))


;;;; Aggressive visit-and-fix

(defcustom org-node-backlink-aggressive nil
  "On save, detect added/deleted links and fix backlinks.

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
  :group 'org-node
  :type 'boolean)

(defun org-node-backlink--maybe-fix-aggressively (_)
  "Designed for `org-node-rescan-functions'."
  (when org-node-backlink-aggressive
    (if org-node-perf-eagerly-update-link-tables
        (let (affected-dests)
          (cl-loop
           for (dest . old-links) in org-node--old-link-sets
           when (cl-set-exclusive-or
                 (mapcar #'org-node-link-origin old-links)
                 (mapcar #'org-node-link-origin
                         (gethash dest org-node--dest<>links))
                 :test #'equal)
           do (let* ((id dest)
                     (node (or (gethash id org-node--id<>node)
                               (and (setq id (gethash dest org-node--ref<>id))
                                    (gethash id org-node--id<>node)))))
                ;; (#59) This could be an empty link like [[id:]]
                (when node
                  (push id (alist-get (org-node-get-file-path node)
                                      affected-dests nil nil #'equal)))))
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
                      (org-node-backlink--fix-entry-here)))
                  (unless user-is-editing
                    ;; Normally, `org-node--with-quick-file-buffer' only saves
                    ;; buffers it had to open itself
                    (let ((before-save-hook nil)
                          (after-save-hook nil))
                      (save-buffer)))))))
      (message "Option `org-node-backlink-aggressive' has no effect when `org-node-perf-eagerly-update-link-tables' is nil"))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
