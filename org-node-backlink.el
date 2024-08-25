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

;; TODO: Wrap each backlink in quotes, it's safer
;; TODO: Reuse much of this logic to make org-super-links drawers

(require 'org-node)
(require 'cl-lib)

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

-----"
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        (add-hook 'org-roam-post-node-insert-hook
                  #'org-node-backlink--add-in-target nil t)
        (add-hook 'org-node-insert-link-hook
                  #'org-node-backlink--add-in-target nil t)
        (add-hook 'after-change-functions
                  #'org-node-backlink--flag-buffer-modification nil t)
        (add-hook 'before-save-hook
                  #'org-node-backlink--fix-flagged-parts-of-buffer nil t)
        ;; Advices cannot be buffer-local, so we leave this advice on even
        ;; after the mode is disabled.  It no-ops where the mode is inactive.
        (advice-add 'org-insert-link :after
                    #'org-node-backlink--add-in-target)
        (org-node-cache-ensure 'must-async))
    (remove-hook 'org-roam-post-node-insert-hook
                 #'org-node-backlink--add-in-target t)
    (remove-hook 'org-node-insert-link-hook
                 #'org-node-backlink--add-in-target t)
    (remove-hook 'after-change-functions
                 #'org-node-backlink--flag-buffer-modification t)
    (remove-hook 'before-save-hook
                 #'org-node-backlink--fix-flagged-parts-of-buffer t)))

(defun org-node-backlink--enable ()
  "Enable `org-node-backlink-mode' if buffer is Org-mode."
  (when (derived-mode-p 'org-mode)
    (org-node-backlink-mode)))

;;;###autoload
(define-globalized-minor-mode org-node-backlink-global-mode
  org-node-backlink-mode
  org-node-backlink--enable
  :group 'org-node)


;;;; Validation of one buffer at a time

(defvar org-node-backlink--fix-ctr 0)
(defvar org-node-backlink--files-to-fix nil)

;;;###autoload
(defun org-node-backlink-regret ()
  "Visit all `org-id-locations' and remove :BACKLINKS: property."
  (interactive)
  (org-node-backlink-fix-all 'remove))

;;;###autoload
(defun org-node-backlink-fix-all (&optional remove?)
  "Add :BACKLINKS: property to all known nodes.
Optional argument REMOVE? t means remove them instead, the same
as the user command \\[org-node-backlink-regret].

Can be quit midway through and resumed later.  With
\\[universal-argument], start over instead of resuming."
  (interactive)
  (when (or (null org-node-backlink--files-to-fix) current-prefix-arg)
    ;; Start over
    (org-node-cache-ensure t t)
    (setq org-node-backlink--fix-ctr 0)
    (setq org-node-backlink--files-to-fix
          (-uniq (hash-table-values org-id-locations))))
  (when (or (not (= 0 org-node-backlink--fix-ctr)) ;; resume interrupted
            (and (y-or-n-p
                  (format "Edit the %d files found in `org-id-locations'?"
                          (length org-node-backlink--files-to-fix)))
                 (y-or-n-p
                  (string-fill "You understand that this may trigger your auto git-commit systems and similar because many files are about to be edited and saved?"
                               fill-column))))
    (let ((file-name-handler-alist nil))
      ;; Do 500 at a time, because Emacs cries about opening too many file
      ;; buffers in one loop... even though we close each one as we go
      (garbage-collect) ;; Reap open file handles
      (dotimes (_ 500)
        (when-let ((file (pop org-node-backlink--files-to-fix)))
          (message "Adding/updating :BACKLINKS:... (you may quit and resume anytime) (%d) %s"
                   (cl-incf org-node-backlink--fix-ctr) file)
          (org-node--with-quick-file-buffer file
            (org-node-backlink--fix-whole-buffer remove?)))))
    (if org-node-backlink--files-to-fix
        ;; Keep going
        (run-with-timer 1 nil #'org-node-backlink-fix-all remove?))))

(defun org-node-backlink--fix-whole-buffer (&optional remove?)
  "Update :BACKLINKS: property for all nodes in buffer.
If REMOVE? is non-nil, remove the property."
  (goto-char (point-min))
  (let ((case-fold-search t))
    (while (re-search-forward "^[[:space:]]*:id: " nil t)
      (org-node-backlink--fix-entry-here remove?))))

(defun org-node-backlink--fix-entry-here (&optional remove?)
  "Update the :BACKLINKS: property in the current entry.
If REMOVE? is non-nil, remove it instead."
  (if remove?
      (org-entry-delete nil "BACKLINKS")
    (when-let* ((id (org-entry-get nil "ID"))
                (node (gethash id org-node--id<>node)))
      (catch 'break
        (let* ((sorted-uuids (thread-last
                               (append (org-node-get-id-links node)
                                       (org-node-get-reflinks node))
                               (-map #'org-node-link-origin)
                               (-uniq)
                               (-non-nil) ;; REVIEW: no nils anymore, I hope
                               (-sort #'string-lessp)))
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

(defun org-node-backlink--fix-flagged-parts-of-buffer ()
  "Fix backlinks around parts of buffer that have been modified.

Look for areas flagged by
`org-node-backlink--flag-buffer-modification' and run
`org-node-backlink--fix-entry-here' at each affected heading.
For a huge file, this is much faster than using
`org-node-backlink--fix-whole-buffer' -- imagine a thousand
headings but you have only done work under one of them."
  (unless (derived-mode-p 'org-mode)
    (error "Backlink function called in non-Org buffer"))
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
            ;; that property at all.  So if position START is in some
            ;; unmodified area -- "org-node-flag" is effectively valued at nil
            ;; -- this way of calling `text-property-not-all' means search
            ;; forward until it is t.  Then calling it again has the opposite
            ;; effect, searching until it is nil again.
            (let ((start (point-min-marker))
                  (end (make-marker))
                  (case-fold-search t)
                  prop)
              ;; (goto-char start)
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
                    (let ((id-here (org-entry-get nil "ID" t)))
                      (and id-here
                           (re-search-backward
                            (concat "^[[:space:]]*:id: +"
                                    (regexp-quote id-here))
                            nil t)
                           (re-search-forward ":id: +" (pos-eol))
                           (org-node-backlink--fix-entry-here))))
                  ;; ...and if the change-area is massive, spanning multiple
                  ;; subtrees (like after a big yank), update each subtree
                  ;; within
                  (while (and (< (point) end)
                              (re-search-forward
                               "^[[:space:]]*:id: +" end t))
                    (org-node-backlink--fix-entry-here))
                  (remove-text-properties start end 'org-node-flag))
                ;; This change-area dealt with, move on
                (set-marker start (marker-position end)))
              (set-marker start nil)
              (set-marker end nil))))
      (( error debug )
       (message "org-node: Updating backlinks ran into an issue: %S" err)
       (remove-text-properties (point-min) (point-max) 'org-node-flag)
       ;; Provide backtrace even tho we don't signal an error
       (when debug-on-error
         (message "org-node: Printing backtrace")
         (backtrace))))))

(defun org-node-backlink--flag-buffer-modification (beg end _n-deleted-chars)
  "Add text property `org-node-flag' to region between BEG and END.

Designed for `after-change-functions', so this effectively flags
all areas where text is added/changed/deleted.  Where text was
purely deleted, it flags the preceding and succeeding char."
  (unless (derived-mode-p 'org-mode)
    (error "Backlink function called in non-Org buffer"))
  (when org-node-backlink-mode
    (with-silent-modifications
      (if (= beg end)
          (put-text-property (max (1- beg) (point-min))
                             (min (1+ end) (point-max))
                             'org-node-flag t)
        (put-text-property beg end 'org-node-flag t)))))


;;;; Link-insertion advice

;; This logic is independent from the per-buffer validation, because that
;; operates on the file being saved -- in other words, making the file
;; navel-gaze its own content to see if it looks correct according to current
;; links tables.  Technically, that would be enough to result in correct
;; backlinks everywhere if you just run it on all files, and that's
;; more-or-less how `org-node-backlink-fix-all' works, but we don't want to do
;; that on every save.

;; By contrast, the below code does not look up tables, just reacts to the
;; exact link being inserted, which has two benefits:

;; 1. You can observe backlinks appearing in realtime before a buffer is saved

;; 2. It's actually necessary, because a link being inserted does not mean we
;;    should check the current file but rather visit and edit the target file.
;;    If we didn't have the below code, we'd have save the current buffer (in
;;    order to update tables) and then open the target file and run
;;    `org-node-backlink--fix-whole-buffer', which can easily take a while for
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
                  (src-id (org-entry-get nil "ID" t)))
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
                  (let ((org-node--imminent-recovery-msg
                         (string-fill "Org-node going to add a backlink to the target of the link you just inserted, but it's likely you will first get a prompt to recover an auto-save file, ready? "
                                      fill-column)))
                    (org-node--with-quick-file-buffer file
                      (org-node-backlink--add-at
                       id src-title src-id))))))))))))

(defun org-node-backlink--add-at (target-id src-title src-id)
  "Add a backlink at TARGET-ID.
Seek the :ID: property in current buffer that matches TARGET-ID,
then compose a link string out of SRC-ID and SRC-TITLE and insert
it in the nearby :BACKLINKS: property."
  (goto-char (point-min))
  (if (not (re-search-forward
            (concat "^[ \t]*:id: +" (regexp-quote target-id))
            nil t))
      (push target-id org-node-backlink--fails)
    (when (get-text-property (point) 'read-only)
      ;; If for some reason the search landed us in a transclude region or
      ;; other read-only area...  Note that `org-entry-put' inhibits read-only,
      ;; so it wouldn't signal any error.
      (error "org-node: Property drawer seems to be read-only at %d in %s"
             (point) (buffer-name)))
    (let ((current-backlinks-value (org-entry-get nil "BACKLINKS"))
          (src-link (org-link-make-string (concat "id:" src-id) src-title))
          new-value)
      (and current-backlinks-value
           (string-search "\f" current-backlinks-value)
           (error "Form-feed character in BACKLINKS property near %d in %s"
                  (point) buffer-file-name))
      (if current-backlinks-value
          ;; Build a temp list to check we don't add the same link twice.
          ;; There is an Org builtin `org-entry-add-to-multivalued-property',
          ;; but we cannot use it since the link descriptions may contain
          ;; spaces.  Further, they may contain quotes(!), so we cannot use
          ;; `split-string-and-unquote', thus this technique.  That's why we do
          ;; not bother to wrap the links in quotes.
          (let ((links (split-string (replace-regexp-in-string
                                      "]][\s\t]+\\[\\["
                                      "]]\f[["
                                      (string-trim current-backlinks-value))
                                     "\f")))
            (dolist (dup (--filter (string-search src-id it) links))
              (setq links (remove dup links)))
            (push src-link links)
            (when (-any-p #'null links)
              (org-node--die "nils in %S" links))
            ;; Enforce deterministic order to prevent unnecessarily reordering
            ;; every time a node is linked that already has the backlink
            (sort links #'string-lessp)
            (setq new-value (string-join links "  ")))
        (setq new-value src-link))
      (unless (equal current-backlinks-value new-value)
        (let ((after-change-functions
               (remq 'org-node-backlink--flag-buffer-modification
                     after-change-functions)))
          (org-entry-put nil "BACKLINKS" new-value))))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
