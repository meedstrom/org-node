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

(require 'cl-lib)
(require 'fileloop)
(require 'org)
(require 'org-element)
(require 'ol)
(require 'llama)
(require 'org-mem)
(require 'org-mem-updater)
(require 'org-node)
(declare-function org-mem-updater-ensure-id-node-at-point-known "org-mem-updater")
(declare-function org-mem-updater-ensure-link-at-point-known "org-mem-updater")

(defgroup org-node-backlink nil "In-file backlinks."
  :group 'org-node)

(defcustom org-node-backlink-do-drawers t
  "Manage drawers instead of properties.
A :BACKLINKS: property is more compact, but can run off the edge of the
visible window without `visual-line-mode' or similar."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-protect-org-super-links t
  "Do not try to manage drawers if user has org-super-links installed.
Print a message instead, ensuring the user knows what they are doing
and can invert this setting."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defun org-node-backlink--check-osl-user-p ()
  "Maybe warn users of org-super-links, not to clobber their stuff.
If a warning was not needed, return nil."
  (and org-node-backlink-do-drawers
       org-node-backlink-protect-org-super-links
       (fboundp 'org-super-links-convert-link-to-super)
       (display-warning 'org-node-backlink "A notice to users of org-super-links:
To protect your pre-existing drawers,
`org-node-backlink-mode' will do nothing.
If you're OK with how it would reformat your backlinks drawers,
set `org-node-backlink-protect-org-super-links' to nil.")))

(defun org-node-backlink--check-v2-misaligned-setting-p ()
  "Warn if `org-node-backlink-do-drawers' is t but properties exist.
If a warning was not needed, return nil."
  (and org-node-backlink-do-drawers
       (cl-some (##org-mem-entry-property "BACKLINKS" %) (org-mem-all-entries))
       (display-warning 'org-node-backlink "User option `org-node-backlink-do-drawers' is t,
but found :BACKLINKS: lines in some property drawers, so doing nothing.
This is a new default in v2, you probably just need to toggle it.
Or run this command once: `org-node-backlink-mass-delete-props'.")))


;;; Drawer config

(defcustom org-node-backlink-drawer-positioner #'org-node-goto-new-drawer-site
  "Function for moving point before placing a new drawer.
Called in a buffer narrowed to one Org entry, excluding any other
headings.  Point is at the beginning of the body text as determined by
`org-node--end-of-meta-data'.

The function may return anything, but if it returns an integer or
marker, point will move to that position.

Only called if a drawer was not already present."
  :type '(radio (function-item org-node-goto-new-drawer-site)
                (function-item org-entry-end-position)
                (function-item org-node--end-of-meta-data)
                (function :tag "Custom function" :value (lambda ())))
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-sort-in-reverse nil
  "Whether to reverse how lines are sorted in the backlinks drawer."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-sorter
  #'org-node-backlink-timestamp-lessp
  "Function for sorting lines in the backlinks drawer."
  :type '(radio
          (function-item org-node-backlink-timestamp-lessp)
          (function-item org-node-backlink-link-description-lessp)
          (function-item org-node-backlink-link-description-collate-lessp)
          (function-item org-node-backlink-id-lessp)
          (function-item org-node-backlink-id-reversed-lessp)
          (function-item org-node-backlink-id-blind-string-lessp)
          (function-item org-node-backlink-id-blind-string-collate-lessp)
          (function-item string-lessp)
          (function-item string-collate-lessp)
          (function :tag "Custom function" :value (lambda (s1 s2))))
  :package-version '(org-node . "2.0.0"))

(defun org-node-backlink-timestamp-lessp (s1 s2)
  "Sort on first Org timestamp in the line.
S1 before S2 if timestamp in S1 is earlier in time."
  (let ((ts-1 (org-node-backlink--extract-timestamp s1))
        (ts-2 (org-node-backlink--extract-timestamp s2)))
    (or (and ts-1 (not ts-2))
        (and ts-1 ts-2 (org-time< ts-1 ts-2)))))

(defun org-node-backlink-id-lessp (s1 s2)
  "Sort on content of [[id:...]].
S1 before S2 if the IDs inside satisfy `string<'.

May be useful with a non-default `org-id-method'."
  (string< (org-node-backlink--extract-id s1)
           (org-node-backlink--extract-id s2)))

(defun org-node-backlink-reversed-id-lessp (s1 s2)
  "Sort on content of [[id:...]] after reversing.
S1 before S2 if the mirror images of IDs inside satisfy `string<'.

May be useful when `org-id-method' is set to `org', because that is a
timestamp with the digits reversed.  Then, the result is a
chronological order of when those IDs were originally created
\(not when the backlinks were created)."
  (string< (org-node-backlink--extract-id (reverse s1))
           (org-node-backlink--extract-id (reverse s2))))

(defun org-node-backlink-link-description-lessp (s1 s2)
  "Sort on first link description in the line.
S1 before S2 if link descriptions inside satisfy `string<'."
  (string< (org-node-backlink--extract-link-desc s1)
           (org-node-backlink--extract-link-desc s2)))

(defun org-node-backlink-link-description-collate-lessp (s1 s2)
  "Sort on first link description in the line.
S1 before S2 if link descriptions inside satisfy `string-collate-lessp'."
  (string-collate-lessp
   (org-node-backlink--extract-link-desc s1)
   (org-node-backlink--extract-link-desc s2)))

(defun org-node-backlink-id-blind-string-lessp (s1 s2)
  "Sort lexicographically, but ignoring nonsense inside [[id:...]].
S1 before S2 if the strings sans org-ids satisfy `string<'."
  (string< (replace-regexp-in-string "\\[\\[id:.*?]" "" s1)
           (replace-regexp-in-string "\\[\\[id:.*?]" "" s2)))

(defun org-node-backlink-id-blind-string-collate-lessp (s1 s2)
  "Sort lexicographically, but ignoring nonsense inside [[id:...]].
S1 before S2 if the strings sans org-ids satisfy `string-collate-lessp'."
  (string-collate-lessp
   (replace-regexp-in-string "\\[\\[id:.*?]" "" s1)
   (replace-regexp-in-string "\\[\\[id:.*?]" "" s2)))

(defcustom org-node-backlink-drawer-formatter
  #'org-node-backlink-format-like-org-super-links-default
  "Function to format a new line for the backlinks drawer.

It takes three arguments ID, TITLE and TIME.  The first two
are strings, while the third is a Lisp time value.

It should return a string, with constraints:

- No initial whitespace.
- No newlines.
- Not more than one [[id:...]] construct."
  :type '(radio
          (function-item org-node-backlink-format-like-org-super-links-default)
          (function-item org-node-backlink-format-as-bullet-with-time)
          (function-item org-node-backlink-format-as-bullet-no-time)
          (function :tag "Custom function" :value (lambda (id title time))))
  :package-version '(org-node . "2.0.0"))

(defun org-node-backlink-format-like-org-super-links-default
    (id desc &optional time)
  "Example: \"[2025-02-21 Fri 14:39] <- [[id:ID][Node title]]\".
ID and DESC are link id and description, TIME a Lisp time value."
  (concat (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " <- "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-with-time (id desc &optional time)
  "Example: \"- [2025-02-21 Fri 14:39] [[id:ID][Node title]]\".
ID and DESC are link id and description, TIME a Lisp time value."
  (concat "- "
          (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-no-time (id desc &optional _time)
  "Example: \"- [[id:ID][Node title]]\".
ID and DESC are link id and description, TIME a Lisp time value."
  (concat "- " (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink--reformat-line (line)
  "Pass LINE back through `org-node-backlink-drawer-formatter'."
  (let ((time (org-node-backlink--extract-timestamp line)))
    (funcall org-node-backlink-drawer-formatter
             (org-node-backlink--extract-id line)
             (org-node-backlink--extract-link-desc line)
             (and time
                  (encode-time (parse-time-string
                                (org-node-backlink--extract-timestamp line)))))))

(defun org-node-backlink--extract-timestamp (text)
  "Get Org timestamp out of TEXT."
  (when (string-match org-ts-regexp-both text)
    (match-string 0 text)))

(defun org-node-backlink--extract-id (text)
  "Get first link description out of TEXT.
That means the first part of a [[id][description]]."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (search-forward "[[id:" nil t)
      (buffer-substring-no-properties (point)
                                      (- (re-search-forward "].\\|::")
                                         2)))))

(defun org-node-backlink--extract-link-desc (text)
  "Get first link description out of TEXT.
That means the second part of a [[id][description]]."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (when (and (search-forward "[[id:" nil t)
               (search-forward "][" nil t))
      (buffer-substring-no-properties (point)
                                      (- (search-forward "]]")
                                         2)))))


;;; Commands

(defun org-node-backlink-mass-update-drawers ()
  "Add or update backlinks drawers in all files."
  (interactive)
  (unless org-node-backlink-do-drawers
    (user-error "Asked to update :BACKLINKS: drawers, but `org-node-backlink-do-drawers' is nil"))
  (org-node-backlink--fix-all-files 'update-drawers))

(defun org-node-backlink-mass-update-props ()
  "Add or update backlinks properties in all files."
  (interactive)
  (when org-node-backlink-do-drawers
    (user-error "Asked to update :BACKLINKS: properties, but `org-node-backlink-do-drawers' is t"))
  (org-node-backlink--fix-all-files 'update-props))

(defun org-node-backlink-mass-delete-drawers ()
  "Delete all backlinks drawers in all files."
  (interactive)
  (org-node-backlink--fix-all-files 'del-drawers))

(defun org-node-backlink-mass-delete-props ()
  "Delete all backlinks properties in all files."
  (interactive)
  (org-node-backlink--fix-all-files 'del-props))

(defvar org-node-backlink--work-remaining nil)
(defvar org-node-backlink--work-kind nil)
(defun org-node-backlink--fix-all-files (kind)
  "Update :BACKLINKS: in all known nodes.
Argument KIND controls how to update them.

Can be quit midway through and resumed later.  With
\\[universal-argument], start over instead of resuming."
  (interactive)
  (unless (boundp 'fileloop--operate-function)
    (error "Dependency fileloop may have changed since org-node-backlink was written"))
  (unless (or (and (memq kind '(update-drawers update-props))
                   (org-node-backlink--check-v2-misaligned-setting-p))
              (and (eq kind 'update-drawers)
                   (org-node-backlink--check-osl-user-p)))
    (let ((proceed (and org-node-backlink--work-remaining
                        (eq org-node-backlink--work-kind kind)
                        (equal fileloop--operate-function
                               #'org-node-backlink--loop-operator))))
      (unless proceed
        (org-mem--scan-full)
        (unless (org-mem-await 'org-node-backlink 30)
          (user-error "org-node-backlink: Waited weirdly long for org-mem"))
        (let* ((files (org-mem-all-files))
               (dirs (org-node--root-dirs files))
               (problematic (seq-filter (##and (boundp %) (symbol-value %))
                                        '(org-node-backlink-mode
                                          auto-save-visited-mode
                                          git-auto-commit-mode))))
          (when (y-or-n-p
                 (format "Edit %d Org files in these %d directories?\n%S"
                         (length files) (length dirs) dirs))
            (when problematic
              (y-or-n-p (concat "Disable "
                                (string-join (mapcar #'symbol-name problematic)
                                             ", ")
                                "?"))
              (dolist (mode problematic)
                (funcall mode 0))
              (setq problematic nil))
            (when (not problematic)
              (setq org-node-backlink--work-remaining files)
              (setq org-node-backlink--work-kind kind)
              (fileloop-initialize
               files
               (lambda ()
                 (pop org-node-backlink--work-remaining)
                 (and buffer-file-name
                      (string-prefix-p "org" (file-name-extension
                                              buffer-file-name))))
               #'org-node-backlink--loop-operator)
              (setq proceed t)))))
      (when proceed
        (cl-letf (((symbol-function 'recentf-add-file) #'ignore))
          (let ((delay-mode-hooks t)
                (org-inhibit-startup t)
                (org-element-cache-persistent nil))
            (fileloop-continue)))))))

(defun org-node-backlink--loop-operator ()
  "An OPERATE-FUNCTION for `fileloop-initialize'.
Wrapper to call `org-node-backlink-fix-buffer'."
  (let ((buffer-seems-new (and (not (buffer-modified-p))
                               (not buffer-undo-list))))
    (if (not (derived-mode-p 'org-mode))
        (message "Failed to enter Org mode in %s" (current-buffer))
      (org-node-backlink-fix-buffer
       org-node-backlink--work-kind)
      (when buffer-seems-new
        (when (buffer-modified-p)
          (let ((before-save-hook nil)
                (after-save-hook nil))
            (save-buffer)))
        (kill-buffer)))
    t))

(defvar org-node-backlink--checked nil)
(defun org-node-backlink-fix-buffer (&optional kind)
  "Update :BACKLINKS: properties or drawers in all nodes in buffer.
Let user option `org-node-backlink-do-drawers' determine which.

Or if KIND is symbol `update-drawers', `del-drawers', `update-props', or
`del-props', do the corresponding thing."
  (interactive)
  (unless (or (and (memq kind '(update-drawers update-props))
                   (org-node-backlink--check-v2-misaligned-setting-p))
              (and (eq kind 'update-drawers)
                   (org-node-backlink--check-osl-user-p)))
    ;; (message "Fixing file %s" buffer-file-name)
    (goto-char (point-min))
    (org-node--assert-transclusion-safe)
    (let ((case-fold-search t))
      ;; NOTE: If there is an entry that has :BACKLINKS:, but that has lost its
      ;;       :ID:, it will never be touched again, but that's on the user.
      (while (re-search-forward "^[ \t]*:id:[ \t]*[[:graph:]]" nil t)
        (org-node-backlink--fix-nearby kind)
        (outline-next-heading)))))


;;; Save-hook to update only the changed parts of current buffer

(defvar org-node-backlink--inhibit-flagging nil)
(defun org-node-backlink--flag-buffer-modification (beg end _n-deleted-chars)
  "Add text property `org-node-flag' to region between BEG and END.

Designed for `after-change-functions', where this effectively flags
all areas where text is added/changed/deleted.  Where text was
purely deleted, this flags the preceding and succeeding char."
  (unless org-node-backlink--inhibit-flagging
    (with-silent-modifications
      (if (= beg end)
          (put-text-property (max (1- beg) (point-min))
                             (min (1+ end) (point-max))
                             'org-node-flag t)
        (put-text-property beg end 'org-node-flag t)))))

(defun org-node-backlink--fix-flagged-parts-of-buffer ()
  "Fix backlinks around parts of buffer that have been modified.

Look for areas flagged by
`org-node-backlink--flag-buffer-modification' and run
`org-node-backlink--fix-nearby' at each affected heading.

For a huge file, this is much faster than using
`org-node-backlink-fix-buffer' -- imagine a thousand
headings but you have only done work under one of them."
  (when (derived-mode-p 'org-mode)
    (if (or (org-node-backlink--check-v2-misaligned-setting-p)
            (org-node-backlink--check-osl-user-p))
        (org-node-backlink-mode 0)
      ;; Catch any error, because this runs at `before-save-hook' which MUST
      ;; fail gracefully and let the user save anyway
      (condition-case err
          (save-excursion
            (without-restriction
              ;; Iterate over each change-region.  Algorithm borrowed from
              ;; `ws-butler-map-changes', odd but elegant.  Worth knowing
              ;; that if you tell Emacs to search for text that has a given
              ;; text-property with a nil value, that's the same as searching
              ;; for text without that property at all.  So if position START
              ;; is in some unmodified area -- property `org-node-flag' is
              ;; effectively valued at nil -- this way of calling
              ;; `text-property-not-all' means search forward until it is t.
              ;; Then calling it again has the opposite effect, searching
              ;; until it is nil again.
              (let ((start (point-min-marker))
                    (end (make-marker))
                    (case-fold-search t)
                    prop)
                (while (< start (point-max))
                  (setq prop (get-text-property start 'org-node-flag))
                  (set-marker end (or (text-property-not-all
                                       start (point-max) 'org-node-flag prop)
                                      (point-max)))
                  (cl-assert (not (= start end)))
                  (when prop
                    (goto-char start)
                    ;; START and END delineate an area where changes were
                    ;; detected, but the area rarely envelops the current
                    ;; tree's property drawer, likely placed long before
                    ;; START, so search back for it
                    (save-excursion
                      (and (org-entry-get-with-inheritance "ID")
                           (goto-char org-entry-property-inherited-from)
                           (not (org-node--in-transclusion-p))
                           (org-node-backlink--fix-nearby)))
                    ;; ...and if the change-area is massive, spanning multiple
                    ;; subtrees (like after a big yank), update each subtree
                    ;; within
                    (while (and (< (point) end)
                                (re-search-forward "^[\t\s]*:id: +" end t))
                      (unless (org-node--in-transclusion-p)
                        (org-node-backlink--fix-nearby)))
                    (remove-text-properties start end 'org-node-flag))
                  ;; This change-area dealt with, move on
                  (set-marker start (marker-position end)))
                (set-marker start nil)
                (set-marker end nil))))
        (( error )
         (unless (remove-text-properties 1 (point-max) 'org-node-flag)
           (message "org-node: Did not remove org-node-flag text property"))
         (message "org-node: Updating backlinks ran into an issue: %S" err))))))


;;; Proactive updating (broken & disabled for now)

(defcustom org-node-backlink-lazy nil
  "Inhibit cleaning up backlinks until user edits affected entry.

Background: Regardless of this value, links inserted via most commands
will insert a backlink in real time, so long as
`org-node-backlink-mode' is enabled.

If in the future the user deletes that link, the corresponding backlink
becomes stale.  This value controls what to do upon noticing that.

When t, they are not cleaned until you carry out some edits under the
heading that holds the stale backlink, and save that buffer.
That can be desirable for e.g. quieter git diffs.

When nil, all affected nodes are silently visited after a save if needed
to ensure that their :BACKLINKS: properties or drawers reflect reality.

To clarify, this is solely about the textual contents of :BACKLINKS:
properties or drawers; the underlying link tables are up to date anyway.

Minor side effect: `org-element-cache-reset' is called in the buffers
where backlinks are fixed.

To force an update at any time, use one of these commands:
- \\[org-node-backlink-fix-buffer]
- \\[org-node-backlink-mass-update-drawers]
- \\[org-node-backlink-mass-update-props]"
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

;; XXX Broken 2025-05-22 (visits too many entries => saving big file is super slow)
;;     No-op for now
(defun org-node-backlink--maybe-fix-proactively (_)
  "Designed for `org-mem-post-targeted-scan-functions'."
  (unless nil ;; org-node-backlink-lazy
    (let (affected-targets)
      (cl-loop
       for target being each hash-key of org-mem-updater--id-or-ref-target<>old-links
       using (hash-values old-links)
       as entry = (or (org-mem-entry-by-id target)
                      (org-mem-entry-by-roam-ref target))
       ;; Entry must have ID or we can't use `org-find-property' later.
       when (and entry (org-mem-entry-id entry))
       when (not (seq-set-equal-p
                  (seq-keep #'org-mem-link-nearby-id old-links)
                  (seq-keep #'org-mem-link-nearby-id
                            (append (org-mem-roam-reflinks-to-entry entry)
                                    (org-mem-id-links-to-entry entry)))))
       ;; Something changed in the set of links targeting this entry.  So we'll
       ;; visit the entry to re-print backlinks.
       ;; Alist `affected-targets' looks like:
       ;;   ((file1 . (id1 id2 id3 ...))
       ;;    (file2 . (...))
       ;;    (file3 . (...)))
       do
       (push (org-mem-entry-id entry)
             (alist-get (org-mem-entry-file-truename entry)
                        affected-targets
                        () () #'equal)))
      (cl-loop for target in org-mem-updater--new-id-or-ref-targets
               do (cl-loop
                   for link in (gethash target org-mem--target<>links)
                   as id = (org-mem-link-nearby-id link)
                   when id do
                   (push id (alist-get (org-mem-link-file-truename link)
                                       affected-targets
                                       () () #'equal))))

      (cl-loop
       for (file . ids) in affected-targets
       if (not (file-readable-p file))
       do (message "Cannot edit backlinks in unreadable file: %s" file)
       else if (not (file-writable-p file))
       do (message "Cannot edit backlinks in unwritable file: %s" file)
       else do
       (org-node--with-quick-file-buffer file
         :about-to-do "About to fix backlinks"
         (org-node--assert-transclusion-safe)
         (let ((user-is-editing (buffer-modified-p)))
           (dolist (id (delete-dups ids))
             (if-let* ((pos (and id (org-find-property "ID" id))))
                 (progn (goto-char pos)
                        (org-node-backlink--fix-nearby))
               (error "Could not find ID %s in file %s" id file)))
           (unless user-is-editing
             ;; Normally, `org-node--with-quick-file-buffer' only saves buffers
             ;; it had to open anew.  Let's save even if it was open previously.
             (let ((before-save-hook nil)
                   (after-save-hook nil)
                   (save-silently t)
                   (inhibit-message t))
               (save-buffer)))))))))


;;; Subroutine: "Fix nearby"

(defun org-node-backlink--fix-nearby (&optional kind)
  "In current entry, fix the backlinks drawer or property.
Let `org-node-backlink-do-drawers' determine which.

Or if KIND is symbol `update-drawers', `del-drawers', `update-props', or
`del-props', do the corresponding thing."
  (if kind
      (pcase kind
        ('del-props   (org-node-backlink--fix-nearby-property t))
        ('del-drawers (org-node-backlink--fix-nearby-drawer t))
        ('update-props   (org-node-backlink--fix-nearby-property))
        ('update-drawers (org-node-backlink--fix-nearby-drawer)))
    (if org-node-backlink-do-drawers
        (org-node-backlink--fix-nearby-drawer)
      (org-node-backlink--fix-nearby-property))))

(defun org-node-backlink--fix-nearby-property (&optional remove)
  "Update the :BACKLINKS: property in the current entry.
If REMOVE is non-nil, remove it instead."
  (when-let* ((prop-pos (car (org-get-property-block))))
    (when (get-text-property prop-pos 'read-only)
      ;; Because `org-entry-put' is so unsafe that it inhibits read-only
      (error "org-node-backlink: Area seems to be read-only at %d in %s"
             prop-pos (buffer-name))))
  (if remove
      (org-entry-delete nil "BACKLINKS")
    (let* ((id (org-entry-get nil "ID"))
           (entry (gethash id org-nodes)))
      (if (not (and id entry))
          (org-entry-delete nil "BACKLINKS")
        (let* ((origins (thread-last
                          (append (org-mem-id-links-to entry)
                                  (org-mem-roam-reflinks-to entry))
                          (mapcar #'org-mem-link-nearby-id)
                          (delete-dups)
                          ;; Sort deterministically for less noisy diffs.
                          (seq-sort #'string<)
                          (seq-keep #'org-mem-entry-by-id)))
               (backlinks (cl-loop
                           for ogn in origins
                           collect (org-link-make-string
                                    (concat "id:" (org-mem-entry-id ogn))
                                    (org-mem-entry-title ogn))))
               (new-value (string-join backlinks "  ")))
          (if backlinks
              (unless (equal new-value (org-entry-get nil "BACKLINKS"))
                (org-entry-put nil "BACKLINKS" new-value))
            (org-entry-delete nil "BACKLINKS")))))))

(defun org-node-backlink--fix-nearby-drawer (&optional remove)
  "Update nearby backlinks drawer so it reflects current reality.
Designed for use by `org-node-backlink--fix-nearby'.
If REMOVE non-nil, remove it instead."
  (if remove
      (org-node--delete-drawer "BACKLINKS")
    (let* ((id (org-entry-get nil "ID"))
           (entry (org-mem-entry-by-id id))
           (origins (when entry
                      (thread-last
                        (append (org-mem-id-links-to-entry entry)
                                (org-mem-roam-reflinks-to-entry entry))
                        (mapcar #'org-mem-link-nearby-id)
                        (delete-dups)
                        ;; Shouldn't be necessary if tables are correct,
                        ;; but don't assume `org-mem-updater-mode' is flawless
                        (seq-filter #'org-mem-entry-by-id))))
           (org-node-backlink--inhibit-flagging t))
      (if (null origins)
          (org-node--delete-drawer "BACKLINKS")
        (save-excursion
          (save-restriction
            (org-node-narrow-to-drawer-create
             "BACKLINKS" org-node-backlink-drawer-positioner)
            (let* ((lines (split-string (buffer-string) "\n" t))
                   (already-present-ids
                    (seq-keep #'org-node-backlink--extract-id lines))
                   (to-add      (seq-difference   origins already-present-ids))
                   (to-remove   (seq-difference   already-present-ids origins))
                   (to-reformat (seq-intersection already-present-ids origins)))
              ;; Add new, remove stale, reformat the rest
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
                (let ((title (org-mem-title-maybe (org-mem-entry-by-id id))))
                  (insert (funcall org-node-backlink-drawer-formatter id title))
                  (newline-and-indent)))
              ;; Membership is correct, now re-sort
              (let ((sorted-lines (sort (split-string (buffer-string) "\n" t)
                                        org-node-backlink-drawer-sorter)))
                (when org-node-backlink-drawer-sort-in-reverse
                  (setq sorted-lines (nreverse sorted-lines)))
                (atomic-change-group
                  (delete-region (point-min) (point-max))
                  (insert (string-join sorted-lines "\n")))))))))))


;;; Subroutine: "Add in target" (to advise real-time link insertion)

;; This logic is independent from the per-buffer validation, because that
;; operates on the file being saved -- in other words, making the file
;; navel-gaze its own content to see if it looks correct according to current
;; links tables.  Technically, that would be enough to result in correct
;; backlinks everywhere if you just run it on all files, and that's
;; more-or-less how `org-node-backlink--fix-all-files' works, but we don't want
;; to do that on every save.

;; By contrast, the below code does not look up tables, just reacts to the
;; exact link being inserted, which has two benefits:

;; 1. You can observe backlinks appearing in realtime before a buffer is saved

;; 2. It's actually necessary, because a link being inserted does not mean we
;;    should check the current file but rather visit and edit the target file.
;;    If we didn't have the below code, we'd have to save the current buffer
;;    (in order to update tables) and then open the target file and run
;;    `org-node-backlink-fix-buffer', which can easily take a while for a big
;;    target file.

;; REVIEW: In theory, it is possible to drop these advices, letting user insert
;; links with zero Emacs lag, if we instead use something like
;; `org-node-backlink--maybe-fix-proactively' after some idle...

;; In fact, it might let us reason more easily about
;; `org-node-backlink--maybe-fix-proactively' if we stop doing
;; `org-mem-updater-ensure-link-at-point-known'.

(defun org-node-backlink--add-in-target (&rest _)
  "For known link at point, leave a backlink in the target node."
  (unless (derived-mode-p 'org-mode)
    (error "Backlink function called in non-Org buffer"))
  (org-node-cache-ensure)
  (let* ((elm (org-element-context))
         (path (org-element-property :path elm))
         (type (org-element-property :type elm))
         target-id target-file)
    ;; In a link such as [[id:abc1234]], TYPE is "id" and PATH is "abc1234".
    (when (and type path)
      (org-mem-updater-ensure-link-at-point-known)
      (if (equal "id" type)
          ;; A classic backlink
          (progn
            (setq target-id path)
            ;; `org-id-find-id-file' has terrible fallback behavior
            (setq target-file (ignore-errors
                                (org-mem-entry-file
                                 (gethash target-id org-nodes)))))
        ;; A "reflink"
        (setq target-id (gethash path org-mem--roam-ref<>id))
        (setq target-file (ignore-errors
                            (org-mem-entry-file
                             (gethash target-id org-nodes)))))
      (when (null target-file)
        (message "`org-node-backlink--add-in-target' could not resolve ID: %s" target-id))
      (when (and target-id target-file)
        (org-node--assert-transclusion-safe)
        (let ((origin-id (org-entry-get-with-inheritance "ID")))
          (when (and origin-id (not (equal origin-id target-id)))
            (when-let* ((origin-title
                         (save-excursion
                           (without-restriction
                             (goto-char org-entry-property-inherited-from)
                             (or (org-get-heading t t t t)
                                 (org-get-title))))))
              ;; Ensure that
              ;; `org-node-backlink--fix-flagged-parts-of-buffer' will not
              ;; later remove the backlink we're adding
              (org-mem-updater-ensure-id-node-at-point-known)
              (org-node--with-quick-file-buffer target-file
                :about-to-do "Org-node going to add backlink in target of link you just inserted"
                (org-node--assert-transclusion-safe)
                (if-let* ((pos (org-find-property "ID" target-id)))
                    (progn (goto-char pos)
                           (org-node-backlink--add-nearby origin-id origin-title))
                  (message "`org-node-backlink--add-in-target' could not find ID %s in file %s"
                           target-id target-file))))))))))

(defun org-node-backlink--add-nearby (id title)
  "Add link with ID and TITLE into local backlink drawer or property."
  (if org-node-backlink-do-drawers
      (org-node-backlink--add-to-drawer id title)
    (org-node-backlink--add-to-property id title)))

(defun org-node-backlink--add-to-property (id title)
  "Insert a link with ID and TITLE into nearby :BACKLINKS: property."
  (when-let* ((prop-pos (car (org-get-property-block))))
    (when (get-text-property prop-pos 'read-only)
      ;; Because `org-entry-put' is so unsafe that it inhibits read-only
      (error "org-node-backlink: Area seems to be read-only at %d in %s"
             prop-pos (buffer-name))))
  (let ((current-backlinks-value (org-entry-get nil "BACKLINKS"))
        (new-link (org-link-make-string (concat "id:" id) title))
        new-value)
    (when (and current-backlinks-value
               (string-search "\f" current-backlinks-value))
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
                                    (rx "]]" (+ space) "[[")
                                    "]]\f[["
                                    (string-trim current-backlinks-value))
                                   "\f")))
          (cl-loop for link in links
                   when (string-search id link)
                   do (setq links (delete link links)))
          (push new-link links)
          (when (seq-some #'null links)
            (error "org-node: nils in %S" links))
          ;; Enforce deterministic order to prevent unnecessary reordering
          ;; every time a node is linked that already has the backlink
          (sort links #'string-lessp)
          (setq new-value (string-join links "  ")))
      ;; Only one link
      (setq new-value new-link))
    (unless (equal new-value current-backlinks-value)
      (let ((user-is-editing (buffer-modified-p))
            ;; Prevent reacting to this edit (would be redundant at best)
            (org-node-backlink--inhibit-flagging t))
        (org-entry-put nil "BACKLINKS" new-value)
        (unless user-is-editing
          (let ((before-save-hook nil)
                (after-save-hook nil))
            (save-buffer)))))))

(defun org-node-backlink--add-to-drawer (id title)
  "Add new backlink with ID and TITLE to nearby drawer.
Designed for use by `org-node-backlink--add-in-target'."
  (cl-assert id) ;; Don't want to insert empty [[id:]] ever
  (if (or (org-node-backlink--check-v2-misaligned-setting-p)
          (org-node-backlink--check-osl-user-p))
      (org-node-backlink-mode 0)
    (save-excursion
      (save-restriction
        (let ((org-node-backlink--inhibit-flagging t))
          (org-node-narrow-to-drawer-create
           "BACKLINKS" org-node-backlink-drawer-positioner)
          (unless (search-forward (concat "[[id:" id) nil t) ;; Already has it
            (insert (funcall org-node-backlink-drawer-formatter id title))
            (newline-and-indent)
            ;; Re-sort so just-inserted link is placed correct among them
            (let ((lines (sort (split-string (buffer-string) "\n" t)
                               org-node-backlink-drawer-sorter)))
              (when org-node-backlink-drawer-sort-in-reverse
                (setq lines (nreverse lines)))
              (atomic-change-group
                (delete-region (point-min) (point-max))
                (insert (string-join lines "\n"))))))))))


;;; Global minor mode

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties or drawers updated.

See Info node `(org-node)'."
  :global t
  (if org-node-backlink-mode
      (progn
        (add-hook 'org-mode-hook                        #'org-node-backlink--local-mode)
        (add-hook 'org-mem-post-targeted-scan-functions #'org-node-backlink--maybe-fix-proactively)
        (add-hook 'org-node-relocation-hook             #'org-node-backlink--fix-nearby)
        (add-hook 'org-roam-post-node-insert-hook       #'org-node-backlink--add-in-target)
        (add-hook 'org-node-insert-link-hook            #'org-node-backlink--add-in-target)
        (advice-add 'org-insert-link :after             #'org-node-backlink--add-in-target)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-node-backlink--local-mode)))))
    (remove-hook 'org-mode-hook                        #'org-node-backlink--local-mode)
    (remove-hook 'org-mem-post-targeted-scan-functions #'org-node-backlink--maybe-fix-proactively)
    (remove-hook 'org-node-relocation-hook             #'org-node-backlink--fix-nearby)
    (remove-hook 'org-roam-post-node-insert-hook       #'org-node-backlink--add-in-target)
    (remove-hook 'org-node-insert-link-hook            #'org-node-backlink--add-in-target)
    (advice-remove 'org-insert-link                    #'org-node-backlink--add-in-target)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (org-node-backlink--local-mode 0)))))

(define-minor-mode org-node-backlink--local-mode
  "Buffer-local part of `org-node-backlink-mode'.
Not meant to be toggled on its own."
  :interactive nil
  (if org-node-backlink--local-mode
      (progn
        (add-hook 'after-change-functions #'org-node-backlink--flag-buffer-modification nil t)
        (add-hook 'before-save-hook       #'org-node-backlink--fix-flagged-parts-of-buffer nil t))
    (remove-hook 'after-change-functions  #'org-node-backlink--flag-buffer-modification t)
    (remove-hook 'before-save-hook        #'org-node-backlink--fix-flagged-parts-of-buffer t)))

(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer
       .1 nil #'display-warning 'org-node
       "Your initfiles may have misspelled `org-node-backlink-mode' as `org-node-backlinks-mode'"))
    (apply #'org-node-backlink-mode args)))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
