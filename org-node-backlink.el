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
(require 'seq)
(require 'llama)
(require 'org)
(require 'org-node-changes)
(require 'org-node)

(defgroup org-node-backlink nil "In-file backlinks."
  :group 'org-node)

(defcustom org-node-backlink-do-drawers t
  "Manage drawers instead of properties."
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
       (display-warning 'org-node-backlink "
A notice to users of org-super-links:
To protect your pre-existing drawers,
`org-node-backlink-mode' will do nothing.
If you're OK with how it would reformat your backlinks drawers,
set `org-node-backlink-protect-org-super-links' to nil.")))

(defun org-node-backlink--check-v2-misaligned-setting-p ()
  "Warn if `org-node-backlink-do-drawers' is t but properties exist.
If a warning was not needed, return nil."
  (and org-node-backlink-do-drawers
       (org-node-backlink--props-exist-p)
       (display-warning 'org-node-backlink "
User option `org-node-backlink-do-drawers' is t,
but found :BACKLINKS: lines in some property drawers,
so doing nothing.
This is a new default in v2, you probably just need to toggle it.
Or use `org-node-backlink-mass-delete-props'.")))

(defun org-node-backlink--props-exist-p ()
  "Return t if property lines called :BACKLINKS: exist in some file."
  (cl-loop for node being the hash-values of org-nodes
           when (assoc-string "BACKLINKS" (org-node-get-properties node) t)
           return t))


;;; Global minor mode

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated.

See Info node `(org-node)'.

-----"
  :global t
  (if org-node-backlink-mode
      (progn
        (advice-add 'org-insert-link :after  #'org-node-backlink--add-in-target)
        (add-hook 'org-node-rescan-functions #'org-node-backlink--maybe-fix-proactively)
        (add-hook 'org-mode-hook             #'org-node-backlink--local-mode)
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (derived-mode-p 'org-mode)
              (org-node-backlink--local-mode)))))
    (advice-remove 'org-insert-link          #'org-node-backlink--add-in-target)
    (remove-hook 'org-node-rescan-functions  #'org-node-backlink--maybe-fix-proactively)
    (remove-hook 'org-mode-hook              #'org-node-backlink--local-mode)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (org-node-backlink--local-mode 0)))))

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

(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer
       .1 nil #'display-warning 'org-node
       "Your initfiles may have misspelled `org-node-backlink-mode' as `org-node-backlinks-mode'"))
    (apply #'org-node-backlink-mode args)))


;;; Mass operations

(defvar org-node-backlink--work-kind nil)
(defvar org-node-backlink--work-files nil)
(defvar org-node-backlink--work-reenable-on-done nil)

(defun org-node-backlink-mass-update-drawers ()
  "Add or update backlinks drawers in all files."
  (interactive)
  (unless org-node-backlink-do-drawers
    (user-error "Asked to update :BACKLINKS: drawers, but `org-node-backlink-do-drawers' is nil"))
  (org-node-backlink--fix-all-files 'add-drawers))

(defun org-node-backlink-mass-update-props ()
  "Add or update backlinks properties in all files."
  (interactive)
  (when org-node-backlink-do-drawers
    (user-error "Asked to update :BACKLINKS: properties, but `org-node-backlink-do-drawers' is t"))
  (org-node-backlink--fix-all-files 'add-props))

(defun org-node-backlink-mass-delete-drawers ()
  "Delete all backlinks drawers in all files."
  (interactive)
  (org-node-backlink--fix-all-files 'del-drawers))

(defun org-node-backlink-mass-delete-props ()
  "Delete all backlinks properties in all files."
  (interactive)
  (org-node-backlink--fix-all-files 'del-props))

(defun org-node-backlink--fix-all-files (kind)
  "Update :BACKLINKS: in all known nodes.
Argument KIND controls how to update them.

Can be quit midway through and resumed later.  With
\\[universal-argument], start over instead of resuming."
  (interactive)
  (org-node-cache-ensure t)
  (unless (or (and (memq kind '(add-drawers add-props))
                   (org-node-backlink--check-v2-misaligned-setting-p))
              (and (eq kind 'add-drawers)
                   (org-node-backlink--check-osl-user-p)))
    ;; Maybe reset file list
    (unless (eq org-node-backlink--work-kind kind)
      (setq org-node-backlink--work-kind kind)
      (setq org-node-backlink--work-files nil))
    (when (or (equal current-prefix-arg '(4))
              (null org-node-backlink--work-files))
      (let* ((files (org-node-list-files t))
             (dirs (org-node--root-dirs files)))
        (when (y-or-n-p
               (format "Confirm: edit %d Org files in these %d directories?\n%s"
                       (length files) (length dirs) dirs))
          (setq org-node-backlink--work-files files))))
    ;; Resume working thru the file list
    (when org-node-backlink--work-files
      (setq org-node-backlink--work-files
            (org-node--in-files-do
              :files org-node-backlink--work-files
              :msg (if (memq kind '(del-drawers del-props))
                       "Removing :BACKLINKS: (you can quit and resume)"
                     "Adding/updating :BACKLINKS: (you can quit and resume)")
              :about-to-do "About to edit :BACKLINKS:"
              :call (##org-node-backlink-fix-buffer kind)
              :too-many-files-hack t
              :cleanup
              (when org-node-backlink-mode
                (setq org-node-backlink--work-reenable-on-done t)
                (org-node-backlink-mode 0)
                (lambda ()
                  (when org-node-backlink--work-reenable-on-done
                    (setq org-node-backlink--work-reenable-on-done nil)
                    (org-node-backlink-mode))))))
      (when (null org-node-backlink--work-files)
        (if (memq kind '(del-drawers del-props))
            (message "Done removing :BACKLINKS:!")
          (message "Done updating :BACKLINKS:!"))))))


;;; Buffer validation

(defvar org-node-backlink--checked nil)
(defun org-node-backlink-fix-buffer (&optional kind)
  "Update :BACKLINKS: properties or drawers in all nodes in buffer.
Let `org-node-backlink-do-drawers' determine which.

Or if KIND is symbol `add-drawers', `del-drawers', `add-props', or
`del-props', do the corresponding thing."
  (interactive)
  (unless (or (and (memq kind '(add-drawers add-props))
                   (org-node-backlink--check-v2-misaligned-setting-p))
              (and (eq kind 'add-drawers)
                   (org-node-backlink--check-osl-user-p)))
    ;; (message "Fixing file %s" buffer-file-name)
    (goto-char (point-min))
    (let ((case-fold-search t))
      ;; NOTE: If there is an entry that has :BACKLINKS:, but that has lost its
      ;;       :ID:, it will never be touched again, but that's on the user.
      (while (re-search-forward "^[ \t]*:id:[ \t]*[[:graph:]]" nil t)
        (org-node-backlink--fix-nearby kind)))))

(defun org-node-backlink--fix-nearby (&optional kind)
  "In current entry, fix the backlinks drawer or property.
Let `org-node-backlink-do-drawers' determine which.

Or if KIND is symbol `add-drawers', `del-drawers', `add-props', or
`del-props', do the corresponding thing."
  (if kind
      (pcase kind
        ('del-props   (org-node-backlink--fix-nearby-property t))
        ('del-drawers (org-node-backlink--fix-nearby-drawer t))
        ('add-props   (org-node-backlink--fix-nearby-property))
        ('add-drawers (org-node-backlink--fix-nearby-drawer)))
    (if org-node-backlink-do-drawers
        (org-node-backlink--fix-nearby-drawer)
      (org-node-backlink--fix-nearby-property))))

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
                           (org-node-backlink--fix-nearby)))
                    ;; ...and if the change-area is massive, spanning multiple
                    ;; subtrees (like after a big yank), update each subtree
                    ;; within
                    (while (and (< (point) end)
                                (re-search-forward "^[\t\s]*:id: +" end t))
                      (org-node-backlink--fix-nearby))
                    (remove-text-properties start end 'org-node-flag))
                  ;; This change-area dealt with, move on
                  (set-marker start (marker-position end)))
                (set-marker start nil)
                (set-marker end nil))))
        (( error )
         (unless (remove-text-properties 1 (point-max) 'org-node-flag)
           (message "org-node: Did not remove org-node-flag text property"))
         ;; Provide backtrace even tho we don't signal an error
         (when debug-on-error
           (backtrace))
         (message "org-node: Updating backlinks ran into an issue: %S" err))))))


;;; Property

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
                          (seq-sort #'string<)))
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

(defun org-node-backlink--add-to-property (id title)
  "Insert a link with ID and TITLE into nearby :BACKLINKS: property."
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
            ;; Prevent reacting to this edit (would be redundant at best)
            (org-node-backlink--inhibit-flagging t))
        (org-entry-put nil "BACKLINKS" new-value)
        (unless user-is-editing
          (let ((before-save-hook nil)
                (after-save-hook nil))
            (save-buffer)))))))


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
            ;; `org-id-find-id-file' has terrible fallback behavior
            (setq dest-file (ignore-errors
                              (org-node-get-file
                               (gethash dest-id org-nodes)))))
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
                           (org-get-title)
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
                      (if (get-text-property (point) 'read-only)
                          ;; If for some reason the search landed us in a
                          ;; transclude region or other read-only area...  Note
                          ;; that `org-entry-put' not only doesn't signal an
                          ;; error, it inhibits read-only and goes ahead to
                          ;; make edits!
                          (error "org-node: Area seems to be read-only at %d in %s"
                                 (point) (buffer-name))
                        (org-node-backlink--add origin-id origin-title))
                    (push dest-id org-node-backlink--fails)))))))))))

(defun org-node-backlink--add (id title)
  "Add link with ID and TITLE into local backlink drawer or property."
  (if org-node-backlink-do-drawers
      (org-node-backlink--add-to-drawer id title)
    (org-node-backlink--add-to-property id title)))


;;; Drawers

(defcustom org-node-backlink-drawer-sorter
  #'org-node-backlink-timestamp-lessp
  "Function for sorting lines in the backlinks drawer."
  :type '(radio
          (function-item org-node-backlink-timestamp-lessp)
          (function-item org-node-backlink-link-description-lessp)
          (function-item org-node-backlink-id-lessp)
          (function-item org-node-backlink-id-blind-simple-lessp)
          (function-item string-lessp)
          (function-item string-collate-lessp)
          (function :tag "Custom function" :value (lambda (s1 s2))))
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-backlink-drawer-sort-in-reverse nil
  "Whether to reverse how lines are sorted in the backlinks drawer."
  :type 'boolean
  :package-version '(org-node . "2.0.0"))

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

(defcustom org-node-backlink-drawer-positioner nil
  "Function for moving point before placing a new drawer.
Called in a buffer narrowed to one Org entry, excluding any other
headings.  Point is at the beginning of the body text as determined by
`org-node--end-of-meta-data'.

The function may return anything, but if it returns an integer or
marker, point will move to that position.

Only called if a drawer was not already present."
  :type '(radio (const :tag "Leave at beginning of body" nil)
                (function-item org-entry-end-position)
                (function :tag "Custom function" :value (lambda ())))
  :package-version '(org-node . "2.0.0"))

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

(defun org-node-backlink-timestamp-lessp (s1 s2)
  "Sort on first Org timestamp in the line.
S1 before S2 if timestamp in S1 is earlier in time."
  (let ((ts-1 (org-node-backlink--extract-timestamp s1))
        (ts-2 (org-node-backlink--extract-timestamp s2)))
    (or (and ts-1 (not ts-2))
        (and ts-1 ts-2 (org-time< ts-1 ts-2)))))

(defun org-node-backlink-link-description-lessp (s1 s2)
  "Sort on first link description in the line.
S1 before S2 if link descriptions inside satisfy `string<'."
  (string< (org-node-backlink--extract-link-desc s1)
           (org-node-backlink--extract-link-desc s2)))

(defun org-node-backlink-id-blind-simple-lessp (s1 s2)
  "Sort lexicographically, but ignoring nonsense inside [[id:...]].
S1 before S2 if the strings sans org-ids satisfy `string<'."
  (string< (replace-regexp-in-string "\\[\\[id:.*?]" "" s1)
           (replace-regexp-in-string "\\[\\[id:.*?]" "" s2)))

(defun org-node-backlink-id-lessp (s1 s2)
  "Sort on first [[id:...]] in the line.
S1 before S2 if the IDs inside satisfy `string<'.

May be useful with a non-default `org-id-method'."
  (string< (org-node-backlink--extract-id s1)
           (org-node-backlink--extract-id s2)))

(defun org-node-backlink-format-like-org-super-links-default
    (id desc &optional time)
  "Example: \"[2025-02-21 Fri 14:39] <- [[id:ID][Node title]]\".
ID and DESC are link id: and description, TIME a Lisp time value."
  (concat (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " <- "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-with-time (id desc &optional time)
  "Example: \"- [2025-02-21 Fri 14:39] [[id:ID][Node title]]\".
ID and DESC are link id: and description, TIME a Lisp time value."
  (concat "- "
          (format-time-string (org-time-stamp-format t t)
                              (or time (current-time)))
          " "
          (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink-format-as-bullet-no-time (id desc &optional _time)
  "Example: \"- [[id:ID][Node title]]\".
ID and DESC are link id: and description, TIME a Lisp time value."
  (concat "- " (org-link-make-string (concat "id:" id) desc)))

(defun org-node-backlink--add-to-drawer (id title)
  "Add new backlink with ID and TITLE to nearby drawer.
Designed for use by `org-node-backlink--add-in-target'."
  (if (or (org-node-backlink--check-v2-misaligned-setting-p)
          (org-node-backlink--check-osl-user-p))
      (org-node-backlink-mode 0)
    (save-excursion
      (save-restriction
        (org-node-narrow-to-drawer-create
          "BACKLINKS" org-node-backlink-drawer-positioner)
        (catch 'break
          (let (lines)
            (while (search-forward "[[id:" (pos-eol) t)
              (let ((id-found (buffer-substring-no-properties
                               (point)
                               (- (re-search-forward "].\\|::") 2))))
                (if (equal id-found id)
                    ;; No need to link to itself
                    (throw 'break nil)
                  (push (buffer-substring-no-properties (pos-bol) (pos-eol))
                        lines)
                  (forward-line 1))))
            (let ((org-node-backlink--inhibit-flagging t))
              (insert "\n"
                      (funcall org-node-backlink-drawer-formatter id title)
                      "\n")
              ;; Re-sort so the just-inserted link ends up in the correct place
              (let ((sorted-lines
                     (sort (split-string (buffer-string) "\n" t)
                           org-node-backlink-drawer-sorter)))
                (when org-node-backlink-drawer-sort-in-reverse
                  (setq sorted-lines (nreverse sorted-lines)))
                (atomic-change-group
                  (delete-region (point-min) (point-max))
                  (insert (string-join sorted-lines "\n")))))))))))

(defun org-node-backlink--fix-nearby-drawer (&optional remove)
  "Update nearby backlinks drawer so it reflects current reality.
Designed for use by `org-node-backlink--fix-nearby'.
If REMOVE non-nil, remove it instead."
  (if remove
      (org-node--delete-drawer "BACKLINKS")
    (when-let* ((id (org-entry-get nil "ID"))
                (node (gethash id org-nodes))
                (origins (thread-last
                           (append (org-node-get-id-links-to node)
                                   (org-node-get-reflinks-to node))
                           (mapcar (##plist-get % :origin))
                           (delete-dups))))
      (save-excursion
        (save-restriction
          (org-node-narrow-to-drawer-create
            "BACKLINKS" org-node-backlink-drawer-positioner)
          (let* ((org-node-backlink--inhibit-flagging t)
                 (lines (split-string (buffer-string) "\n" t))
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
              (when-let* ((known-node (gethash id org-nodes)))
                (let ((title (org-node-get-title known-node)))
                  (indent-according-to-mode)
                  (insert (funcall org-node-backlink-drawer-formatter id title)
                          "\n"))))
            ;; Membership is correct, now re-sort so the order is correct
            (let ((sorted-lines
                   (sort (split-string (buffer-string) "\n" t)
                         org-node-backlink-drawer-sorter)))
              (when org-node-backlink-drawer-sort-in-reverse
                (setq sorted-lines (nreverse sorted-lines)))
              (atomic-change-group
                (delete-region (point-min) (point-max))
                (insert (string-join sorted-lines "\n"))))))))))

(defun org-node-backlink--reformat-line (line)
  "Pass LINE back through `org-node-backlink-drawer-formatter'."
  (funcall org-node-backlink-drawer-formatter
           (org-node-backlink--extract-id line)
           (org-node-backlink--extract-link-desc line)
           (encode-time (parse-time-string
                         (org-node-backlink--extract-timestamp line)))))


;;; Proactive fixing

;; REVIEW: Only comes into effect on `org-node-rescan-functions', but could
;;         also other times?  Possible the algo could be simpler, rather than
;;         diffing `org-node--old-link-sets' from current, simply look in
;;         `org-node-get-properties' of all nodes...

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

(defun org-node-backlink--maybe-fix-proactively (_)
  "Designed for `org-node-rescan-functions'."
  (unless org-node-backlink-lazy
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
                           ;; See if this is a ref and find the real id
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
                  (org-node-backlink--fix-nearby)))
              ;; Normally, `org-node--with-quick-file-buffer' only saves
              ;; buffers it had to open anew.  Let's save even if it was
              ;; open previously.
              (unless user-is-editing
                (let ((before-save-hook nil)
                      (after-save-hook nil))
                  (save-buffer)))))))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
