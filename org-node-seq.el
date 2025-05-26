;;; org-node-seq.el --- Experimental way to define node sequences -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Support programmatically defining node-sequences, based on such things as
;; Org tags, time-stamps and file-names.  Then support easily navigating them.

;; Trivia: Here was a failed rename.  The concept was initally called "series",
;; not "seq", and in retrospect that was better name IMO as:
;; 1. It's less ambiguous, you don't have to clarify that you're talking
;;    about a "node seq" as oppposed to some other kind of seq.
;; 2. The name doesn't beg to be abbreviated like "sequence" does.
;; Anyway, it's not a big enough deal to be worth changing back now.

;;; Code:

;; TODO: More API
;;  - goto latest in a seq
;;  - goto earliest
;;  - list files in a seq
;;  - get a seq w/o needing (alist-get "d" org-node-seqs () () #'equal)

(require 'subr-x)
(require 'cl-lib)
(require 'calendar)
(require 'transient)
(require 'org-node)
(require 'org-mem)
(defvar org-node-proposed-seq)
(defvar org-mem--next-message)
(declare-function org-entry-get-with-inheritance "org")
(declare-function org-up-heading-or-point-min "org")

;;; Easy wrappers to define a sequence

;;;###autoload
(defun org-node-seq-def-on-any-sort-by-property
    (key name prop &optional capture)
  "Define a sequence sorted by property PROP.
If an ID-node does not have property PROP, it is excluded.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'."
  `(,key
    :name ,name
    :version 2
    :capture ,capture
    :classifier (lambda (node)
                  (let ((sortstr (org-mem-entry-property ,prop node)))
                    (when (and sortstr (not (string-blank-p sortstr)))
                      (cons (concat sortstr " " (org-mem-entry-title node))
                            (org-mem-entry-id node)))))
    :whereami (lambda ()
                (when-let* ((sortstr (org-entry-get nil ,prop t))
                            (node (org-node-at-point)))
                  (concat sortstr " " (org-mem-entry-title node))))
    :prompter (lambda (key)
                (let ((seq (cdr (assoc key org-node-seqs))))
                  (completing-read "Go to: " (plist-get seq :sorted-items))))
    :try-goto (lambda (item)
                (org-node-seq-try-goto-id (cdr item)))
    :creator (lambda (sortstr key)
               (let ((adder (lambda () (org-entry-put nil ,prop sortstr))))
                 (add-hook 'org-node-creation-hook adder)
                 (unwind-protect (org-node-create sortstr (org-id-new) key)
                   (remove-hook 'org-node-creation-hook adder))))))

;;;###autoload
(defun org-node-seq-def-on-tags-sort-by-property
    (key name tags prop &optional capture)
  "Define a sequence filtered by TAGS sorted by property PROP.
If a node does not have property PROP, it is excluded.
TAGS is a string of tags separated by colons.

Tag inheritance does not apply; a node must have one or more of TAGS
itself, even if a parent in the outline tree also has them.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'."
  `(,key
    :name ,name
    :version 2
    :capture ,capture
    :classifier (lambda (node)
                  (let ((sortstr (org-mem-entry-property ,prop node))
                        (tagged (seq-intersection (split-string ,tags ":" t)
                                                  (org-mem-entry-tags-local node))))
                    (when (and sortstr tagged (not (string-blank-p sortstr)))
                      (cons (concat sortstr " " (org-mem-entry-title node))
                            (org-mem-entry-id node)))))
    :whereami (lambda ()
                (when (seq-intersection (split-string ,tags ":" t)
                                        (org-get-tags))
                  (let ((sortstr (org-entry-get nil ,prop t))
                        (node (org-node-at-point)))
                    (when (and sortstr node)
                      (concat sortstr " " (org-mem-entry-title node))))))
    :prompter (lambda (key)
                (let ((seq (cdr (assoc key org-node-seqs))))
                  (completing-read "Go to: " (plist-get seq :sorted-items))))
    :try-goto (lambda (item)
                (org-node-seq-try-goto-id (cdr item)))
    ;; NOTE: The sortstr should not necessarily become the title, but we make
    ;;       it so anyway, and the user can edit afterwards.
    ;; REVIEW: This should probably change, better to prompt for title.  But
    ;;         how?
    :creator (lambda (sortstr key)
               (let ((adder (lambda ()
                              (org-entry-put nil ,prop sortstr)
                              (org-node-add-tags (split-string ,tags ":" t)))))
                 (add-hook 'org-node-creation-hook dadder)
                 (unwind-protect (org-node-create sortstr (org-id-new) key)
                   (remove-hook 'org-node-creation-hook adder))))))

;;;###autoload
(defun org-node-seq-def-on-filepath-sort-by-basename
    (key name dir &optional capture date-picker)
  "Define a sequence as all files under directory DIR.
The files need not contain a top-level property drawer, but
they do need to contain at least one ID-node.

For KEY, NAME and CAPTURE, see `org-node-seq-defs'.

When optional argument DATE-PICKER is non-nil, let the prompter use the
interactive Org date picker.

\(Tip: No idea how to use the Org date picker?  See `org-read-date'!)
\(Tip: If you never make notes for the future, you might prefer setting
       `org-read-date-prefer-future' to nil.)

For the date-picker to work as expected, you need file names in
YYYY-MM-DD format, e.g. \"2024-01-31.org\"."
  (cl-assert (file-name-absolute-p dir))
  (setq dir (file-truename dir))
  `(,key
    :name ,name
    :version 2
    :capture ,capture
    :classifier (lambda (node)
                  (when (string-prefix-p ,(file-truename dir)
                                         (org-mem-entry-file-truename node))
                    (let* ((path (org-mem-entry-file node))
                           (sortstr (file-name-base path)))
                      (cons sortstr path))))
    :whereami (lambda ()
                (let ((file-name (org-mem--truename-maybe buffer-file-name)))
                  (when (string-prefix-p ,dir file-name)
                    (file-name-base file-name))))
    :prompter (lambda (key)
                (if ,date-picker
                    (let ((org-node-seq-that-marks-calendar key))
                      (org-read-date))
                  (let ((seq (cdr (assoc key org-node-seqs))))
                    (completing-read "Go to: " (plist-get seq :sorted-items)))))
    :try-goto (lambda (item)
                (org-node-seq-try-goto-file (cdr item)))
    :creator (lambda (sortstr key)
               (let ((org-node-creation-fn #'org-node-new-file)
                     (org-node-file-directory-ask ,dir))
                 (org-node-create sortstr (org-id-new) key)))))


;;; Helpers to use in a seq definition

;;;###autoload
(defun org-node-seq-try-goto-id (id)
  "Try to visit org-id ID and return non-nil, else return nil."
  (let ((node (org-mem-entry-by-id id)))
    (when node
      (org-node--goto node)
      t)))

;;;###autoload
(defun org-node-seq-try-goto-file (file)
  "If FILE exists or a buffer has it as filename, visit that.
On success, return non-nil; else nil.  Never create FILE anew."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (switch-to-buffer buf)
      (when (file-readable-p file)
        (find-file file)))))

;;;###autoload
(defun org-node-seq-filename->ymd (path)
  "Check the filename PATH for a date, and return that date.
On failing to coerce a date, return nil.

Only works for names starting with either an YYYY-MM-DD date, or a
datestamp matching the style of `org-node-file-timestamp-format'.

The latter uses a sloppy algorithm so not all formats work, see
`org-node-seq-extract-ymd'."
  (when path
    (let ((clipped-name (file-name-base path)))
      (if (string-match (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
                        clipped-name)
          (match-string 0 clipped-name)
        ;; Even in a non-daily file, pretend it is a daily if possible,
        ;; to allow entering the sequence at a more relevant date
        (when-let* ((stamp (org-node-extract-file-name-datestamp path)))
          (org-node-seq-extract-ymd stamp org-node-file-timestamp-format))))))

;; TODO: Handle %s, %V, %y...  is there a library?
;;;###autoload
(defun org-node-seq-extract-ymd (instance time-format)
  "Try to extract a YYYY-MM-DD date out of string INSTANCE.
Assume INSTANCE is a string produced by TIME-FORMAT, e.g. if
TIME-FORMAT is %Y%m%dT%H%M%SZ then a possible INSTANCE is
20240814T123307Z.  In that case, return 2024-08-14.

Will throw an error if TIME-FORMAT does not include either %F or
all three of %Y, %m and %d.  May return odd results if other
format-constructs occur before these."
  (let ((verify-re (org-node--make-regexp-for-time-format time-format)))
    (when (string-match-p verify-re instance)
      (let ((case-fold-search nil))
        (let ((pos-year (string-search "%Y" time-format))
              (pos-month (string-search "%m" time-format))
              (pos-day (string-search "%d" time-format))
              (pos-ymd (string-search "%F" time-format)))
          (if (seq-some #'null (list pos-year pos-month pos-day))
              (progn (cl-assert pos-ymd)
                     (substring instance pos-ymd (+ pos-ymd 10)))
            ;; TODO: Explanatory comment please
            (when (> pos-month pos-year) (cl-incf pos-month 2))
            (when (> pos-day pos-year) (cl-incf pos-day 2))
            (concat (substring instance pos-year (+ pos-year 4))
                    "-"
                    (substring instance pos-month (+ pos-month 2))
                    "-"
                    (substring instance pos-day (+ pos-day 2)))))))))

(defun org-node-seq-create-roam-daily (ymd seq-key &optional goto keys)
  "Create an org-roam-dailies note, for a day implied by YMD.
YMD must be a time string in YYYY-MM-DD form.

SEQ-KEY is the key that corresponds to the member of `org-node-seq-defs'
that should grow with the captured item after the capture is done.

GOTO and KEYS like in `org-roam-dailies--capture'."
  (unless (require 'org-roam-dailies nil t)
    (error "`org-node-seq-create-roam-daily' requires library \"org-roam\""))
  (when (fboundp 'org-roam-dailies--capture)
    (let ((creation-hook-runner (lambda () (run-hooks 'org-node-creation-hook))))
      (add-hook 'org-roam-capture-new-node-hook creation-hook-runner)
      (setq org-node-proposed-seq seq-key)
      (unwind-protect
          (org-roam-dailies--capture
           (encode-time
            (parse-time-string (concat ymd (format-time-string " %T %z"))))
           goto keys)
        (remove-hook 'org-roam-capture-new-node-hook creation-hook-runner)
        (setq org-node-proposed-seq nil)))))


;;;; Plumbing

(defcustom org-node-seq-defs nil
  "Alist defining each node sequence.

Each item looks like

\(KEY :name NAME
     :classifier CLASSIFIER
     :whereami WHEREAMI
     :prompter PROMPTER
     :try-goto TRY-GOTO
     :creator CREATOR
     :capture CAPTURE
     :version VERSION)

KEY uniquely identifies the sequence, and is the key to type after
\\[org-node-seq-dispatch] to select it.  It may not be \"j\",
\"n\", \"p\" or \"c\", these keys are reserved for
Jump/Next/Previous/Capture actions.

NAME describes the sequence, in one or a few words.

CLASSIFIER is a single-argument function taking an `org-node' object
and should return a list or cons cell if a sequence-item was found,
otherwise nil.

The list or cons cell may contain anything, but the first element must
be a sort-string, i.e. a string suitable for sorting on.  An example is
a date in the format YYYY-MM-DD, but not in the format MM/DD/YY.

This is what determines the order of items in the sequence: after
all nodes have been processed by CLASSIFIER, the items found
are sorted by the sort-string, using `string>'.

Aside from returning a single item, CLASSIFIER may also return a list of
multiple such items.  This can be useful if e.g. you have a special type
of node that \"defines\" a sequence by simply containing links to each
item that should go into it.

Function PROMPTER may be used during jump/capture/refile to
interactively prompt for a sort-string.  This highlights the
other use of the sort-string: finding our way back from scant
context.

For example, in a sequence of daily-notes sorted on YYYY-MM-DD,
a prompter could use `org-read-date'.

PROMPTER receives one argument: the whole node-seq object, which is a
plist with the same form as one of the values in `org-node-seq-defs' but
includes two extra members :key, corresponding to KEY, and
:sorted-items, which may be useful for interactive completion.

Function WHEREAMI is like PROMPTER in that it should return a
sort-string or nil.  However, it should do this without user
interaction, and may return nil.  For example, if the user is not
currently in a daily-note, the daily-notes\\=' WHEREAMI should
return nil.  It receives no arguments.

Function TRY-GOTO takes a single argument: one of the items
originally created by CLASSIFIER.  That is, a list of not only a
sort-string but any associated data you put in.  If TRY-GOTO
succeeds in using this information to visit a place of interest,
it should return non-nil, otherwise nil.  It should not create or
write anything on failure - reserve that for the CREATOR
function.

Function CREATOR creates a place that did not exist.  For
example, if the user picked a date from `org-read-date' but no
daily-note exists for that date, CREATOR is called to create that
daily-note.  It receives a would-be sort-string as argument.

Optional string CAPTURE indicates the keys to a capture template
to autoselect, when you choose the capture option in the
`org-node-seq-dispatch' menu.

Integer VERSION indicates the definition language used for this
variable.  New sequences should use version 2, as of 2024-09-05.  When
org-node updates the seq definition language, old versions may
still work, but this is not heavily tested, so it will start printing a
message to remind you to check out the wiki on GitHub and port your
definitions."
  :type 'alist
  :group 'org-node
  :package-version '(org-node . "1.9.0")
  :set #'org-node--set-and-remind-reset)

;;;###autoload
(defvar org-node-seqs nil
  "Alist of data for each node sequence.")

(defun org-node-seq--add-item (&optional key)
  "Analyze node near point to maybe grow a node sequence.

The sequence is identified either by KEY, or if that is nil, by the
current value of `org-node-proposed-seq'.  If that is also nil, do
nothing."
  (when (bound-and-true-p org-node-proposed-sequence)
    ;; Old name existed 1.9.0 -> 2.0.0
    (display-warning
     'org-node "Variable org-node-proposed-sequence should be org-node-proposed-seq, check initfiles"))
  (when (or key org-node-proposed-seq)
    (let* ((seq (cdr (assoc (or key org-node-proposed-seq)
                            org-node-seqs)))
           (node-here (org-mem-entry-by-id
                       (org-entry-get-with-inheritance "ID")))
           (new-item (when node-here
                       (funcall (plist-get seq :classifier) node-here))))
      (when new-item
        (unless (member new-item (plist-get seq :sorted-items))
          (push new-item (plist-get seq :sorted-items))
          (sort (plist-get seq :sorted-items)
                (lambda (item1 item2)
                  (string> (car item1) (car item2)))))))))

(defun org-node-seq--jump (key)
  "Prompt for and jump to an entry in node seq identified by KEY."
  (org-node--kill-blank-unsaved-buffers)
  (require 'org)
  (let* ((seq (cdr (assoc key org-node-seqs)))
         (sortstr (funcall (plist-get seq :prompter) key))
         (item (assoc sortstr (plist-get seq :sorted-items))))
    (if item
        (unless (funcall (plist-get seq :try-goto) item)
          (delete item (plist-get seq :sorted-items))
          (funcall (plist-get seq :creator) sortstr key))
      (funcall (plist-get seq :creator) sortstr key))))

(defun org-node-seq--goto-next (key)
  "Visit the next entry in node seq identified by KEY."
  (org-node-seq--goto-previous key t))

(defun org-node-seq--goto-previous (key &optional next)
  "Visit the previous entry in node seq identified by KEY.
With non-nil argument NEXT, visit the next entry, not previous."
  (let* ((seq (cdr (assoc key org-node-seqs)))
         (tail (plist-get seq :sorted-items))
         head
         here)
    (unless tail
      (error "No items in sequence \"%s\"" (plist-get seq :name)))
    ;; Depending on the design of the :whereami lambda, being in a sub-heading
    ;; may block discovering that a parent heading is a member of the sequence,
    ;; so re-try until the top level
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (without-restriction
          (while (and (not (setq here (funcall (plist-get seq :whereami))))
                      (org-up-heading-or-point-min))))))
    (when (or (when here
                ;; Find our location in the sequence
                (cl-loop for item in tail
                         while (string> (car item) here)
                         do (push (pop tail) head))
                (when (equal here (caar tail))
                  (pop tail)
                  ;; Opportunistically clean up duplicate keys
                  (while (equal here (caar tail))
                    (setcar tail (cadr tail))
                    (setcdr tail (cddr tail))))
                t)
              (when (y-or-n-p
                     (format "Not in \"%s\".  Jump to latest in that sequence?"
                             (plist-get seq :name)))
                (setq head (take 1 tail))
                t))
      ;; Usually this should return on the first try, but sometimes stale
      ;; items refer to something that has been erased from disk, so
      ;; deregister each item that TRY-GOTO failed to visit, and try again.
      (cl-loop for item in (if next head tail)
               if (funcall (plist-get seq :try-goto) item)
               return t
               else do (delete item (plist-get seq :sorted-items))
               finally do (message "No %s item in sequence \"%s\""
                                   (if next "next" "previous")
                                   (plist-get seq :name))))))

(defvar org-node-seq--current-key nil
  "Key identifying the node seq currently being browsed in dispatch.
Unlike `org-node-proposed-seq', does not need to revert to nil.")

(defun org-node-seq-capture-target ()
  "Experimental."
  (org-node-cache-ensure)
  (org-node--kill-blank-unsaved-buffers)
  (let ((key (or org-node-seq--current-key
                 (let* ((valid-keys (mapcar #'car org-node-seq-defs))
                        (elaborations
                         (cl-loop for seq in org-node-seq-defs
                                  concat
                                  (format " %s(%s)"
                                          (car seq)
                                          (plist-get (cdr seq) :name))))
                        (input (read-char-from-minibuffer
                                (format "Press any of [%s] to capture into sequence: %s "
                                        (string-join valid-keys ",")
                                        elaborations)
                                (mapcar #'string-to-char valid-keys))))
                   (char-to-string input)))))
    ;; Almost identical to `org-node-seq--jump'
    (require 'org)
    (let* ((seq (cdr (assoc key org-node-seqs)))
           (sortstr (or org-node-proposed-title
                        (funcall (plist-get seq :prompter) key)))
           (item (assoc sortstr (plist-get seq :sorted-items))))
      (when (or (null item)
                (not (funcall (plist-get seq :try-goto) item)))
        ;; TODO: Move point after creation to most appropriate place
        (funcall (plist-get seq :creator) sortstr key)))))

(defun org-node-seq--build-from-def (def)
  "From DEF, make a plist for `org-node-seqs'.
DEF is a seq-def from `org-node-seq-defs'."
  (unless (plist-get (cdr def) :version)
    (user-error "Seq def :version must be 2 or higher"))
  (let ((classifier (org-node--try-ensure-compiled
                     (plist-get (cdr def) :classifier))))
    (nconc
     (cl-loop for elt in (cdr def)
              if (functionp elt)
              collect (org-node--try-ensure-compiled elt)
              else collect elt)
     (cl-loop for node in (org-mem-all-id-nodes)
              as result = (funcall classifier node)
              if (listp (car result))
              nconc result into items
              else collect result into items
              finally return
              ;; Sort by `string>' due to most recent dailies probably being
              ;; most relevant, thus cycling recent dailies will be best perf.
              (list :key (car def)
                    :sorted-items (delete-consecutive-dups
                                   (cl-sort items #'string> :key #'car)))))))

(defcustom org-node-seq-that-marks-calendar nil
  "Key for the node sequence that should mark days in the calendar.

This affects the appearance of the `org-read-date' calendar
popup.  For example, you can use it to indicate which days have a
daily-journal entry.

This need usually not be customized!  When you use
`org-node-seq-dispatch' to jump to a daily-note or some
other date-based sequence, that sequence may be designed to
temporarily set this variable.

Customize this mainly if you want a given node seq to always be
indicated, any time Org pops up a calendar for you.

The sort-strings in the node seq for this key
should be correctly parseable by `parse-time-string'."
  :group 'org-node
  :package-version '(org-node . "1.9.0")
  :type '(choice key (const nil)))

;; TODO: How to cooperate with preexisting marks?
(defun org-node-seq--mark-days ()
  "Mark days in the calendar popup.
The user option `org-node-seq-that-marks-calendar' controls
which dates to mark.

Meant to sit on these hooks:
- `calendar-today-invisible-hook'
- `calendar-today-visible-hook'"
  (when (bound-and-true-p org-node-series-that-marks-calendar)
    ;; Old name existed until 1.9.0
    (display-warning
     'org-node "Variable org-node-series-that-marks-calendar should be org-node-seq-that-marks-calendar, check initfiles"))
  (when org-node-seq-that-marks-calendar
    (calendar-unmark)
    (let* ((seq (cdr (assoc org-node-seq-that-marks-calendar
                            org-node-seqs)))
           (sortstrs (mapcar #'car (plist-get seq :sorted-items)))
           mdy)
      (dolist (date sortstrs)
        ;; Use `parse-time-string' rather than `iso8601-parse' to fail quietly
        (let ((decoded-time (parse-time-string date)))
          ;; Basic check that it could be parsed
          (when (seq-some #'natnump decoded-time)
            (setq mdy (seq-let (_ _ _ d m y) decoded-time
                        (list m d y)))
            (when (calendar-date-is-visible-p mdy)
              (calendar-mark-visible-date mdy))))))))

;;;###autoload
(define-minor-mode org-node-seq-mode
  "Populate sequences according to `org-node-seq-defs'.
This permits \\[org-node-seq-dispatch] to work."
  :global t
  :group 'org-node
  (if org-node-seq-mode
      (progn
        (org-node-cache-ensure t)
        (org-node-seq--reset)
        ;; FIXME: A new node (cached w `org-mem-updater-ensure-id-node-at-point-known')
        ;;        eventually disappears from cache if its buffer is never
        ;;        saved, and then the node seq stops working
        (add-hook 'org-node-creation-hook        #'org-node-seq--add-item)
        (add-hook 'org-mem-post-full-scan-functions #'org-node-seq--reset 50)
        ;; Put ourselves in front of org-roam-dailies unhygienic hook.
        (add-hook 'calendar-today-invisible-hook #'org-node-seq--mark-days 5)
        (add-hook 'calendar-today-visible-hook   #'org-node-seq--mark-days 5))
    (remove-hook 'org-node-creation-hook        #'org-node-seq--add-item)
    (remove-hook 'org-mem-post-full-scan-functions #'org-node-seq--reset)
    (remove-hook 'calendar-today-invisible-hook #'org-node-seq--mark-days)
    (remove-hook 'calendar-today-visible-hook   #'org-node-seq--mark-days)))

(defvar org-node-seq--auto-enabled-once nil)

;; Not used inside this package; a convenience for end users.
(defun org-node-seq-goto (key sortstr)
  "Visit an entry in sequence identified by KEY.
The entry to visit has sort-string SORTSTR.  Create if it does
not exist."
  (when (not (assoc key org-node-seqs))
    (if org-node-seq-defs
        (if org-node-seq-mode
            (error "No seq for key \"%s\", maybe do M-x org-mem-reset?" key)
          (if org-node-seq--auto-enabled-once
              (user-error "`org-node-seq-mode' not enabled")
            (setq org-node-seq--auto-enabled-once t)
            (org-node-seq-mode)))
      (error "`org-node-seq-defs' not configured for key \"%s\"" key)))
  (let* ((seq (cdr (assoc key org-node-seqs)))
         (item (assoc sortstr (plist-get seq :sorted-items))))
    (require 'org)
    ;; TODO: When `item' not found, still run :try-goto and pass it a
    ;;       list that contains only `sortstr', so it has a shot at finding a
    ;;       pre-existing file/id even if `org-node-seqs' is not up to date
    ;;       for some reason.
    ;;       Would be nice, but we'd need to be able to assume that any
    ;;       :try-goto in the wild can handle being passed such input, so
    ;;       we first need to update the definition language and document
    ;;       the new constraint.
    (when (or (null item)
              (if (funcall (plist-get seq :try-goto) item)
                  nil
                (delete item (plist-get seq :sorted-items))
                t))
      ;; FIXME: Sometimes we hit an error here if :creator tries to create a
      ;; file that already exists -- :try-goto failed because the file
      ;; contains no IDs so :classifier did not create an item for it.
      (funcall (plist-get seq :creator) sortstr key))))

(defun org-node-seq--reset (&optional _)
  "Wipe and re-build all seqs."
  (setq org-node-seqs nil)
  (let ((T (current-time)))
    (dolist (def org-node-seq-defs)
      (setf (alist-get (car def) org-node-seqs () () #'equal)
            (org-node-seq--build-from-def def))
      ;; TODO: Clear any old seq from menu
      (org-node-seq--add-to-dispatch (car def) (plist-get (cdr def) :name)))
    (when (and org-mem--next-message
               org-node-seqs)
      (setq org-mem--next-message
            (concat org-mem--next-message
                    (format " + %.2fs caching org-node-seqs"
                            (float-time (time-since T))))))))

(defun org-node-seq--add-to-dispatch (key name)
  "Use KEY and NAME to add a sequence to the Transient menu."
  (when (ignore-errors (transient-get-suffix 'org-node-seq-dispatch key))
    (transient-remove-suffix 'org-node-seq-dispatch key))
  (transient-append-suffix 'org-node-seq-dispatch '(0 -1)
    (list key name key))
  ;; Make the switches mutually exclusive
  (let ((old (car (slot-value (get 'org-node-seq-dispatch 'transient--prefix)
                              'incompatible))))
    (setf (slot-value (get 'org-node-seq-dispatch 'transient--prefix)
                      'incompatible)
          (list (seq-uniq (cons key old))))))

(transient-define-suffix org-node-seq--goto-previous* (args)
  (interactive (list (transient-args 'org-node-seq-dispatch)))
  (if args
      (org-node-seq--goto-previous (car args))
    (message "Choose sequence before navigating")))

(transient-define-suffix org-node-seq--goto-next* (args)
  (interactive (list (transient-args 'org-node-seq-dispatch)))
  (if args
      (org-node-seq--goto-next (car args))
    (message "Choose sequence before navigating")))

(transient-define-suffix org-node-seq--jump* (args)
  (interactive (list (transient-args 'org-node-seq-dispatch)))
  (if args
      (org-node-seq--jump (car args))
    (message "Choose sequence before navigating")))

(transient-define-suffix org-node-seq--capture (args)
  (interactive (list (transient-args 'org-node-seq-dispatch)))
  (if args
      (progn (setq org-node-seq--current-key (car args))
             (unwind-protect
                 (let* ((seq (cdr (assoc (car args) org-node-seqs)))
                        (capture-keys (plist-get seq :capture)))
                   (if capture-keys
                       (org-capture nil capture-keys)
                     (message "No capture template for sequence: %s"
                              (plist-get seq :name))))
               (setq org-node-seq--current-key nil)))
    (message "Choose sequence before navigating")))

;; TODO: In Emacs 30, simplify to just ###autoload.
;;;###autoload (autoload 'org-node-seq-dispatch "org-node-seq" nil t)
(transient-define-prefix org-node-seq-dispatch ()
  ["Sequence"
   ;; TODO: Don't use the invisible placeholder.
   ;; https://github.com/magit/transient/issues/49#issuecomment-2289762426
   ("|" "Invisible" "Placeholder" :if-nil t)]
  ["Navigation"
   ("p" "Previous in sequence" org-node-seq--goto-previous* :transient t)
   ("n" "Next in sequence" org-node-seq--goto-next* :transient t)
   ("j" "Jump (or create)" org-node-seq--jump*)
   ("c" "Capture into" org-node-seq--capture)]
  (interactive)
  (when org-node-seq-defs
    (unless (or org-node-seq--auto-enabled-once
                org-node-seq-mode)
      (setq org-node-seq--auto-enabled-once t)
      (org-node-seq-mode)))
  (cond ((not org-node-seq-defs)
         (message "`org-node-seq-defs' not defined"))
        ((not org-node-seq-mode)
         (message "`org-node-seq-mode' not enabled"))
        ((not org-node-seqs)
         (org-node-seq--reset)
         (transient-setup 'org-node-seq-dispatch))
        (t
         (transient-setup 'org-node-seq-dispatch))))

(define-obsolete-function-alias 'org-node-seq-try-visit-file
  #'org-node-seq-try-goto-file "3.0.0 (May 2025)")

(provide 'org-node-seq)

;;; org-node-seq.el ends here
