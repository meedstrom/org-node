;;; org-node.el --- Fast org-roam replacement -*- lexical-binding: t; -*-

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

;; Author:   Martin Edström <meedstrom91@gmail.com>
;; URL:      https://github.com/meedstrom/org-node
;; Created:  2024-04-13
;; Keywords: org, hypermedia
;; Package-Requires: ((emacs "29.1") (llama "0.5.0") (org-mem "0.16.0") (magit-section "4.3.0"))

;; Looking for Package-Version?  Consult the Git tag.
;;       MELPA versions above 20250303 is v2.
;;       MELPA versions above 20250515 is v3.

;;; Commentary:

;; If you were the sort of person to prefer "id:" links,
;; over "file:" links, radio-targets or any other type of link,
;; you're in the right place!

;; Now you can worry less about mentally tracking subtree hierarchies
;; and directory structures.  Once you've assigned an ID to something,
;; you can find it later.

;; The philosophy is the same as org-roam: if you assign an ID every
;; time you make an entry that you know you might want to link to from
;; elsewhere, then it tends to work out that the `org-node-find' command
;; can jump to more-or-less every entry you'd ever want to jump to.

;; That's just the core of it as described to someone not familiar with
;; zettelkasten-inspired software.  In fact, out of the simplicity
;; arises something powerful, more to be experienced than explained.

;; Compared to Org-roam:

;;   + Compatible (you can use both packages and compare)
;;   + Fast
;;   + No SQLite
;;   + If you want, opt out of those file-level :PROPERTIES: drawers
;;     + See user option `org-node-prefer-with-heading'
;;   + Try to rely in a bare-metal way on upstream org-id and org-capture
;;   + Extra utilities, notably to auto-rename files and links
;;   + An alternative way to display backlinks

;;   - No support for "roam:" links
;;   - Smaller package ecosystem
;;     + Packages based on org-roam can still work!  Either by using
;;       org-roam at the same time, or by configuring
;;       `org-mem-roamy-do-overwrite-real-db' and enabling
;;       `org-mem-roamy-db-mode'.
;;   - Blind to TRAMP files (for now)

;; Compared to Denote:

;;   + Compatible (you can use both packages and compare)
;;   + No mandatory filename style (can match Denote format if you like)
;;   + You can have as many "notes" as you want inside one file.
;;     + You could possibly use Denote for coarse browsing,
;;       and Org-node for more granular browsing.

;;   - No support for "denote:" links
;;   - No support for Markdown or other file types
;;   - Blind to TRAMP files (for now)

;;; Code:

(eval-when-compile
  (require 'org)
  (require 'org-id)
  (require 'org-macs)
  (require 'org-fold)
  (require 'org-element))
(require 'cl-lib)
(require 'subr-x)
(require 'fileloop)
(require 'llama)
(require 'org-node-changes)
(require 'org-mem)
(require 'org-mem-updater)

(declare-function consult--grep "ext:consult")
(declare-function consult--grep-make-builder "ext:consult")
(declare-function consult--ripgrep-make-builder "ext:consult")
(declare-function org-at-heading-p "org")
(declare-function org-back-to-heading "org")
(declare-function org-before-first-heading-p "org")
(declare-function org-collect-keywords "org")
(declare-function org-current-level "org")
(declare-function org-cut-subtree "org")
(declare-function org-end-of-meta-data "org")
(declare-function org-entry-end-position "org")
(declare-function org-entry-get "org")
(declare-function org-entry-get-with-inheritance "org")
(declare-function org-entry-properties "org")
(declare-function org-entry-put "org")
(declare-function org-find-property "org")
(declare-function org-fold-reveal "org-fold")
(declare-function org-fold-show-children "org-fold")
(declare-function org-fold-show-context "org-fold")
(declare-function org-get-buffer-tags "org")
(declare-function org-get-tags "org")
(declare-function org-get-title "org")
(declare-function org-in-block-p "org")
(declare-function org-in-regexp "org-macs")
(declare-function org-in-src-block-p "org")
(declare-function org-insert-drawer "org")
(declare-function org-insert-heading "org")
(declare-function org-invisible-p "org-macs")
(declare-function org-link-display-format "ol")
(declare-function org-link-make-string "ol")
(declare-function org-lint "org-lint")
(declare-function org-map-region "org")
(declare-function org-mem-roamy-mk-backlinks "org-mem-roamy")
(declare-function org-mem-roamy-mk-reflinks "org-mem-roamy")
(declare-function org-mem-updater-ensure-id-node-at-point-known "org-mem-updater")
(declare-function org-paste-subtree "org")
(declare-function org-promote "org")
(declare-function org-remove-empty-drawer-at "org")
(declare-function org-roam-node-id "ext:org-roam-node")
(declare-function org-set-tags "org")
(declare-function org-time-stamp-format "org")
(declare-function org-up-heading-or-point-min "org")
(declare-function outline-next-heading "outline")
(defvar consult-ripgrep-args)
(defvar org-drawer-regexp)
(defvar org-node-backlink-mode)
(defvar org-roam-capture-templates)
(defvar org-roam-preview-function)
(defvar org-roam-preview-postprocess-functions)


;;;; Some options

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

(defcustom org-node-data-dir user-emacs-directory
  "Directory in which to persist data between sessions."
  :type `(choice (const :value ,user-emacs-directory)
                 directory)
  :package-version '(org-node . "2.0.0"))

(defcustom org-node-prefer-with-heading nil
  "Make a heading even when creating isolated file nodes.
If nil, write a #+TITLE and a file-level property-drawer instead.

In other words:

- if nil, make file with no heading (outline level 0)
- if t, make file with heading (outline level 1)

This affects the behavior of `org-node-new-file',
`org-node-extract-subtree', and `org-node-capture-target'.

If you change your mind about this setting, you can
transition the files you already have with the Org-roam commands
`org-roam-promote-entire-buffer' and `org-roam-demote-entire-buffer'."
  :type 'boolean
  :package-version '(org-node . "0.4"))

(defun org-node--set-and-remind-reset (sym val)
  "Set SYM to VAL.
Then remind the user to run \\[org-mem-reset]."
  (let ((caller (cadr (backtrace-frame 5))))
    (when (and (boundp 'org-node--first-init)
               (not org-node--first-init)
               ;; TIL: loading a theme calls ALL custom-setters?!
               (not (memq caller '(custom-theme-recalc-variable load-theme))))
      (lwarn 'org-node :debug
             "org-node--set-and-remind-reset called by %s" caller)
      (run-with-timer
       .1 nil #'message
       "Remember to run M-x org-mem-reset after configuring %S" sym)))
  (custom-set-default sym val))

(defcustom org-node-custom-link-format-fn nil
  "Function to format inserted links specially.
Takes a node as argument, should return a string."
  :type '(choice (const nil) function)
  :package-version '(org-node . "2.3.3"))

;; TODO: Compose a list of functions?
(defcustom org-node-filter-fn #'org-node-filter-no-roam-exclude-p
  "Predicate returning non-nil to include a node, or nil to exclude it.

The filter affects two tables:
- `org-node--candidate<>entry', used by completions in the minibuffer
- `org-node--title<>affixations', used by `org-node-complete-at-point-mode'

In other words, returning nil means the user cannot autocomplete to the
node, but Lisp code can still find it in the output of
`org-mem-all-id-nodes', and backlinks are discovered normally.

This function is applied once for every ID-node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-peek] and specified
in the type `org-mem-entry' (C-h o org-mem-entry RET)."
  :type '(radio (function-item org-node-filter-no-roam-exclude-p)
                (function-item org-node-filter-watched-dir-p)
                (function :tag "Custom function"
                          :value (lambda (node) t)))
  :set #'org-node--set-and-remind-reset
  :package-version '(org-node . "3.3.0"))

(defun org-node-filter-no-roam-exclude-p (node)
  "Hide NODE if it has a :ROAM_EXCLUDE: property.
Does not hide if it merely inherits that property from an ancestor."
  (not (org-mem-property "ROAM_EXCLUDE" node)))

(defun org-node-filter-watched-dir-p (node)
  "Show NODE only if it is found inside `org-mem-watch-dirs'."
  (let ((file (org-mem-file-truename node)))
    (cl-some (lambda (dir) (string-prefix-p dir file))
             (with-memoization (org-mem--table 0 'true-watch-dirs)
               (with-temp-buffer ;; No buffer-env
                 (mapcar #'file-truename org-mem-watch-dirs))))))

(defcustom org-node-insert-link-hook nil
  "Hook run after inserting a link, with point in the new link."
  :type 'hook
  :package-version '(org-node . "0.1"))

(defcustom org-node-creation-hook nil
  "Hook run with point in the newly created file or entry.

A good function for this hook is `org-node-ensure-crtime-property',
since the default `org-node-file-timestamp-format' is empty."
  :type 'hook
  :package-version '(org-node . "0.1"))

(defcustom org-node-relocation-hook nil
  "Hook run with point in the newly relocated file or entry.

A relocation is an operation like `org-node-refile' or
`org-node-extract-subtree', such that some of the node\\='s data was
already known."
  :type 'hook
  :package-version '(org-node . "3.2.0"))

(defun org-node--hack-record-candidate ()
  "Ensure you can spam-create nodes with `org-node-titlegen-untitled'."
  (when-let* ((title (or (org-get-heading t t t t)
                         (org-get-title))))
    (puthash title (list title "" "") org-node--title<>affixations)
    (puthash title (org-mem-updater-mk-entry-atpt) org-node--candidate<>entry)))

(unless (featurep 'org-node)
  (add-hook 'org-node-insert-link-hook #'org-mem-updater-ensure-link-at-point-known -50)
  (add-hook 'org-node-creation-hook    #'org-id-get-create -90)
  (add-hook 'org-node-creation-hook    #'org-node-ensure-crtime-property -85)
  (add-hook 'org-node-creation-hook    #'org-node--hack-record-candidate -80)
  (add-hook 'org-node-creation-hook    #'org-mem-updater-ensure-id-node-at-point-known -70)
  (add-hook 'org-node-relocation-hook  #'org-mem-updater-ensure-id-node-at-point-known -70))


;;;; Pretty completion

(defcustom org-node-alter-candidates nil
  "Whether to alter completion candidates instead of affixating.

This means that org-node will concatenate the results of
`org-node-affixation-fn' into a single string, so what the user types in
the minibuffer can match against the prefix and suffix as well as
against the node title.

In other words: you can match against the node's outline path, if
as `org-node-affixation-fn' is set to `org-node-prepend-olp'
\(default).

\(Tip: users of the \"orderless\" library do not need this
setting, they can always match against the prefix and suffix via
`orderless-annotation', bound to the character \& by default.\)

Another consequence: this setting can lift the uniqueness constraint on
note titles: you\\='ll be able to have two nodes with the same name, so
long as their prefix or suffix differ in some way.

After changing this setting, run \\[org-mem-reset]."
  :type 'boolean
  :set #'org-node--set-and-remind-reset
  :package-version '(org-node . "0.2"))

(defcustom org-node-affixation-fn #'org-node-prepend-olp
  "Function to give prefix and suffix to minibuffer completions.

After changing this setting, run \\[org-mem-reset].

------
Info for writing a custom function

The function receives two arguments: NODE and TITLE, and it must return
a list of three strings: title, prefix and suffix.  Of those three, the
title should be TITLE unmodified.

NODE is an object which form you can observe in examples from
\\[org-node-peek] and specified in type `org-mem-entry'
\(for docs, type \\[describe-symbol] org-mem-entry RET).

If a node has aliases, the same node is passed to this function
again for every alias, in which case TITLE is actually one of the
aliases."
  :type '(radio
          (function-item org-node-affix-bare)
          (function-item org-node-prepend-olp)
          (function-item org-node-prepend-tags)
          (function-item org-node-prepend-tags-and-olp)
          (function-item org-node-prepend-olp-append-tags)
          (function-item org-node-prepend-olp-append-tags-use-frame-width)
          (function-item org-node-append-tags-use-frame-width)
          (function :tag "Custom function"
                    :value (lambda (node title) (list title "" ""))))
  :package-version '(org-node . "0.2")
  :set #'org-node--set-and-remind-reset)

(defun org-node-affix-bare (_node title)
  "Use TITLE as-is."
  (list title "" ""))

(defun org-node-prepend-tags (node title)
  "Prepend NODE\\='s tags to TITLE."
  (list title
        (let ((tags (org-mem-entry-tags node)))
          (if tags
              (propertize (concat "(" (string-join tags ", ") ") ")
                          'face 'org-tag)
            ""))
        ""))

(defun org-node-prepend-olp (node title)
  "Prepend NODE\\='s outline path to TITLE."
  (list title
        (if-let* ((fontified-ancestors
                   (cl-loop
                    for ancestor in (org-mem-olpath-with-file-title node)
                    collect
                    (propertize ancestor 'face 'completions-annotations))))
            (concat (string-join fontified-ancestors " > ") " > ")
          "")
        ""))

(defun org-node-prepend-tags-and-olp (node title)
  "Prepend NODE's tags and outline path to TITLE."
  (list title
        (let ((tags (org-mem-entry-tags node))
              (fontified-ancestors
               (cl-loop
                for ancestor in (org-mem-olpath-with-file-title node)
                collect (propertize ancestor 'face 'completions-annotations))))
          (concat
           ;; TODO: Fallback on other face before org init
           (and tags (propertize (concat "(" (string-join tags ", ") ") ")
                                 'face 'org-tag))
           (and fontified-ancestors
                (concat (and tags " ")
                        (string-join fontified-ancestors " > ") " > "))))
        ""))

(defun org-node-prepend-olp-append-tags (node title)
  "Prepend NODE's outline path to TITLE, and append NODE\\='s tags."
  (list title
        (if (org-mem-entry-subtree-p node)
            (let ((ancestors (org-mem-olpath-with-file-title node))
                  (result nil))
              (dolist (anc ancestors)
                (push (propertize anc 'face 'completions-annotations) result)
                (push " > " result))
              (setq result (apply #'concat (nreverse result)))
              result)
          "")
        (let ((tags (org-mem-entry-tags node)))
          (if tags (propertize (concat "   :" (string-join tags ":") ":")
                               'face 'org-tag)
            ""))))

(defun org-node-append-tags-use-frame-width (node title)
  "Append NODE tags after TITLE and justify them to `frame-width'."
  (list title
        ""
        (concat
         (let ((tags (org-mem-entry-tags node)))
           (when tags
             (setq tags (propertize (concat ":" (string-join tags ":") ":")
                                    'face 'org-tag))
             (concat (make-string (max 2 (- (frame-width)
                                            (string-width title)
                                            (string-width tags)
                                            (fringe-columns 'right)
                                            (fringe-columns 'left)))
                                  ?\s)
                     tags))))))

(defun org-node-prepend-olp-append-tags-use-frame-width (node title)
  "Prepend NODE outline path to TITLE, and put NODE tags at frame edge."
  (let (olp)
    (list title
          (concat
           (when (org-mem-entry-subtree-p node)
             (let ((ancestors (org-mem-olpath-with-file-title node)))
               (dolist (anc ancestors)
                 (push (propertize anc 'face 'completions-annotations) olp)
                 (push " > " olp))
               (setq olp (apply #'concat (nreverse olp))))))
          (concat
           (let ((tags (org-mem-entry-tags node)))
             (when tags
               (setq tags (propertize (concat ":" (string-join tags ":") ":")
                                      'face 'org-tag))
               (concat (make-string (max 2 (- (frame-width)
                                              (string-width title)
                                              (if olp (string-width olp) 0)
                                              (string-width tags)
                                              (fringe-columns 'right)
                                              (fringe-columns 'left)))
                                    ?\s)
                       tags)))))))

(defcustom org-node-blank-input-hint
  (propertize "(untitled node)" 'face 'completions-annotations)
  "Whether to add an empty completion candidate to some commands.
It helps indicate that a blank input can be used there to
create an untitled node.

If non-nil, the value is actually the annotation added to it, a
propertized string."
  :type '(choice string (const nil))
  :package-version '(org-node . "3.3.0"))

;; Perhaps it would be handy if the blank input could do smart things like
;; create a daily-note, but it was not obvious how to shoehorn that into the
;; system we already have.  This was what we could easily do.
(defcustom org-node-blank-input-title-generator #'org-node-titlegen-untitled
  "Function to generate a title, given no user input.
Used in some commands when exiting minibuffer with a blank string."
  :type '(radio (function-item org-node-titlegen-untitled)
                (function-item org-node-titlegen-today)
                (function-item org-node-titlegen-this-week)
                (function :tag "Custom function" :value (lambda ())))
  :package-version '(org-node . "3.3.12"))

(defvar org-node-untitled-format "untitled-%d")
(defvar org-node--untitled-ctr 1)
(defun org-node-titlegen-untitled ()
  "Combine `org-node-untitled-format' with a new integer on every call."
  (cl-loop for title = (format org-node-untitled-format org-node--untitled-ctr)
           while (or (gethash title org-node--title<>affixations)
                     (file-exists-p (org-node-title-to-filename-quiet title)))
           do (cl-incf org-node--untitled-ctr)
           finally return title))

(defun org-node-titlegen-today ()
  "Make a node title for the day\\='s date."
  (format-time-string "Assorted for %A, %d %b %Y"))

(defun org-node-titlegen-this-week ()
  "Make a node title for the current ISO8601 week."
  (format-time-string "Assorted for Week %V, %G"))

(defvar org-node--candidate<>entry (make-hash-table :test 'equal)
  "1:1 table mapping minibuffer completion candidates to ID-nodes.
These candidates may or may not be pre-affixated, depending on
user option `org-node-alter-candidates'.")

(defvar org-node--title<>affixations (make-hash-table :test 'equal)
  "1:1 table mapping titles or aliases to affixation triplets.
Even when the triplets are not used, this table serves double-duty such
that its keys constitute the subset of `org-mem--title<>id' that
passed `org-node-filter-fn'.")

(defun org-node-collection-main (&rest args)
  "Pass ARGS to the right collection depending on user settings.
The collections are trivial variants of `org-node-collection-basic'."
  (if org-node-blank-input-hint
      (apply #'org-node-collection--with-empty args)
    (apply #'org-node-collection-basic args)))

;; TODO: Assign a category `org-node', then add an embark action to embark?
;; TODO: Bind a custom exporter to `embark-export'
;; TODO: Add user options to set 'group-function, 'display-sort-function etc?
;; TODO: See consult-org-roam.
(defun org-node-collection-basic (str pred action)
  "Custom COLLECTION for `completing-read'.

Ahead of time, org-node takes titles and aliases from all nodes, runs
`org-node-affixation-fn' on each, and depending on the user option
`org-node-alter-candidates', it either saves the affixated thing
directly into `org-node--candidate<>entry', or into a secondary table
`org-node--title<>affixations'.  Finally, this function then either
simply reads candidates off the candidates table, or attaches the
affixations in realtime.

Regardless of which, all completions except the empty string are
guaranteed to be keys of `org-node--candidate<>entry' \(which is the
main reason for this design\), but remember that it is possible for
`completing-read' to exit with user-entered input that didn\\='t match
anything.

Arguments STR, PRED and ACTION are handled behind the scenes,
see Info node `(elisp)Programmed Completion'."
  (if (eq action 'metadata)
      (cons 'metadata (list (cons 'affixation-function #'org-node--affixate)))
    (complete-with-action action org-node--candidate<>entry str pred)))

(defun org-node-collection--with-empty (str pred action)
  "A collection that includes an empty candidate at the front.
STR, PRED and ACTION as in `org-node-collection-basic'."
  (let ((blank (if (bound-and-true-p helm-mode) " " "")))
    (if (eq action 'metadata)
        (cons 'metadata (list (cons 'affixation-function #'org-node--affixate)))
      (complete-with-action
       action
       (cons blank (hash-table-keys org-node--candidate<>entry))
       str
       pred))))

(defun org-node--affixate (collection)
  "From list COLLECTION, make an alist of ((TITLE PREFIX SUFFIX) ...)."
  (if (and (car collection) (string-blank-p (car collection)))
      (nconc (list (list (car collection) "" (or org-node-blank-input-hint "")))
             (if org-node-alter-candidates
                 (cl-loop for candidate in (cdr collection)
                          collect (list candidate "" ""))
               (cl-loop for title in (cdr collection)
                        collect (or (gethash title org-node--title<>affixations)
                                    (list title "" "")))))
    (if org-node-alter-candidates
        (cl-loop for candidate in collection
                 collect (list candidate "" ""))
      (cl-loop for title in collection
               collect (or (gethash title org-node--title<>affixations)
                           (list title "" ""))))))

(defvar org-node-hist nil
  "Minibuffer history.")

;; Second hist introduced in 3.3.16; transition by copying from first.
(if (boundp 'org-node-hist-altered)
    (defvar org-node-hist-altered nil
      "Minibuffer history.")
  (defvar org-node-hist-altered org-node-hist
    "Minibuffer history."))

;; Boost completion hist to at least 1000 elements, unless user has nerfed
;; the default `history-length'.
;; Because you often narrow down the completions majorly, and still want to
;; sort among what's left.
(when (and (>= history-length (car (get 'history-length 'standard-value)))
           (< history-length 1000))
  (put 'org-node-hist 'history-length 1000)
  (put 'org-node-hist-altered 'history-length 1000))

;; Finally, tying it all together
(defun org-node-read-candidate (&optional prompt blank-ok)
  "PROMPT for a known node and return the input.
If the input is not a key of `org-node--candidate<>entry',
you can assume no such node exists.

BLANK-OK means to obey `org-node-blank-input-hint'."
  (completing-read (or prompt "Node: ")
                   (if blank-ok #'org-node-collection-main
                     #'org-node-collection-basic)
                   () () ()
                   (if org-node-alter-candidates 'org-node-hist-altered
                     'org-node-hist)))


;;;; The cache mode

(defun org-node--let-refs-be-aliases (node)
  "Add ROAM_REFS of NODE as extra minibuffer completions."
  (dolist (ref (org-mem-entry-roam-refs node))
    (puthash (concat (when-let* ((type (gethash ref org-mem--roam-ref<>type)))
                       (propertize (concat type ":")
                                   'face 'completions-annotations))
                     (propertize ref 'face 'org-cite))
             node
             org-node--candidate<>entry)))

(defun org-node--record-completion-candidates (node)
  "Cache fancy completion candidates for NODE and its aliases."
  (when (and (org-mem-entry-id node)
             (org-mem-entry-title node)
             (funcall org-node-filter-fn node))
    (dolist (title (cons (org-mem-entry-title node)
                         (org-mem-entry-roam-aliases node)))
      (let ((affx (funcall org-node-affixation-fn node title)))
        (puthash title affx org-node--title<>affixations)
        (if org-node-alter-candidates
            ;; Absorb the affixations into one candidate string
            (puthash (concat (nth 1 affx) (nth 0 affx) (nth 2 affx))
                     node
                     org-node--candidate<>entry)
          ;; Bare title, to be affixated later
          (puthash title node org-node--candidate<>entry))))))

(defun org-node--wipe-completions (_parse-results)
  "Clear completions tables."
  (clrhash org-node--title<>affixations)
  (clrhash org-node--candidate<>entry))

;; Could have used `org-mem-forget-file-functions', but more efficient to loop
;; over the whole parse-results.
(defun org-node--forget-completions-in-results (parse-results)
  "Remove old completions where PARSE-RESULTS has new data."
  (seq-let (bad-paths file-data) parse-results
    (org-node--forget-completions-in-files
     (append bad-paths (mapcar #'car file-data)))))

(defun org-node--forget-completions-in-files (files)
  "Remove the minibuffer completions for all nodes in FILES."
  (when files
    (maphash (lambda (candidate entry)
               (when (member (org-mem-entry-file-truename entry) files)
                 (remhash candidate org-node--candidate<>entry)))
             org-node--candidate<>entry)))

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Cache completion candidates every time Org-mem updates its cache.

Enabling this mode asks org-mem to reset its cache.
You should enable `org-mem-updater-mode' at nearly the same time in your
init process, because then org-mem will tend to only scan files once
rather than twice."
  :global t
  (cond
   (org-node-cache-mode
    (add-hook 'org-mem-pre-full-scan-functions #'org-node--wipe-completions)
    (add-hook 'org-mem-pre-targeted-scan-functions #'org-node--forget-completions-in-results)
    (add-hook 'org-mem-record-entry-functions #'org-node--record-completion-candidates)
    (add-hook 'org-mem-record-entry-functions #'org-node--let-refs-be-aliases)
    (org-mem-reset))
   (t
    (remove-hook 'org-mem-pre-full-scan-functions #'org-node--wipe-completions)
    (remove-hook 'org-mem-pre-targeted-scan-functions #'org-node--forget-completions-in-results)
    (remove-hook 'org-mem-record-entry-functions #'org-node--record-completion-candidates)
    (remove-hook 'org-mem-record-entry-functions #'org-node--let-refs-be-aliases))))

;;;###autoload
(define-obsolete-function-alias 'org-node-reset #'org-mem-reset "3.0.0 (May 2025)")

;; TODO: Deprecate somehow
(defvar org-node--first-init t
  "Non-nil until org-node has been initialized, then nil.")

(defun org-node-cache-ensure (&optional block force)
  "Ensure that org-node is ready for use.
Ensure that modes `org-node-cache-mode' and `org-mem-updater-mode' are
enabled.  If FORCE, trigger org-mem to rebuild cache.  If BLOCK and a
cache build is underway \(perhaps started by FORCE), block Emacs until
it finishes.

If cache has never been built, act as if both FORCE and BLOCK.

It is good to call this function at the start of autoloaded commands.
Most of the time, you can expect it to no-op.

These cache builds are normally async, so without BLOCK, this returns
immediately and can mean that the data you will next query from org-mem
is still out of date.  That usually only matters if you had done
something to change the facts on the ground just prior."
  (org-node-changes--onetime-warn-and-copy)
  (setq org-node--first-init nil)
  (unless org-node-cache-mode
    (when (y-or-n-p "Org-node needs `org-node-cache-mode', enable? ")
      (org-node-cache-mode))
    (setq force t))
  (unless org-mem-updater-mode
    (when (y-or-n-p "Org-node needs `org-mem-updater-mode', enable? ")
      (org-mem-updater-mode))
    (setq force t))
  (when (hash-table-empty-p org-node--candidate<>entry)
    (setq block t)
    (setq force t))
  (when force
    (org-mem-reset nil "Org-node waiting for org-mem..."))
  (when block
    (org-mem-await "Org-node waiting for org-mem..." 10)))


;;;; Filename functions

(defcustom org-node-file-directory-ask nil
  "Whether to ask the user where to save a new file.

- Value nil: Let `org-node-guess-dir' decide
- Value t: Ask every time
- String: A directory path in which to put the file"
  :type '(choice boolean directory)
  :package-version '(org-node . "0.1"))

;; This setting needs care with `org-node-rename-file-by-title' after changing.
;; https://blog.ganssle.io/articles/2023/01/attractive-nuisances.html
(defcustom org-node-file-timestamp-format ""
  "Passed to `format-time-string' to prepend to filenames.

Example from Org-roam: %Y%m%d%H%M%S-
Example from Denote: %Y%m%dT%H%M%S--

For the rest of the filename, configure `org-node-file-slug-fn'."
  :type '(radio
          (const :tag "None" :value "")
          (const :tag "Like Org-roam: %Y%m%d%H%M%S-" :value "%Y%m%d%H%M%S-")
          (const :tag "Like Denote: %Y%m%dT%H%M%S--" :value "%Y%m%dT%H%M%S--")
          (string :tag "Custom"))
  :package-version '(org-node . "0.4"))

(defcustom org-node-file-slug-fn #'org-node-slugify-for-web
  "Function taking a node title and returning a filename component.
Receives one argument: the value of an Org #+TITLE keyword, or
the first heading in a file that has no #+TITLE.

It is popular to also prefix filenames with a datestamp.  To do
that, configure `org-node-file-timestamp-format'."
  :type '(radio
          (function-item org-node-slugify-for-web)
          (function-item org-node-slugify-like-roam-default)
          (function :tag "Custom function" :value (lambda (title) title)))
  :package-version '(org-node . "0.4"))

(defun org-node--root-dirs (file-list)
  "Infer root directories of FILE-LIST.

By root, we mean the longest directory path common to a set of files,
as long as that directory contains at least one member of
FILE-LIST itself.  For example, if you have the 3 members

- \"/home/me/Syncthing/foo.org\"
- \"/home/kept/archive/bar.org\"
- \"/home/kept/baz.org\"

the return value will not be \(\"/home/\"), even though
the substring \"/home/\" is common to all three,
but \(\"/home/kept/\" \"/home/me/Syncthing/\").


On finding more than one root, sort by count of files they contain
recursively, so that the most populous root directory will be the first
element.

This function does not consult the filesystem, so FILE-LIST must be a
list of full paths that can be compared as strings, so e.g. there must
e.g. not be instances of substring \"~\" as well as instances of
substring \"/home/me\" referring to the same location."
  (let* (file-name-handler-alist ;; Otherwise 90% cpu in `file-name-directory'
         ;; A lot of dups from e.g. (hash-table-values org-id-locations)
         (files (seq-uniq file-list))
         (dirs (sort (delete-consecutive-dups
                      (sort (mapcar #'file-name-directory files) #'string<))
                     (##length< %1 (length %2))))
         root-dirs)
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child dir.
    (while-let ((dir (car (last dirs))))
      ;; REVIEW: Maybe more elegant to use `nreverse' twice?
      (setq dirs (nbutlast dirs))
      (cl-loop for other-dir in dirs
               when (string-prefix-p other-dir dir)
               return (delete dir dirs)
               finally return (push dir root-dirs)))
    (if (= (length root-dirs) 1)
        root-dirs
      ;; Found more than 1 root, sort them by count of files inside.
      (cl-loop
       with dir-counters = (cl-loop for dir in root-dirs collect (cons dir 0))
       for file in files
       do (cl-loop for dir in root-dirs
                   when (string-prefix-p dir file)
                   return (cl-incf (cdr (assoc dir dir-counters))))
       finally return (mapcar #'car (cl-sort dir-counters #'> :key #'cdr))))))

(defun org-node-title-to-filename-quiet (title)
  "From TITLE, make a full file path, and never prompt."
  (file-name-concat (if (stringp org-node-file-directory-ask)
                        org-node-file-directory-ask
                      (org-node-guess-dir))
                    (org-node-title-to-basename title)))

(defun org-node-title-to-basename (title)
  "From TITLE, make the non-directory component of a file name."
  (concat (format-time-string org-node-file-timestamp-format)
          (funcall (org-node--try-ensure-compiled org-node-file-slug-fn) title)
          ".org"))

(defun org-node-guess-dir ()
  "Return the root level of user\\='s apparently most-used directories."
  (car (org-node--root-dirs (org-mem-all-files))))

;; TODO: It'd be more user-friendly if the interactive prompt also lets you
;;       change the basename.  So, conditionally call `org-node-file-slug-fn'
;;       here, then use `read-file-name' rather than `read-directory-name'.
;;       But first we need some refactoring elsewhere.
(defun org-node-guess-or-ask-dir (prompt)
  "Maybe prompt for a directory, and if so, use string PROMPT.
Behavior depends on user option `org-node-file-directory-ask'."
  (if (eq t org-node-file-directory-ask) (read-directory-name prompt)
    (if (stringp org-node-file-directory-ask) org-node-file-directory-ask
      (org-node-guess-dir))))

(defun org-node-slugify-like-roam-default (title)
  "From TITLE, make a filename slug in default org-roam style.
Does not require org-roam installed.

A title like \"Löb\\='s Theorem\" becomes \"lob_s_theorem\".

Diacritical marks U+0300 to U+0331 are stripped \(mostly used with Latin
alphabets).  Also stripped are all glyphs not categorized in Unicode as
belonging to an alphabet or number system.

If you seek to emulate org-roam filenames, you may also want to
configure `org-node-file-timestamp-format'."
  (thread-last title
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[^[:alnum:]]" "_")
               (replace-regexp-in-string "__*" "_")
               (replace-regexp-in-string "^_" "")
               (replace-regexp-in-string "_$" "")))

(defun org-node-slugify-for-web (title)
  "From TITLE, make a filename slug meant to look nice as URL component.

A title like \"Löb\\='s Theorem\" becomes \"lobs-theorem\".

Diacritical marks U+0300 to U+0331 are stripped \(mostly used with Latin
alphabets).  Also stripped are all glyphs not categorized in Unicode as
belonging to an alphabet or number system."
  (thread-last title
               (string-glyph-decompose)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (string-glyph-compose)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

;; Hacking on the above?  Some useful test cases!

;; (org-node-slugify-for-web "A/B testing")
;; (org-node-slugify-for-web "\"But there's still a chance, right?\"")
;; (org-node-slugify-for-web "Löb's Theorem")
;; (org-node-slugify-for-web "Mañana Çedilla")
;; (org-node-slugify-for-web "How to convince me that 2 + 2 = 3")
;; (org-node-slugify-for-web "E. T. Jaynes")
;; (org-node-slugify-for-web "Amnesic recentf? Solution: Run kill-emacs-hook every 2 minutes.")
;; (org-node-slugify-for-web "Slimline/\"pizza box\" computer chassis")
;; (org-node-slugify-for-web "#emacs")
;; (org-node-slugify-for-web "칹え🐛")

;; TODO: For Denote users, you get valid filename with just the default slug fn,
;;       and a datestamp "%Y%m%dT%H%M%S--".  However, the full format including
;;       optional parts looks like:
;;       DATE==SIGNATURE--TITLE__KEYWORDS.EXT
;;
;;       So either the user adds SIGNATURE and KEYWORDS themselves, and never
;;       uses `org-node-rename-file-by-title'.  Or we could pass more arguments
;;       into the slug fn, that it can use to systematically add these parts
;;       based on some rule, like perhaps the content of an Org property.


;;;; Creation functions

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.
Automatically set, should be nil most of the time.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.
Automatically set, should be nil most of the time.")

(defvar org-node-proposed-seq nil
  "Key that identifies a node sequence about to be added-to.
Automatically set, should be nil most of the time.")

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called to create a node that does not yet exist.
Used by commands such as `org-node-find'.

Some choices:
- `org-node-new-file'
- `org-node-new-via-roam-capture'
- `org-capture'

It is pointless to choose `org-capture' here unless you configure
`org-capture-templates' such that some capture templates use
`org-node-capture-target' as their target.

If you wish to write a custom function instead of any of the
above three choices, know that two variables are set at the time
the function is called: `org-node-proposed-title' and
`org-node-proposed-id', which it is expected to obey."
  :group 'org-node
  :type '(radio
          (function-item org-node-new-file)
          (function-item org-node-new-via-roam-capture)
          (function-item org-capture)
          (function :tag "Custom function" :value (lambda ())))
  :package-version '(org-node . "0.1"))

(defun org-node-create (title id &optional seq-key)
  "Call `org-node-creation-fn' with necessary variables set.

TITLE will be title of node, ID will be id of node \(pass
an output of `org-id-new' if you don\\='t know\).

Optional argument SEQ-KEY means use the resulting node to
maybe grow the corresponding sequence.

When calling from Lisp, you should not assume anything about
which buffer will be current afterwards, since it depends on
`org-node-creation-fn', whether TITLE or ID had existed, and
whether the user carried through with the creation.

To operate on a node after creating it, hook onto
`org-node-creation-hook' temporarily:

    \(let ((fix-up (lambda () ...)))
      (add-hook \\='org-node-creation-hook fix-up)
      (unwind-protect (org-node-create TITLE ID)
        (remove-hook \\='org-node-creation-hook fix-up))"
  (let ((org-node-proposed-title title)
        (org-node-proposed-id id)
        (org-node-proposed-seq seq-key))
    (funcall org-node-creation-fn)))

(defun org-node-new-file ()
  "Create a new file with a new node.
Designed for `org-node-creation-fn'."
  (unless org-node-proposed-title (error "Proposed title was nil"))
  (unless org-node-proposed-id (error "Proposed ID was nil"))
  (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
         (path-to-write
          (file-name-concat dir (org-node-title-to-basename org-node-proposed-title))))
    (when (file-exists-p path-to-write)
      (let ((msg (format "org-node: Resetting cache because file already exists: %s"
                         path-to-write)))
        (org-mem-reset t msg)
        (user-error "%s" msg)))
    (when (find-buffer-visiting path-to-write)
      (error "A buffer already exists for filename %s" path-to-write))
    (mkdir dir t)
    (find-file path-to-write)
    (if org-node-prefer-with-heading
        (insert "* " org-node-proposed-title
                "\n:PROPERTIES:"
                "\n:ID:       " org-node-proposed-id
                "\n:END:"
                "\n")
      (insert ":PROPERTIES:"
              "\n:ID:       " org-node-proposed-id
              "\n:END:"
              "\n#+title: " org-node-proposed-title
              "\n"))
    (goto-char (point-max))
    (push (current-buffer) org-node--new-unsaved-buffers)
    (run-hooks 'org-node-creation-hook)))

(defun org-node-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Designed for `org-node-creation-fn'."
  (unless org-node-proposed-title (error "Proposed title was nil"))
  (unless org-node-proposed-id (error "Proposed ID was nil"))
  (unless (require 'org-roam nil t)
    (error "`org-node-new-via-roam-capture' requires library \"org-roam\""))
  (when (and (fboundp 'org-roam-capture-)
             (fboundp 'org-roam-node-create))
    (let ((creation-hook-runner (##run-hooks 'org-node-creation-hook)))
      (add-hook 'org-roam-capture-new-node-hook creation-hook-runner)
      (unwind-protect (org-roam-capture-
                       :node (org-roam-node-create
                              :title org-node-proposed-title
                              :id    org-node-proposed-id))
        (remove-hook 'org-roam-capture-new-node-hook creation-hook-runner)))))

(defun org-node-capture-target ()
  "Can be used as target in a capture template.
See `org-capture-templates' for more info about targets.

In simple terms, let\\='s say you have configured
`org-capture-templates' so it has a template that
targets `(function org-node-capture-target)'.  Now here\\='s a
possible workflow:

1. Run `org-capture'
2. Select your template
3. Type name of known or unknown node
4a. If it was known, it will capture into that node.
4b. If it was unknown, it will create a file-level node and then
    capture into there.

Additionally, with (setq org-node-creation-fn #\\='org-capture),
commands like `org-node-find' will outsource to `org-capture' when you
type the name of a node that does not exist.  That enables this
\"inverted\" workflow, familiar to Org-roam users:

1. Run `org-node-find'
2. Type name of an unknown node
3. Select your template
4. Same as 4b earlier."
  (org-node-cache-ensure)
  (let (title node id)
    (if org-node-proposed-title
        ;; Was called from `org-node-create', so the user had typed the
        ;; title and no such node exists yet, or was invoked externally
        ;; by something that pre-set the title and id
        (progn
          (setq title org-node-proposed-title)
          (setq id (or org-node-proposed-id (error "Proposed ID was nil")))
          (setq node (org-mem-entry-by-id id)))
      (when org-node-proposed-id (error "Proposed title was nil but not ID"))
      ;; Was called from `org-capture', which means the user has not yet typed
      ;; the title; let them type it now
      (let ((input (org-node-read-candidate nil t)))
        (when (string-blank-p input)
          (setq input (funcall org-node-blank-input-title-generator)))
        (setq node (gethash input org-node--candidate<>entry))
        (if node
            (progn
              (setq id (org-mem-entry-id node))
              (setq title (org-mem-entry-title node)))
          (setq id (org-id-new))
          (setq title input))))

    (unless title (error "Given title was nil"))
    (unless id (error "Given ID was nil"))
    (if node
        ;; Node exists; capture into it
        (progn
          (org-node--goto node t)
          ;; TODO: Figure out how to play well with :prepend vs not :prepend.
          ;; Now it's just like it always prepends, I think?
          (unless (and (= 1 (point)) (org-at-heading-p))
            ;; Go to just before next heading, or end of buffer if there are no
            ;; more headings.  This allows the template to insert subtrees
            ;; without swallowing content that was already there.
            (when (outline-next-heading)
              (backward-char 1))))
      ;; Node does not exist; capture into new file
      (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
             (path-to-write
              (file-name-concat dir (org-node-title-to-basename title))))
        (when (file-exists-p path-to-write)
          (error "File already exists: %s" path-to-write))
        ;; TODO: Maybe just use the buffer, especially if it is blank.
        (when (find-buffer-visiting path-to-write)
          (error "A buffer already has the filename %s" path-to-write))
        (mkdir (file-name-directory path-to-write) t)
        (find-file path-to-write)
        (if org-node-prefer-with-heading
            (insert "* " title
                    "\n:PROPERTIES:"
                    "\n:ID:       " id
                    "\n:END:"
                    "\n")
          (insert ":PROPERTIES:"
                  "\n:ID:       " id
                  "\n:END:"
                  "\n#+title: " title
                  "\n"))
        (push (current-buffer) org-node--new-unsaved-buffers)
        (run-hooks 'org-node-creation-hook)))))


;;;; Commands 1: Cute little commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To behave like Org-roam when creating new nodes,
set `org-node-creation-fn' to `org-node-new-via-roam-capture'."
  (interactive)
  (org-node-cache-ensure)
  (let* ((input (org-node-read-candidate "Visit or create node: " t))
         (_ (when (string-blank-p input)
              (setq input (funcall org-node-blank-input-title-generator))))
         (node (gethash input org-node--candidate<>entry)))
    (require 'org-id)
    (if node
        (org-node--goto node)
      (org-node-create input (org-id-new)))))

;;;###autoload
(defun org-node-insert-heading ()
  "Insert a heading with ID and run `org-node-creation-hook'.
Tip: Command `org-node-nodeify-entry' is more widely applicable, it can
be sufficient to key-bind that one."
  (interactive "*" org-mode)
  (org-insert-heading)
  (org-node-nodeify-entry))

;;;###autoload
(defun org-node-nodeify-entry ()
  "Add an ID to entry at point and run `org-node-creation-hook'."
  (interactive "*" org-mode)
  (org-node-cache-ensure) ;; Because the hook could contain anything
  (run-hooks 'org-node-creation-hook))

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive "*" org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED"
                   (format-time-string (org-time-stamp-format t t)))))
(defalias 'org-node-ensure-crtime-property 'org-node-put-created)

(defun org-node-visit-random-1 ()
  "Visit a random node."
  (interactive)
  (org-node-cache-ensure)
  (org-node--goto (seq-random-elt
                   (hash-table-values org-node--candidate<>entry))))

;;;###autoload
(defun org-node-visit-random ()
  "Visit a random node.
Repeatable when bound to a key and `repeat-on-final-keystroke' is t."
  (interactive)
  (let ((repeat-message-function #'ignore))
    (setq last-repeatable-command #'org-node-visit-random-1)
    (repeat nil)))

;; TODO: Optionally obey `org-node-filter-fn'
;;;###autoload
(defun org-node-grep ()
  "Grep across all files known to org-node."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "This command requires package \"consult\""))
  (org-node-cache-ensure)
  ;; Prevent consult from turning the names relative, with such enlightening
  ;; directory paths as ../../../../../../.
  (cl-letf (((symbol-function #'file-relative-name)
             (lambda (name &optional _dir) name)))
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --type=org")))
      (if (executable-find "rg")
          (consult--grep "Grep in files known to org-mem: "
                         #'consult--ripgrep-make-builder
                         (org-node--root-dirs (org-mem-all-files))
                         nil)
        ;; Much slower!  Vanilla grep does not have Ripgrep's --type=org, so
        ;; must target thousands of files and not a handful of dirs, a calling
        ;; pattern that would also slow Ripgrep down.
        (consult--grep "(Ripgrep not found) Grep in files known to org-mem: "
                       #'consult--grep-make-builder
                       (org-mem-all-files)
                       nil)))))


;;;; Commands 2: Inserting things

;;;###autoload
(defun org-node-insert-link (&optional region-as-initial-input novisit)
  "Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*', or pass REGION-AS-INITIAL-INPUT t.

Argument NOVISIT for use by `org-node-insert-link-novisit'."
  (interactive "*" org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (let* ((beg nil)
         (end nil)
         (region-text (when (region-active-p)
                        (setq end (region-end))
                        (goto-char (region-beginning))
                        (skip-chars-forward "\n[:space:]")
                        (setq beg (point))
                        (goto-char end)
                        (skip-chars-backward "\n[:space:]")
                        (setq end (point))
                        (org-link-display-format
                         (buffer-substring-no-properties beg end))))
         (initial (if (or region-as-initial-input
                          (and region-text
                               (try-completion region-text
                                               org-node--title<>affixations)))
                      region-text
                    nil))
         (input (if (and novisit initial)
                    initial
                  (org-node-read-candidate nil t)))
         (_ (when (string-blank-p input)
              (setq input (funcall org-node-blank-input-title-generator))))
         (node (gethash input org-node--candidate<>entry))
         (id (if node (org-mem-id node) (org-id-new)))
         (link-desc (or region-text
                        (and node
                             org-node-custom-link-format-fn
                             (funcall org-node-custom-link-format-fn node))
                        (and (not org-node-alter-candidates) input)
                        (and node (seq-find (##string-search % input)
                                            (org-mem-entry-roam-aliases node)))
                        (and node (org-mem-entry-title node))
                        input)))
    (atomic-change-group
      (when region-text
        (delete-region beg end))
      ;; TODO: When inserting a citation, insert a [cite:] instead of a normal
      ;;       link
      ;; (if (string-prefix-p "@" input))
      (insert (org-link-make-string (concat "id:" id) link-desc)))
    (run-hooks 'org-node-insert-link-hook)
    ;; TODO: Delete the link if a node was not created
    ;;       See `org-node-insert-transclusion'
    (unless node
      (org-node-create input id))))

;;;###autoload
(defun org-node-insert-link* ()
  "Insert a link to one of your ID nodes.

Unlike `org-node-insert-link', emulate `org-roam-node-insert' by
always copying any active region as initial input.

That behavior can be convenient if you often want to use the
selected region as a new node title, or you already know it
matches a node title.

On the other hand if you always find yourself erasing the
minibuffer before selecting some other node you had in mind, to
which the region should be linkified, you\\='ll prefer
`org-node-insert-link'.

The commands are the same, just differing in initial input."
  (interactive "*" org-mode)
  (org-node-insert-link t))

;;;###autoload
(defun org-node-insert-link-novisit ()
  "(Unimplemented) Only supported with `org-node-new-via-roam-capture'.
Insert a link to one of your ID nodes without ever visiting it.

Normally, if the node does not exist, `org-node-insert-link' would
create it and then visit it.  This will not visit it."
  (interactive "*" org-mode)
  (let ((org-roam-capture-templates
         (list (append (car (bound-and-true-p org-roam-capture-templates))
                       '(:immediate-finish t)))))
    (org-node-insert-link nil t)))

;;;###autoload
(defun org-node-insert-link-novisit* ()
  "(Unimplemented) Only supported with `org-node-new-via-roam-capture'.
Insert a link to one of your ID nodes without ever visiting it.

Normally, if the node does not exist, `org-node-insert-link*' would
create it and then visit it.  This will not visit it."
  (interactive "*" org-mode)
  (let ((org-roam-capture-templates
         (list (append (car (bound-and-true-p org-roam-capture-templates))
                       '(:immediate-finish t)))))
    (org-node-insert-link t t)))

(defun org-node-insert-include (&optional node)
  "Insert an #+include referring to NODE.
Tip: It can be previewed with package \"org-include-inline\".
https://github.com/yibie/org-include-inline

The result includes NODE\\='s current file name, unfortunately required."
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (unless node
    (setq node (org-node-read)))
  (if (not node)
      (message "Node not known, create it first")
    (goto-char (pos-eol))
    (insert "\n#+include: \""
            (org-mem-file node)
            "::"
            (org-mem-id node)
            "\"")
    (backward-char)
    (run-hooks 'org-node-insert-link-hook)))

;;;###autoload
(defun org-node-insert-transclusion (&optional node)
  "Insert a #+transclude: referring to NODE."
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (let* ((input (org-node-read-candidate "Transclude node content: " t))
         (_ (when (string-blank-p input)
              (setq input (funcall org-node-blank-input-title-generator))))
         (node (or node (gethash input org-node--candidate<>entry)))
         (id (if node (org-mem-id node) (org-id-new)))
         (title (if node (org-mem-title node) input))
         (buf (current-buffer))
         (here (progn (back-to-indentation) (point-marker))))
    (unless node
      (org-node-create input id))
    (with-current-buffer buf
      (goto-char here)
      (set-marker here nil)
      (unless (eolp) (goto-char (pos-eol)) (newline-and-indent))
      (insert "#+transclude: ")
      (save-excursion (insert " :level " (number-to-string
                                          (+ 1 (or (org-current-level) 0)))))
      (insert (org-link-make-string (concat "id:" id) title))
      (run-hooks 'org-node-insert-link-hook))))

;; TODO: Consider whether to use `org-node-custom-link-format-fn' here.
;;;###autoload
(defun org-node-insert-transclusion-as-subtree (&optional node)
  "Insert a link and a transclusion.
Prompt for NODE if needed.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level 3

but adapt to the surrounding outline level.
If you often transclude file-level nodes, consider adding keywords to
`org-transclusion-exclude-elements':

\(setq org-transclusion-exclude-elements
      \\='(property-drawer comment keyword))"
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (unless node
    (setq node (org-node-read)))
  (if (not node)
      (message "Node not known, create it first")
    (let ((id (org-mem-entry-id node))
          (title (org-mem-entry-title node))
          (level (or (org-current-level) 0))
          (m1 (make-marker)))
      (insert (org-link-make-string (concat "id:" id) title))
      (set-marker m1 (1- (point)))
      (duplicate-line)
      (goto-char (pos-bol))
      (insert (make-string (+ 1 level) ?\*) " ")
      (forward-line 1)
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level " (number-to-string (+ 2 level)))
      (goto-char (marker-position m1))
      (set-marker m1 nil)
      (run-hooks 'org-node-insert-link-hook))))

;; TODO: New default once this PR is merged:
;; https://github.com/nobiot/org-transclusion/pull/268
(defun org-node-insert-transclusion-as-subtree-pull268 (&optional node)
  "Insert a link and a transclusion.
Prompt for NODE if needed.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level :no-first-heading

but adapt to the surrounding outline level.
Requires https://github.com/nobiot/org-transclusion/pull/268

If you often transclude file-level nodes, consider adding keywords to
`org-transclusion-exclude-elements':

\(setq org-transclusion-exclude-elements
      \\='(property-drawer comment keyword))"
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (unless node
    (setq node (org-node-read)))
  (if (not node)
      (message "Node not known, create it first")
    (let ((id (org-mem-entry-id node))
          (title (org-mem-entry-title node))
          (level (org-current-level))
          (link-marker (make-marker)))
      (goto-char (pos-eol))
      (newline)
      (insert (org-link-make-string (concat "id:" id) title))
      (set-marker link-marker (1- (point)))
      (duplicate-line)
      (goto-char (pos-bol))
      (if level
          (insert (make-string (+ (if org-odd-levels-only 2 1) level) ?*) " ")
        (insert "* "))
      (forward-line 1)
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level :no-first-heading")
      (goto-char (marker-position link-marker))
      (set-marker link-marker nil)
      (run-hooks 'org-node-insert-link-hook))))

(defun org-node-insert-raw-link ()
  "Insert input at point, completing to any link ever seen.
Works in non-Org buffers."
  (interactive)
  (insert (completing-read "Insert raw link: "
                           (org-node--list-known-raw-links)
                           nil nil nil 'org-node-link-hist)))

;; TODO: Try to include all Firefox bookmarks and so on
(defun org-node--list-known-raw-links ()
  "Return list of all \(non-ID\) links seen in all known files."
  (let (result)
    (maphash (lambda (target links)
               (let ((types (seq-keep #'org-mem-link-type
                                      (seq-remove #'org-mem-link-citation-p
                                                  links))))
                 (dolist (type (delete-dups (delete "id" types)))
                   (push (concat type ":" target) result))))
             org-mem--target<>links)
    result))

(defcustom org-node-name-of-links-drawer "RELATED"
  "Name of drawer created by command `org-node-insert-into-related'."
  :type 'string
  :package-version '(org-node . "3.3.3"))

(defun org-node-insert-into-related ()
  "Insert a link into a RELATED drawer near the end of current entry.
Unlike the BACKLINKS drawer, this drawer is not \"smart\" and will never
be sorted, reformatted or erased, and the links count like any other
forward-link.  The drawer can be moved, so long as it maintains the name
set in `org-node-name-of-links-drawer'."
  (interactive "*" org-mode)
  (when-let* ((input (org-node-read-candidate))
              (node (gethash input org-node--candidate<>entry)))
    (save-excursion
      (save-restriction
        (org-node-narrow-to-drawer-create org-node-name-of-links-drawer
                                          #'org-entry-end-position)
        (atomic-change-group
          (unless (eolp)
            (let ((col (current-indentation)))
              (newline)
              (indent-to col))
            (forward-line -1)
            (back-to-indentation))
          (insert (format-time-string (org-time-stamp-format t t)) " -> "
                  (org-link-make-string (org-mem-id node)
                                        (org-mem-title node))))))))


;;;; Commands 3: Extract and refile

;;;###autoload
(defun org-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and display the newly created file.

You may find it a common situation that the subtree had not yet
been assigned an ID nor any other property that you normally
assign to your nodes.  Thus, this creates an ID if there was
no ID, copies over all inherited tags \(making them explicit),
and runs `org-node-creation-hook'.

Adding to that, see below for an example advice that copies any
inherited \"CREATED\" property, if an ancestor had such a
property.  It is subjective whether you\\='d want this behavior,
but it can be desirable if you know the subtree had been part of
the source file for ages so that you regard the ancestor\\='s
creation-date as more truthful or useful than today\\='s date.

\(advice-add \\='org-node-extract-subtree :around
            (defun my-inherit-creation-date (orig-fn &rest args)
                   (let ((parent-creation
                          (org-entry-get-with-inheritance \"CREATED\")))
                     (apply orig-fn args)
                     ;; Now in the new buffer
                     (org-entry-put nil \"CREATED\"
                                    (or parent-creation
                                        (format-time-string
                                         (org-time-stamp-format t t)))))))"
  (interactive "*" org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command expects an org-mode buffer"))
  (org-node-cache-ensure)
  (let ((dir (org-node-guess-or-ask-dir "Extract to new file in directory: ")))
    (when (org-invisible-p)
      (user-error "Better not run this command in an invisible region"))
    (org-back-to-heading t)
    (save-buffer)
    (when (org-invisible-p)
      (user-error "Better not run this command in an invisible region"))
    (let* ((tags (org-get-tags))
           (title (org-get-heading t t t t))
           (id (org-id-get-create))
           (boundary (save-excursion
                       (org-end-of-meta-data t)
                       (point)))
           (case-fold-search t)
           ;; Why is CATEGORY autocreated by `org-entry-properties'...  It's
           ;; an invisible property that's always present and usually not
           ;; interesting, unless user has entered some explicit value
           (explicit-category (save-excursion
                                (when (search-forward ":category:" boundary t)
                                  (org-entry-get nil "CATEGORY"))))
           (properties (seq-remove
                        (##string-equal-ignore-case "CATEGORY" (car %))
                        (org-entry-properties nil 'standard)))
           (path-to-write
            (file-name-concat dir (org-node-title-to-basename title)))
           (parent-pos (save-excursion
                         (without-restriction
                           (org-up-heading-or-point-min)
                           (point)))))
      (if (file-exists-p path-to-write)
          (message "A file already exists named %s" path-to-write)
        (if (org-at-heading-p) (org-fold-show-entry) (org-fold-show-context))
        (org-cut-subtree)
        ;; Try to leave a link at the end of parent entry, pointing to the
        ;; ID of subheading that was extracted.
        (unless (bound-and-true-p org-capture-mode)
          (widen)
          (goto-char parent-pos)
          (goto-char (org-entry-end-position))
          (if (org-invisible-p)
              (message "Invisible area, not inserting link to extracted")
            (open-line 1)
            (insert "\n"
                    (format-time-string
                     (format "%s Created " (org-time-stamp-format t t)))
                    (org-link-make-string (concat "id:" id) title)
                    "\n")
            (org-mem-updater-ensure-link-at-point-known id)))
        (find-file path-to-write)
        (org-paste-subtree)
        (unless org-node-prefer-with-heading
          ;; Replace the root heading and its properties with file-level
          ;; keywords &c.
          (goto-char (point-min))
          (org-end-of-meta-data)
          (kill-region (point-min) (point))
          (org-map-region #'org-promote (point-min) (point-max))
          (insert
           ":PROPERTIES:\n"
           (string-join (mapcar (##concat ":" (car %) ": " (cdr %))
                                properties)
                        "\n")
           "\n:END:"
           (if explicit-category
               (concat "\n#+category: " explicit-category)
             "")
           (if tags
               (concat "\n#+filetags: :" (string-join tags ":") ":")
             "")
           "\n#+title: " title
           "\n"))
        (push (current-buffer) org-node--new-unsaved-buffers)
        (run-hooks 'org-node-relocation-hook)))))

;; TODO: Allow refile from some capture templates.  (During a capture,
;; `org-refile' is rebound to `org-capture-refile', which see).  It seems we
;; can just temp override definition of `org-refile' with this one, if I am
;; correct that that long function basically does the equivalent of
;; org-cut-subtree and paste.
;;;###autoload
(defun org-node-refile ()
  "Prompt for a node and refile subtree at point into it.
No support yet for refiling a file."
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command expects an org-mode buffer"))
  (when (org-invisible-p)
    (user-error "Better not run this command in an invisible region"))
  (if (org-before-first-heading-p)
      (user-error "`org-node-refile' only works on subtrees for now")
    (org-node-cache-ensure)
    (let* ((input (org-node-read-candidate "Refile into ID-node: "))
           (node (gethash input org-node--candidate<>entry))
           (origin-buffer (current-buffer)))
      (unless node
        (error "Node not found %s" input))
      (org-cut-subtree)
      (condition-case err
          (org-node--goto node t)
        ((t error)
         (switch-to-buffer origin-buffer)
         (org-paste-subtree)
         (signal (car err) (cdr err)))
        (:success
         (widen)
         (forward-char)
         (org-paste-subtree)
         (run-hooks 'org-node-relocation-hook))))))


;;;; Commands 4: Renaming things

(defun org-node-extract-file-name-datestamp (path)
  "From filename PATH, get the datestamp prefix if it has one.
Do so by comparing with `org-node-file-timestamp-format'.

High risk of false positives if you have been changing formats over
time without renaming existing files."
  (when (and org-node-file-timestamp-format
             (not (string-blank-p org-node-file-timestamp-format)))
    (let ((name (file-name-nondirectory path)))
      (when (string-match
             (org-node--make-regexp-for-time-format org-node-file-timestamp-format)
             name)
        (match-string 0 name)))))

;; "Some people, when confronted with a problem, think
;; 'I know, I'll use regular expressions.'
;; Now they have two problems." —Jamie Zawinski
(defvar org-node--make-regexp-for-time-format nil)
(defun org-node--make-regexp-for-time-format (format)
  "Make regexp to match a result of (format-time-string FORMAT).

In other words, if e.g. FORMAT is %Y-%m-%d, which can be
instantiated in many ways such as 2024-08-10, then this should
return a regexp that can match any of those ways it might turn
out, with any year, month or day."
  (if (equal format (car org-node--make-regexp-for-time-format))
      ;; Reuse memoized value on consecutive calls with same input
      (cdr org-node--make-regexp-for-time-format)
    (cdr (setq org-node--make-regexp-for-time-format
               (cons format
                     (let ((example (format-time-string format)))
                       (if (string-match-p (rx (any "^*+([\\")) example)
                           ;; TODO: Improve error message, now it presumes caller
                           (error "org-node: Unable to safely rename with current `org-node-file-timestamp-format'.  This is not inherent in your choice of format, I am just not smart enough")
                         (concat "^"
                                 (string-replace
                                  "." "\\."
                                  (replace-regexp-in-string
                                   "[[:digit:]]+" "[[:digit:]]+"
                                   (replace-regexp-in-string
                                    "[[:alpha:]]+" "[[:alpha:]]+"
                                    example t)))))))))))

(defcustom org-node-renames-allowed-dirs nil
  "Dirs in which files may be auto-renamed.
Used if you have the function `org-node-rename-file-by-title' on
`after-save-hook' or otherwise call it from Lisp.

To add exceptions, see `org-node-renames-exclude'."
  :type '(repeat directory)
  :package-version '(org-node . "0.7"))

(defcustom org-node-renames-exclude "\\(?:daily\\|dailies\\|journal\\)/"
  "Regexp matching paths of files not to auto-rename.
For use by `org-node-rename-file-by-title'.

Only applied to files under `org-node-renames-allowed-dirs'.  If
a file is not there, it is not considered in any case."
  :type 'string
  :package-version '(org-node . "0.7"))

;;;###autoload
(defun org-node-rename-file-by-title (&optional interactive)
  "Rename the current file according to `org-node-file-slug-fn'.

Also attempt to check for a prefix in the style of
`org-node-file-timestamp-format', and preserve such a prefix.
Otherwise, add one.

Suitable at the end of `after-save-hook'.  If called from a hook
\(or from Lisp in general), only operate on files in
`org-node-renames-allowed-dirs'.  When called interactively as a
command, always prompt for confirmation.

Argument INTERACTIVE automatically set."
  (interactive "p" org-mode)
  ;; Apparently the variable `buffer-file-truename' returns an abbreviated path
  (let ((path (file-truename buffer-file-name))
        (buf (current-buffer))
        (title nil))
    (cond
     ((or (not (derived-mode-p 'org-mode))
          (not (equal "org" (file-name-extension path))))
      (when interactive
        (message "Will only rename Org files")))
     ((and (not interactive)
           (null org-node-renames-allowed-dirs))
      (message "User option `org-node-renames-allowed-dirs' should be configured"))
     ((or interactive
          (cl-loop
           for dir in (mapcar #'file-truename org-node-renames-allowed-dirs)
           if (string-match-p org-node-renames-exclude dir)
           do (error "Regexp `org-node-renames-exclude' would directly match a directory from `org-node-renames-allowed-dirs'")
           else if (and (string-prefix-p dir path)
                        (not (string-match-p org-node-renames-exclude path)))
           return t))
      (setq title
            (or (org-get-title)
                ;; No #+TITLE keyword, so treat first heading as title
                (save-excursion
                  (without-restriction
                    (goto-char 1)
                    (if (org-at-heading-p)
                        (org-get-heading t t t t)
                      (if (org-entry-properties 1 "ID")
                          ;; In the special case when content before the first
                          ;; heading has a property drawer with :ID: (despite
                          ;; absence of #+TITLE), do nothing.
                          nil
                        (outline-next-heading)
                        (org-get-heading t t t t)))))))
      (if (not title)
          (message "org-node-rename-file-by-title: No title in file %s" path)

        (let* ((name (file-name-nondirectory path))
               (date-prefix (or (org-node-extract-file-name-datestamp path)
                                ;; Couldn't find date prefix, give a new one
                                (format-time-string org-node-file-timestamp-format)))
               (slug (funcall org-node-file-slug-fn title))
               (new-name (concat date-prefix slug ".org"))
               (new-path
                (file-name-concat (file-name-directory path) new-name)))
          (cond
           ((equal path new-path)
            (when interactive
              (message "Filename already correct: %s" path)))
           ((string-empty-p slug)
            (when interactive
              (message "Filename would become blank: %s" path)))
           ((or (buffer-modified-p buf)
                (buffer-modified-p (buffer-base-buffer buf)))
            (when interactive
              (message "Unsaved file, letting it be: %s" path)))
           ((find-buffer-visiting new-path)
            (error "Wanted to rename, but a buffer already visits target: %s"
                   new-path))
           ((or (not (file-writable-p path))
                (not (file-writable-p new-path)))
            (error "No permissions to rename file: %s"
                   path))
           ((or (not interactive)
                (y-or-n-p (format "Rename file '%s' to '%s'?" name new-name)))
            (rename-file path new-path)
            (with-current-buffer buf
              (set-visited-file-name new-path t t))
            (message "File '%s' renamed to '%s'" name new-name)))))))))

(defun org-node--consent-to-bothersome-modes-for-mass-edit ()
  "Confirm about certain modes being enabled.
These are modes such as `auto-save-visited-mode' that can
interfere with user experience during or after mass-editing operation."
  ;; TODO: Expand the list, there are probably other annoying modes
  (cl-loop for mode in '(auto-save-visited-mode
                         git-auto-commit-mode)
           when (and (boundp mode)
                     (symbol-value mode)
                     (not (y-or-n-p
                           (format "%S is active - proceed anyway?" mode))))
           return nil
           finally return t))

;; TODO: Kill opened buffers that were not edited.
;;       But first make sure it can pick up where it left off if canceled
;;       midway.  Maybe use `org-node--in-files-do'.
;; REVIEW: Verify `org-node-filter-fn' is used correctly.
;;;###autoload
(defun org-node-rewrite-links-ask (&optional files)
  "Update desynced link descriptions, interactively.

Search all files, or just FILES if non-nil, for ID-links where
the link description has gotten out of sync from the
destination\\='s current title.

At each link, prompt for user consent, then auto-update the link
so it matches the destination\\='s current title."
  (interactive)
  (require 'ol)
  (require 'org-faces)
  (defface org-node--rewrite-face
    `((t :inherit 'org-link
         :inverse-video ,(not (face-inverse-video-p 'org-link))))
    "Face for use in `org-node-rewrite-links-ask'.")
  (org-node-cache-ensure)
  (when (org-node--consent-to-bothersome-modes-for-mass-edit)
    (let ((n-links 0)
          (n-files 0))
      (dolist (file (or files (sort (org-mem-all-files)
                                    (lambda (_ _) (natnump (random))))))
        (cl-incf n-files)
        (with-current-buffer (delay-mode-hooks (find-file-noselect file))
          (save-excursion
            (without-restriction
              (goto-char (point-min))
              (while-let ((end (re-search-forward org-link-bracket-re nil t)))
                (message "Checking... link %d (file #%d)"
                         (cl-incf n-links) n-files)
                (let* ((beg (match-beginning 0))
                       (link (substring-no-properties (match-string 0)))
                       (exact-link (rx (literal link)))
                       (parts (split-string link "]\\["))
                       (target (substring (car parts) 2))
                       (desc (when (cadr parts)
                               (substring (cadr parts) 0 -2)))
                       (id (when (string-prefix-p "id:" target)
                             (substring target 3)))
                       (node (org-mem-entry-by-id id))
                       (true-title (when node
                                     (org-mem-entry-title node)))
                       (custom-desc
                        (and org-node-custom-link-format-fn
                             node
                             (funcall org-node-custom-link-format-fn node)))
                       (answered-yes nil))
                  (when (and id node desc
                             (funcall org-node-filter-fn node)
                             (if custom-desc
                                 (not (string-equal desc custom-desc))
                               (and (not (string-equal-ignore-case
                                          desc
                                          true-title))
                                    (not (member-ignore-case
                                          desc
                                          (org-mem-entry-roam-aliases node))))))
                    (switch-to-buffer (current-buffer))
                    (goto-char end)
                    (if (org-at-heading-p)
                        (org-fold-show-entry)
                      (org-fold-show-context))
                    (recenter)
                    (highlight-regexp exact-link 'org-node--rewrite-face)
                    (unwind-protect
                        (setq answered-yes
                              (y-or-n-p
                               (format "Rewrite link? Will become:  \"%s\""
                                       (or custom-desc true-title))))
                      (unhighlight-regexp exact-link))
                    (when answered-yes
                      (goto-char beg)
                      (atomic-change-group
                        (delete-region beg end)
                        (insert (org-link-make-string
                                 target (or custom-desc true-title))))
                      ;; Give user a moment to glimpse the result before
                      ;; hopping to the next link, in case of a replacement
                      ;; gone wrong
                      (redisplay)
                      (sleep-for .12))
                    (goto-char end)))))))))))

;;;###autoload
(defun org-node-rename-asset-and-rewrite-links ()
  "Helper for renaming images and all links that point to them.

Prompt for an asset such as an image file to be renamed, then search
recursively for Org files containing a link to that asset, open a wgrep
buffer of the search hits, and start an interactive search-replace that
updates the links.  After the user consents or doesn\\='t consent to
replacing all the links, finally rename the asset file itself.  If the
user quits, do not apply any modifications."
  (interactive)
  (unless (require 'wgrep nil t)
    (user-error "This command requires the wgrep package"))
  (when (and (fboundp 'wgrep-change-to-wgrep-mode)
             (fboundp 'wgrep-finish-edit))
    (let ((root (car (org-node--root-dirs (org-mem-all-files))))
          (default-directory default-directory))
      (or (equal default-directory root)
          (if (y-or-n-p (format "Go to folder \"%s\"?" root))
              (setq default-directory root)
            (setq default-directory
                  (read-directory-name
                   "Directory with Org notes to operate on: "))))
      (when-let* ((bufs (seq-filter (##string-search "*grep*" (buffer-name %))
                                    (buffer-list))))
        (when (yes-or-no-p "Kill other *grep* buffers to be sure this works?")
          (mapc #'kill-buffer bufs)))
      (let* ((filename (file-relative-name (read-file-name "File to rename: ")))
             (new (read-string "New name: " filename)))
        (mkdir (file-name-directory new) t)
        (unless (file-writable-p new)
          (error "New path wouldn't be writable"))
        (rgrep (regexp-quote filename) "*.org")
        ;; HACK Doesn't work right away, so wait a sec, then it works
        (run-with-timer
         1 nil
         (lambda ()
           (pop-to-buffer (seq-find (##string-search "*grep*" (buffer-name %))
                                    (buffer-list)))
           (wgrep-change-to-wgrep-mode)
           (goto-char (point-min))
           ;; Interactive replaces
           (query-replace filename new)
           ;; NOTE: If the user quits the replaces with C-g, the following code
           ;;       never runs, which is good.
           (when (buffer-modified-p)
             (wgrep-finish-edit)
             (rename-file filename new)
             (message "File renamed from %s to %s" filename new))))
        (message "Waiting for rgrep to populate buffer...")))))


;;;; Commands 5: Listing things

(defun org-node-peek (&optional _deprecated-arg)
  "Print a random `org-mem-entry' object."
  (interactive)
  (org-node-cache-ensure)
  (let ((entry (seq-random-elt (hash-table-values org-mem--id<>entry)))
        (1arg-funs '(org-mem-active-timestamps
                     org-mem-clocks
                     org-mem-clocks-int
                     org-mem-closed
                     org-mem-deadline
                     org-mem-id
                     org-mem-level
                     org-mem-lnum
                     org-mem-olpath
                     org-mem-olpath-with-self
                     org-mem-olpath-with-file-title-with-self
                     org-mem-olpath-with-file-title
                     org-mem-pos
                     org-mem-priority
                     org-mem-properties
                     org-mem-roam-aliases
                     org-mem-roam-refs
                     org-mem-scheduled
                     org-mem-subtree-p
                     org-mem-tags
                     org-mem-tags-inherited
                     org-mem-tags-local
                     org-mem-title
                     org-mem-title-maybe
                     org-mem-file
                     org-mem-file-attributes
                     org-mem-file-coding-system
                     org-mem-file-id-strict
                     org-mem-file-id-topmost
                     org-mem-file-line-count
                     org-mem-file-mtime
                     org-mem-file-mtime-floor
                     org-mem-file-ptmax
                     org-mem-file-size
                     org-mem-file-title-or-basename
                     org-mem-file-title-strict
                     org-mem-file-title-topmost
                     org-mem-file-truename
                     org-mem-id-links-to-entry
                     org-mem-links-in-entry
                     org-mem-next-entry
                     org-mem-previous-entry
                     org-mem-roam-reflinks-to-entry)))
    (pop-to-buffer (get-buffer-create "*org-mem example*" t))
    (emacs-lisp-mode)
    (setq-local buffer-read-only t)
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert "Example data taken from random node titled \""
              (org-mem-entry-title entry) "\"\n\n")
      (cl-loop for func in 1arg-funs
               do (insert "(" (symbol-name func) " NODE) => "
                          (prin1-to-string (funcall func entry))
                          "\n"))
      (align-regexp (point-min) (point-max) "\\(\\s-*\\) => "))
    (goto-char (point-min))))

(defun org-node-list-reflinks ()
  "List all reflinks and the ID-nodes in which they were found.

Useful to see how many times you\\='ve inserted a link that is very
similar to another link, but not identical, so that perhaps only
one of them is associated with a ROAM_REFS property."
  (interactive)
  (require 'org-mem-list)
  (let ((entries
         (cl-loop
          for link in (org-mem-all-links)
          unless (equal "id" (org-mem-link-type link))
          as node = (org-mem-entry-by-id (org-mem-link-nearby-id link))
          when node
          collect
          (let ((type (org-mem-link-type link))
                (target (org-mem-link-target link))
                (pos (org-mem-link-pos link))
                (origin (org-mem-link-nearby-id link))
                (file-name (org-mem-link-file link)))
            (list (sxhash link)
                  (vector
                   (if (gethash target org-mem--roam-ref<>id)
                       "*"
                     "")
                   (if (and node (org-mem-entry-title node))
                       (buttonize (org-mem-entry-title node)
                                  `(lambda (_button)
                                     (find-file ,file-name)
                                     (goto-char ,pos)
                                     (org-fold-show-entry)
                                     (org-fold-show-context)))
                     (or origin ""))
                   (if type (concat type ":" target) target)))))))
    (if entries
        (org-mem-list--pop-to-tabulated-buffer
         :buffer "*org-node reflinks*"
         :format [("Ref" 4 t) ("Inside node" 30 t) ("Link" 0 t)]
         :reverter #'org-node-list-reflinks
         :entries entries)
      (message "No links found"))))

;; TODO: Make something like a find-dired buffer, handy!
(defun org-node-list-files ()
  "List files and associated information."
  (interactive)
  (require 'org-mem-list)
  (org-mem-list--pop-to-tabulated-buffer
   :buffer "*org-node files list*"
   :format [("Modified" 11 t) ("Size" 7 t) ("File" 70 t) ("Title" 40 t) ("Coding system" 15 t) ("Properties" 10 t)]
   :entries
   (cl-loop
    for file in (org-mem-all-files)
    as props = (org-mem-properties (car (org-mem-entries-in file)))
    as kb = (/ (org-mem-file-size file) 1024.0)
    collect
    (list file
          (vector (format-time-string "%F" (org-mem-file-mtime file))
                  (format (if (< kb 1) "" "%d kB") kb)
                  (buttonize file #'find-file file)
                  (or (org-mem-file-title-topmost file) "")
                  (prin1-to-string (org-mem-file-coding-system file))
                  (if props (prin1-to-string props) ""))))
   :reverter #'org-node-list-files))

(defun org-node-list-feedback-arcs ()
  "Show a feedback-arc-set of forward id-links.

Requires GNU R installed, with R packages stringr, readr, igraph.

A feedback arc set is a set of links such that if they are all
cut (though sometimes it suffices to reverse the direction rather
than cut them), the remaining links in the network will
constitute a DAG (directed acyclic graph).

You may consider this as merely one of many ways to view your
network to quality-control it."
  (interactive)
  (require 'org-mem-list)
  (unless (executable-find "Rscript")
    (user-error
     "This command requires GNU R, with R packages: stringr, readr, igraph"))
  (let* ((r-code "
          library(stringr)
          library(readr)
          library(igraph)

          tsv <- commandArgs(TRUE)[1]
          g <- graph_from_data_frame(read_tsv(tsv), directed = TRUE)
          fas1 <- feedback_arc_set(g, algo = \"approx_eades\")

          lisp_data <- str_c(\"(\\\"\", as_ids(fas1), \"\\\")\") |>
            str_replace(\"\\\\|\", \"\\\" . \\\"\") |>
            str_flatten(\"\n \") |>
            (function(x) {
              str_c(\"(\", x, \")\")
            })()

          write_file(lisp_data,
                     file.path(dirname(tsv),
                     \"feedback-arcs.eld\"))")
         (tmpdir (file-name-concat temporary-file-directory "org-node"))
         (script-file (file-name-concat tmpdir "analyze_feedback_arcs.R"))
         (input-tsv (file-name-concat tmpdir "id_node_digraph.tsv"))
         (output-eld (file-name-concat tmpdir "feedback-arcs.eld")))
    (mkdir tmpdir t)
    (write-region r-code () script-file () 'quiet)
    (write-region (org-node--make-digraph-tsv-string) () input-tsv () 'quiet)
    (with-temp-buffer
      (unless (= 0 (call-process "Rscript" () t () script-file input-tsv))
        (error "%s" (buffer-string))))
    (let ((feedbacks (with-temp-buffer
                       (insert-file-contents output-eld)
                       (read (buffer-string)))))
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org-node feedback arcs*"
       :format [("Node containing link" 39 t) ("Target of link" 0 t)]
       :entries (cl-loop
                 for (origin . target) in feedbacks
                 as origin-node = (org-mem-entry-by-id origin)
                 as target-node = (org-mem-entry-by-id target)
                 collect
                 (list (+ (sxhash origin) (sxhash target))
                       (vector (buttonize (org-mem-title origin-node)
                                          #'org-node--goto
                                          origin-node)
                               (buttonize (org-mem-title target-node)
                                          #'org-node--goto
                                          target-node))))))))

;; TODO: Temp merge all refs into corresponding ID
(defun org-node--make-digraph-tsv-string ()
  "Generate a string in Tab-Separated Values form.
The string is a 2-column table of destination-origin pairs, made
from ID links found in `org-mem--target<>links'."
  (concat
   "src\tdest\n"
   (string-join
    (seq-uniq (cl-loop
               for target being each hash-key of org-mem--target<>links
               using (hash-values links)
               append (cl-loop
                       for link in links
                       when (and (org-mem-link-nearby-id link)
                                 (equal "id" (org-mem-link-type link)))
                       collect (concat target "\t" (org-mem-link-nearby-id link)))))
    "\n")))

(defvar org-node--unlinted nil)
(defvar org-node--lint-current nil)
(defvar org-node--lint-operator nil)
(defvar org-node--lint-warnings nil)
(defun org-node-lint-all-files ()
  "Run `org-lint' on all known Org files, and report results."
  (interactive)
  (require 'org-lint)
  (let ((proceed (and org-node--unlinted
                      (equal fileloop--operate-function
                             #'org-node--lint-operator)))
        (files (org-mem-all-files)))
    (unless proceed
      (when (y-or-n-p (format "Lint %d files?" (length files)))
        (setq org-node--unlinted files)
        (fileloop-initialize
         files
         (lambda ()
           (let ((file (pop org-node--unlinted)))
             (when (string-prefix-p "org" (file-name-extension file))
               (setq org-node--lint-current file))))
         #'org-node--lint-operator)
        (setq org-node--lint-warnings nil)
        (setq proceed t)))
    (condition-case _
        (when proceed
          (cl-letf (((symbol-function 'recentf-add-file) #'ignore))
            (let ((find-file-hook nil)
                  (enable-local-variables :safe)
                  (delay-mode-hooks t)
                  (org-agenda-files nil)
                  (org-inhibit-startup t)
                  (org-element-cache-persistent nil))
              (fileloop-continue))))
      (quit)))

  (when org-node--lint-warnings
      (org-mem-list--pop-to-tabulated-buffer
       :buffer "*org lint results*"
       :format [("File" 30 t) ("Line" 5 t) ("Trust" 5 t) ("Explanation" 0 t)]
       :reverter #'org-node-lint-all-files
       :entries (cl-loop
                 for (file . warning) in org-node--lint-warnings
                 collect (let ((array (cadr warning)))
                           (list (sxhash warning)
                                 (vector
                                  (buttonize (file-name-nondirectory file)
                                             `(lambda (_button)
                                                (find-file ,file)
                                                (goto-line ,(string-to-number
                                                             (elt array 0)))))
                                  (elt array 0)
                                  (elt array 1)
                                  (elt array 2))))))))

;; FIXME: ...Just one lint warning per file?
(defun org-node--lint-operator ()
  "An OPERATE-FUNCTION for `fileloop-initialize'."
  (let ((buffer-seems-new (and (not (buffer-modified-p))
                               (not buffer-undo-list))))
    (unless (derived-mode-p 'org-mode) (org-mode))
    (let ((inhibit-message t)) ; Muffle spam from `org-lint-invalid-id-link'
      (when-let* ((warning (org-lint)))
        (push (cons org-node--lint-current (car warning))
              org-node--lint-warnings)))
    (when buffer-seems-new
      (kill-buffer))
    t))


;;;; Commands 6: Editing tags / refs / aliases

(defun org-node--call-at-nearest-node (function &rest args)
  "With point at the relevant heading, call FUNCTION with ARGS.

Prefer the closest ancestor heading that has an ID property, else go to
the file-level property drawer if that has an ID, else fall back on
the heading for the current entry.

Afterwards, maybe restore point to where it had been previously, so long
as the heading where FUNCTION was called would still be visible in the
window."
  (let* ((where-i-was (point-marker))
         (id (org-entry-get-with-inheritance "ID"))
         (heading-pos
          (save-excursion
            (without-restriction
              (when id
                (goto-char (point-min))
                (re-search-forward
                 (rx bol (* space) ":ID:" (+ space) (literal id))))
              (org-back-to-heading-or-point-min)
              (point)))))
    (when (and heading-pos (< heading-pos (point-min)))
      (widen))
    (save-excursion
      (when heading-pos
        (goto-char heading-pos))
      (apply function args))
    (when heading-pos
      (unless (pos-visible-in-window-p heading-pos)
        (goto-char heading-pos)
        (recenter 0)
        (when (pos-visible-in-window-p where-i-was)
          (forward-char (- where-i-was (point))))))
    (set-marker where-i-was nil)))

(defun org-node--add-to-property-keep-space (property value)
  "Add VALUE to PROPERTY for node at point.

If the current entry has no ID, but an ancestor entry has an ID, then
operate on that entry instead.

Then behave like `org-entry-add-to-multivalued-property' but
preserve spaces: instead of percent-escaping each space character
as \"%20\", wrap VALUE in quotes if it has spaces."
  (org-node--call-at-nearest-node
   (lambda ()
     (let ((old (org-entry-get nil property)))
       (when old
         (setq old (split-string-and-unquote old)))
       (unless (member value old)
         (org-entry-put nil property (combine-and-quote-strings
                                      (cons value old))))))))

(defun org-node-add-alias ()
  "Add alias to ROAM_ALIASES in nearest relevant property drawer."
  (interactive () org-mode)
  (org-node--add-to-property-keep-space
   "ROAM_ALIASES" (string-trim (read-string "Alias: "))))

;; FIXME: What if user yanks a [cite:... @key1 ... @key2 ...]?
(defun org-node-add-refs ()
  "Add a link to ROAM_REFS in nearest relevant property drawer.
Wrap the link in double-brackets if necessary."
  (interactive () org-mode)
  (dolist (ref (mapcar #'string-trim
                       (completing-read-multiple
                        "Add ref(s): "
                        (org-node--list-known-raw-links)
                        nil
                        nil
                        nil
                        'org-node-link-hist)))
    (when (string-search " " ref)
      (if (string-match-p org-link-plain-re ref)
          ;; If it is a link, ensure it is enclosed in brackets
          (setq ref (concat "[[" (string-trim ref (rx "[[") (rx "]]")) "]]"))
        (message "Spaces in ref, not sure how to format correctly: %s" ref)))
    (org-node--add-to-property-keep-space "ROAM_REFS" ref)))

(defun org-node-add-tags (tags)
  "Add TAGS to the node at point or nearest ancestor that is a node.

To always operate on the current entry, use `org-node-add-tags-here'."
  (interactive (list (org-node--read-tags)) org-mode)
  (org-node--call-at-nearest-node #'org-node-add-tags-here tags))

(defun org-node-add-tags-here (tags)
  "Add TAGS to the entry at point."
  (interactive (list (org-node--read-tags)) org-mode)
  (setq tags (ensure-list tags))
  (if (org-before-first-heading-p)
      ;; There's no Org builtin to set filetags yet
      ;; so we have to do it ourselves.
      (let* ((filetags (cl-loop
                        for raw in (cdar (org-collect-keywords '("FILETAGS")))
                        append (split-string raw ":" t)))
             (new-tags (seq-uniq (append filetags tags)))
             (case-fold-search t))
        (save-excursion
          (without-restriction
            (goto-char (point-min))
            (if (search-forward "\n#+filetags:" nil t)
                (atomic-change-group
                  (skip-chars-forward " ")
                  (delete-region (point) (pos-eol))
                  (insert ":" (string-join new-tags ":") ":"))
              (org-node-full-end-of-meta-data)
              (insert "#+filetags: :" (string-join new-tags ":") ":\n")))))
    (save-excursion
      (org-back-to-heading)
      (org-set-tags (seq-uniq (append (org-get-tags nil t) tags))))))

(defun org-node--read-tags ()
  "Prompt for an Org tag or several.
Pre-fill completions by collecting tags from all known Org files, as
well as the members of `org-tag-persistent-alist' and `org-tag-alist'."
  (completing-read-multiple
   "Tags: "
   (delete-dups
    (nconc (thread-last (append org-tag-persistent-alist
                                org-tag-alist
                                (and org-element-use-cache
                                     (derived-mode-p 'org-mode)
                                     (org-get-buffer-tags)))
                        (mapcar #'car)
                        (cl-remove-if #'keywordp)
                        (mapcar #'substring-no-properties))
           (cl-loop for entry in (org-mem-all-entries)
                    append (org-mem-entry-tags entry))))
   nil nil nil 'org-tags-history))


;;;; Gotos

(defun org-node--goto (node &optional exact)
  "Visit NODE file, and move point to NODE unless already inside.
EXACT means always move point."
  (when (numberp exact)
    (error "Function org-node--goto no longer takes a position argument"))
  (unless node
    (error "Function org-node--goto received a nil argument"))
  (let ((file (org-mem-file-truename node)))
    (if (file-exists-p file)
        (progn
          (when (not (file-readable-p file))
            (error "org-node: Couldn't visit unreadable file %s" file))
          (find-file file)
          (widen)
          (org-node--assert-transclusion-safe)
          ;; Search for the ID rather than naively using
          ;; (goto-char (org-mem-pos node)), so that it will find the
          ;; correct place even in a buffer with unsaved modifications.
          (let* ((id (org-mem-id node))
                 (pos (and id (org-find-property "ID" id))))
            (unless pos
              (error "Could not find ID \"%s\" in buffer %s" id (current-buffer)))
            ;; Having visited FILE, point may already be at a desirable
            ;; position because a buffer was already visiting it, or because of
            ;; save-place.  Otherwise, move directly to the heading.
            (when (or exact
                      (not (equal id (org-entry-get-with-inheritance "ID")))
                      (not (pos-visible-in-window-p pos))
                      (org-invisible-p pos))
              (goto-char pos)
              (when (org-mem-subtree-p node)
                (org-fold-show-entry)
                (org-fold-show-children)
                (recenter 0)))))
      (org-mem-reset t "org-node: Didn't find file, resetting..."))))

(defun org-node-goto-new-drawer-site ()
  "Go to just after properties drawer.
Actually, if before the first heading, also skip past any other drawer
that follows the file properties drawer."
  (if (org-before-first-heading-p)
      (org-node--after-drawers-before-keyword)
    (org-end-of-meta-data))
  nil)

;; Keyword lines work as a natural barrier, because Org expects file-level
;; :PROPERTIES: to come before #+title and other keyword lines.
(defun org-node--after-drawers-before-keyword ()
  "Move point to after all drawers in the front matter."
  (goto-char (point-min))
  (let ((case-fold-search t)
        (bound (org-entry-end-position))
        (junk (rx (*? space) (or "# " "\n"))))
    ;; NOTE: We also skip any comments and blanklines in between
    ;; these drawers.  This implementation detail allows
    ;; `org-node-full-end-of-meta-data' to presume there are no more drawers.
    (while (or (looking-at-p junk)
               (and (looking-at-p org-drawer-regexp)
                    (or (re-search-forward "^[ \t]*:END:[ \t]*$" bound t)
                        (error "Missing :END: in %s" (buffer-name)))))
      (forward-line))
    ;; On a "final stretch" of comments and blanklines, go back to the first.
    (while (progn (forward-line -1)
                  (looking-at-p junk)))
    (forward-line))
  nil)

(defun org-node-full-end-of-meta-data (&optional _deprecated-arg)
  "Skip properties and other drawers, and at the file-level, skip keywords.

As in `org-end-of-meta-data', point always lands on a newline \(or the
end of buffer).  Since that newline may be the beginning of the next
heading, you should probably verify that `org-at-heading-p' is nil,
else do `backward-char' or `open-line' prior to inserting text."
  (if (org-before-first-heading-p)
      (progn
        (org-node--after-drawers-before-keyword)
        ;; Jump past #+keywords, comments and blank lines.
        (while (looking-at-p (rx (*? space) (or "#+" "# " "\n")))
          (forward-line))
        ;; On a "final stretch" of empty lines, go back to the first.
        (when (not (= 0 (skip-chars-backward "\n")))
          (forward-char)))
    ;; PERF: Override a bottleneck inside `org-end-of-meta-data'.
    (cl-letf (((symbol-function 'org-back-to-heading)
               #'org-node--back-to-heading-or-point-min))
      (org-end-of-meta-data t)))
  (point))

(defun org-node--back-to-heading-or-point-min (&optional invisible-ok)
  "Alternative to `org-back-to-heading-or-point-min'.
Argument INVISIBLE-OK as in that function.

Like `org-back-to-heading-or-point-min' but should be faster in the case
that an org-element cache has not been built for the buffer.  This can
be the case in a buffer spawned by `org-roam-with-temp-buffer'
or `org-node--work-buffer-for'.

As bonus, do not land on an inlinetask, seek a real heading."
  (let ((inlinetask-re (when (fboundp 'org-inlinetask-outline-regexp)
                         (org-inlinetask-outline-regexp))))
    (cl-loop until (and (org-at-heading-p (not invisible-ok))
                        (not (and inlinetask-re (looking-at-p inlinetask-re))))
             unless (re-search-backward org-outline-regexp-bol nil t)
             return (goto-char (point-min)))
    (point)))


;;;; CAPF (Completion-At-Point Function) aka. in-buffer completion

(defun org-node-complete-at-point ()
  "Expand word at point to a known node title, and linkify.
Designed for `completion-at-point-functions'."
  (when-let* ((bounds (bounds-of-thing-at-point 'word)))
    (and (not (org-in-src-block-p))
         (not (save-match-data (org-in-regexp org-link-any-re)))
         (list (car bounds)
               (cdr bounds)
               org-node--title<>affixations
               :exclusive 'no
               :exit-function
               (lambda (text _)
                 (when-let* ((id (gethash text org-mem--title<>id)))
                   (atomic-change-group
                     (delete-char (- (length text)))
                     (insert (org-link-make-string (concat "id:" id) text)))
                   (run-hooks 'org-node-insert-link-hook)))))))

(define-minor-mode org-node-complete-at-point-local-mode
  "Let completion at point insert links to nodes.

-----"
  :require 'org-node
  (if org-node-complete-at-point-local-mode
      (add-hook 'completion-at-point-functions
                #'org-node-complete-at-point nil t)
    (remove-hook 'completion-at-point-functions
                 #'org-node-complete-at-point t)))

(defun org-node-complete-at-point--try-enable ()
  "Enable `org-node-complete-at-point-local-mode' if in Org file."
  (and (derived-mode-p 'org-mode)
       buffer-file-name
       (org-node-complete-at-point-local-mode)))

;;;###autoload
(define-globalized-minor-mode org-node-complete-at-point-mode
  org-node-complete-at-point-local-mode
  org-node-complete-at-point--try-enable)


;;;; Misc features

(defun org-node-try-visit-ref-node ()
  "Designed for `org-open-at-point-functions'.

For the link at point, if there exists an ID-node that has
the same link in its ROAM_REFS property, visit that node rather than
following the link normally.

If already visiting that node, then follow the link normally."
  (when-let* ((url (thing-at-point 'url)))
    ;; Rarely more than one car
    (let* ((target (car (org-mem--split-roam-refs-field url)))
           (found (cl-loop for node in (org-mem-all-id-nodes)
                           when (member target (org-mem-entry-roam-refs node))
                           return node)))
      (if (and found
               ;; Check that point is not already in said ref node (if so,
               ;; better to fallback to default `org-open-at-point' logic)
               (not (and (derived-mode-p 'org-mode)
                         (equal (org-entry-get-with-inheritance "ID")
                                (org-mem-entry-id found)))))
          (progn (org-node--goto found t)
                 t)
        nil))))


;;;; Drawer subroutines

(defun org-node--delete-drawer (name)
  "In current entry, seek and destroy drawer named NAME."
  (save-excursion
    (save-restriction
      (when (org-node-narrow-to-drawer-p name)
        (delete-region (point-min) (point-max))
        (widen)
        (delete-line) ;; There is always 1 empty line
        (org-remove-empty-drawer-at (point))))))

;; Org ships no general tool to find/create a drawer, so roll our own.
;; There's a trick used by org-super-links: temporarily set
;; `org-log-into-drawer' to "BACKLINKS", and then call `org-log-beginning'.
;; Only works under a heading though.
;; And our argument CREATE-WHERE turns out to be pretty handy.

(defun org-node-narrow-to-drawer-create (name &optional create-where)
  "Narrow to pre-existing drawer named NAME, creating it if necessary.
Search current entry only, via subroutine `org-node-narrow-to-drawer-p'.

When drawer is created, insert it near the beginning of the entry
\(after any properties drawer\), unless CREATE-WHERE is a function, in
which case call it and hope it moved `point' to some appropriate
position.  If CREATE-WHERE returns an integer or marker, go to that
position."
  (org-node-narrow-to-drawer-p name t create-where))

(defun org-node-narrow-to-drawer-p (name &optional create-missing create-where)
  "Seek a drawer named NAME in the current entry, then narrow to it.
This also works at the file top level, before the first entry.

If a drawer was found, return t.
Otherwise do not narrow, and return nil.

Narrow to the region between :NAME:\\n and \\n:END:, exclusive.
Place point at the beginning of that region, after any indentation.
If the drawer was empty, ensure one blank line.
\(Put another way, if you delete all visible buffer contents,
one blank line remains after un-narrowing.)

A way you might invoke this function:

    \(save-restriction
      \(if \(org-node-narrow-to-drawer-p \"MY_DRAWER\")
           ...edit the narrowed buffer...
        \(message \"No such drawer in this entry\")))

Instead of passing the optional arguments, consider calling
`org-node-narrow-to-drawer-create' instead, which is equivalent to
CREATE-MISSING t.  Also see that function for meaning of CREATE-WHERE."
  (let ((start (window-start))
        (point (window-point))
        (entry-end (org-entry-end-position))
        (case-fold-search t))
    (org-back-to-heading-or-point-min t)
    (if (cl-loop while (re-search-forward
                        (rx bol (* space) ":" (literal name) ":" (* space) eol)
                        entry-end
                        t)
                 unless (org-in-block-p '("src" "example"))
                 return t)
        ;; Found pre-existing drawer
        (let ((drawbeg (progn (forward-line) (point))))
          (unless (re-search-forward "^[ \t]*:END:[ \t]*$" entry-end t)
            (error "No :END: of drawer at %d in %s" (point) (buffer-name)))
          (goto-char (pos-bol))
          (if (= drawbeg (point))
              ;; Empty drawer with no newlines; add one newline
              ;; so the result looks similar to after `org-insert-drawer'.
              (progn (open-line 1)
                     (indent-according-to-mode))
            ;; Not an empty drawer, so it is safe to back up from :END: line
            (backward-char))
          (narrow-to-region drawbeg (point))
          (goto-char drawbeg)
          (back-to-indentation)
          t)
      (if create-missing
          ;; Create new
          (let ((where (if create-where (funcall create-where)
                         (org-node-goto-new-drawer-site))))
            (when (number-or-marker-p where) (goto-char where))
            (org-insert-drawer nil name)
            (narrow-to-region (point) (point))
            t)
        ;; Do nothing and restore scroll position
        (set-window-start (selected-window) start t)
        (set-window-point (selected-window) point)
        nil))))


;;;; API used by some submodule(s)

(defvar org-node--work-buffers nil
  "All buffers spawned by `org-node--work-buffer-for'.")

(defun org-node--kill-work-buffers ()
  "Kill all buffers in `org-node--work-buffers'."
  (while-let ((buf (pop org-node--work-buffers)))
    (when buf (kill-buffer buf))))

(defun org-node--work-buffer-for (file)
  "Get or create a hidden buffer, and read FILE contents into it.
Also enable `org-mode', but ignore `org-mode-hook' and startup options.

A buffer persists for each FILE, so calling again with the same FILE
skips the overhead of creation.  To clean up, call
`org-node--kill-work-buffers' explicitly."
  (let ((bufname (format " *org-node-work-%d*" (sxhash file))))
    (or (get-buffer bufname)
        (with-current-buffer (org-mem-org-mode-scratch bufname)
          (push (current-buffer) org-node--work-buffers)
          (insert-file-contents file)
          ;; May be useful for `org-node-roam-accelerator-mode'.
          ;; No idea why `org-roam-with-temp-buffer' sets default directory,
          ;; but Chesterton's Fence.
          (setq-local default-directory (file-name-directory file))
          ;; Ensure that attempted edits trip an error, since the buffer may be
          ;; reused any number of times, and should always reflect FILE.
          (setq-local buffer-read-only t)
          (current-buffer)))))

;; Very important macro for the backlink mode, because backlink insertion opens
;; the target Org file in the background, and if doing that is laggy, then
;; every link insertion is laggy.
(defmacro org-node--with-quick-file-buffer (file &rest body)
  "Like `org-with-file-buffer' and `org-with-wide-buffer'.
Tries to execute minimal hooks in order to open and close FILE as
quickly as possible.

In detail:

1. If a buffer was visiting FILE, reuse that buffer, else visit
   FILE in a new buffer, in which case ignore most of the Org
   startup checks and don\\='t ask about file-local variables.

2. Temporarily `widen' the buffer, execute BODY, then restore
   point.

3a. If a new buffer had to be opened: save and kill it.
    \(Mandatory because buffers opened in the quick way look
    \"wrong\", e.g. no indent-mode, no visual wrap etc.)  Also
    skip any save hooks and kill hooks.

3b. If a buffer had been open: leave it open and leave it
    unsaved.

Optional keyword argument ABOUT-TO-DO as in
`org-node--in-files-do'.

\(fn FILE [:about-to-do ABOUT-TO-DO] &rest BODY)"
  (declare (indent 1) (debug t))
  `(let ((enable-local-variables :safe)
         (org-inhibit-startup t) ;; Don't apply startup #+options
         (find-file-hook nil)
         (org-agenda-files nil)
         (kill-buffer-hook nil) ;; Inhibit save-place etc
         (kill-buffer-query-functions nil)
         (buffer-list-update-hook nil)
         (delay-mode-hooks t)
         (--file-- ,file))
     (when (file-directory-p --file--)
       (error "Is a directory: %s" --file--))
     (org-element-with-disabled-cache
       (let* ((--was-open-- (find-buffer-visiting --file--))
              (--buf-- (or --was-open--
                           (find-file-noselect --file--))))
         (when (bufferp --buf--)
           (with-current-buffer --buf--
             (save-excursion
               (without-restriction
                 (goto-char (point-min))
                 ,@body))
             (if --was-open--
                 ;; Because the cache gets confused by changes
                 (org-element-cache-reset)
               (when (buffer-modified-p)
                 (let ((before-save-hook nil)
                       (after-save-hook nil)
                       (save-silently t)
                       (inhibit-message t))
                   (save-buffer)))
               (kill-buffer))))))))

(defvar org-node--compile-timers nil)
(defvar org-node--compiled-lambdas (make-hash-table :test 'equal))
(defun org-node--try-ensure-compiled (fn)
  "Try to return FN as a compiled function.

- If FN is a symbol with uncompiled function definition, return
  the same symbol, and arrange to natively compile it after some
  idle time.  Or if native-comp missing, byte-compile right away.

- If FN is an anonymous lambda, compile it, cache the resulting
  bytecode, and return that bytecode.

Normally Emacs already does this kind of thing for definitions in an
installed library, but it can be handy for user-provided lambdas that
must be called a lot."
  (cond ((compiled-function-p fn) fn)
        ((symbolp fn)
         (if (compiled-function-p (symbol-function fn))
             fn
           (let (byte-compile-warnings)
             (if (native-comp-available-p)
                 (unless (alist-get fn org-node--compile-timers)
                   (setf (alist-get fn org-node--compile-timers)
                         (run-with-idle-timer
                          (+ 5 (random 5)) nil (##native-compile fn))))
               (byte-compile fn)))
           ;; May remain uncompiled until native comp is done
           fn))
        ((gethash fn org-node--compiled-lambdas))
        ((let (byte-compile-warnings)
           (puthash fn (byte-compile fn) org-node--compiled-lambdas)))))

(defvar org-node--new-unsaved-buffers nil
  "List of file-visiting buffers that have never written to the file.")

;; TODO: Could the :creator just check if the buffer is unmodified and empty?
;;       But possible pitfall that the user may have important stuff in undo.
(defun org-node--kill-blank-unsaved-buffers (&rest _)
  "Kill buffers created by org-node that have always been blank.

This exists to allow you to create a node, especially a journal note for
today via package \"org-node-seq\", change your mind, do an `undo' to
empty the buffer, then browse to the previous day\\='s note.  When later
you want to create today\\='s note after all, the seq\\='s :creator
function should be made to run again, but it will not do so if the
buffer already exists, so the buffer stays blank.  Thus this hook."
  (unless (minibufferp)
    (dolist (buf org-node--new-unsaved-buffers)
      (if (or (not (buffer-live-p buf))
              (file-exists-p (buffer-file-name buf)))
          ;; Stop checking the buffer
          (setq org-node--new-unsaved-buffers
                (delq buf org-node--new-unsaved-buffers))
        (with-current-buffer buf
          (when (and (not (get-buffer-window buf t))
                     (not (buffer-modified-p))
                     (string-blank-p (buffer-string)))
            (when buffer-auto-save-file-name
              ;; Hopefully throw away a stale autosave
              ;; since its existence annoys the user on re-creating the file
              (do-auto-save nil t))
            (when-let* ((file (org-mem--truename-maybe buffer-file-name)))
              (org-mem-updater--forget-file-contents file)
              (org-node--forget-completions-in-files file))
            (kill-buffer buf)))))))


;;;; Transclusion safety

(defun org-node--in-transclusion-p (&optional pos)
  "Non-nil if POS is inside a transclude-region."
  (get-text-property (or pos (point)) 'org-transclusion-id))

(defun org-node--assert-transclusion-safe ()
  "Signal error if an active transclusion in buffer may affect Org-node.
If so, inform user how to configure \"org-transclusion\" to be safe.

Various Org-node commands use `org-entry-get-with-inheritance' or
`org-find-property' to find a node in a file, and assume on success
that it is the canonical node, not some mirror of it.
This helps keep that assumption safe."
  (when (boundp 'org-transclusion-exclude-elements)
    (save-excursion
      (without-restriction
        (goto-char (point-min))
        (let (danger-from-prop-drawer danger-from-keyword)
          (when (and (text-property-search-forward 'org-transclusion-id)
                     (or (and (not (memq 'property-drawer
                                         org-transclusion-exclude-elements))
                              (setq danger-from-prop-drawer t))
                         (and (not (memq 'keyword
                                         org-transclusion-exclude-elements))
                              (org-collect-keywords "PROPERTY")
                              (setq danger-from-keyword t))))
            (display-warning 'org-node
                             (concat "
Found active org-transclusion in buffer "
                                     (prin1-to-string (current-buffer))
                                     ".
To keep using them safely:"
                                     (and danger-from-prop-drawer
                                          "
- eval (add-to-list 'org-transclusion-exclude-elements 'property-drawer)")
                                     (and danger-from-keyword
                                          "
- do one of
  - remove existing #+property: lines
  - eval (add-to-list 'org-transclusion-exclude-elements 'keyword)")))
            (user-error "Active org-transclusion in buffer %S, quitting"
                        (current-buffer))))))))


;;;; Org-roam accelerator

(defvar org-node--ad-roam-src-node nil)
(defun org-node--ad-roam-node-insert-section (orig-fn &rest args)
  "Designed as around-advice for `org-roam-node-insert-section'.

Run ORIG-FN with ARGS, while overriding
`org-roam-fontify-like-in-org-mode' so it does nothing.
While we\\='re at it, inspect ARGS for its SOURCE-NODE argument and
stash it in the variable `org-node--ad-roam-src-node'."
  (setq org-node--ad-roam-src-node
        (plist-get args :source-node))
  (cl-letf (((symbol-function 'org-roam-fontify-like-in-org-mode) #'identity))
    (apply orig-fn args)))

(defvar org-node--ad-roam--id<>previews (make-hash-table :test #'equal))
(defun org-node--ad-roam-preview-get-contents (file link-pos)
  "Designed as override for `org-roam-preview-get-contents'.

Normally the first time you open an org-roam buffer, Emacs hangs for as
long as a minute on a slow machine when huge files are involved, due to
having to fontify each file\\='s entire contents in a hidden buffer,
then applying #+startup:indent and other options, then org-element cache
has to do its thing even though the cache will be thrown away.

This tries to eliminate all those problems.

Aside from huge files, it is also slow when there are backlinks coming
from many sources.  To deal with that:

1. this reuses buffers if several backlinks originate from the same file
2. even if all originate from different files, this caches all snippets
   so that it should only be slow the first time."
  (let* ((src-id (org-roam-node-id org-node--ad-roam-src-node))
         (src (org-mem-entry-by-id src-id))
         (_ (unless src
              (error "Org-roam node unknown to Org-mem: %s" src-id)))
         (pos-diff (- link-pos (org-mem-entry-pos src))))
    (with-memoization
        (alist-get pos-diff (gethash src-id org-node--ad-roam--id<>previews))
      (let (snippet)
        (with-current-buffer (org-node--work-buffer-for file)
          (goto-char link-pos)
          ;; PERF: Override a bottleneck in `org-end-of-meta-data'.
          (cl-letf (((symbol-function 'org-back-to-heading-or-point-min)
                     #'org-node--back-to-heading-or-point-min))
            (setq snippet (funcall org-roam-preview-function))
            (dolist (fn org-roam-preview-postprocess-functions)
              (setq snippet (funcall fn snippet)))))
        (org-mem-fontify-like-org snippet)))))

;;;###autoload
(define-minor-mode org-node-roam-accelerator-mode
  "Advise the \"*org-roam*\" buffer to be faster.
As a side effect, it can be used without the rest of org-roam."
  :global t
  (require 'org-mem-roamy)
  (if org-node-roam-accelerator-mode
      (progn
        (advice-add 'org-roam-node-insert-section :around    #'org-node--ad-roam-node-insert-section)
        (advice-add 'org-roam-preview-get-contents :override #'org-node--ad-roam-preview-get-contents)
        (advice-add 'org-roam-buffer-render-contents :after  #'org-node--kill-work-buffers)
        (advice-add 'org-roam-backlinks-get :override        #'org-mem-roamy-mk-backlinks)
        (advice-add 'org-roam-reflinks-get  :override        #'org-mem-roamy-mk-reflinks))
    (advice-remove 'org-roam-node-insert-section    #'org-node--ad-roam-node-insert-section)
    (advice-remove 'org-roam-preview-get-contents   #'org-node--ad-roam-preview-get-contents)
    (advice-remove 'org-roam-buffer-render-contents #'org-node--kill-work-buffers)
    (advice-remove 'org-roam-backlinks-get          #'org-mem-roamy-mk-backlinks)
    (advice-remove 'org-roam-reflinks-get           #'org-mem-roamy-mk-reflinks)))


;;;; API not used within this package

(defun org-node-at-point ()
  "Return the ID-node near point.

This may refer to the current Org heading, else an ancestor
heading, else the file-level node, whichever has an ID first."
  (org-mem-entry-by-id (org-entry-get-with-inheritance "ID")))

;; TODO: Add an extended variant that checks active region, like
;;       `org-node-insert-link' does.
(defun org-node-read (&optional prompt)
  "PROMPT for a known node and return it."
  (gethash (org-node-read-candidate prompt)
           org-node--candidate<>entry))

(defun org-node-p (entry)
  "Return non-nil if ENTRY satisfies `org-node-filter-fn'."
  (funcall org-node-filter-fn entry))

(defun org-node-all-filtered-nodes ()
  "List currently cached org-nodes that satisfied `org-node-filter-fn'."
  (delete-dups (hash-table-values org-node--candidate<>entry)))

(defun org-node-guess-node-by-title (title)
  (and (gethash title org-node--title<>affixations)
       (org-mem-entry-by-id (gethash title org-mem--title<>id))))


(define-obsolete-function-alias 'org-node-prefix-with-olp                  #'org-node-prepend-olp               "3.3.0 (May 2025)")
(define-obsolete-function-alias 'org-node-prefix-with-tags-tags            #'org-node-prepend-tags              "3.3.0 (May 2025)")
(define-obsolete-function-alias 'org-node-prefix-with-tags-and-olp         #'org-node-prepend-tags-and-olp      "3.3.0 (May 2025)")
(define-obsolete-function-alias 'org-node-affix-with-olp-and-tags          #'org-node-prepend-olp-append-tags   "3.3.0 (May 2025)")
(define-obsolete-function-alias 'org-node-affix-with-olp-and-tags-legacy   #'org-node-prepend-olp-append-tags   "3.3.0 (May 2025)")
(define-obsolete-function-alias 'org-node-insert-link-into-drawer          #'org-node-insert-into-related       "3.3.3 (May 2025)")
(define-obsolete-function-alias 'org-node--end-of-meta-data                #'org-node-full-end-of-meta-data     "3.3.10 (May 2025)")
(define-obsolete-function-alias 'org-node-list-file-coding-systems         #'org-node-list-files                "3.3.10 (May 2025)")
(define-obsolete-function-alias 'org-node-collection                       #'org-node-collection-basic          "3.3.12 (May 2025)")
(define-obsolete-function-alias 'org-node-mk-auto-title                    #'org-node-titlegen-untitled         "3.3.12 (May 2025)")
(define-obsolete-function-alias 'org-node-title-to-filename                #'org-node-title-to-filename-quiet   "3.4.3 (June 2025)")

(provide 'org-node)

;;; org-node.el ends here
