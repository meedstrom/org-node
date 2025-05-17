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
;; Package-Requires: ((emacs "29.1") (llama "0.5.0") (org-mem "0.8.2") (magit-section "4.3.0"))

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

;; Compared to org-roam:

;;   + Compatible (you can use both packages and compare)
;;   + Fast
;;   + No SQLite
;;   + Never again sit through a slow `org-id-update-id-locations'
;;   + If you want, opt out of those file-level :PROPERTIES: drawers
;;     + Set `org-node-prefer-with-heading'
;;   + Try to rely in a bare-metal way on upstream org-id and org-capture
;;   + Extra utilities, notably to auto-rename files and links
;;   + An alternative way to display backlinks

;;   - No support for "roam:" links
;;   - No `org-roam-db-query'
;;     - There's an elisp API, but if you're more familiar with SQL,
;;       it's still a downgrade

;; Compared to denote:

;;   + Compatible (you can use both packages and compare)
;;   + No mandatory filename style (can match Denote format if you like)
;;   + You can have as many "notes" as you want inside one file.
;;     + You could possibly use Denote for coarse browsing,
;;       and org-node for more granular browsing.

;;   - No support for "denote:" links
;;   - No support for Markdown or other file types

;; Finally, an oddity:

;; Due to relying on the org-id table, if a vendor README.org or other
;; downloaded Org file has a heading with an ID, it's considered part of
;; your collection -- simply because of the 1:1 correspondence, that if
;; it's known to org-id, it's known to org-node.
;;
;; These headings can be filtered after-the-fact by `org-node-filter-fn',
;; so that you do not see them in org-node commands.

;;; Code:

;; Built-in
(require 'cl-lib)
(require 'subr-x)
(eval-when-compile
  (require 'org)
  (require 'org-id)
  (require 'org-macs)
  (require 'org-element))

;; External
(require 'llama)
(require 'org-node-changes)
(require 'org-mem)
(require 'org-mem-x)
(require 'org-mem-list)

(defvar consult-ripgrep-args)
(defvar org-roam-capture-templates)
(defvar org-node-backlink-mode)
(declare-function org-node-backlink--fix-nearby "org-node-backlink")
(declare-function tramp-tramp-file-p "tramp")
(declare-function org-lint "org-lint")
(declare-function consult--grep "ext:consult")
(declare-function consult--grep-make-builder "ext:consult")
(declare-function consult--ripgrep-make-builder "ext:consult")
(declare-function org-mem-x-ensure-entry-at-point-known "org-mem")
(declare-function org-mem-roamy-mk-backlinks "org-mem-roamy")
(declare-function org-mem-roamy-mk-reflinks "org-mem-roamy")

(defvaralias 'org-nodes 'org-mem--id<>entry)


;;;; Options

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
  :type 'boolean)

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
  :type '(choice (const nil)
                 function)
  :package-version '(org-node . "2.3.3"))

(defcustom org-node-filter-fn
  (lambda (node)
    (not (org-mem-entry-property "ROAM_EXCLUDE" node)))
  "Predicate returning non-nil to include a node, or nil to exclude it.

The filtering affects two tables:
- `org-node--candidate<>entry', used by completions in the minibuffer
- `org-mem--title<>id', used by `org-node-complete-at-point-mode'

In other words, passing nil means the user cannot autocomplete to the
node, but Lisp code can still find it in the output of
`org-mem-all-id-nodes', and backlinks are discovered normally.

This function is applied once for every ID-node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-peek] and specified
in the type `org-mem-entry' (C-h o org-mem-entry RET).

See the following example for a way to filter out nodes with a
ROAM_EXCLUDE property, or that have any kind of TODO state, or
are tagged :drill:, or where the full file path contains a
directory named \"archive\".

\(setq org-node-filter-fn
      (lambda (node)
        (not (or (org-mem-property \"ROAM_EXCLUDE\" node)
                 (org-mem-todo-state node)
                 (string-search \"/archive/\" (org-mem-file node))
                 (member \"drill\" (org-mem-tags node))))))" ;; XXX docstring
  :type 'function
  :set #'org-node--set-and-remind-reset)

(defcustom org-node-insert-link-hook '(org-mem-x-ensure-link-at-point-known)
  "Hook run after inserting a link to an Org-ID node.

Called with point in the new link."
  :type 'hook)

(defcustom org-node-creation-hook nil
  "Hook run with point in the newly created buffer or entry.

Applied by `org-node-new-file', `org-node-capture-target',
`org-node-insert-heading', `org-node-nodeify-entry' and
`org-node-extract-subtree'.

NOT applied by `org-node-new-via-roam-capture' -- see
org-roam\\='s `org-roam-capture-new-node-hook' instead.

A good function for this hook is `'org-node-ensure-crtime-property',
since the default `org-node-datestamp-format' is empty.

In the author\\='s experience, recording the creation-date somewhere may
prove useful later on, e.g. when publishing to a blog.
Filesystem creation-time cannot be relied on."
  :type 'hook)

(unless (featurep 'org-node)
  (add-hook 'org-node-creation-hook #'org-node-ensure-crtime-property -95)
  (add-hook 'org-node-creation-hook #'org-mem-x-ensure-entry-at-point-known -90))


;;;; Pretty completion

(defcustom org-node-alter-candidates nil
  "Whether to alter completion candidates instead of affixating.

This means that org-node will concatenate the results of
`org-node-affixation-fn' into a single string, so what the user types in
the minibuffer can match against the prefix and suffix as well as
against the node title.

In other words: you can match against the node's outline path, if
as `org-node-affixation-fn' is set to `org-node-prefix-with-olp'
\(default).

\(Tip: users of the \"orderless\" library do not need this
setting, they can always match against the prefix and suffix via
`orderless-annotation', bound to the character \& by default.)

Another consequence: this setting can lift the uniqueness constraint on
note titles: you\\='ll be able to have two nodes with the same name, so
long as their prefix or suffix differ in some way.

After changing this setting, run \\[org-mem-reset]."
  :type 'boolean
  :set #'org-node--set-and-remind-reset)

;; For context see :affixation-function in `completion-extra-properties',
;; however the following function is expected to operate on one candidate at a
;; time, instead of a list.  The code flow is a bit roundabout, but the results
;; are ultimately used by `org-node-collection'.
(defcustom org-node-affixation-fn #'org-node-prefix-with-olp
  "Function to give prefix and suffix to completion candidates.

The results will style the appearance of completions during
\\[org-node-find], \\[org-node-insert-link] et al.

Built-in choices:

- `org-node-affix-bare'
- `org-node-prefix-with-olp'
- `org-node-prefix-with-tags'
- `org-node-affix-with-olp-and-tags'

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
          (function-item org-node-prefix-with-olp)
          (function-item org-node-prefix-with-tags)
          (function-item org-node-prefix-with-tags-and-olp)
          (function-item org-node-affix-with-olp-and-tags)
          (function-item org-node-affix-with-olp-and-tags-legacy)
          (function :tag "Custom function"
                    :value (lambda (node title) (list title "" ""))))
  :package-version '(org-node . "0.9")
  :set #'org-node--set-and-remind-reset)

(defun org-node-affix-bare (_node title)
  "Use TITLE as-is.
For use as `org-node-affixation-fn'."
  (list title "" ""))

(defun org-node-prefix-with-tags (node title)
  "Prepend NODE's tags to TITLE.
For use as `org-node-affixation-fn'."
  (list title
        (let ((tags (org-mem-entry-tags node)))
          (if tags
              (propertize (concat "(" (string-join tags ", ") ") ")
                          'face 'org-tag)
            ""))
        ""))

(defun org-node-prefix-with-olp (node title)
  "Prepend NODE's outline path to TITLE."
  (list title
        (if-let* ((fontified-ancestors
                   (cl-loop
                    for ancestor in (org-mem-entry-olpath-with-title node)
                    collect
                    (propertize ancestor 'face 'completions-annotations))))
            (concat (string-join fontified-ancestors " > ") " > ")
          "")
        ""))

(defun org-node-prefix-with-tags-and-olp (node title)
  "Prepend NODE's tags and outline path to TITLE."
  (list title
        (let ((tags (org-mem-entry-tags node))
              (fontified-ancestors
               (cl-loop
                for ancestor in (org-mem-entry-olpath-with-title node)
                collect (propertize ancestor 'face 'completions-annotations))))
          (concat
           ;; TODO: Fallback on other face before org init
           (and tags (propertize (concat "(" (string-join tags ", ") ") ")
                                 'face 'org-tag))
           (and fontified-ancestors
                (concat (and tags " ")
                        (string-join fontified-ancestors " > ") " > "))))
        ""))

(defun org-node-affix-with-olp-and-tags (node title)
  "Prepend NODE's outline path to TITLE, and append NODE's tags."
  (list title
        (if (org-mem-entry-subtree-p node)
            (let ((ancestors (org-mem-entry-olpath-with-title node))
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

;; Doubt many used this
(defun org-node-affix-with-olp-and-tags-legacy (node title)
  "Prepend NODE's outline path to TITLE, and append NODE's tags.
Legacy version."
  (let ((prefix-len 0))
    (list title
          (if (org-mem-entry-subtree-p node)
              (let ((ancestors (org-mem-entry-olpath-with-title node))
                    (result nil))
                (dolist (anc ancestors)
                  (push (propertize anc 'face 'completions-annotations) result)
                  (push " > " result))
                (setq result (apply #'concat (nreverse result)))
                (setq prefix-len (length result))
                result)
            "")
          (let ((tags (org-mem-entry-tags node)))
            (if tags
                (progn
                  (setq tags (propertize (concat (string-join tags ":"))
                                         'face 'org-tag))
                  (concat (make-string
                           (max 2 (- (default-value 'fill-column)
                                     (+ prefix-len (length title) (length tags))))
                           ?\s)
                          tags))
              "")))))

(defvar org-node--title<>affixation-triplet (make-hash-table :test 'equal)
  "1:1 table mapping titles or aliases to affixation triplets.")

(defun org-node--affixate-collection (coll)
  "From list COLL, make an alist of affixated members."
  (cl-loop for title in coll
           collect (gethash title org-node--title<>affixation-triplet)))

;; TODO: Assign a category `org-node', then add an embark action to embark?
;; TODO: Bind a custom exporter to `embark-export'
(defun org-node-collection (str pred action)
  "Custom COLLECTION for `completing-read'.

Ahead of time, org-node takes titles and aliases from
`org-mem--title<>id', runs `org-node-affixation-fn' on each, and
depending on the user option `org-node-alter-candidates', it
either saves the affixated thing directly into
`org-node--candidate<>entry', or into a secondary table
`org-node--title<>affixation-triplet'.  Finally, this function
then either simply reads candidates off the candidates table, or
attaches the affixes in realtime.

Regardless of which, all completions are guaranteed to be keys of
`org-node--candidate<>entry', but remember that it is possible for
`completing-read' to exit with user-entered input that didn\\='t
match anything.

Arguments STR, PRED and ACTION are handled behind the scenes,
see Info node `(elisp)Programmed Completion'."
  (if (eq action 'metadata)
      (cons 'metadata (unless org-node-alter-candidates
                        (list (cons 'affixation-function
                                    #'org-node--affixate-collection))))
    (complete-with-action action org-node--candidate<>entry str pred)))

(defvar org-node-hist nil
  "Minibuffer history.")

;; Boost this completion hist to at least 1000 elements, unless user has nerfed
;; the default `history-length'.
;; Because you often narrow down the completions majorly, and still want to
;; sort among what's left.
(and (>= history-length (car (get 'history-length 'standard-value)))
     (< history-length 1000)
     (put 'org-node-hist 'history-length 1000))


;;;; The mode

(defvar org-node--candidate<>entry (make-hash-table :test 'equal)
  "1:1 table mapping completion candidates to nodes.")

(defun org-node--let-refs-be-aliases (node)
  "Add ROAM_REFS of NODE as extra completion candidates."
  (dolist (ref (org-mem-entry-roam-refs node))
    (puthash ref node org-node--candidate<>entry)
    (puthash ref
             (let ((type (gethash ref org-mem--roam-ref<>type)))
               (list (propertize ref 'face 'org-cite)
                     (when type
                       (propertize (concat type ":")
                                   'face 'completions-annotations))
                     nil))
             org-node--title<>affixation-triplet)))

(defun org-node--record-completion-candidates (node)
  "Cache completion candidates for NODE and its aliases."
  (when (and (org-mem-entry-id node)
             (org-mem-entry-title-maybe node)
             (funcall (org-node--try-ensure-compiled org-node-filter-fn) node))
    (dolist (title (cons (org-mem-entry-title node)
                         (org-mem-entry-roam-aliases node)))
      (let ((affx (funcall (org-node--try-ensure-compiled org-node-affixation-fn)
                           node
                           title)))
        (if org-node-alter-candidates
            ;; Absorb the affixations into one candidate string
            (puthash (concat (nth 1 affx) (nth 0 affx) (nth 2 affx))
                     node
                     org-node--candidate<>entry)
          ;; Bare title, to be affixated later by `org-node-collection'
          (puthash title affx org-node--title<>affixation-triplet)
          (puthash title node org-node--candidate<>entry))))))

(defun org-node--wipe-completions (_parse-results)
  "Clear table `org-node--candidate<>entry'."
  (clrhash org-node--candidate<>entry))

;; Could have used `org-mem-forget-file-functions', but more efficient to loop
;; over entire parse-results once.
(defun org-node--forget-completions-in-results (parse-results)
  "Remove old completions where PARSE-RESULTS has new data."
  (seq-let (bad-paths file-data) parse-results
    (org-node--forget-completions-in-files
     (append bad-paths (mapcar #'car file-data)))))

(defun org-node--forget-completions-in-files (files)
  "Remove the completion candidates for all nodes in FILES."
  (when files
    (org-mem-delete (##member (org-mem-entry-file %2) files)
                    org-node--candidate<>entry)
    ;; Undo an effect of `org-node--let-refs-be-aliases'
    (dolist (ref (seq-mapcat #'org-mem-entry-roam-refs
                             (org-mem-entries-in-files files)))
      (remhash ref org-mem--title<>id))))

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Cache completion candidates every time Org-mem updates its cache.
You should also turn on `org-mem-updater-mode'."
  :global t
  (cond
   (org-node-cache-mode
    (add-hook 'org-mem-pre-full-scan-functions #'org-node--wipe-completions)
    (add-hook 'org-mem-pre-targeted-scan-functions #'org-node--forget-completions-in-results)
    (add-hook 'org-mem-record-entry-functions #'org-node--record-completion-candidates)
    (add-hook 'org-mem-record-entry-functions #'org-node--let-refs-be-aliases)
    (org-mem--scan-full))
   (t
    (remove-hook 'org-mem-pre-full-scan-functions #'org-node--wipe-completions)
    (remove-hook 'org-mem-pre-targeted-scan-functions #'org-node--forget-completions-in-results)
    (remove-hook 'org-mem-record-entry-functions #'org-node--record-completion-candidates)
    (remove-hook 'org-mem-record-entry-functions #'org-node--let-refs-be-aliases))))

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
            (org-mem-x--forget-file-contents (list buffer-file-truename))
            (org-node--forget-completions-in-files (list buffer-file-truename))
            (kill-buffer buf)))))))


;;;; Scanning files to cache info about them

;;;###autoload
(define-obsolete-function-alias 'org-node-reset #'org-mem-reset "2025-05-09")

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
    (org-mem--scan-full))
  (when block
    (org-mem-block 'org-node 10)))

(defvar org-node--first-init t
  "Non-nil until org-node has been initialized, then nil.
Mainly for muffling some messages.")

;; FIXME
(defvar org-node--old-link-sets nil
  "For use by `org-node-backlink-lazy'.

Alist of ((TARGET . LINKS) (TARGET . LINKS) ...), where LINKS is are sets of
links with destination TARGET.  These reflect a past state of
`org-mem--target<>links', allowing for a diff operation against the
up-to-date set.")

(defvar org-node--compile-timers nil)
(defvar org-node--compiled-lambdas (make-hash-table :test 'equal)
  "1:1 table mapping lambda expressions to compiled bytecode.")

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


;;;; Etc

(defun org-node--die (format-string &rest args)
  "Like `error' but make sure the user sees it.
Useful because not everyone has `debug-on-error' t, and then
errors are very easy to miss.

Arguments FORMAT-STRING and ARGS as in `format-message'."
  (let ((err-string (apply #'format-message format-string args)))
    (unless debug-on-error
      (display-warning 'org-node err-string :error))
    (error "%s" err-string)))

(defun org-node--consent-to-bothersome-modes-for-mass-edit ()
  "Confirm about certain modes being enabled.
These are modes such as `auto-save-visited-mode' that can
interfere with user experience during an incremental mass editing
operation."
  (cl-loop for mode in '(auto-save-visited-mode
                         git-auto-commit-mode)
           when (and (boundp mode)
                     (symbol-value mode)
                     (not (y-or-n-p
                           (format "%S is active - proceed anyway?" mode))))
           return nil
           finally return t))


;;;; Filename functions

;; To benchmark:
;; (progn (byte-compile #'org-node--root-dirs) (benchmark-run 10 (org-node--root-dirs (hash-table-values org-id-locations))))
;; REVIEW: 75% of compute is in `file-name-directory', can that be improved?
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
substring \"/home/me\" referring to the same location.


For Org users, it is pragmatic to know that if FILE-LIST was the
output of something like

   \(hash-table-values org-id-locations)

this function will in many cases spit out a list of exactly one item
because many people keep their Org files in one root directory \(with
various subdirectories)."
  (let* ((files (delete-dups (copy-sequence file-list)))
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

(defcustom org-node-ask-directory nil
  "Whether to ask the user where to save a new file.

- Symbol nil: put file in the most populous root directory in
              `org-id-locations' without asking
- String: a directory path in which to put the file
- Symbol t: ask every time

This variable determines the directory component, but the file basename
is determined by `org-node-slug-fn' and `org-node-datestamp-format'."
  :group 'org-node
  :type '(choice boolean string))

(defun org-node-guess-or-ask-dir (prompt)
  "Maybe prompt for a directory, and if so, use string PROMPT.
Behavior depends on user option `org-node-ask-directory'.
In any case, return a directory."
  (if (eq t org-node-ask-directory)
      (read-directory-name prompt)
    (if (stringp org-node-ask-directory)
        org-node-ask-directory
      (car (org-node--root-dirs (org-mem-all-files))))))

(defcustom org-node-datestamp-format ""
  "Passed to `format-time-string' to prepend to filenames.

Example from Org-roam: %Y%m%d%H%M%S-
Example from Denote: %Y%m%dT%H%M%S--

For the rest of the filename, configure `org-node-slug-fn'."
  :type '(radio
          (const :tag "None" :value "")
          (const :tag "Like Org-roam: %Y%m%d%H%M%S-" :value "%Y%m%d%H%M%S-")
          (const :tag "Like Denote: %Y%m%dT%H%M%S--" :value "%Y%m%dT%H%M%S--")
          (string :tag "Custom")))

;; TODO: It should prolly receive the entire node as second argument
(defcustom org-node-slug-fn #'org-node-slugify-for-web
  "Function taking a node title and returning a filename component.
Receives one argument: the value of an Org #+TITLE keyword, or
the first heading in a file that has no #+TITLE.

Built-in choices:
- `org-node-slugify-for-web'
- `org-node-slugify-like-roam-default'

It is popular to also prefix filenames with a datestamp.  To do
that, configure `org-node-datestamp-format'."
  :type '(radio
          (function-item org-node-slugify-for-web)
          (function-item org-node-slugify-like-roam-default)
          (function :tag "Custom function" :value (lambda (title) title))))

(defun org-node-slugify-like-roam-default (title)
  "From TITLE, make a filename slug in default org-roam style.
Does not require org-roam installed.

A title like \"Löb\\='s Theorem\" becomes \"lob_s_theorem\".

Diacritical marks U+0300 to U+0331 are stripped \(mostly used with Latin
alphabets).  Also stripped are all glyphs not categorized in Unicode as
belonging to an alphabet or number system.

If you seek to emulate org-roam filenames, you may also want to
configure `org-node-datestamp-format'."
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


;;;; How to create new nodes

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.
Automatically set, should be nil most of the time.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.
Automatically set, should be nil most of the time.")

(defvar org-node-proposed-seq nil
  "Key that identifies a node sequence about to be added-to.
Automatically set, should be nil most of the time.")

(defun org-node--goto (node &optional pos)
  "Visit NODE."
  (if node
      (let ((file (org-mem-entry-file node)))
        (if (file-exists-p file)
            (let ((pos (or pos (org-mem-entry-pos node))))
              (when (not (file-readable-p file))
                (error "org-node: Couldn't visit unreadable file %s" file))
              (find-file file)
              (widen)
              ;; TODO: Be friendly in this special case, but what's appropriate?
              ;; (when (string-blank-p (buffer-string)))

              ;; Now `save-place-find-file-hook' has potentially already
              ;; moved point, and that could be good enough.  So: move
              ;; point to node heading, unless heading is already inside
              ;; visible part of buffer and point is at or under it
              (if (/= 0 (org-mem-entry-level node))
                  (unless (and (pos-visible-in-window-p pos)
                               (not (org-invisible-p pos))
                               (equal (org-mem-entry-title-maybe node)
                                      (org-get-heading t t t t)))
                    (goto-char pos)
                    (org-fold-show-entry)
                    (org-fold-show-children)
                    (recenter 0))
                (unless (pos-visible-in-window-p pos)
                  (goto-char pos))))
          ;; NOTE: If this happens a lot,
          ;;       maybe change to just (org-mem-x--forget-file-contents file).
          (message "org-node: Didn't find file, resetting...")
          (org-mem--scan-full)))
    (error "`org-node--goto' received a nil argument")))

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
          (function :tag "Custom function" :value (lambda ()))))

;; TODO: "Create" sounds unspecific, rename to "New node"?
(defun org-node-create (title id &optional seq-key)
  "Call `org-node-creation-fn' with necessary variables set.

TITLE will be title of node, ID will be id of node \(use
`org-id-new' if you don\\='t know\).

Optional argument SEQ-KEY means use the resulting node to
maybe grow the corresponding sequence.

When calling from Lisp, you should not assume anything about
which buffer will be current afterwards, since it depends on
`org-node-creation-fn', whether TITLE or ID had existed, and
whether the user carried through with the creation.

To operate on a node after creating it, either let-bind
`org-node-creation-fn' so you know what you get, or hook
`org-node-creation-hook' temporarily, or write:

    (org-node-create TITLE ID)
    (org-node-cache-ensure t)
    (let ((node (gethash ID org-nodes)))
      (if node (org-node--goto node)))"
  (setq org-node-proposed-title title)
  (setq org-node-proposed-id id)
  (setq org-node-proposed-seq seq-key)
  (unwind-protect
      (funcall org-node-creation-fn)
    (setq org-node-proposed-title nil)
    (setq org-node-proposed-id nil)
    (setq org-node-proposed-seq nil)))

(defun org-node-new-file ()
  "Create a file-level node.
Meant to be called indirectly as `org-node-creation-fn', so that some
necessary variables are set."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "org-node-new-file is meant to be called indirectly")
    (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
           (path-to-write
            (file-name-concat
             dir
             (concat (format-time-string org-node-datestamp-format)
                     (funcall org-node-slug-fn org-node-proposed-title)
                     ".org"))))
      (when (file-exists-p path-to-write)
        (error "File already exists: %s" path-to-write))
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
      (run-hooks 'org-node-creation-hook))))

(defun org-node-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn'."
  (when (or (null org-node-proposed-title)
            (null org-node-proposed-id))
    (error "`org-node-new-via-roam-capture' is meant to be called indirectly via `org-node-create'"))
  (unless (require 'org-roam nil t)
    (error "`org-node-new-via-roam-capture' requires library \"org-roam\""))
  (when (and (fboundp 'org-roam-capture-)
             (fboundp 'org-roam-node-create))
    (org-roam-capture- :node (org-roam-node-create
                              :title org-node-proposed-title
                              :id    org-node-proposed-id))))

(defalias 'org-node-fakeroam-new-via-roam-capture
  #'org-node-new-via-roam-capture)

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
\"inverted\" workflow:

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
          (setq id org-node-proposed-id)
          (setq node (gethash title org-node--candidate<>entry)))
      ;; Was called from `org-capture', which means the user has not yet typed
      ;; the title; let them type it now
      (let ((input (completing-read "Node: " #'org-node-collection
                                    () () () 'org-node-hist)))
        (setq node (gethash input org-node--candidate<>entry))
        (if node
            (progn
              (setq title (org-mem-entry-title node))
              (setq id (org-mem-entry-id node)))
          (setq title input)
          (setq id (org-id-new)))))
    (if node
        ;; Node exists; capture into it
        (progn
          (find-file (org-mem-entry-file node))
          (widen)
          (goto-char (org-mem-entry-pos node))
          (org-reveal)
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
              (file-name-concat
               dir (concat (format-time-string org-node-datestamp-format)
                           (funcall org-node-slug-fn title)
                           ".org"))))
        (when (file-exists-p path-to-write)
          (error "File already exists: %s" path-to-write))
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


;;;; Commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To make this behave like `org-roam-node-find' when creating new nodes,
set `org-node-creation-fn' to `org-node-new-via-roam-capture'."
  (interactive)
  (org-node-cache-ensure)
  (let* ((input (completing-read "Go to ID-node: " #'org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node--candidate<>entry)))
    (if node
        (org-node--goto node)
      (if (string-blank-p input)
          (message "Won't create untitled node")
        (org-node-create input (org-id-new))))))

;;;###autoload
(defun org-node-visit-random ()
  "Visit a random node."
  (interactive)
  (org-node-cache-ensure)
  (org-node--goto (nth (random (hash-table-count org-node--candidate<>entry))
                       (hash-table-values org-node--candidate<>entry))))

;;;###autoload
(defun org-node-insert-link (&optional region-as-initial-input novisit)
  "Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*' and its docstring.

Optional argument REGION-AS-INITIAL-INPUT t means behave as
`org-node-insert-link*'.

Argument NOVISIT means behave as
`org-node-insert-link-novisit'."
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
                          (when region-text
                            (try-completion region-text org-mem--title<>id)))
                      region-text
                    nil))
         (input (if (and novisit initial)
                    initial
                  (completing-read "Node: " #'org-node-collection
                                   () () initial 'org-node-hist)))
         (node (gethash input org-node--candidate<>entry))
         (id (if node (org-mem-entry-id node) (org-id-new)))
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
    (unless node
      (if (string-blank-p input)
          (message "Won't create untitled node")
        (org-node-create input id)))))

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
  "Insert a link to one of your ID nodes without ever visiting it.

Normally, if the node does not exist, `org-node-insert-link' would
create it and then visit it.  This will not visit it."
  (interactive "*" org-mode)
  (let ((org-roam-capture-templates
         (list (append (car (bound-and-true-p org-roam-capture-templates))
                       '(:immediate-finish t)))))
    (org-node-insert-link t t)))

;;;###autoload
(defun org-node-insert-link-novisit* ()
  "Insert a link to one of your ID nodes without ever visiting it.

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
  (unless node
    (setq node (org-node-read)))
  (if (not node)
      (message "Node not known, create it first")
    (let ((id (org-mem-entry-id node))
          (title (org-mem-entry-title node))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id) title))
      (goto-char (pos-bol))
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level " (number-to-string (+ 1 level))))))

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

;; TODO: Maybe concat the link type...
(defun org-node-insert-raw-link ()
  "Insert at point a link that exists in some Org file somewhere.
Works in non-Org buffers."
  (interactive)
  (insert (completing-read "Insert raw link: "
                           (org-node--list-known-raw-links)
                           nil nil nil 'org-node-link-hist)))

;;;###autoload
(defun org-node-refile ()
  "Experimental."
  (interactive () org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command expects an org-mode buffer"))
  (org-node-cache-ensure)
  (when (org-invisible-p)
    (user-error "Better not run this command in an invisible region"))
  (let* ((input (completing-read "Refile into ID-node: " #'org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node--candidate<>entry)))
    (unless node
      (error "Node not found %s" input))
    (org-back-to-heading t)
    (when (org-invisible-p)
      ;; IDK... I'm scared of invisible regions, don't know how they work
      (user-error "Better not run this command in an invisible region"))
    (org-cut-subtree)
    (org-node--goto node)
    (widen)
    (when (outline-next-heading)
      (backward-char 1))
    (org-paste-subtree)
    (org-mem-x-ensure-entry-at-point-known)))

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
            (file-name-concat
             dir (concat (format-time-string org-node-datestamp-format)
                         (funcall org-node-slug-fn title)
                         ".org")))
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
            (org-mem-x-ensure-link-at-point-known id)))
        (find-file path-to-write)
        (org-paste-subtree)
        (unless org-node-prefer-with-heading
          ;; Replace the root heading and its properties with file-level
          ;; keywords &c.
          (goto-char (point-min))
          (org-end-of-meta-data t)
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
        (org-mem-x-ensure-entry-at-point-known)
        (push (current-buffer) org-node--new-unsaved-buffers)
        (run-hooks 'org-node-creation-hook)
        (when (bound-and-true-p org-node-backlink-mode)
          (org-node-backlink--fix-nearby))))))

(defun org-node-extract-file-name-datestamp (path)
  "From filename PATH, get the datestamp prefix if it has one.
Do so by comparing with `org-node-datestamp-format'.

High risk of false positives if you have been changing formats over
time without renaming existing files."
  (when (and org-node-datestamp-format
             (not (string-blank-p org-node-datestamp-format)))
    (let ((name (file-name-nondirectory path)))
      (when (string-match
             (org-node--make-regexp-for-time-format org-node-datestamp-format)
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
out, with any year, month or day.

Memoize the value, so consecutive calls with the same FORMAT only
need to compute once."
  (if (equal format (car org-node--make-regexp-for-time-format))
      ;; Reuse memoized value on consecutive calls with same input
      (cdr org-node--make-regexp-for-time-format)
    (cdr (setq org-node--make-regexp-for-time-format
               (cons format
                     (let ((example (format-time-string format)))
                       (if (string-match-p (rx (any "^*+([\\")) example)
                           ;; TODO: Improve error message, now it presumes caller
                           (error "org-node: Unable to safely rename with current `org-node-datestamp-format'.  This is not inherent in your choice of format, I am just not smart enough")
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
`after-save-hook' or similar place.

To add exceptions, see `org-node-renames-exclude'."
  :type '(repeat string))

(defcustom org-node-renames-exclude "\\(?:daily\\|dailies\\|journal\\)/"
  "Regexp matching paths of files not to auto-rename.
For use by `org-node-rename-file-by-title'.

Only applied to files under `org-node-renames-allowed-dirs'.  If
a file is not there, it is not considered in any case."
  :type 'string)

;;;###autoload
(defun org-node-rename-file-by-title (&optional interactive)
  "Rename the current file according to `org-node-slug-fn'.

Also attempt to check for a prefix in the style of
`org-node-datestamp-format', and preserve any such prefix.  Otherwise, add one.

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
                                (format-time-string org-node-datestamp-format)))
               (slug (funcall org-node-slug-fn title))
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
                       (node (gethash id org-nodes))
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

;;;###autoload
(defun org-node-insert-heading ()
  "Insert a heading with ID and run `org-node-creation-hook'."
  (interactive "*" org-mode)
  (org-insert-heading)
  (org-node-nodeify-entry))

;;;###autoload
(defun org-node-nodeify-entry ()
  "Add an ID to entry at point and run `org-node-creation-hook'."
  (interactive "*" org-mode)
  (org-node-cache-ensure) ;; Because the hook could contain anything
  (org-id-get-create)
  (run-hooks 'org-node-creation-hook))

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive "*" org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED"
                   (format-time-string (org-time-stamp-format t t)))))
(defalias 'org-node-ensure-crtime-property 'org-node-put-created)

;;;###autoload
(defun org-node-forget-dir (dir)
  "Remove references in `org-id-locations' to files in DIR.

Note that if DIR descends from a member of `org-mem-watch-dirs',
this action may make no practical impact unless you add DIR to
`org-mem-watch-dirs-exclude'.

Tip: In case of unsolvable problems, eval this to wipe org-id-locations:

\(progn
 (delete-file org-id-locations-file)
 (setq org-id-locations nil)
 (setq org-id--locations-checksum nil)
 (setq org-agenda-text-search-extra-files nil)
 (setq org-id-files nil)
 (setq org-id-extra-files nil))"
  (interactive "DForget all IDs in directory: ")
  (org-node-cache-ensure t)
  (let ((files (nconc (org-mem--dir-files-recursive dir ".org_exclude" nil)
                      (org-mem--dir-files-recursive dir ".org" nil))))
    (when files
      (setq files (nconc files (mapcar #'org-mem--abbr-truename files)))
      (message "Forgetting all IDs in directory %s..." dir)
      (redisplay)
      (maphash (lambda (id file)
                 (when (member file files)
                   (remhash id org-id-locations)))
               org-id-locations)
      (dolist (file files)
        (remhash file org-mem--file<>entries)
        (remhash file org-mem--file<>metadata))
      (org-id-locations-save)
      (org-mem-reset))))

;; TODO: Optionally obey filter-fn
;;;###autoload
(defun org-node-grep ()
  "Grep across all files known to org-node."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "This command requires the consult package"))
  (require 'consult)
  (org-node-cache-ensure)
  ;; Prevent consult from turning the names relative, with such enlightening
  ;; directory paths as ../../../../../../.
  (cl-letf (((symbol-function #'file-relative-name)
             (lambda (name &optional _dir) name)))
    (let ((consult-ripgrep-args (concat consult-ripgrep-args " --type=org")))
      (if (executable-find "rg")
          (consult--grep "Grep in files known to org-node: "
                         #'consult--ripgrep-make-builder
                         (org-node--root-dirs (org-mem-all-files))
                         nil)
        ;; Much slower!  Vanilla grep does not have Ripgrep's --type=org, so
        ;; must target thousands of files and not a handful of dirs, a calling
        ;; pattern that would also slow Ripgrep down.
        (consult--grep "(Ripgrep not found) Grep in files known to org-node: "
                       #'consult--grep-make-builder
                       (org-mem-all-files)
                       nil)))))

(defun org-node-insert-link-into-drawer ()
  "Experimental; insert a link into a RELATED drawer in current entry."
  (interactive "*" org-mode)
  (save-excursion
    (save-restriction
      (org-node-narrow-to-drawer-create "RELATED" t)
      ;; Here is something to ponder in the design of
      ;; `org-node-narrow-to-drawer-create'. Should it ensure a blank line?
      (let ((already-blank-line (eolp)))
        (atomic-change-group
          (org-node-insert-link nil t)
          (back-to-indentation)
          (insert (format-time-string (org-time-stamp-format t t)) " <- ")
          (unless already-blank-line
            (newline-and-indent)))))))

;; TODO: Make something like a find-dired buffer instead, handy!  Not the
;; actual find-dired, that'll be slow if we begin the search from fs
;; root... Dired operates on lines output by `ls', we ought to be able to
;; prepare such lines.
(defun org-node-list-files (&optional deprecated-arg)
  (interactive)
  (if deprecated-arg
      (progn
        (message "Function `org-node-list-files' obsoleted by `org-mem-all-files'")
        (org-mem-all-files))
    (org-mem-list--pop-to-tabulated-buffer
     :buffer "*org-node files*"
     :format [("File" 0 t)]
     :entries (cl-loop
               for file in (org-mem-all-files)
               collect (list (sxhash file)
                             (vector (buttonize file #'find-file file)))))))

(defvar org-node--unlinted nil)
(defvar org-node--lint-warnings nil)
(defun org-node-lint-all-files ()
  "Run `org-lint' on all known Org files, and report results.

If last run was interrupted, resume working through the file list
from where it stopped.  With prefix argument, start over
from the beginning."
  (interactive)
  (require 'org-lint)
  (let (files)
    (when (or (equal current-prefix-arg '(4))
              (and (null org-node--unlinted)
                   (setq files (org-mem-all-files))
                   (y-or-n-p (format "Lint %d files?"
                                     (length files)))))
      (setq org-node--unlinted files)
      (setq org-node--lint-warnings nil)))
  (setq org-node--unlinted
        (org-node--in-files-do
          :files org-node--unlinted
          :msg "Running org-lint (you can quit and resume anytime)"
          :about-to-do "About to visit a file to run org-lint"
          :call (lambda ()
                  ;; Org-lint's `org-lint-invalid-id-link' can cause spam, bc
                  ;; `org-id-update-id-locations' SILENT argument not perfect.
                  (let ((inhibit-message t))
                    (when-let* ((warning (org-lint)))
                      (push (cons buffer-file-name (car warning))
                            org-node--lint-warnings))))))
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

(defun org-node-list-feedback-arcs ()
  "Show a feedback-arc-set of forward id-links.

Requires GNU R installed, with R packages stringr, readr, igraph.

A feedback arc set is a set of links such that if they are all
cut (though sometimes it suffices to reverse the direction rather
than cut them), the remaining links in the network will
constitute a DAG (directed acyclic graph).

You may consider this as merely one of many ways to view your
network to quality-control it.  Rationale:

    https://edstrom.dev/zvjjm/slipbox-workflow#ttqyc"
  (interactive)
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
                 as origin-node = (gethash origin org-nodes)
                 as target-node = (gethash target org-nodes)
                 collect
                 (list (+ (sxhash origin) (sxhash target))
                       (vector (buttonize (org-mem-entry-title origin-node)
                                          #'org-node--goto
                                          origin-node)
                               (buttonize (org-mem-entry-title target-node)
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
                       when (equal "id" (org-mem-link-type link))
                       collect (concat target "\t" (org-mem-link-nearby-id link)))))
    "\n")))

;; TODO: Maybe expunge to org-mem.el after I figure out how to use fileloop well
(defvar org-node--found-coding-systems nil)
(defvar org-node--list-file-coding-systems-files nil)
(defun org-node-list-file-coding-systems ()
  "Check coding systems of files found by `org-node-list-files'.
This is done by temporarily visiting each file and checking what
Emacs decides to decode it as.  To start over, run the command
with \\[universal-argument] prefix."
  (interactive)
  (when (or (equal current-prefix-arg '(4))
            (and (null org-node--list-file-coding-systems-files)
                 (y-or-n-p (format "Check coding systems in %d files?  They will not be modified."
                                   (length (org-mem-all-files))))))
    (setq org-node--list-file-coding-systems-files (org-mem-all-files))
    (setq org-node--found-coding-systems nil))
  (setq org-node--list-file-coding-systems-files
        (org-node--in-files-do
          :files org-node--list-file-coding-systems-files
          :fundamental-mode t
          :msg "Checking file coding systems (you can quit and resume)"
          :about-to-do "About to check file coding system"
          :call (lambda ()
                  (push (cons buffer-file-name buffer-file-coding-system)
                        org-node--found-coding-systems))))
  (org-mem-list--pop-to-tabulated-buffer
   :buffer "*org file coding systems*"
   :format [("Coding system" 20 t) ("File" 40 t)]
   :entries (cl-loop for (file . sys) in org-node--found-coding-systems
                     collect (list file (vector (symbol-name sys) file)))
   :reverter #'org-node-list-file-coding-systems))

(defun org-node-list-reflinks ()
  "List all reflinks and their locations.

Useful to see how many times you\\='ve inserted a link that is very
similar to another link, but not identical, so that likely only
one of them is associated with a ROAM_REFS property.

Excludes reflinks not coming from an ID node."
  (interactive)
  (let ((entries
         (cl-loop
          for link in (org-mem-all-links)
          unless (equal "id" (org-mem-link-type link))
          as node = (org-mem-entry-by-id (org-mem-link-nearby-id link))
          ;; when entry
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


;;;; Misc 2

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
  (let ((why (if (eq (car body) :about-to-do)
                 (progn (pop body) (pop body))
               "Org-node is about to look inside a file")))
    `(let ((enable-local-variables :safe)
           (org-inhibit-startup t) ;; Don't apply startup #+options
           (find-file-hook nil)
           (after-save-hook nil)
           (before-save-hook nil)
           (org-agenda-files nil)
           (kill-buffer-hook nil) ;; Inhibit save-place etc
           (kill-buffer-query-functions nil)
           (buffer-list-update-hook nil))
       ;; The cache is buggy, disable to be safe
       (org-element-with-disabled-cache
         (let* ((--was-open-- (find-buffer-visiting ,file))
                (_ (when (file-directory-p ,file)
                     (error "Is a directory: %s" ,file)))
                (--buf-- (or --was-open--
                             (delay-mode-hooks
                               (org-node--find-file-noselect
                                (org-mem--abbr-truename ,file)
                                ,why)))))
           (when (bufferp --buf--)
             (with-current-buffer --buf--
               (save-excursion
                 (without-restriction
                   ,@body))
               (if --was-open--
                   ;; Because the cache gets confused by changes
                   (org-element-cache-reset)
                 (when (buffer-modified-p)
                   (let ((save-silently t)
                         (inhibit-message t))
                     (save-buffer)))
                 (kill-buffer)))))))))

(define-error 'org-node-must-retry "Unexpected signal org-node-must-retry")

;; REVIEW: Check out fileloop.el
;;         and contribute to it if I feel anything's missing
(cl-defun org-node--in-files-do
    (&key files fundamental-mode msg about-to-do call
          too-many-files-hack cleanup)
  "Temporarily visit each file in FILES and call function CALL.

Take care!  This function presumes that FILES satisfy the assumptions
made by `org-node--find-file-noselect'.  This is safe if FILES is
the output of `org-node-list-files', but easily violated otherwise.

While the loop runs, print a message every now and then, composed
of MSG and a counter for the amount of files left to visit.

On running across a problem such as the auto-save being newer than the
canonical file, prompt the user to recover it using string ABOUT-TO-DO
to clarify why the file is about to be accessed, and break the loop if
the user declines.

If the user quits mid-way through the loop, or it is broken,
return the remainder of FILES that have not yet been visited.

In each file visited, the behavior is much like
`org-node--with-quick-file-buffer'.

The files need not be Org files, and if optional argument
FUNDAMENTAL-MODE is t, do not activate any major mode.

If a buffer had already been visiting FILE, reuse that buffer.
Naturally, FUNDAMENTAL-MODE has no effect in that case.

For explanation of TOO-MANY-FILES-HACK, see code comments.
CLEANUP is a kludge related to that."
  (declare (indent defun))
  (cl-assert (and msg files call about-to-do))
  (setq call (org-node--try-ensure-compiled call))
  (let ((enable-local-variables :safe)
        (org-inhibit-startup t) ;; Don't apply startup #+options
        (file-name-handler-alist nil)
        (find-file-hook nil)
        (after-save-hook nil)
        (before-save-hook nil)
        (org-agenda-files nil)
        (kill-buffer-hook nil) ;; Inhibit save-place etc
        (kill-buffer-query-functions nil)
        (write-file-functions nil) ;; recentf-track-opened-file
        (buffer-list-update-hook nil)
        (org-node-renames-allowed-dirs nil))
    (let ((files* files)
          interval
          file
          was-open
          buf
          (start-time (current-time))
          (ctr 0))
      ;; The cache is buggy, disable to be safe
      (org-element-with-disabled-cache
        (condition-case err
            (while files*
              (cl-incf ctr)
              (when (zerop (% ctr 200))
                ;; Reap open file handles (max is 1024, and Emacs bug can keep
                ;; them open during the loop despite killed buffers)
                (garbage-collect)
                (when too-many-files-hack
                  ;; Sometimes necessary to drop the call stack to actually
                  ;; reap file handles, sometimes not.  Don't understand it.
                  ;; E.g. `org-node-lint-all-files' does not seem to need this hack,
                  ;; but `org-node-backlink-fix-all-files' does.
                  (signal 'org-node-must-retry nil)))
              (if interval
                  (when (zerop (% ctr interval))
                    (message "%s... %d files to go" msg (length files*)))
                ;; Set a reasonable interval between `message' calls, since they
                ;; can be surprisingly expensive.
                (when (> (float-time (time-since start-time)) 0.2)
                  (setq interval ctr)))
              (setq file (pop files*))
              (setq was-open (find-buffer-visiting file))
              (setq buf (or was-open
                            (if fundamental-mode
                                (let ((auto-mode-alist nil))
                                  (org-node--find-file-noselect
                                   file about-to-do))
                              (delay-mode-hooks
                                (org-node--find-file-noselect
                                 file about-to-do)))))
              (when buf
                (with-current-buffer buf
                  (save-excursion
                    (without-restriction
                      (funcall call)))
                  (unless was-open
                    (when (buffer-modified-p)
                      (let ((save-silently t)
                            (inhibit-message t))
                        (save-buffer)))
                    (kill-buffer)))))
          (( org-node-must-retry )
           (run-with-timer .1 nil #'org-node--in-files-do
                           :msg msg
                           :about-to-do about-to-do
                           :fundamental-mode fundamental-mode
                           :files files*
                           :call call
                           :too-many-files-hack too-many-files-hack)
           ;; Because of the hack, the caller only receives `files*' once, and
           ;; each timer run after that would not modify global variable like
           ;; `org-node-backlink--files-to-fix', so try to modify as a nicety.
           ;; The consequence is that user can quit and resume without
           ;; restarting nearly the entire file list.
           ;; This works until the last iteration, because a cons cell cannot
           ;; be destructively reassigned to nil.
           (setcar files (car files*))
           (setcdr files (cdr files*))
           files*)
          (( quit )
           (unless (or was-open (not buf) (buffer-modified-p buf))
             (kill-buffer buf))
           (when (functionp cleanup) (funcall cleanup))
           (cons file files*))
          (( error )
           (lwarn 'org-node :warning "%s: Loop interrupted by signal %S\n\tBuffer: %s\n\tFile: %s\n\tNext file: %s\n\tValue of ctr: %d"
                  (format-time-string "%T") err buf file (car files*) ctr)
           (unless (or was-open (not buf) (buffer-modified-p buf))
             (kill-buffer buf))
           files*)
          (:success
           (when (functionp cleanup) (funcall cleanup))
           (message "%s... done" msg)
           nil))))))

;; Somewhat faster than `find-file-noselect', not benchmarked.
;; More importantly, the way it fails is better suited for loop usage, IMO.
;; Also better for silent background usage.  The argument ABOUT-TO-DO clarifies
;; what would otherwise be a mysterious error that's difficult for the user to
;; track down to this package.
(defun org-node--find-file-noselect (abbr-truename about-to-do)
  "Read file ABBR-TRUENAME into a buffer and return the buffer.
If there's a problem such as an auto-save file being newer, prompt the
user to proceed with a message based on string ABOUT-TO-DO, else do
nothing and return nil.

Very presumptive!  Like `find-file-noselect' but intended as a
subroutine for `org-node--in-files-do' or any caller that has
already ensured that ABBR-TRUENAME:

- is an abbreviated file truename
  - \(i.e. the equivalent of passing a wild filename through
     `file-truename' and then `abbreviate-file-name')
- does not satisfy `backup-file-name-p'
- is not already being visited in another buffer
- is not a directory"
  (let ((attrs (file-attributes abbr-truename))
        (buf (create-file-buffer abbr-truename)))
    (when (null attrs)
      (kill-buffer buf)
      (error "File appears to be gone/renamed: %s" abbr-truename))
    (if (or (not (and large-file-warning-threshold
                      (> (file-attribute-size attrs)
                         large-file-warning-threshold)))
            (y-or-n-p
             (format "%s, but file %s is large (%s), open anyway? "
                     about-to-do
                     (file-name-nondirectory abbr-truename)
                     (funcall byte-count-to-string-function
                              large-file-warning-threshold))))
        (with-current-buffer buf
          (condition-case nil
              (progn (insert-file-contents abbr-truename t)
                     (set-buffer-multibyte t))
	    (( file-error )
             (kill-buffer buf)
             (error "Problems reading file: %s" abbr-truename)))
          (setq buffer-file-truename abbr-truename)
          (setq buffer-file-number (file-attribute-file-identifier attrs))
          (setq default-directory (file-name-directory buffer-file-name))
          (if (and (not (and buffer-file-name auto-save-visited-file-name))
                   (file-newer-than-file-p (or buffer-auto-save-file-name
				               (make-auto-save-file-name))
			                   buffer-file-name))
              ;; Could try to call `recover-file' here, but I'm not sure the
              ;; resulting behavior would be sane, so just bail
              (progn
                (message "%s, but skipped because it has an auto-save file: %s"
                         about-to-do buffer-file-name)
                nil)
            (normal-mode t)
            (current-buffer)))
      (message "%s, but skipped because file exceeds `large-file-warning-threshold': %s"
               about-to-do buffer-file-name)
      nil)))


;;;; Commands to add tags/refs/alias

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

;; TODO: Try to include all Firefox bookmarks and so on
(defun org-node--list-known-raw-links ()
  "Return list of all links seen in all known files."
  (let (result)
    (maphash
     (lambda (target links)
       (let ((types (mapcar #'org-mem-link-type links)))
         (when (memq nil types)
           ;; Type nil is a @citation
           (push target result)
           (setq types (delq nil types)))
         (dolist (type (delete-dups (delete "id" types)))
           (push (concat type ":" target) result))))
     org-mem--target<>links)
    result))

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
             (new-tags (seq-uniq (append tags filetags)))
             (case-fold-search t))
        (save-excursion
          (without-restriction
            (goto-char 1)
            (if (search-forward "\n#+filetags:" nil t)
                (progn
                  (skip-chars-forward " ")
                  (atomic-change-group
                    (delete-region (point) (pos-eol))
                    (insert ":" (string-join new-tags ":") ":")))
              (org-node--end-of-meta-data)
              (insert "#+filetags: :" (string-join new-tags ":") ":\n")))))
    (save-excursion
      (org-back-to-heading)
      (org-set-tags (seq-uniq (append tags (org-get-tags)))))))

(defun org-node--read-tags ()
  "Prompt for an Org tag or several.
Pre-fill completions by collecting tags from all known ID-nodes, as well
as the members of `org-tag-persistent-alist' and `org-tag-alist'.

Also collect current buffer tags, but only if `org-element-use-cache' is
non-nil, because it may cause noticeable lag otherwise."
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
           (cl-loop for node being each hash-value of org-nodes
                    append (org-mem-entry-tags node))))
   nil nil nil 'org-tags-history))

(defun org-node--end-of-meta-data (&optional full)
  "Like `org-end-of-meta-data', but supports file-level metadata.

As in `org-end-of-meta-data', point always lands on a newline \(or the
end of buffer).  Since that newline may be the beginning of the next
heading, you should probably verify that `org-at-heading-p' is nil and
`point' has changed, else do `backward-char' or `open-line' prior to
inserting any text.

Argument FULL same as in `org-end-of-meta-data' when point is in a
subtree, but meaningless when point is before the first heading, in which
case always skip past all file-level properties and keywords."
  (if (org-before-first-heading-p)
      (progn
        (goto-char (point-min))
        ;; Jump past comment lines and blank lines (NOT keywords).
        (while (looking-at-p (rx (*? space) (or "# " "\n")))
          (forward-line))
        (let ((case-fold-search t))
          ;; Jump past top-level PROPERTIES drawer.
          (when (looking-at-p "[ \t]*?:PROPERTIES: *?$")
            (forward-line)
            (while (looking-at-p "[ \t]*?:")
              (forward-line))))
        ;; Jump past #+keywords, comment lines and blank lines.
        (while (looking-at-p (rx (*? (any "\s\t")) (or "#+" "# " "\n")))
          (forward-line))
        ;; Don't go past a "final stretch" of blanklines.
        (unless (zerop (skip-chars-backward "\n"))
          (forward-char 1)))
    ;; PERF: Override a bottleneck in `org-end-of-meta-data'.
    (cl-letf (((symbol-function 'org-back-to-heading)
               #'org-node--back-to-heading-or-point-min))
      (org-end-of-meta-data full)))
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
               org-mem--title<>id
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


;;;; Misc

(defun org-node-peek (&optional ht)
  "Print some random `org-mem-entry' objects.
Tip: way better is a package called \"inspector\"!"
  (interactive)
  (let ((rows (hash-table-values (or ht org-nodes)))
        (print-length nil))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length rows)) rows)))))

(defun org-node-try-visit-ref-node ()
  "Designed for `org-open-at-point-functions'.

For the link at point, if there exists an org-ID node that has
the same link in its ROAM_REFS property, visit that node rather than
following the link normally.

If already visiting that node, then follow the link normally."
  (when-let* ((url (thing-at-point 'url)))
    ;; Rarely more than one car
    (let* ((target (car (org-mem--split-roam-refs-field url)))
           (found (cl-loop for node being each hash-value of org-nodes
                           when (member target (org-mem-entry-roam-refs node))
                           return node)))
      (if (and found
               ;; Check that point is not already in said ref node (if so,
               ;; better to fallback to default `org-open-at-point' logic)
               (not (and (derived-mode-p 'org-mode)
                         (equal (org-entry-get-with-inheritance "ID")
                                (org-mem-entry-id found)))))
          (progn (org-node--goto found)
                 t)
        nil))))


;;;; API used by some submodule(s)

(defun org-node--delete-drawer (name)
  "In current entry, seek and destroy drawer named NAME."
  (save-restriction
    (when (org-node-narrow-to-drawer-p name)
      (delete-region (point-min) (point-max))
      (widen)
      (delete-blank-lines)
      (forward-line -1)
      (delete-line)
      (delete-line))))

;; Home-made subroutine to find/create a drawer.  There's a trick used by
;; org-super-links to let upstream code do the work: temporarily set
;; `org-log-into-drawer' to "BACKLINKS", and then call `org-log-beginning'.
;; That only works under an Org entry though, not before the first heading.
;; Plus, our version is flexible.

(defun org-node-narrow-to-drawer-create (name &optional create-where)
  "Narrow to pre-existing drawer named NAME, creating it if necessary.

Search current entry only, via subroutine `org-node-narrow-to-drawer-p'.

When drawer is created, insert it near the beginning of the entry
\(after any properties and logbook drawers\), unless CREATE-WHERE is a
function, in which case call it to reposition point prior to creating
the drawer.

If CREATE-WHERE returns an integer or marker, go to that position."
  (org-node-narrow-to-drawer-p name t create-where))

(defun org-node-narrow-to-drawer-p
    (name &optional create-missing create-where)
  "Seek a drawer named NAME in the current entry, then narrow to it.
This also works at the file top level, before the first entry.

If a drawer was found, return t.
Otherwise do nothing, do not narrow, and return nil.

Narrow to the region between :NAME: and :END:, exclusive.
Place point at the beginning of that region, after any indentation.
If the drawer was empty, ensure there is one blank line.

A way you might invoke this function:

    \(save-restriction
      \(if \(org-node-narrow-to-drawer-p \"MY_DRAWER\")
          ...edit the narrowed buffer...
        \(message \"No such drawer in this entry\")))

With CREATE-MISSING t, create a new drawer if one was not found.
However, instead of passing that argument, it is clearer
to call `org-node-narrow-to-drawer-create' instead.
Also see that function for meaning of CREATE-WHERE."
  (let ((start (point))
        (entry-end (org-entry-end-position))
        (case-fold-search t))
    (org-node--end-of-meta-data)
    (cond
     ;; Empty entry? (next line after heading was another heading)
     ((org-at-heading-p)
      (if create-missing
          (progn
            (org-insert-drawer nil name)
            (narrow-to-region (point) (point))
            t)
        (goto-char start)
        nil))
     ;; Pre-existing drawer?
     ((org-node--re-search-forward-skip-some-regions
       (concat "^[ \t]*:" (regexp-quote name) ":[ \t]*$")
       entry-end)
      (if (get-text-property (point) 'org-transclusion-id)
          (error "Was about to operate on a drawer inside an org-transclusion, probably not intended")
        (let ((drawbeg (progn (forward-line 1) (point))))
          (re-search-forward "^[ \t]*:END:[ \t]*$" entry-end)
          (forward-line 0)
          (if (= drawbeg (point))
              ;; Empty drawer with no newlines in-between; add one newline
              ;; so the result looks similar to after `org-insert-drawer'.
              (open-line 1)
            ;; Not an empty drawer, so it is safe to back up from the :END: line
            (backward-char))
          (let ((drawend (point)))
            (goto-char drawbeg)
            (back-to-indentation)
            (narrow-to-region (pos-bol) drawend))
          t)))
     ;; No drawer; create new.
     (create-missing
      (when create-where
        (let ((where (funcall create-where)))
          (when (number-or-marker-p where)
            (goto-char where))))
      (org-insert-drawer nil name)
      (narrow-to-region (point) (point))
      t)
     (t (goto-char start)
        nil))))

;; ... I don't know the org-element API at all.
(defun org-node--re-search-forward-skip-some-regions (regexp &optional bound)
  "Like `re-search-forward', but do not search inside certain regions.
These regions are delimited by lines that start with \"#+BEGIN\" or
\"#+END\".  Upon finding such regions, jump to the end of that region,
then continue the search.

Argument BOUND as in `re-search-forward'.
Always behave as if passing NOERROR t to `re-search-forward'.

Each invocation has overhead, so to search for the same REGEXP
repeatedly, it performs better not to iterate as in
\"(while (re-search-forward REGEXP nil t) ...)\",
but to use `org-node--map-matches-skip-some-regions' instead."
  (let ((starting-pos (point))
        (skips (org-node--find-regions-to-skip bound)))
    (catch 'found
      (cl-loop for (beg . end) in skips
               if (re-search-forward regexp beg t)
               do (throw 'found (point))
               else do (goto-char end))
      (if (re-search-forward regexp bound t)
          (throw 'found (point))
        (goto-char starting-pos)
        nil))))

;; unused for now; likely something like this will go in org-mem-parser.el
(defun org-node--map-matches-skip-some-regions (regexp fn &optional bound)
  "Go to each match for REGEXP and call FN.
BOUND as in `re-search-forward'."
  (let ((last-search-hit (point))
        (skips (org-node--find-regions-to-skip bound)))
    (cl-loop for (beg . end) in skips do
             (while (re-search-forward regexp beg t)
               (setq last-search-hit (point))
               (funcall fn))
             (goto-char end))
    (while (re-search-forward regexp bound t)
      (setq last-search-hit (point))
      (funcall fn))
    (goto-char last-search-hit)))

(defun org-node--find-regions-to-skip (&optional bound)
  "Subroutine for `org-node--re-search-forward-skip-some-regions'.
BOUND as in `re-search-forward'."
  (let ((starting-pos (point))
        (case-fold-search t)
        skips)
    (while (re-search-forward "^[ \t]*#\\+BEGIN" bound t)
      (forward-line 0)
      (let ((beg (point)))
        (unless (re-search-forward "^[ \t]*#\\+END" bound t)
          (error "org-node: Could not find #+end%s"
                 (if bound (format " before search boundary %d" bound) "")))
        (forward-line 1)
        (push (cons beg (point)) skips)))
    (goto-char starting-pos)
    (nreverse skips)))

(defvar org-node--work-buffers nil
  "All buffers spawned by `org-node--work-buffer-for'.")

(defun org-node--kill-work-buffers ()
  "Kill all buffers in `org-node--work-buffers'."
  (while-let ((buf (pop org-node--work-buffers)))
    (kill-buffer buf)))

;; TODO: Either duplicate the org element cache of FILE,
;;       or just actually visit FILE.
;;       It would permit us to use the org element API without incurring a
;;       massive performance hit,
;;       since the parse tree would typically already be cached (one hopes).
;;       (If we go the route of actually visiting the file, can this be merged
;;       with `org-node--with-quick-file-buffer'?)
(defun org-node--work-buffer-for (file)
  "Get or create a hidden buffer, and read FILE contents into it.
Also enable `org-mode', but ignore `org-mode-hook' and startup options.

The buffer persists, so calling again with the same FILE skips the
overhead of creation.  To clean up, call
`org-node--kill-work-buffers' explicitly."
  (let ((bufname (format " *org-node-context-%d*" (sxhash file)))
        (org-inhibit-startup t)
        (org-element-cache-persistent nil))
    (or (get-buffer bufname)
        (with-current-buffer (get-buffer-create bufname t)
          (push (current-buffer) org-node--work-buffers)
          (delay-mode-hooks (org-mode))
          (setq-local org-element-cache-persistent nil)
          (insert-file-contents file)
          ;; May be useful for `org-node-roam-accelerator-mode'.
          ;; No idea why `org-roam-with-temp-buffer' sets default directory,
          ;; but Chesterton's Fence.
          (setq-local default-directory (file-name-directory file))
          ;; Ensure that attempted edits trip an error, since the buffer may be
          ;; reused any number of times, and should always reflect FILE.
          (setq-local buffer-read-only t)
          (current-buffer)))))

(defun org-node--general-org-work-buffer ()
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
          (setq-local org-element-cache-persistent nil)
          (current-buffer)))))


;;; Org-roam accelerator
;; Just because we can.

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

(declare-function org-roam-node-id "ext:org-roam-node")
(defvar org-roam-preview-function)
(defvar org-roam-preview-postprocess-functions)
(defvar org-node--ad-roam-id<>previews (make-hash-table :test #'equal))
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
        (alist-get pos-diff (gethash src-id org-node--ad-roam-id<>previews))
      (let (snippet)
        (with-current-buffer (org-node--work-buffer-for file)
          (goto-char link-pos)
          (cl-letf (((symbol-function 'org-back-to-heading-or-point-min)
                     #'org-node--back-to-heading-or-point-min))
            (setq snippet (funcall org-roam-preview-function))
            (dolist (fn org-roam-preview-postprocess-functions)
              (setq snippet (funcall fn snippet)))))
        (with-current-buffer (org-node--general-org-work-buffer)
          (erase-buffer)
          (insert snippet)
          (font-lock-ensure)
          (buffer-string))))))

;;;###autoload
(define-minor-mode org-node-roam-accelerator-mode
  "Advise the \"*org-roam*\" buffer to be faster.
As a side effect, it can be used without the rest of org-roam."
  :global t
  (require 'org-roam)
  (require 'org-mem-roamy)
  (if org-node-roam-accelerator-mode
      (progn
        (advice-add 'org-roam-node-insert-section :around    #'org-node--ad-roam-node-insert-section)
        (advice-add 'org-roam-preview-get-contents :override #'org-node--ad-roam-preview-get-contents)
        (advice-add 'org-roam-buffer-render-contents :after  #'org-node--kill-work-buffers)
        (advice-add 'org-roam-backlinks-get :override        #'org-mem-roamy-mk-backlinks)
        (advice-add 'org-roam-reflinks-get  :override        #'org-mem-roamy-mk-reflinks))
    (advice-remove 'org-roam-node-insert-section    #'org-node--ad-roam-node-insert-section)
    (advice-remove 'org-roam-node-insert-section    #'org-node--ad-roam-node-insert-section)
    (advice-remove 'org-roam-preview-get-contents   #'org-node--ad-roam-preview-get-contents)
    (advice-remove 'org-roam-buffer-render-contents #'org-node--kill-work-buffers)
    (advice-remove 'org-roam-backlinks-get          #'org-mem-roamy-mk-backlinks)
    (advice-remove 'org-roam-reflinks-get           #'org-mem-roamy-mk-reflinks)))


;;;; API not used within this package

(let (tipped)
  (defun org-node-id-at-point ()
    "Get the ID property in entry at point or some ancestor."
    (declare (obsolete org-entry-get-with-inheritance "2024-11-18"))
    (unless tipped
      (setq tipped t)
      (message "Use `org-entry-get-with-inheritance' rather than `org-node-id-at-point'"))
    (org-entry-get-with-inheritance "ID")))

(defun org-node-at-point ()
  "Return the ID-node near point.

This may refer to the current Org heading, else an ancestor
heading, else the file-level node, whichever has an ID first."
  (gethash (org-entry-get-with-inheritance "ID") org-nodes))

;; TODO: Add an extended variant that checks active region, like
;;       `org-node-insert-link' does.
(defun org-node-read (&optional prompt)
  "PROMPT for a known node and return it."
  (gethash (completing-read (or prompt "Node: ") #'org-node-collection
                            () () () 'org-node-hist)
           org-node--candidate<>entry))

(provide 'org-node)

;;; org-node.el ends here
