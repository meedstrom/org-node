;;; org-node.el --- Link org-id entries into a network -*- lexical-binding: t; -*-

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

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "28.1") (compat "30") (dash "2.19.1") (transient "0.7.4") (persist "0.6.1"))
;; URL:              https://github.com/meedstrom/org-node

;;; Commentary:

;; If you were the sort of person to prefer "id:" links over "file:" links or
;; any other type of link, you're in the right place!  Now you can rely on IDs
;; and worry less about mentally tracking your subtree hierarchies and
;; directory structures.  As long as you've assigned an ID to something, you
;; can find it later.

;; The philosophy is the same as org-roam: if you assign an ID every time you
;; make an entry that you know you might want to link to from elsewhere, then
;; it tends to work out that the `org-node-find' command can jump to more or
;; less every entry you'd ever want to jump to.  Pretty soon you've forgot that
;; your files have names.

;; Anyway, that's just the core of it as described to someone not familiar with
;; zettelkasten.  In fact, out of the simplicity arises something powerful,
;; more to be experienced than explained.

;; Compared to other systems:

;; - org-roam: Same idea, compatible disk format(!), but org-node is faster,
;;   does not depend on SQLite, lets you opt out of file-level property
;;   drawers, does not support "roam:" links, and tries to rely in a bare-metal
;;   way on upstream org-id and org-capture.  As a drawback, if a heading in
;;   some Git README has an ID, it's considered part of your collection --
;;   simply because if it's known to org-id, it's known to org-node.  These
;;   headings can be filtered after-the-fact.

;; - denote: Org-node is Org only, no Markdown, no support for "denote:" links.
;;   Filenames have no meaning (so could match the Denote format if you like),
;;   and you can have as many "notes" as you want inside one file.  You could
;;   possibly use Denote to search just files and org-node as a more granular
;;   search.

;;; Code:

(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)
(require 'transient)
(require 'find-func)
(require 'org)
(require 'org-id)
(require 'org-macs)
(require 'org-element)

(require 'dash)
(require 'compat)
(require 'persist)
(require 'org-node-parser)
(require 'org-node-changes)

;; Satisfy compiler
(defvar org-roam-directory)
(defvar org-roam-dailies-directory)
(defvar consult-ripgrep-args)
(defvar $i)
(defvar org-node-backlink-mode)
(declare-function org-node-backlink--fix-entry-here "org-node-backlink")
(declare-function profiler-report "profiler")
(declare-function profiler-stop "profiler")
(declare-function org-lint "org-lint")


;;;; Options

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

;; UNUSED: A custom-setter that causes all kinds of compiler warnings
(defun org-node--set-and-remind-reset (sym val)
  (when org-node-cache-mode
    (run-with-timer
     .1 nil #'message "Remember to run `org-node-reset' after configuring %S" sym))
  (custom-set-default sym val))

(defcustom org-node-rescan-functions nil
  "Hook run after scanning specific files.
Not run after a full cache reset, only after e.g. a file is
saved or renamed causing an incremental update to the cache.

Called with one argument: the list of files re-scanned.  It may
include deleted files."
  :type 'hook)

(defcustom org-node-prefer-with-heading nil
  "Make a heading even when creating isolated file nodes.
If nil, write a #+TITLE and a file-level property-drawer instead.
In other words:

- if nil, make file with no heading (outline level 0)
- if t, make file with heading (outline level 1)

This affects the behavior of `org-node-new-file',
`org-node-extract-subtree', and `org-node-capture-target'.

If you change your mind about this setting, the Org-roam commands
`org-roam-promote-entire-buffer' and
`org-roam-demote-entire-buffer' can help you transition the
files you already have."
  :type 'boolean)

(defcustom org-node-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that org-node can scan for ID nodes inside .org.gpg files.  Also,
`org-node-perf-keep-file-name-handlers' should include the EPG
handler.

I do not use EPG, so that is probably not enough to make it work.
Report an issue on https://github.com/meedstrom/org-node/issues
or drop me a line on Mastodon: @meedstrom@emacs.ch"
  :type 'alist)

;; TODO: Maybe suggest `utf-8-auto-unix', but is it a sane system for write?
(defcustom org-node-perf-assume-coding-system nil
  "Coding system to assume while scanning ID nodes.

Picking a specific coding system can speed up `org-node-reset'.
Set nil to let Emacs figure it out anew on every file.

On MS Windows this probably should be nil.  Same if you access
your files from multiple platforms.

Modern GNU/Linux, BSD and MacOS systems almost always encode new
files as `utf-8-unix'.

Note that if your Org collection is old and has survived several
system migrations, or some of it was generated via Pandoc
conversion or downloaded, it\\='s very possible that there\\='s a mix
of coding systems among them.  In that case, setting this
variable may cause org-node to fail to scan some of them, or
display strange-looking data."
  :type '(choice coding-system (const nil)))

(defcustom org-node-perf-keep-file-name-handlers nil
  "Which file handlers to respect while scanning for ID nodes.

Normally, `file-name-handler-alist' reacts specially to seeing
some file names: TRAMP paths, compressed files or .org.gpg files.

It\\='s infamous for (somewhat) slowing down the access of very
many files, since it is a series of regexps applied to every file
name visited.  The smaller this list, the faster
`org-node-reset'."
  :type '(choice (const :tag "Keep all" t)
                 (set
                  (function-item jka-compr-handler)
                  (function-item epa-file-handler)
                  (function-item tramp-file-name-handler)
                  (function-item tramp-completion-file-name-handler)
                  (function-item tramp-archive-file-name-handler)
                  (function-item file-name-non-special))))

(defcustom org-node-filter-fn
  (lambda (node)
    (not (assoc "ROAM_EXCLUDE" (org-node-get-properties node))))
  "Predicate returning t to include a node, or nil to exclude it.

The filtering only has an impact on the table
`org-node--candidate<>node', which forms the basis for
completions in the minibuffer, and `org-node--title<>id', used
by the in-buffer `org-node-complete-at-point'.  In other words,
passing nil means the user cannot autocomplete to the node, but
Lisp code can still find it in the \"main\" table,
`org-node--id<>node'.

This function is applied once for every org-id node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-peek] and specified
in the type `org-node' (C-h o org-node RET).

See the following example for a way to filter out nodes with a
ROAM_EXCLUDE property, or that have any kind of TODO state, or
are tagged :drill:, or where the full file path contains a
directory named \"archive\".

\(setq org-node-filter-fn
      (lambda (node)
        (not (or (assoc \"ROAM_EXCLUDE\" (org-node-get-properties node))
                 (org-node-get-todo node)
                 (string-search \"/archive/\" (org-node-get-file-path node))
                 (member \"drill\" (org-node-get-tags node))))))"
  :type 'function)

(defcustom org-node-insert-link-hook '()
  "Hook run after inserting a link to an Org-ID node.

Called with point in the new link."
  :type 'hook)

(defcustom org-node-creation-hook '(org-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied only by `org-node-new-file', `org-node-capture-target',
`org-node-insert-heading', `org-node-nodeify-entry' and
`org-node-extract-subtree'.

NOT applied by `org-node-new-via-roam-capture' -- see org-roam\\='s
`org-roam-capture-new-node-hook' instead.

A good function for this hook is `org-node-put-created', since
the default `org-node-datestamp-format' is empty.  In the
author\\='s experience, recording the creation-date somewhere may
prove useful later on, e.g. when publishing to a blog."
  :type 'hook)

(defcustom org-node-extra-id-dirs nil
  "Directories in which to search Org files for IDs.

Unlike variable `org-id-extra-files', accept directories.  Unlike
variable `org-agenda-files', directories are checked again over
time in order to find new files that have appeared.

These directories are only checked as long as
`org-node-cache-mode' is active.  They are checked
recursively (looking in subdirectories, sub-subdirectories etc).

EXCEPTION: Subdirectories that start with a dot, such as
\".emacs.d/\", are not checked.  To check these, add them
explicitly.

To avoid accidentally picking up duplicate files such as
versioned backups, causing org-id complaints about duplicate
IDs, configure `org-node-extra-id-dirs-exclude'.

Tip: If it happened anyway, try \\[org-node-forget-dir], because
merely removing a directory from this list does not forget the
IDs already found."
  :type '(repeat directory))

;; TODO: Figure out how to permit .org.gpg and fail gracefully if
;;       the EPG settings are insufficient. easier to test with .org.gz first
(defcustom org-node-extra-id-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-")
  "Path substrings of files that should not be searched for IDs.

This option only influences how the function `org-node-list-files'
should seek files found in `org-node-extra-id-dirs'.  It is meant
as a way to avoid collecting IDs inside versioned backup files
causing org-id complaints about duplicate IDs.

For all other \"excludey\" purposes, you probably mean to
configure `org-node-filter-fn' instead.

If you have accidentally added a directory of backup files, try
\\[org-node-forget-dir].

It is not necessary to exclude backups or autosaves that end in ~
or # or .bak since `org-node-list-files' only considers files
that end in precisely \".org\" anyway.

You can eke out a performance boost by excluding directories with
a humongous amount of files, such as the infamous
\"node_modules\", even if they contain no Org files.  However,
directories that start with a period are always ignored, so no
need to specify e.g. \"~/.local/\" or \"/.git/\" for that reason."
  :type '(repeat string))


;;;; Pretty completion

(defcustom org-node-alter-candidates nil
  "Whether to alter completion candidates instead of affixating.

This means that org-node will concatenate the result of
`org-node-affixation-fn' into a single string, so what the
user types in the minibuffer can match against the prefix and
suffix as well as against the node title.

\(Tip: users of the orderless library on versions from 2024-07-11
can match the prefix and suffix via `orderless-annotation',
without need for this setting.)

Another consequence is it lifts the uniqueness constraint on note
titles: you\\='ll be able to have two headings with the same name so
long as their prefix or suffix differ.

After changing this setting, please run \\[org-node-reset]."
  :type 'boolean)

(defcustom org-node-affixation-fn #'org-node-affix-with-olp
  "Function to give prefix and suffix to completion candidates.

The results will style the appearance of completions during
\\[org-node-find] et al.

To read more about affixations, see docstring
`completion-extra-properties', however this function operates on
one candidate at a time, not the whole collection.

It receives two arguments: NODE and TITLE, and it must return a
list of three strings: title, prefix and suffix.  Actually,
prefix and suffix can be nil.  Title should be TITLE unmodified.

NODE is an object which form you can observe in examples from
\\[org-node-peek] and specified in type `org-node'
\(type \\[describe-symbol] org-node RET).

If a node has aliases, it is passed to this function again for
every alias, in which case TITLE is actually one of the aliases."
  :type '(radio
          (function-item org-node-affix-bare)
          (function-item org-node-affix-with-olp)
          function))

(defun org-node-affix-bare (_node title)
  "Use TITLE as-is.
For use as `org-node-affixation-fn'."
  (list title nil nil))

(defun org-node-affix-with-olp (node title)
  "Prepend TITLE with NODE's outline path.
For use as `org-node-affixation-fn'."
  (list title
        (if (org-node-get-is-subtree node)
            (let ((ancestors (cons (org-node-get-file-title-or-basename node)
                                   (org-node-get-olp node)))
                  (result nil))
              (dolist (anc ancestors)
                (push (propertize anc 'face 'completions-annotations) result)
                (push " > " result))
              (apply #'concat (nreverse result)))
          nil)
        nil))

(defvar org-node--title<>affixation-triplet (make-hash-table :test #'equal)
  "1:1 table mapping titles or aliases to affixation triplets.")

(defun org-node--affixate-collection (coll)
  "Take COLL and return precomputed affixations for each member."
  (cl-loop for title in coll
           collect (gethash title org-node--title<>affixation-triplet)))

;; TODO: Assign a category 'org-node, then add an embark action to embark
;; TODO: Bind a custom exporter to `embark-export'
(defun org-node-collection (str pred action)
  "Custom COLLECTION for `completing-read'.

Ahead of time, org-node takes titles/aliases from
`org-node--title<>id', runs `org-node-affixation-fn' on each, and
depending on the user option `org-node-alter-candidates' it
either saves the affixed thing directly into
`org-node--candidate<>node' or into a secondary table
`org-node--title<>affixation-triplet'.  Finally, this function
then either simply reads candidates off the candidates table or
attaches the affixations in realtime.

Regardless of which, all completions are guaranteed to be keys of
`org-node--candidate<>node', but remember that it is possible for
`completing-read' to exit with user-entered input that didn\\='t
match anything.

Arguments STR, PRED and ACTION are handled behind the scenes,
read more at Info node `(elisp)Programmed Completion'."
  (if (eq action 'metadata)
      (cons 'metadata (unless org-node-alter-candidates
                        (list (cons 'affixation-function
                                    #'org-node--affixate-collection))))
    (complete-with-action action org-node--candidate<>node str pred)))

(defvar org-node-hist nil
  "Minibuffer history.")


;;;; The metadata struct

(cl-defstruct (org-node (:constructor org-node--make-obj)
                        (:copier nil)
                        (:conc-name org-node-get-))
  "An org-node object holds information about an Org ID node.
By the term \"Org ID node\", we mean either a subtree with
an ID property, or a file with a file-level ID property.  The
information is stored in slots listed below.

For each slot, there exists a getter function
\"org-node-get-FIELD\".

For example, the field \"deadline\" has a getter
`org-node-get-deadline'.  So you would type
\"(org-node-get-deadline NODE)\", where NODE is one of the
elements of the `hash-table-values' of `org-node--id<>node'.

For real-world usage of these getters, see examples in the
documentation of `org-node-filter-fn' or the package README.

You may be able to find the README by typing:

    M-x find-library RET org-node RET

or you can visit the homepage:

    https://github.com/meedstrom/org-node"
  (aliases    nil :read-only t :type list :documentation
              "Returns list of ROAM_ALIASES registered on the node.")
  (deadline   nil :read-only t :type string :documentation
              "Returns node's DEADLINE state.")
  (file-path  nil :read-only t :type string :documentation
              "Returns node's full file path.")
  (file-title nil :read-only t :type string :documentation
              "Returns the #+title of the file where this node is. May be nil.")
  (file-title-or-basename nil :read-only t :type string  :documentation
                          "Returns the title of the file where this node is, or its filename if untitled.")
  (id         nil :read-only t :type string :documentation
              "Returns node's ID property.")
  (is-subtree nil :read-only t :type boolean :documentation
              "Returns t if node is a subtree, nil if it is file-level.")
  (level      nil :read-only t :type integer :documentation
              "Returns number of stars in the node heading. File-level node always 0.")
  (olp        nil :read-only t :type list :documentation
              "Returns list of ancestor headings to this node.")
  (pos        nil :read-only t :type integer :documentation
              "Returns char position of the node. File-level node always 1.")
  (priority   nil :read-only t :type string :documentation
              "Returns priority such as [#A], as a string.")
  (properties nil :read-only t :type alist :documentation
              "Returns alist of properties from the :PROPERTIES: drawer.")
  (refs       nil :read-only t :type list :documentation
              "Returns list of ROAM_REFS registered on the node.")
  (scheduled  nil :read-only t :type string :documentation
              "Returns node's SCHEDULED state.")
  (tags       nil :read-only t :type list :documentation
              "Returns list of tags local to the node.")
  (title      nil :read-only t :type string :documentation
              "Returns the node's heading, or #+title if it is not a subtree.")
  (todo       nil :read-only t :type string :documentation
              "Returns node's TODO state."))

;; It's safe to alias an accessor, because they are all read only
(defalias 'org-node-get-props #'org-node-get-properties)

(cl-defstruct (org-node-link (:constructor org-node-link--make-obj)
                             (:copier nil))
  "Please see docstring of `org-node-get-id-links'."
  origin
  pos
  type
  dest)


;;;; Tables

(defvaralias 'org-nodes 'org-node--id<>node)

(defvar org-node--id<>node (make-hash-table :test #'equal)
  "1:1 table mapping IDs to nodes.
To peek on the contents, try \\[org-node-peek] a few times, which
can demonstrate the data format.  See also the type `org-node'.")

(defvar org-node--candidate<>node (make-hash-table :test #'equal)
  "1:1 table mapping completion candidates to nodes.")

(defvar org-node--title<>id (make-hash-table :test #'equal)
  "1:1 table mapping raw titles (and ROAM_ALIASES) to IDs.")

(defvar org-node--ref<>id (make-hash-table :test #'equal)
  "1:1 table mapping ROAM_REFS members to the ID property near.")

(defvar org-node--uri-path<>uri-type (make-hash-table :test #'equal)
  "1:1 table mapping //paths to types:.")

(defvar org-node--dest<>links (make-hash-table :test #'equal)
  "1:N table of links.

The table keys are destinations (uuids, uri paths or citekeys),
and the corresponding table value is a list of `org-node-link'
records describing each link to that destination, with info such
as from which ID-node the link originates.  See
`org-node-get-id-links' for more info.")

(persist-defvar org-node--file<>previews (make-hash-table :test #'equal)
  "1:N table mapping files to previews of backlink contexts.
For use by `org-node-fakeroam--accelerate-get-contents'.")

(persist-defvar org-node--file<>mtime (make-hash-table :test #'equal)
  "1:1 table mapping files to their last-modification times.")

(when (memq system-type '(windows-nt ms-dos))
  (persist-unpersist 'org-node--file<>mtime)
  (persist-unpersist 'org-node--file<>previews))

(defun org-node-get-id-links (node)
  "Get list of ID-link objects pointing to NODE.
Each object is of type `org-node-link' with these fields:

origin - ID of origin node (where the link was found)
pos - buffer position where the link was found
dest - ID of destination node, or a ref that belongs to it
type - link type, such as \"https\", \"ftp\", \"info\" or
       \"man\".  For ID-links this is always \"id\".  For a
       citation this is always nil.

This function only returns ID-links, so you can expect the :dest
to always equal the ID of NODE.  To see other link types, use
`org-node-get-reflinks'."
  (gethash (org-node-get-id node) org-node--dest<>links))

(defun org-node-get-reflinks (node)
  "Get list of reflink objects pointing to NODE.
Typical reflinks are URLs or @citekeys occurring in any document,
and they are considered to point to NODE when NODE has a
:ROAM_REFS: property that includes that same string.

The reflink object has the same shape as an ID-link object (see
`org-node-get-id-links'), but instead of an ID in the DEST field,
you have a ref string such an URL.  Common gotcha: for a web
address such as \"http://gnu.org\", the DEST field holds only
\"//gnu.org\", and the \"http\" part goes into the TYPE
field.  Colon is not stored anywhere.

Citations such as \"@gelman2001\" have TYPE nil, so you can
distinguish citations from other links this way."
  (cl-loop for ref in (org-node-get-refs node)
           append (gethash ref org-node--dest<>links)))

(defun org-node-peek (&optional ht)
  "Print some random rows of table `org-nodes'.
For reference, see type `org-node'.
When called from Lisp, peek on any hash table HT."
  (interactive)
  (let ((rows (hash-table-values (or ht org-nodes)))
        (print-length nil))
    (dotimes (_ 3)
      (print '----------------------------)
      (cl-prin1 (nth (random (length rows)) rows)))))


;;;; The mode

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Instruct various hooks to keep the cache updated.

-----"
  :global t
  (remove-hook 'org-mode-hook #'org-node-cache-mode)
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node--handle-save)
        (add-hook 'org-node-creation-hook #'org-node--dirty-ensure-node-known -50)
        (add-hook 'org-node-creation-hook #'org-node--add-series-item 90)
        (add-hook 'org-node-insert-link-hook #'org-node--dirty-ensure-link-known -50)
        (add-hook 'org-roam-post-node-insert-hook #'org-node--dirty-ensure-link-known -50)
        (add-hook 'calendar-today-invisible-hook #'org-node--mark-days 5)
        (add-hook 'calendar-today-visible-hook #'org-node--mark-days 5)
        (add-hook 'window-buffer-change-functions #'org-node--kill-blank-unsaved-buffers)
        (advice-add 'org-insert-link :after #'org-node--dirty-ensure-link-known)
        (advice-add 'rename-file :after #'org-node--handle-rename)
        (advice-add 'delete-file :after #'org-node--handle-delete)
        (org-node-cache-ensure 'must-async t)
        (org-node--maybe-adjust-idle-timer))
    (cancel-timer org-node--idle-timer)
    (remove-hook 'after-save-hook #'org-node--handle-save)
    (remove-hook 'org-node-creation-hook #'org-node--dirty-ensure-node-known)
    (remove-hook 'org-node-creation-hook #'org-node--add-series-item)
    (remove-hook 'org-node-insert-link-hook #'org-node--dirty-ensure-link-known)
    (remove-hook 'org-roam-post-node-insert-hook #'org-node--dirty-ensure-link-known)
    (remove-hook 'calendar-today-invisible-hook #'org-node--mark-days)
    (remove-hook 'calendar-today-visible-hook #'org-node--mark-days)
    (remove-hook 'window-buffer-change-functions #'org-node--kill-blank-unsaved-buffers)
    (advice-remove 'org-insert-link #'org-node--dirty-ensure-link-known)
    (advice-remove 'rename-file #'org-node--handle-rename)
    (advice-remove 'delete-file #'org-node--handle-delete)))

(defun org-node--handle-rename (file newname &rest _)
  "Arrange to scan NEWNAME for nodes and links, and forget FILE."
  (when (equal (file-name-extension file) "org")
    (org-node--scan-targeted
     (seq-remove #'backup-file-name-p (list file newname)))))

(defun org-node--handle-delete (file &rest _)
  "Arrange to forget nodes and links in FILE."
  (when (and (equal (file-name-extension file) "org"))
    (org-node--scan-targeted file)))

(defun org-node--handle-save ()
  "Arrange to re-scan nodes and links in current buffer."
  (when (and (equal (file-name-extension buffer-file-name) "org")
             (not (backup-file-name-p buffer-file-name)))
    (org-node--scan-targeted buffer-file-name)))

;; FIXME: Sure, it detects them, but won't run `org-node-rescan-functions' on
;;        them
(defvar org-node--idle-timer (timer-create)
  "Timer for intermittently checking `org-node-extra-id-dirs'.
for new, changed or deleted files.

This redundant behavior helps detect changes made by something
other than the current instance of Emacs, such as an user typing
rm on the command line instead of using \\[delete-file].

This timer is set by `org-node--maybe-adjust-idle-timer'.
Override that function to configure timer behavior.")

(defun org-node--maybe-adjust-idle-timer ()
  "Adjust `org-node--idle-timer' based on duration of last scan.
If not running, start it."
  (let ((new-delay (* 25 (1+ org-node--time-elapsed))))
    (when (or (not (member org-node--idle-timer timer-idle-list))
              ;; Don't repeat potentially forever
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer org-node--idle-timer)
      (setq org-node--idle-timer
            (run-with-idle-timer new-delay t #'org-node--scan-all)))))

;; (defun org-node--catch-unknown-modifications ()
;;   (let ((new (-difference (org-node-list-files) (org-node-list-files t)))))
;;   (if (> 10 )
;;       (org-node--scan-all)
;;     (org-node--scan-targeted))
;;   )

(defvar org-node--not-yet-saved nil
  "List of buffers created to hold a new node.")

(defun org-node--kill-blank-unsaved-buffers (&rest _)
  "Kill buffers created by org-node that have become blank.

This exists to allow you to create a node, especially a journal
note for today, change your mind, do an undo to empty the buffer,
then browse to the previous day\\='s note.  When later you want
to create today\\='s note after all, the series :creator function
should be made to run again, but will only do so if the buffer
has been properly deleted since, thus this hook."
  (unless (minibufferp)
    (dolist (buf org-node--not-yet-saved)
      (if (or (not (buffer-live-p buf))
              (file-exists-p (buffer-file-name buf)))
          (setq org-node--not-yet-saved (delq buf org-node--not-yet-saved))
        (and (not (get-buffer-window buf t)) ;; buffer not visible
             (string-blank-p (with-current-buffer buf (buffer-string)))
             (kill-buffer buf))))))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that org-node is ready for use.
Specifically, do the following:

- Run `org-node--init-ids'.
- \(Re-)build the cache if it is empty, or if FORCE is t.

The primary use case is at the start of autoloaded commands.

Optional argument SYNCHRONOUS t means that if a cache build is
needed or already ongoing, block Emacs until it is done.

When SYNCHRONOUS is nil, return immediately and let the caching
proceed in the background.  As that may take a few seconds, that
would mean that the `org-node--id<>node' table could be still outdated
by the time you query it, but that is acceptable in many
situations such as in an user command since the table is mostly
correct - and fully correct by the time of the next invocation.

If the `org-node--id<>node' table is currently empty, behave as if
SYNCHRONOUS t, unless SYNCHRONOUS is the symbol `must-async'."
  (unless (eq synchronous 'must-async)
    ;; The warn-function becomes a no-op after the first run, so gotta
    ;; run it as late as possible in case of late variable settings.  By
    ;; running it here, we've waited until the user runs a command.
    (org-node-changes--warn-and-copy))
  (org-node--init-ids)
  (when (hash-table-empty-p org-nodes)
    (setq synchronous (if (eq synchronous 'must-async) nil t))
    (setq force t))
  ;; (when (not org-node-cache-mode)
  ;;   (setq force t))
  (when force
    ;; Launch the async processes
    (org-node--scan-all))
  (when (eq t synchronous)
    ;; Block until all processes finish
    (when (seq-some #'process-live-p org-node--processes)
      (if org-node-cache-mode
          (message "org-node first-time caching...")
        (message "org-node caching... (Hint: Avoid this hang by enabling org-node-cache-mode at some point before use)")))
    (mapc #'accept-process-output org-node--processes)
    ;; Just in case... see docstring of `org-node-create'.
    ;; Not super happy about this edge-case, it's a wart of the current design
    ;; of `org-node--try-launch-scan'.
    (while (member org-node--retry-timer timer-list)
      (cancel-timer org-node--retry-timer)
      (funcall (timer--function org-node--retry-timer))
      (mapc #'accept-process-output org-node--processes))))

;; FIXME:
;; A heisenbug lurks in org-id.  https://emacs.stackexchange.com/questions/81794/
;; When it appears, backtrace will show this, which makes no sense -- it's
;; CLEARLY called on a list:
;;     Debugger entered--Lisp error: (wrong-type-argument listp #<hash-table equal 3142/5277) 0x190d581ba129>
;;       org-id-alist-to-hash((("/home/kept/roam/semantic-tabs-in-2024.org" "f21c984c-13f3-428c-8223-0dc1a2a694df") ("/home/kept/roam/semicolons-make-javascript-h..." "b40a0757-bff4-4188-b212-e17e3fc54e13") ...))
;;       org-node--init-ids()
;;       ...
(defun org-node--init-ids ()
  "Ensure that org-id is ready for use.

In broad strokes:
- Run `org-id-locations-load' if needed.
- Ensure `org-id-locations' is a hash table and not an alist.
- Throw error if `org-id-locations' is still empty after this,
  unless `org-node-extra-id-dirs' has members.
- Wipe `org-id-locations' if it appears afflicted by a bug that
  makes the symbol value an indeterminate superposition of one of
  two possible values \(a hash table or an alist) depending on
  which code accesses it -- like Schrödinger\\='s cat -- and tell
  the user to rebuild the value, since even org-id\\='s internal
  functions are unable to fix it."
  (require 'org-id)
  (when (not org-id-track-globally)
    (user-error "Org-node requires `org-id-track-globally'"))
  (when org-id-locations-file-relative
    (user-error "Org-node requires nil `org-id-locations-file-relative'"))
  (when (null org-id-locations)
    (when (file-exists-p org-id-locations-file)
      (ignore-errors (org-id-locations-load))))
  (when (listp org-id-locations)
    (ignore-errors
      (setq org-id-locations (org-id-alist-to-hash org-id-locations))))
  (when (listp org-id-locations)
    (setq org-id-locations nil)
    (org-node--die
     "Found org-id heisenbug!  Wiped org-id-locations, repair with `org-node-reset' or `org-roam-update-org-id-locations'"))
  (when (hash-table-p org-id-locations)
    (when (hash-table-empty-p org-id-locations)
      (org-id-locations-load)
      (when (and (hash-table-empty-p org-id-locations)
                 (null org-node-extra-id-dirs))
        (org-node--die
         "org-id-locations empty, try `org-id-update-id-locations' or `org-roam-update-org-id-locations'")))))


;;;; Scanning

(defun org-node--scan-all ()
  "Arrange a full scan."
  (org-node--try-launch-scan t))

(defun org-node--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (org-node--try-launch-scan (ensure-list files))))

(defvar org-node--retry-timer (timer-create))
(defvar org-node--file-queue nil)
(defvar org-node--wait-start nil)
(defvar org-node--full-scan-requested nil)

;; I'd like to remove this code, but complexity arises elsewhere if I do.
;; This makes things easy to reason about.
(defun org-node--try-launch-scan (&optional files)
  "Launch processes to scan FILES, or wait if processes active.

This ensures that multiple calls occurring in a short time \(like
when multiple files are being renamed) will be handled eventually
and not dropped, making `org-node-rescan-functions' trustworthy.

If FILES is t, do a full reset of the cache."
  (if (eq t files)
      (setq org-node--full-scan-requested t)
    (setq org-node--file-queue
          (seq-union org-node--file-queue
                     (mapcar #'abbreviate-file-name files))))
  (let (must-retry)
    (if (seq-some #'process-live-p org-node--processes)
        (progn
          (unless org-node--wait-start
            (setq org-node--wait-start (current-time)))
          (if (> (float-time (time-since org-node--wait-start)) 30)
              ;; Timeout subprocess stuck in some infinite loop
              (progn
                (setq org-node--wait-start nil)
                (message
                 "org-node: processes worked longer than 30 sec, killing")
                (while-let ((old-process (pop org-node--processes)))
                  (delete-process old-process)))
            (setq must-retry t)))
      ;; All clear, scan now
      (setq org-node--wait-start nil)
      (if org-node--full-scan-requested
          (progn
            (setq org-node--full-scan-requested nil)
            (org-node--scan (org-node-list-files) #'org-node--finalize-full)
            (when org-node--file-queue
              (setq must-retry t)))
        ;; Targeted scan of specific files
        (if org-node--file-queue
            (org-node--scan org-node--file-queue #'org-node--finalize-modified)
          (message "`org-node-try-launch-scan' launched with no input"))
        (setq org-node--file-queue nil)))
    (when must-retry
      (cancel-timer org-node--retry-timer)
      (setq org-node--retry-timer
            (run-with-timer 1 nil #'org-node--try-launch-scan)))))

(defvar org-node--processes nil
  "List of subprocesses.")

(defvar org-node--done-ctr 0
  "Count of finished subprocesses.")

(defvar org-node--stderr-name " *org-node*"
  "Name of buffer for the subprocesses shared stderr.")

(defcustom org-node-perf-max-jobs 0
  "Number of subprocesses to run.
If left at 0, will be set at runtime to the result of
`org-node--count-logical-cores'.

Affects the speed of \\[org-node-reset], which mainly matters at
the first-time init, since it may block Emacs while executing
that same function."
  :type 'natnum)

(defun org-node--count-logical-cores ()
  "Return sum of available processor cores, minus 1."
  (max (1- (string-to-number
            (pcase system-type
              ((or 'gnu 'gnu/linux 'gnu/kfreebsd 'berkeley-unix)
               (if (executable-find "nproc")
                   (shell-command-to-string "nproc --all")
                 (shell-command-to-string "lscpu -p | egrep -v '^#' | wc -l")))
              ((or 'darwin)
               (shell-command-to-string "sysctl -n hw.logicalcpu_max"))
              ;; No idea if this works
              ((or 'cygwin 'windows-nt 'ms-dos)
               (or (ignore-errors
                     (with-temp-buffer
                       (call-process "echo" nil t nil "%NUMBER_OF_PROCESSORS%")
                       (buffer-string)))
                   "1")))))
       1))

(defun org-node--ensure-compiled-lib (lib-name)
  "Return path to freshly compiled version of LIB-NAME.
Recompile if needed, in case the user\\='s package manager
didn\\='t do so already, or local changes have been made.

LIB-NAME should be something that works with `find-library-name'."
  (let* ((file-name-handler-alist nil)
         (lib (find-library-name lib-name))
         (native-path (when (native-comp-available-p)
                        (comp-el-to-eln-filename lib)))
         (byte-compile-warnings '(not free-vars)))
    (mkdir (org-node-parser--tmpfile) t)
    (if (and native-path (file-newer-than-file-p native-path lib))
        ;; The .eln is fresh, use it
        native-path
      (when native-path (native-compile-async (list lib)))
      ;; Use an .elc this time, as it compiles much faster
      (let ((elc-path (org-node-parser--tmpfile
                       (concat (file-name-base lib-name) ".elc"))))
        (unless (file-newer-than-file-p elc-path lib)
          ;; Must compile. Ensure the .elc won't clutter some source directory.
          (let ((byte-compile-dest-file-function `(lambda (&rest _) ,elc-path)))
            (byte-compile-file lib)))
        elc-path))))

(defcustom org-node-link-types
  '("http" "https" "file" "id")
  "Link types that may count as backlinks.
Types other than \"id\" only result in a backlink when there is
some node with the same link in its ROAM_REFS property.

The fewer types, the faster \\[org-node-reset].
Tip: eval `(org-link-types)' to see all types.

There is no need to add the \"cite\" type."
  :type '(repeat string))

(defvar org-node--debug nil)
(defun org-node--scan (files finalizer)
  "Begin async scanning FILES for id-nodes and links.
Other functions have similar docstrings, but this function
actually launches the processes - the rubber hits the road.

When finished, pass a list of scan results to the FINALIZER
function to update current tables."
  (when (= 0 org-node-perf-max-jobs)
    (setq org-node-perf-max-jobs (org-node--count-logical-cores)))
  ;; FIXME: When working on a checked-out repo, the developer has to paste the
  ;;        checked-out library path here.  Without a full path, it just looks
  ;;        in load-path.
  (let ((compiled-lib (org-node--ensure-compiled-lib "org-node-parser"))
        (file-name-handler-alist nil)
        (coding-system-for-read org-node-perf-assume-coding-system)
        (coding-system-for-write org-node-perf-assume-coding-system))
    (setq org-node--time-at-scan-begin (current-time))
    (setq org-node--done-ctr 0)
    (when (-any-p #'process-live-p org-node--processes)
      ;; We should never end up here, but just in case
      (mapc #'delete-process org-node--processes)
      (message "org-node processes alive, bug report would be appreciated"))
    (setq org-node--processes nil)
    (with-current-buffer (get-buffer-create org-node--stderr-name)
      (erase-buffer))
    (with-temp-file (org-node-parser--tmpfile "work-variables.eld")
      (let ((standard-output (current-buffer))
            (print-length nil)
            (print-level nil)
            (reduced-plain-re
             ;; Copied from `org-link-make-regexps'
             (let* ((non-space-bracket "[^][ \t\n()<>]")
                    (parenthesis
		     `(seq (any "<([")
		           (0+ (or (regex ,non-space-bracket)
			           (seq (any "<([")
				        (0+ (regex ,non-space-bracket))
				        (any "])>"))))
		           (any "])>"))))
	       (rx-to-string
	        `(seq word-start
		      (regexp ,(regexp-opt
                                (append org-node-link-types
                                        org-node-parser--org-ref-types)
                                t))
		      ":"
                      (group
		       (1+ (or (regex ,non-space-bracket)
			       ,parenthesis))
		       (or (regexp "[^[:punct:][:space:]\n]")
                           ?- ?/ ,parenthesis)))))))
        (prin1
         ;; NOTE The $sigils in the names are to visually distinguish these
         ;;      "external" variables in the body of
         ;;      `org-node-parser--collect-dangerously'.
         (append
          org-node-inject-variables
          `(($plain-re . ,reduced-plain-re)
            ($merged-re . ,(concat org-link-bracket-re "\\|" reduced-plain-re))
            ($assume-coding-system . ,org-node-perf-assume-coding-system)
            ($file-todo-option-re
             . ,(rx bol (* space)
                    (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
            ($file-name-handler-alist
             . ,(--keep (rassoc it org-node-perf-keep-file-name-handlers)
                        file-name-handler-alist))
            ($global-todo-re
             . ,(if (stringp (car org-todo-keywords))
                    (org-node--die "Quit because `org-todo-keywords' is configured with obsolete syntax, please fix")
                  (org-node-parser--make-todo-regexp
                   (string-join (-mapcat #'cdr
                                         (default-value 'org-todo-keywords))
                                " "))))
            ($backlink-drawer-re
             . ,(concat "^[[:space:]]*:"
                        (or (and (require 'org-super-links nil t)
                                 (boundp 'org-super-links-backlink-into-drawer)
                                 (stringp org-super-links-backlink-into-drawer)
                                 org-super-links-backlink-into-drawer)
                            "backlinks")
                        ":")))))))

    (setq org-node--debug nil)
    (if org-node--debug
        ;; Special case for debugging; run single-threaded so we can step
        ;; through the org-node-parser.el functions with edebug
        (let (($i 0)
              (write-region-inhibit-fsync nil)
              (print-length nil))
          (write-region (prin1-to-string (sort files (lambda (_ _)
                                                       (natnump (random)))))
                        nil
                        (org-node-parser--tmpfile "file-list-0.eld")
                        nil
                        'quiet)
          (setq org-node-parser--result-found-links nil)
          (setq org-node-parser--result-problems nil)
          (setq org-node-parser--result-paths-types nil)
          (setq org-node--first-init nil)
          (setq org-node--time-at-scan-begin (current-time))
          (with-current-buffer (get-buffer-create "*org-node debug*")
            (setq buffer-read-only nil)
            (when (eq 'show org-node--debug)
              (pop-to-buffer (current-buffer)))
            (erase-buffer)
            (if (eq 'profile org-node--debug)
                (progn
                  (require 'profiler)
                  (profiler-stop)
                  (profiler-start 'cpu)
                  (org-node-parser--collect-dangerously)
                  (profiler-stop)
                  (profiler-report))
              (org-node-parser--collect-dangerously))
            (org-node--handle-finished-job 1 finalizer)))

      ;; If not debugging, split the work over many child processes
      (let* ((file-lists (org-node--split-into-n-sublists
                          files org-node-perf-max-jobs))
             (n-jobs (length file-lists))
             (write-region-inhibit-fsync nil) ;; Default t in emacs30
             (print-length nil))
        (dotimes (i n-jobs)
          (write-region (prin1-to-string (pop file-lists))
                        nil
                        (org-node-parser--tmpfile "file-list-%d.eld" i)
                        nil
                        'quiet)
          ;; TODO: Maybe prepend a "timeout 30"
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create org-node--stderr-name)
                 :command
                 ;; Ensure the children run the same binary executable as
                 ;; current Emacs, so the compiled-lib fits
                 (list (file-name-concat invocation-directory invocation-name)
                       "--quick"
                       "--batch"
                       "--eval" (format "(setq $i %d)" i)
                       "--eval" (format "(setq gc-cons-threshold %d)"
                                        (* 1000 1000 1000))
                       "--eval" (format "(setq temporary-file-directory \"%s\")"
                                        temporary-file-directory)
                       "--load" compiled-lib
                       "--funcall" "org-node-parser--collect-dangerously")
                 :sentinel (lambda (_process _event)
                             (org-node--handle-finished-job n-jobs finalizer)))
                org-node--processes))))))

(defvar org-node--first-init t
  "Non-nil if org-node has not been initialized yet.
Muffles some messages.")

(defun org-node--handle-finished-job (n-jobs finalizer)
  "If called by the last sentinel, run FINALIZER.
In detail: count up to N-JOBS, then if this call is the one that
hits N-JOBS, read all files into a results variable and pass it
to FINALIZER."
  (when (eq (cl-incf org-node--done-ctr) n-jobs)
    (setq org-node--time-at-finalize (current-time))
    (let ((file-name-handler-alist nil)
          (coding-system-for-read org-node-perf-assume-coding-system)
          (coding-system-for-write org-node-perf-assume-coding-system)
          ;; TODO: General way to inhibit advices on `'insert-file-contents'?
          (editorconfig
           (when (advice-member-p 'editorconfig--advice-insert-file-contents
                                  #'insert-file-contents)
             (advice-remove #'insert-file-contents
                            'editorconfig--advice-insert-file-contents)
             t))
          (result-sets nil))
      (unwind-protect
          (with-temp-buffer
            (dotimes (i n-jobs)
              (let ((results-file (org-node-parser--tmpfile "results-%d.eld" i)))
                (if (not (file-exists-p results-file))
                    (let ((buf (get-buffer " *org-node*")))
                      (when buf
                        ;; Had 1+ errors, so unhide stderr buffer from now on
                        (setq org-node--stderr-name "*org-node errors*")
                        (with-current-buffer buf
                          (rename-buffer org-node--stderr-name)))
                      ;; Don't warn on first run, for better UX.  First-time init
                      ;; thru autoloads can have bugs for seemingly magical reasons
                      ;; that go away afterwards (e.g. it says the files aren't on
                      ;; disk but they are).
                      (unless org-node--first-init
                        (message "An org-node worker failed to produce %s.  See buffer %s"
                                 results-file org-node--stderr-name)))
                  (erase-buffer)
                  (insert-file-contents results-file)
                  (push (read (buffer-string)) result-sets)))))
        (if editorconfig
            (advice-add #'insert-file-contents :around
                        'editorconfig--advice-insert-file-contents)))
      (setq org-node--time-at-last-child-done
            (-last-item (sort (-map #'-last-item result-sets) #'time-less-p)))
      ;; Merge N result-sets into one result-set, to run FINALIZER once
      (funcall finalizer (--reduce (-zip-with #'nconc it acc) result-sets)))))

(defvar org-node-before-update-tables-hook nil
  "Hook run just before processing results from scan.")


;;;; Scan-finalizers

;; TODO: Some perf ideas: try integer mtimes, try `string=' over `equal', try
;;       `cl-loop' over `dolist', do fewer remhash/clrhash if possible, define
;;       inline functions, pre-associate each dest with a list of links
(defun org-node--finalize-full (results)
  "Wipe tables and repopulate from data in RESULTS."
  (run-hooks 'org-node-before-update-tables-hook)
  (clrhash org-node--id<>node)
  (clrhash org-node--dest<>links)
  (clrhash org-node--candidate<>node)
  (clrhash org-node--title<>id)
  (clrhash org-node--ref<>id)
  (setq org-node--collisions nil) ;; To be populated by `org-node--record-node'
  (seq-let (missing-files found.mtime nodes path.type links problems) results
    (org-node--forget-id-locations missing-files)
    (dolist (file missing-files)
      (remhash file org-node--file<>mtime))
    (dolist (link links)
      (push link (gethash (org-node-link-dest link) org-node--dest<>links)))
    (cl-loop for (path . type) in path.type
             do (puthash path type org-node--uri-path<>uri-type))
    (cl-loop for (file . mtime) in found.mtime
             do (unless (equal mtime (gethash file org-node--file<>mtime))
                  (puthash file mtime org-node--file<>mtime)
                  ;; Expire stale previews
                  (remhash file org-node--file<>previews)))
    (dolist (node nodes)
      (org-node--record-node node))
    ;; (org-id-locations-save) ;; 10% of exec time on my machine
    (setq org-node--series nil)
    (dolist (def org-node-series-defs)
      (org-node--build-series def))
    (setq org-node--time-elapsed
          ;; For reproducible profiling: don't count time spent on
          ;; other sentinels, timers or I/O in between these periods
          (+ (float-time
              (time-subtract (current-time)
                             org-node--time-at-finalize))
             (float-time
              (time-subtract org-node--time-at-last-child-done
                             org-node--time-at-scan-begin))))
    (org-node--maybe-adjust-idle-timer)
    (while-let ((fn (pop org-node--temp-extra-fns)))
      (funcall fn))
    (when (and org-node--collisions org-node-warn-title-collisions)
      (message "Some nodes share title, see M-x org-node-list-collisions"))
    (when (setq org-node--problems problems)
      (message "Scan had problems, see M-x org-node-list-scan-problems"))
    (setq org-node--first-init nil)))

(defun org-node--finalize-modified (results)
  "Use RESULTS to update tables."
  (run-hooks 'org-node-before-update-tables-hook)
  (seq-let (missing-files found.mtime nodes path.type links problems) results
    (let ((found-files (mapcar #'car found.mtime)))
      (org-node--forget-id-locations missing-files)
      (dolist (file missing-files)
        (remhash file org-node--file<>mtime))
      (org-node--dirty-forget-files missing-files)
      (org-node--dirty-forget-completions-in missing-files)
      ;; In case a title was edited: don't persist old revisions of the title
      (org-node--dirty-forget-completions-in found-files)
      (when org-node-perf-eagerly-update-link-tables
        (cl-loop with ids-of-nodes-scanned = (cl-loop
                                              for node in nodes
                                              collect (org-node-get-id node))
                 with to-clean = nil
                 for link-set being the hash-values of org-node--dest<>links
                 using (hash-keys dest)
                 do (cl-loop
                     with clean-this-dest = nil
                     for link in link-set
                     if (member (org-node-link-origin link) ids-of-nodes-scanned)
                     do (setq clean-this-dest t)
                     else collect link into cleaned-link-set
                     finally do
                     (when clean-this-dest
                       (push (cons dest cleaned-link-set) to-clean)))
                 finally do
                 (dolist (pair to-clean)
                   (puthash (car pair) (cdr pair) org-node--dest<>links)))
        ;; Having erased the links that were known to originate in the
        ;; re-scanned nodes, it's safe to add them (again).
        (dolist (link links)
          (push link (gethash (org-node-link-dest link) org-node--dest<>links))))
      (cl-loop for (path . type) in path.type
               do (puthash path type org-node--uri-path<>uri-type))
      (cl-loop for (file . mtime) in found.mtime
               do (unless (equal mtime (gethash file org-node--file<>mtime))
                    (puthash file mtime org-node--file<>mtime)
                    ;; Expire stale previews
                    (remhash file org-node--file<>previews)))
      (dolist (node nodes)
        (org-node--record-node node))
      (dolist (prb problems)
        (push prb org-node--problems))
      (when problems
        (message "org-node found issues, see M-x org-node-list-scan-problems"))
      (run-hook-with-args 'org-node-rescan-functions
                          (append missing-files found-files)))))

(defcustom org-node-perf-eagerly-update-link-tables t
  "Update backlink tables on every save.

Note that no matter this value, the tables will be corrected
anyway on idle, via `org-node--idle-timer'.

A setting of t MAY slow down saving a big file containing
thousands of links on constrained devices.

Fortunately it is rarely needed, since the insert-link advices of
`org-node-cache-mode' will already record links added during
normal usage!  What\\='s left undone til idle:

1. deleted links remain in the table, leading to undead backlinks
2. :pos values can desync, which can affect the org-roam buffer

The reason for default t is better experience with
`org-node-backlink-mode'."
  :group 'org-node
  :type 'boolean)

(defun org-node--record-node (node)
  "Add NODE to `org-nodes' and related info to other tables."
  (let* ((id (org-node-get-id node))
         (path (org-node-get-file-path node))
         (refs (org-node-get-refs node)))
    ;; Share id location with org-id & do so with manual `puthash' and `push'
    ;; because `org-id-add-location' would run heavy logic we've already done.
    (puthash id path org-id-locations)
    (unless (member path org-id-files)
      (push path org-id-files))
    ;; Register the node
    (puthash id node org-node--id<>node)
    (dolist (ref refs)
      (puthash ref id org-node--ref<>id))
    ;; Setup completion candidates
    (when (funcall (org-node--ensure-compiled org-node-filter-fn) node)
      ;; Let refs work as aliases
      (dolist (ref refs)
        (puthash ref node org-node--candidate<>node)
        (puthash ref
                 (list (propertize ref 'face 'org-cite)
                       (propertize
                        (let ((type (gethash ref org-node--uri-path<>uri-type)))
                          (if type (concat type ":") "@"))
                        'face
                        'completions-annotations)
                       nil)
                 org-node--title<>affixation-triplet))
      (dolist (title (cons (org-node-get-title node)
                           (org-node-get-aliases node)))
        (let ((collision (gethash title org-node--title<>id)))
          (when (and collision (not (equal id collision)))
            (push (list title id collision) org-node--collisions)))
        (puthash title id org-node--title<>id)
        (let ((affx (funcall (org-node--ensure-compiled org-node-affixation-fn)
                             node title)))
          (if org-node-alter-candidates
              ;; Absorb the affixations into one candidate string
              (puthash (concat (nth 1 affx) (nth 0 affx) (nth 2 affx))
                       node
                       org-node--candidate<>node)
            ;; Only title as candidate, to be affixated by `org-node-collection'
            (puthash title node org-node--candidate<>node)
            (puthash title affx org-node--title<>affixation-triplet)))))))

(defun org-node--ensure-compiled (fn)
  "Try to return FN as a compiled function.

- If FN is a symbol with uncompiled function definition, return
  the same symbol and arrange to natively compile it after some
  idle time.

- If FN is an anonymous lambda, compile it, cache the resulting
  bytecode, and return that bytecode.  Then arrange to replace
  the cached value with native bytecode after some idle time."
  (cond ((compiled-function-p fn) fn)
        ((symbolp fn)
         (if (compiled-function-p (symbol-function fn))
             fn
           (if (native-comp-available-p)
               (unless (alist-get fn org-node--compile-timers)
                 (setf (alist-get fn org-node--compile-timers)
                       (run-with-idle-timer
                        (+ 10 (random 10)) nil #'native-compile fn)))
             (byte-compile fn))
           fn))
        ((gethash fn org-node--compiled-lambdas))
        ((prog1 (puthash fn (byte-compile fn) org-node--compiled-lambdas)
           (when (and (native-comp-available-p)
                      (not (eq 'closure (car-safe fn))))
             (run-with-idle-timer (+ 10 (random 10))
                                  nil
                                  `(lambda ()
                                     (puthash ,fn (native-compile ,fn)
                                              org-node--compiled-lambdas))))))))

(defvar org-node--compile-timers nil)
(defvar org-node--compiled-lambdas (make-hash-table :test #'equal)
  "1:1 table mapping lambda expressions to compiled bytecode.")


;;;; "Dirty" functions
;; Help keep the cache reasonably in sync without having to do a full reset

(defun org-node--dirty-forget-files (files)
  "Remove from cache info about nodes/refs in FILES.
You should also run `org-node--dirty-forget-completions-in' for a
thorough cleanup."
  (when files
    (cl-loop
     for node being the hash-values of org-node--id<>node
     when (member (org-node-get-file-path node) files)
     collect (org-node-get-id node) into ids
     and append (org-node-get-refs node) into refs
     and append (cons (org-node-get-title node)
                      (org-node-get-aliases node)) into titles
     finally do
     (dolist (id ids)
       (remhash id org-node--id<>node))
     (dolist (ref refs)
       (remhash ref org-node--ref<>id)
       (remhash ref org-node--title<>id))
     (dolist (title titles)
       (remhash title org-node--title<>id)))))

(defun org-node--dirty-forget-completions-in (files)
  "Remove the completion candidates for all nodes in FILES."
  (when files
    (cl-loop
     for candidate being the hash-keys of org-node--candidate<>node
     using (hash-values node)
     when (member (org-node-get-file-path node) files)
     do (remhash candidate org-node--candidate<>node))))

(defun org-node--dirty-ensure-link-known (&optional id &rest _)
  "Record the ID-link at point.
If optional argument ID is non-nil, do not check the link at
point but assume it is a link to ID."
  (when (derived-mode-p 'org-mode)
    (org-node--init-ids)
    (when-let ((origin (org-node-id-at-point))
               (dest (if (gethash id org-id-locations)
                         id
                       (let ((elm (org-element-context)))
                         (when (equal "id" (org-element-property :type elm))
                           (org-element-property :path elm))))))
      (push (org-node-link--make-obj
             :origin origin
             :pos (point)
             :type "id"
             :dest dest)
            (gethash dest org-node--dest<>links)))))

(defun org-node--dirty-ensure-node-known ()
  "Record the node at point.

Not meant to be perfect, but good enough to:

1. ensure that the node at point will show up among completion
candidates right away, without having to save the buffer.

2. ensure that `org-node-backlink-mode' won\\='t autoclean backlinks
to this node on account of it \"not existing yet\".  Actually,
also necessary is `org-node--dirty-ensure-link-known' elsewhere."
  (let ((id (org-node-id-at-point))
        (case-fold-search t))
    (unless (gethash id org-node--id<>node)
      (save-excursion
        (without-restriction
          (goto-char (point-min))
          (re-search-forward (concat "^[[:space:]]*:id: +" id))
          (let ((props (org-entry-properties))
                (heading (org-get-heading t t t t))
                (fpath (abbreviate-file-name (file-truename buffer-file-name)))
                (ftitle (cadar (org-collect-keywords '("TITLE")))))
            (when heading
              (setq heading (substring-no-properties heading)))
            (org-node--record-node
             (org-node--make-obj
              :title (or heading ftitle)
              :id id
              :is-subtree (if heading t)
              :file-path fpath
              :file-title ftitle
              :file-title-or-basename (or ftitle (file-name-nondirectory fpath))
              :aliases (split-string-and-unquote
                        (or (cdr (assoc "ROAM_ALIASES" props)) ""))
              :refs (org-node-parser--split-refs-field
                     (cdr (assoc "ROAM_REFS" props)))
              :pos (if heading (org-entry-beginning-position) 1)
              :level (or (org-current-level) 0)
              :olp (org-get-outline-path)
              ;; Less important
              :properties props
              :tags (org-get-tags)
              :todo (if heading (org-get-todo-state))
              :deadline (cdr (assoc "DEADLINE" props))
              :scheduled (cdr (assoc "SCHEDULED" props))))))))))


;;;; Scanning: Etc

(defvar org-node--time-elapsed 1.0
  "Duration of the last cache reset.")

(defvar org-node--time-at-scan-begin nil)
(defvar org-node--time-at-last-child-done nil)
(defvar org-node--time-at-finalize nil)

(defun org-node--print-elapsed ()
  "Print time elapsed since `org-node--time-at-scan-begin'.
Also report statistics about the nodes and links found.

Currently, the printed message implies that all of org-node\\='s
data were collected within the time elapsed, so you should not
run this function after only a partial scan, as the message would
be misleading."
  (if (not org-node-cache-mode)
      (message "Scan complete (Hint: Turn on org-node-cache-mode)")
    (let ((n-subtrees (cl-loop
                       for node being the hash-values of org-node--id<>node
                       count (org-node-get-is-subtree node)))
          (n-backlinks (cl-loop
                        for id being the hash-keys of org-node--id<>node
                        sum (length (gethash id org-node--dest<>links))))
          (n-reflinks (cl-loop
                       for ref being the hash-keys of org-node--ref<>id
                       sum (length (gethash ref org-node--dest<>links)))))
      (message "org-node saw %d files, %d subtrees, %d ID-links, %d reflinks in %.2fs"
               (- (hash-table-count org-node--id<>node) n-subtrees)
               n-subtrees
               n-backlinks
               n-reflinks
               org-node--time-elapsed))))

(defun org-node--split-into-n-sublists (big-list n)
  "Split BIG-LIST into a list of N sublists.

In the unlikely case where BIG-LIST contains N or fewer elements,
the return value is just like BIG-LIST except that each element
is wrapped in its own list."
  (let ((sublist-length (max 1 (/ (length big-list) n)))
        result)
    (dotimes (i n)
      (if (= i (1- n))
          ;; Let the last iteration just take what's left
          (push big-list result)
        (push (take sublist-length big-list) result)
        (setq big-list (nthcdr sublist-length big-list))))
    (delq nil result)))


;;;; Etc

;; I wish that `find-file-noselect' when called from Lisp would take an
;; optional argument that explains why the file is about to be opened, amending
;; the autosave recovery prompt.  Hack up something like that.
(defvar org-node--imminent-recovery-msg
  "Org-node is about to look inside a file, but it's likely you will first get a prompt to recover an auto-save file, ready? "
  "For use by `org-node--with-quick-file-buffer'.")

;; NOTE Very important macro for the backlink mode, because backlink insertion
;;      opens the target Org file in the background, and if doing that is
;;      laggy, then every link insertion is laggy.
(defmacro org-node--with-quick-file-buffer (file &rest body)
  "Pseudo-backport of Emacs 29 `org-with-file-buffer'.
Also integrates `org-with-wide-buffer' behavior, and tries to
execute minimal hooks in order to open and close FILE as quickly
as possible.

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
    unsaved."
  (declare (indent 1) (debug t))
  `(let ((find-file-hook nil)
         (after-save-hook nil)
         (before-save-hook nil)
         (enable-local-variables :safe)
         (org-inhibit-startup t) ;; Don't apply startup #+options
         (org-agenda-files nil)
         (kill-buffer-hook nil) ;; Inhibit save-place etc
         (kill-buffer-query-functions nil)
         (buffer-list-update-hook nil))
     (let ((was-open (find-buffer-visiting ,file)))
       (when (or was-open
                 (if (file-newer-than-file-p (let ((buffer-file-name ,file))
                                               (make-auto-save-file-name))
                                             ,file)
                     (y-or-n-p org-node--imminent-recovery-msg)
                   t))
         ;; The cache has been buggy since the 9.5 rewrite, disable to be safe
         (org-element-with-disabled-cache
           (with-current-buffer (or was-open
                                    (delay-mode-hooks
                                      (find-file-noselect ,file)))
             (save-excursion
               (without-restriction
                 ,@body))
             (unless was-open
               (when (buffer-modified-p)
                 (let ((save-silently t)
                       (inhibit-message t))
                   (save-buffer)))
               (kill-buffer))))))))

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

;; (benchmark-call #'org-node-list-files)
;; => (0.009714744 0 0.0)
;; (benchmark-call #'org-roam-list-files)
;; => (1.488666741 1 0.23508516499999388)
(defun org-node-list-files (&optional instant interactive)
  "List files in `org-id-locations' or `org-node-extra-id-dirs'.
With optional argument INSTANT t, return already known files
instead of checking the filesystem.

When called interactively \(INTERACTIVE is non-nil), list the
files in a new buffer."
  (interactive "i\np")
  (if interactive
      ;; TODO: Use tabulated-list (or better, something like a find-dired
      ;;       buffer)
      (progn
        (pop-to-buffer (get-buffer-create "*org-node files*"))
        (let ((files (org-node-list-files))
              (inhibit-read-only t))
          (erase-buffer)
          (insert (format "Found %d Org files\n" (length files)))
          (dolist (file (sort files #'string<))
            (insert-text-button file
                                'face 'link
                                'action `(lambda (_button)
                                           (find-file ,file))
                                'follow-link t)
            (newline)))
        (goto-char (point-min))
        (view-mode))
    (if instant
        (hash-table-keys org-node--file<>mtime)
      (-union ;; Faster than `seq-union' by 10x: 2000ms -> 200ms
       (hash-table-values org-id-locations)
       (let ((file-name-handler-alist nil)) ;; 3x: 200ms -> 70ms
         ;; Abbreviating file names because org-id does too
         (org-node-abbrev-file-names ;; 5x: 70ms -> 14ms
          (cl-loop for dir in org-node-extra-id-dirs
                   nconc (org-node--dir-files-recursively ;; 2x: 14ms -> 7ms
                          (file-truename dir)
                          "org"
                          org-node-extra-id-dirs-exclude))))))))

;; (progn (ignore-errors (native-compile #'org-node--dir-files-recursively)) (benchmark-run 100 (org-node--dir-files-recursively org-roam-directory "org" '("logseq/"))))
(defun org-node--dir-files-recursively (dir suffix excludes)
  "Faster, purpose-made variant of `directory-files-recursively'.
Return a list of all files under directory DIR, its
sub-directories, sub-sub-directories and so on, with provisos:

- Don\\='t follow symlinks to other directories.
- Don\\='t enter directories whose name start with a dot.
- Don\\='t enter directories where some substring of the path
  matches one of strings EXCLUDES literally.
- Don\\='t collect any file where some substring of the name
  matches one of strings EXCLUDES literally.
- Collect only files that end in SUFFIX literally.
- Don\\='t sort final results in any particular order."
  (let (result)
    (dolist (file (file-name-all-completions "" dir))
      (if (directory-name-p file)
          (unless (string-prefix-p "." file)
            (setq file (file-name-concat dir file))
            (unless (or (cl-loop for substr in excludes
                                 thereis (string-search substr file))
                        (file-symlink-p (directory-file-name file)))
              (setq result (nconc result (org-node--dir-files-recursively
        		                  file suffix excludes)))))
        (when (string-suffix-p suffix file)
          (unless (cl-loop for substr in excludes
                           thereis (string-search substr file))
            (push (file-name-concat dir file) result)))))
    result))

;; (progn (ignore-errors (native-compile #'org-node-list-files)) (benchmark-call #'org-node-list-files))
(defvar org-node--homedir nil)
(defun org-node-abbrev-file-names (path-or-paths)
  "Abbreviate all file paths in PATH-OR-PATHS.
PATH-OR-PATHS can be a single path or a list, and are presumed to
be absolute paths.

Faster than calling `abbreviate-file-name' once for each item in
a list.  Also faster than `consult--fast-abbreviate-file-name'.

It is a good idea to abbreviate a path when you don\\='t know
where it came from.  That ensures that it is comparable to a path
provided in either `org-id-locations' or an `org-node' object.

If the wild path may be a symlink or not an absolute path, it
would be safer to process it first with `file-truename', then
pass the result to this function.

Needless to say, you can also just use `file-truename' on both
paths to be compared, if not writing performance-critical code
\(think slow network filesystems)."
  (unless org-node--homedir
    (setq org-node--homedir (file-name-as-directory (expand-file-name "~"))))
  (let* ((case-fold-search nil) ;; Assume case-sensitive filesystem
         (result (cl-loop
                  for path in (ensure-list path-or-paths)
                  do (setq path (directory-abbrev-apply path))
                  and collect
                  (if (string-prefix-p org-node--homedir path)
                      (concat
                       "~" (substring path (1- (length org-node--homedir))))
                    path))))
    (if (listp path-or-paths)
        result
      (car result))))

(defun org-node--forget-id-locations (files)
  "Remove references to FILES in `org-id-locations'.
You might consider \"committing\" the effect afterwards by
calling `org-id-locations-save', which this function will not do
for you."
  (when files
    ;; (setq org-id-files (-difference org-id-files files)) ;; Redundant
    (let ((alist (org-id-hash-to-alist org-id-locations)))
      (cl-loop for file in files do (assoc-delete-all file alist))
      (setq org-id-locations (org-id-alist-to-hash alist)))))


;;;; Filename functions

;; (progn (byte-compile #'org-node--root-dirs) (benchmark-run 10 (org-node--root-dirs (hash-table-values org-id-locations))))
(defun org-node--root-dirs (file-list)
  "Infer root directories of FILE-LIST.

If FILE-LIST is the `hash-table-values' of `org-id-locations',
this function will in many cases spit out a list of one item
because many people keep their Org files in one root
directory \(with various subdirectories).

By \"root\", we mean the longest directory path common to a set
of files.

If it finds more than one root, it sorts by count of files they
contain recursively, so that the most populous root directory
will be the first element.

Note also that the only directories that may qualify are those
that directly contain some member of FILE-LIST, so that if you
have the 3 members

- \"/home/me/Syncthing/foo.org\"
- \"/home/kept/bar.org\"
- \"/home/kept/archive/baz.org\"

the return value will not be \(\"/home/\"), but
\(\"/home/kept/\" \"/home/me/Syncthing/\"), because \"/home\"
itself contains no direct members of FILE-LIST.

FILE-LIST must be a list of full paths; this function does not
consult the filesystem, just compares substrings to each other."
  (let* ((files (seq-uniq file-list))
         (dirs (seq-uniq (mapcar #'file-name-directory files))))
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child
    (cl-loop for dir in dirs
             as dirs-aside-from-this-one = (remove dir dirs)
             when (--any-p (string-prefix-p it dir) dirs-aside-from-this-one)
             do (delete dir dirs))
    ;; Now sort by count of items inside if we found 2 or more roots
    (if (= (length dirs) 1)
        dirs
      (cl-loop
       with dir-counters = (--map (cons it 0) dirs)
       for file in files
       as the-root = (--find (string-prefix-p it file) dirs)
       do (cl-incf (cdr (assoc the-root dir-counters)))
       finally return (mapcar #'car (cl-sort dir-counters #'> :key #'cdr))))))

(defcustom org-node-ask-directory nil
  "Whether to ask the user where to save a new file node.

- Symbol nil: put file in the most populous root directory in
       `org-id-locations' without asking
- String: a directory path in which to put the file
- Symbol t: ask every time, starting from the current directory

This variable controls the directory component, but the file
basename is controlled by `org-node-slug-fn' and
`org-node-datestamp-format'."
  :group 'org-node
  :type '(choice boolean string))

(defun org-node-guess-or-ask-dir (prompt)
  "Maybe prompt for a directory, and if so, show string PROMPT.
Behavior depends on the user option `org-node-ask-directory'."
  (if (eq t org-node-ask-directory)
      (read-directory-name prompt)
    (if (stringp org-node-ask-directory)
        org-node-ask-directory
      (car (org-node--root-dirs (org-node-list-files t))))))

(defcustom org-node-datestamp-format ""
  "Passed to `format-time-string' to prepend to filenames.

Example from Org-roam: %Y%m%d%H%M%S-
Example from Denote: %Y%m%dT%H%M%S--"
  :type 'string)

(defcustom org-node-slug-fn #'org-node-slugify-for-web
  "Function taking a node title and returning a filename.
Receives one argument: the value of an Org #+TITLE keyword, or
the first heading in a file that has no #+TITLE.

Built-in choices:
- `org-node-slugify-for-web'
- `org-node-slugify-like-roam-default'
- `org-node-slugify-like-roam-actual'"
  :type '(radio
          (function-item org-node-slugify-for-web)
          (function-item org-node-slugify-like-roam-default)
          (function-item org-node-slugify-like-roam-actual)
          function))

;; To be removed in 2025 when Debian bumps stable
(defun org-node--strip-diacritics (input-string)
  "Strip diacritics from INPUT-STRING."
  (if (>= emacs-major-version 29)
      ;; Emacs 29+ solution
      (thread-last input-string
                   (string-glyph-decompose)
                   (seq-remove (lambda (char) (< 767 char 818)))
                   (concat)
                   (string-glyph-compose))
    ;; Emacs 25+ solution  https://irreal.org/blog/?p=11896
    (let ((diacritics-alist
           (seq-mapn
            (lambda (a b) (cons a b))
            "ÀÁÂÃÄÅàáâãäåÒÓÔÕÕÖØòóôõöøÈÉÊËèéêëðÇçÐÌÍÎÏìíîïÙÚÛÜùúûüÑñŠšŸÿýŽža"
            "AAAAAAaaaaaaOOOOOOOooooooEEEEeeeeeCcDIIIIiiiiUUUUuuuuNnSsYyyZz")))
      (concat (seq-map (lambda (char)
                         (or (alist-get char diacritics-alist)
                             char))
                       input-string)))))

;; Useful test cases if you want to hack on this!

;; (org-node-slugify-for-web "A/B testing")
;; (org-node-slugify-for-web "\"But there's still a chance, right?\"")
;; (org-node-slugify-for-web "Löb's Theorem")
;; (org-node-slugify-for-web "Mañana Çedilla")
;; (org-node-slugify-for-web "How to convince me that 2 + 2 = 3")
;; (org-node-slugify-for-web "E. T. Jaynes")
;; (org-node-slugify-for-web "Amnesic recentf? Solution: Foo.")
;; (org-node-slugify-for-web "Slimline/\"pizza box\" computer chassis")
;; (org-node-slugify-for-web "#emacs")
;; (org-node-slugify-for-web "칹え🐛")

(defun org-node-slugify-for-web (title)
  "From TITLE, make a string meant to look nice as URL component.

A title like \"Löb\\='s Theorem\" becomes \"lobs-theorem\".  Note
that while diacritical marks are stripped, it retains most
symbols that belong to the Unicode alphabetic category,
preserving for example kanji and Greek letters."
  (thread-last title
               (org-node--strip-diacritics)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

(defun org-node-slugify-like-roam-default (title)
  "From TITLE, make a filename in the default org-roam style.
Unlike `org-node-slugify-like-roam-actual', does not load
org-roam."
  (thread-last title
               (org-node--strip-diacritics)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[^[:alnum:][:digit:]]" "_")
               (replace-regexp-in-string "__*" "_")
               (replace-regexp-in-string "^_" "")
               (replace-regexp-in-string "_$" "")))

(defun org-node-slugify-like-roam-actual (title)
  "Call on `org-roam-node-slug' to transform TITLE."
  (or (and (require 'org-roam nil t)
           (fboundp 'org-roam-node-slug)
           (fboundp 'org-roam-node-create)
           (org-roam-node-slug (org-roam-node-create :title title)))
      (user-error
       "Install org-roam to use `org-node-slugify-like-roam-actual'")))


;;;; How to create new nodes

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.")

(defun org-node--purge-backup-file-names ()
  "Clean backup names accidentally added to org-id database."
  (setq org-id-files (seq-remove #'backup-file-name-p org-id-files))
  (setq org-id-locations
        (org-id-alist-to-hash
         (cl-loop for entry in (org-id-hash-to-alist org-id-locations)
                  unless (backup-file-name-p (car entry))
                  collect entry))))

(defun org-node--goto (node)
  "Visit NODE."
  (if node
      (let ((file (org-node-get-file-path node)))
        (if (backup-file-name-p file)
            (progn
              ;; FIXME: How to prevent this from happening?
              (message "org-node: Somehow recorded backup file, resetting...")
              (org-node--purge-backup-file-names)
              (push (lambda ()
                      (message "org-node: Somehow recorded backup file, resetting... done"))
                    org-node--temp-extra-fns)
              (org-node--scan-all))
          (if (file-exists-p file)
              (progn
                (when (not (file-readable-p file))
                  (error "org-node: Couldn't visit unreadable file %s" file))
                (let ((pos (org-node-get-pos node)))
                  (find-file file)
                  (widen)
                  ;; Move point to node heading, unless heading is already
                  ;; inside visible part of buffer and point is at or under it
                  (if (org-node-get-is-subtree node)
                      (progn
                        (unless (and (pos-visible-in-window-p pos)
                                     (not (org-invisible-p pos))
                                     (equal (org-node-get-title node)
                                            (org-get-heading t t t t)))
                          (goto-char pos)
                          (if (org-at-heading-p)
                              (org-show-entry)
                            (org-show-context))
                          (recenter 0)))
                    (unless (pos-visible-in-window-p pos)
                      (goto-char pos)))))
            (message "org-node: Didn't find file, resetting...")
            (push (lambda ()
                    (message "org-node: Didn't find file, resetting... done"))
                  org-node--temp-extra-fns)
            (org-node--scan-all))))
    (error "`org-node--goto' received a nil argument")))

;; TODO: "Create" sounds unspecific, rename to "New node"?
(defun org-node-create (title id &optional series-key)
  "Call `org-node-creation-fn' with necessary variables set.

TITLE will be title of node, ID will be id of node \(use
`org-id-new' if you don\\='t know\).

Optional argument SERIES-KEY means use the resulting node to
maybe grow the corresponding series.

When calling from Lisp, you should not assume anything about
which buffer will be current afterwards, since it depends on
`org-node-creation-fn', whether TITLE or ID had existed, and
whether the user carried through with the creation.

To operate on a node after creating it, either let-bind
`org-node-creation-fn' so you know what you get, or hook
`org-node-creation-hook' temporarily, or write:

    (org-node-create TITLE ID)
    (org-node-cache-ensure t)
    (let ((node (gethash ID org-node--id<>node)))
      (if node (org-node--goto node)))"
  (setq org-node-proposed-title title)
  (setq org-node-proposed-id id)
  (setq org-node-proposed-series-key series-key)
  (unwind-protect
      (funcall org-node-creation-fn)
    (setq org-node-proposed-title nil)
    (setq org-node-proposed-id nil)
    (setq org-node-proposed-series-key nil)))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called to create a node that does not yet exist.
Used by commands such as `org-node-find'.

Built-in choices:
- `org-node-new-file'
- `org-node-new-via-roam-capture'
- `org-capture'

If you choose `org-capture' here, configure
`org-capture-templates' such that some capture templates use
`org-node-capture-target' as their target, else it is pointless.

If you wish to write a custom function instead of any of the
above three choices, know that two variables are set at the time
the function is called: `org-node-proposed-title' and
`org-node-proposed-id', which you are expected to use."
  :group 'org-node
  :type '(radio
          (function-item org-node-new-file)
          (function-item org-node-new-via-roam-capture)
          (function-item org-capture)
          function))

(defun org-node-new-file ()
  "Create a file-level node.
Meant to be called indirectly as `org-node-creation-fn', during
which it gets some necessary variables."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "org-node-new-file is meant to be called indirectly")
    (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
           (path-to-write
            (file-name-concat
             dir (concat (format-time-string org-node-datestamp-format)
                         (funcall org-node-slug-fn org-node-proposed-title)
                         ".org"))))
      (when (file-exists-p path-to-write)
        (user-error "File already exists: %s" path-to-write))
      (when (find-buffer-visiting path-to-write)
        (user-error "A buffer already exists for filename %s" path-to-write))
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
      (push (current-buffer) org-node--not-yet-saved)
      (run-hooks 'org-node-creation-hook))))

;;; TODO: Maybe move this to fakeroam.el
(defun org-node-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn', at which
time some necessary variables are set."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "`org-node-new-via-roam-capture' is meant to be called indirectly via `org-node-create'")
    (unless (require 'org-roam nil t)
      (user-error "Didn't create node %s! Either install org-roam or %s"
                  org-node-proposed-title
                  "configure `org-node-creation-fn'"))
    (when (and (fboundp 'org-roam-capture-)
               (fboundp 'org-roam-node-create))
      (org-roam-capture- :node (org-roam-node-create
                                :title org-node-proposed-title
                                :id    org-node-proposed-id)))))

(defun org-node-capture-target ()
  "Can be used as target in a capture template.
See `org-capture-templates' for more info about targets.

In simple terms, let\\='s say you have configured
`org-capture-templates' so it has a template that
targets `(function org-node-capture-target)'.  Now here\\='s a
possible workflow:

1. Run org-capture
2. Select your template
3. Type name of known or unknown node
4a. If it was known, it will capture into that node.
4b. If it was unknown, it will create a file-level node and then
    capture into there.

Additionally, with (setq org-node-creation-fn #\\='org-capture),
commands like `org-node-find' will outsource to org-capture when you
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
        ;; title and no such node exists yet
        (progn
          (setq title org-node-proposed-title)
          (setq id org-node-proposed-id))
      ;; Was called from `org-capture', which means the user has not yet typed
      ;; the title; let them type it now
      (let ((input (completing-read "Node: " #'org-node-collection
                                    () () () 'org-node-hist)))
        (setq node (gethash input org-node--candidate<>node))
        (if node
            (progn
              (setq title (org-node-get-title node))
              (setq id (org-node-get-id node)))
          (setq title input)
          (setq id (org-id-new)))))
    (if node
        ;; Node exists; capture into it
        (progn
          (find-file (org-node-get-file-path node))
          (widen)
          (goto-char (org-node-get-pos node))
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
        (push (current-buffer) org-node--not-yet-saved)
        (run-hooks 'org-node-creation-hook)))))


;;;; Examples and helpers for user to define series

(defun org-node-extract-file-name-datestamp (path)
  "From filename PATH, get the datestamp prefix if it has one.
Do so by comparing with `org-node-datestamp-format'.  Not immune
to false positives, if you have been changing formats over time."
  (when (and org-node-datestamp-format
             (not (string-blank-p org-node-datestamp-format)))
    (let ((name (file-name-nondirectory path)))
      (when (string-match
             (org-node--make-regexp-for-time-format org-node-datestamp-format)
             name)
        (match-string 0 name)))))

(defun org-node-mk-series-on-tag-sorted-by-property
    (key name tag prop &optional capture)
  "Quick-define a series filtered by TAG sorted by property PROP.
This would be a series of ID-nodes, not files.
KEY, NAME and CAPTURE explained in `org-node-series-defs'."
  `(,key
    :name ,name
    :version 2
    :capture ,capture
    :classifier (lambda (node)
                  (let ((sortstr (cdr (assoc ,prop (org-node-get-props node))))
                        (tagged (member ,tag (org-node-get-tags node))))
                    (when (and sortstr tagged (not (string-blank-p sortstr)))
                      (cons (concat sortstr " " (org-node-get-title node))
                            (org-node-get-id node)))))
    :whereami (lambda ()
                (when (member ,tag (org-get-tags))
                  (let ((sortstr (org-entry-get nil ,prop t))
                        (node (gethash (org-node-id-at-point) org-nodes)))
                    (when (and sortstr node)
                      (concat sortstr " " (org-node-get-title node))))))
    :prompter (lambda (key)
                (let ((series (cdr (assoc key org-node--series))))
                  (completing-read "Go to: " (plist-get series :sorted-items))))
    :try-goto (lambda (item)
                (org-node-helper-try-goto-id (cdr item)))
    :creator (lambda (sortstr key)
               (let ((adder (lambda () (org-node-tag-add ,tag))))
                 (add-hook 'org-node-creation-hook adder)
                 (unwind-protect (org-node-create sortstr (org-id-new) key)
                   (remove-hook 'org-node-creation-hook adder))))))

(defun org-node--guess-daily-dir ()
  "Do not rely on this."
  (or (bound-and-true-p org-node-fakeroam-daily-dir)
      (bound-and-true-p org-journal-dir)
      (file-name-concat org-directory "daily/")))

(defun org-node-helper-try-goto-id (id)
  "Try to visit org-id ID, returning non-nil on success."
  (let ((node (gethash id org-node--id<>node)))
    (when node
      (org-node--goto node)
      t)))

(defun org-node-helper-try-visit-file (file)
  "Visit FILE if it exists or if a buffer exists on that file.
Return non-nil on success."
  (let ((buf (find-buffer-visiting file)))
    (if buf
        (switch-to-buffer buf)
      (when (file-readable-p file)
        (find-file file)))))

(defun org-node-helper-filename->ymd (path)
  "Check the filename PATH for a date and return it.
On failing to coerce a date, return nil."
  (when path
    (let ((clipped-name (file-name-base path)))
      (if (string-match
           (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit))
           clipped-name)
          (match-string 0 clipped-name)
        ;; Even in a non-daily file, pretend it is a daily if possible,
        ;; to allow entering the series at a more relevant date
        (when-let ((stamp (org-node-extract-file-name-datestamp path)))
          (org-node-extract-ymd stamp org-node-datestamp-format))))))

;; TODO: Handle %s, %V, %y...  is there a library?
(defun org-node-extract-ymd (instance time-format)
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
          (if (-none-p #'null (list pos-year pos-month pos-day))
              (progn
                (if (> pos-month pos-year) (cl-incf pos-month 2))
                (if (> pos-day pos-year) (cl-incf pos-day 2))
                (concat (substring instance pos-year (+ pos-year 4))
                        "-"
                        (substring instance pos-month (+ pos-month 2))
                        "-"
                        (substring instance pos-day (+ pos-day 2))))
            (cl-assert pos-ymd)
            (substring instance pos-ymd (+ pos-ymd 10))))))))


;;;; Series plumbing

(defcustom org-node-series-defs
  (list
   '("d" :name "Daily-files"
     :version 2
     :classifier (lambda (node)
                   (let ((path (org-node-get-file-path node)))
                     (when (string-search (org-node--guess-daily-dir) path)
                       (let ((ymd (org-node-helper-filename->ymd path)))
                         (when ymd
                           (cons ymd path))))))
     :whereami (lambda ()
                 (org-node-helper-filename->ymd buffer-file-name))
     :prompter (lambda (key)
                 (let ((org-node-series-that-marks-calendar key))
                   (org-read-date)))
     :try-goto (lambda (item)
                 (org-node-helper-try-visit-file (cdr item)))
     :creator (lambda (sortstr key)
                (let ((org-node-datestamp-format "")
                      (org-node-ask-directory (org-node--guess-daily-dir)))
                  (org-node-create sortstr (org-id-new) key))))

   ;; Obviously, this series works best if you have `org-node-put-created' on
   ;; `org-node-creation-hook'.
   '("a" :name "All ID-nodes by property :CREATED:"
     :version 2
     :classifier (lambda (node)
                   (let ((time (cdr (assoc "CREATED" (org-node-get-props node)))))
                     (when (and time (not (string-blank-p time)))
                       (cons time (org-node-get-id node)))))
     :whereami (lambda ()
                 (let ((time (org-entry-get nil "CREATED" t)))
                   (and time (not (string-blank-p time)) time)))
     :prompter (lambda (key)
                 (let ((series (cdr (assoc key org-node--series))))
                   (completing-read
                    "Go to: " (plist-get series :sorted-items))))
     :try-goto (lambda (item)
                 (when (org-node-helper-try-goto-id (cdr item))
                   t))
     :creator (lambda (sortstr key)
                (org-node-create sortstr (org-id-new) key))))
  "Alist defining each node series.

Each item looks like

\(KEY :name NAME
     :classifier CLASSIFIER
     :whereami WHEREAMI
     :prompter PROMPTER
     :try-goto TRY-GOTO
     :creator CREATOR
     :capture CAPTURE
     :version VERSION)

KEY uniquely identifies the series, and is the key to type after
\\[org-node-series-dispatch] to select it.  It may not be \"j\",
\"n\", \"p\" or \"c\", these keys are reserved for
Jump/Next/Previous/Capture actions.

NAME describes the series, in one or a few words.

CLASSIFIER is a single-argument function taking an `org-node'
object and should return a cons cell or list if a series-item was
found, otherwise nil.

The list may contain anything, but the first element must be a
sort-string, i.e. a string suitable for sorting on.  An example
is a date in the format YYYY-MM-DD, but not in the format MM/DD/YY.

This is what determines the order of items in the series: after
all nodes have been processed by CLASSIFIER, the non-nil return
values are sorted by the sort-string, using `string>'.

Function PROMPTER may be used during jump/capture/refile to
interactively prompt for a sort-string.  This highlights the
other use of the sort-string: finding our way back from scant
context.

For example, in a series of daily-notes sorted on YYYY-MM-DD, a
prompter could use `org-read-date'.

PROMPTER receives one argument, the series plist, which has the
same form as one of the values in `org-node-series-defs' but
includes two extra members :key, corresponding to KEY, and
:sorted-items, which may be useful for interactive completion.

Function WHEREAMI is like PROMPTER in that it should return a
sort-string.  However, it should do this without user
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
`org-node-series-dispatch' menu.

Integer VERSION indicates the series definition language.  New
series should use version 2, as of 2024-09-05.  When org-node
updates the series definition language, old versions may still
work, but this is not heavily tested, so it will start printing a
message to remind you to check out the wiki on GitHub and port
your definitions."
  :type 'alist)

(defvar org-node--series nil
  "Alist describing each node series, internal use.")

(defvar org-node-proposed-series-key nil
  "Key that identifies the series about to be added to.
Automatically set, should be nil most of the time.  For a
variable that need not stay nil, see
`org-node-current-series-key'.")

(defun org-node--add-series-item ()
  "Look at node near point to maybe add an item to a series.
Only do something if `org-node-proposed-series-key' is non-nil."
  (when org-node-proposed-series-key
    (let* ((series (cdr (assoc org-node-proposed-series-key
                               org-node--series)))
           (node-here (gethash (org-node-id-at-point) org-nodes))
           (new-item (when node-here
                       (funcall (plist-get series :classifier) node-here))))
      (when new-item
        (unless (member new-item (plist-get series :sorted-items))
          (push new-item (plist-get series :sorted-items))
          (sort (plist-get series :sorted-items)
                (lambda (item1 item2)
                  (string> (car item1) (car item2)))))))))

(defun org-node--series-jump (key)
  "Jump to an entry in series identified by KEY."
  (let* ((series (cdr (assoc key org-node--series)))
         (sortstr (if (= 2 (plist-get series :version))
                      (funcall (plist-get series :prompter) key)
                    (funcall (plist-get series :prompter) series)))
         (item (assoc sortstr (plist-get series :sorted-items))))
    (if item
        (unless (funcall (plist-get series :try-goto) item)
          (delete item (plist-get series :sorted-items))
          (if (= 2 (plist-get series :version))
              (funcall (plist-get series :creator) sortstr key)
            (funcall (plist-get series :creator) sortstr)))
      (if (= 2 (plist-get series :version))
          (funcall (plist-get series :creator) sortstr key)
        (funcall (plist-get series :creator) sortstr)))))

(defun org-node--series-goto-next (key)
  "Visit the next entry in series identified by KEY."
  (org-node--series-goto-previous key t))

(defun org-node--series-goto-previous (key &optional next)
  "Visit the previous entry in series identified by KEY.
If argument NEXT is non-nil, actually visit the next entry."
  (let* ((series (cdr (assoc key org-node--series)))
         (here (funcall (plist-get series :whereami)))
         (items (plist-get series :sorted-items))
         head
         first-tail)
    (when here
      ;; Find our location in the series
      (cl-loop for item in items
               if (string> (car item) here)
               do (push item head)
               else return (setq first-tail item)))
    (when (or here
              (when (y-or-n-p
                     (format "Not in series \"%s\".  Jump to latest item in that series?"
                             (plist-get series :name)))
                (setq head (take 1 items))
                t))
      (let* (;; Only 0 if user ran a :creator that did not register the item,
             ;; which would result in skipping backwards twice if still 1.
             (1-if-member (if (equal here (car first-tail)) 1 0))
             (to-check (if next
                           head
                         (drop (+ (length head) 1-if-member) items))))
        ;; Usually this should return on the first try, but sometimes stale
        ;; items refer to something that has been erased from disk, so
        ;; deregister each item that TRY-GOTO failed to visit and try again.
        (cl-loop
         for item in to-check
         if (funcall (plist-get series :try-goto) item)
         return t
         else do (delete item items)
         finally return (message "No %s item in series \"%s\""
                                 (if next "next" "previous")
                                 (plist-get series :name)))))))

(defun org-node-series-goto (key sortstr)
  "Visit an entry in series identified by KEY.
The entry to visit has sort-string SORTSTR.  Create if it does
not exist."
  (let* ((series (cdr (assoc key org-node--series)))
         (item (assoc sortstr (plist-get series :sorted-items))))
    (when (or (null item)
              (if (funcall (plist-get series :try-goto) item)
                  nil
                (delete item (plist-get series :sorted-items))
                t))
      (funcall (plist-get series :creator) sortstr key))))

(defun org-node-series-capture-target ()
  "Experimental."
  (org-node-cache-ensure)
  (let ((key (or org-node-current-series-key
                 (let* ((valid-keys (mapcar #'car org-node-series-defs))
                        (elaborations
                         (cl-loop for series in org-node-series-defs
                                  concat
                                  (format " %s(%s)"
                                          (car series)
                                          (plist-get (cdr series) :name))))
                        (input (read-char-from-minibuffer
                                (format "Press any of [%s] to capture into series: %s "
                                        (string-join valid-keys ",")
                                        elaborations)
                                (mapcar #'string-to-char valid-keys))))
                   (char-to-string input)))))
    ;; Almost identical to `org-node--series-jump'
    (let* ((series (cdr (assoc key org-node--series)))
           (sortstr (or org-node-proposed-title
                        (if (= 2 (plist-get series :version))
                            (funcall (plist-get series :prompter) key)
                          (funcall (plist-get series :prompter) series))))
           (item (assoc sortstr (plist-get series :sorted-items))))
      (when (or (null item)
                (not (funcall (plist-get series :try-goto) item)))
        ;; TODO: Move point after creation to most appropriate place
        (if (= 2 (plist-get series :version))
            (funcall (plist-get series :creator) sortstr key)
          (funcall (plist-get series :creator) sortstr))))))

(defun org-node--build-series (def)
  "From plist DEF, populate `org-node--series'.
Then add a corresponding entry to `org-node-series-dispatch'.

DEF is a member of `org-node-series-defs'."
  (let ((classifier (org-node--ensure-compiled
                     (plist-get (cdr def) :classifier)))
        (unique-sortstrs (make-hash-table :test #'equal)))
    (cl-loop
     for node being the hash-values of org-node--id<>node
     as item = (funcall classifier node)
     when (and item (not (gethash (car item) unique-sortstrs)))
     collect (progn (puthash (car item) t unique-sortstrs)
                    item)
     into items
     finally do
     (setf (alist-get (car def) org-node--series nil nil #'equal)
           (nconc (cl-loop for elt in (cdr def)
                           if (functionp elt)
                           collect (org-node--ensure-compiled elt)
                           else collect elt)
                  (list :key (car def)
                        ;; Using `string>' due to most recent dailies probably
                        ;; being most relevant, thus cycling thru recent
                        ;; dailies will have the best perf.
                        :sorted-items (cl-sort items #'string> :key #'car)))))
    (org-node--add-series-to-dispatch (car def) (plist-get (cdr def) :name))))

(defvar org-node-current-series-key nil
  "Key of the series currently being browsed with the menu.")

(defun org-node--add-series-to-dispatch (key name)
  "Use KEY and NAME to add a series to the Transient menu."
  (when (ignore-errors (transient-get-suffix 'org-node-series-dispatch key))
    (transient-remove-suffix 'org-node-series-dispatch key))
  (transient-append-suffix 'org-node-series-dispatch '(0 -1)
    (list key name key))
  ;; Make the series switches mutually exclusive
  (let ((old (car (slot-value (get 'org-node-series-dispatch 'transient--prefix)
                              'incompatible))))
    (setf (slot-value (get 'org-node-series-dispatch 'transient--prefix)
                      'incompatible)
          (list (seq-uniq (cons key old))))))

;; These suffixes just exist due to a linter complaint, could
;; have been lambdas

(transient-define-suffix org-node--series-goto-previous* (args)
  (interactive (list (transient-args 'org-node-series-dispatch)))
  (if args
      (org-node--series-goto-previous (car args))
    (message "Choose series before navigating")))

(transient-define-suffix org-node--series-goto-next* (args)
  (interactive (list (transient-args 'org-node-series-dispatch)))
  (if args
      (org-node--series-goto-next (car args))
    (message "Choose series before navigating")))

(transient-define-suffix org-node--series-jump* (args)
  (interactive (list (transient-args 'org-node-series-dispatch)))
  (if args
      (org-node--series-jump (car args))
    (message "Choose series before navigating")))

(transient-define-suffix org-node--series-capture (args)
  (interactive (list (transient-args 'org-node-series-dispatch)))
  (if args
      (progn (setq org-node-current-series-key (car args))
             (unwind-protect
                 (let* ((series (cdr (assoc (car args) org-node--series)))
                        (capture-keys (plist-get series :capture)))
                   (if capture-keys
                       (org-capture nil capture-keys)
                     (message "No capture template for series %s"
                              (plist-get series :name))))
               (setq org-node-current-series-key nil)))
    (message "Choose series before navigating")))

;;;###autoload (autoload 'org-node-series-dispatch "org-node" nil t)
(transient-define-prefix org-node-series-dispatch ()
  :incompatible '(("d"))
  ["Series"
   ("|" "Invisible" "Placeholder" :if-nil t)
   ("d" "Dailies" "d")]
  ["Navigation"
   ("p" "Previous in series" org-node--series-goto-previous* :transient t)
   ("n" "Next in series" org-node--series-goto-next* :transient t)
   ("j" "Jump (or create)" org-node--series-jump*)
   ("c" "Capture into" org-node--series-capture)])

(defcustom org-node-series-that-marks-calendar nil
  "Key for the series that should mark days in the calendar.

This affects the appearance of the `org-read-date' calendar
popup.  For example, it can indicate which days have a
daily-journal entry.

This need usually not be customized!  When you use
`org-node-series-dispatch' to jump to a daily-note or some
other date-based series, that series may be designed to
temporarily set this variable.

Customize this mainly if you want a given series to always be
indicated, any time a Org pops up a calendar for you.

The sort-strings in the series that corresponds to this key
should be correctly parseable by `org-parse-time-string'."
  :type '(choice key (const nil)))

;; (defface org-node-calendar-marked
;;   '((t :inherit 'org-link))
;;   "Face used by `org-node--mark-days'.")

;; TODO: How to cooperate with preexisting marks?
(defun org-node--mark-days ()
  "Mark days in the calendar popup.
The user option `org-node-series-that-marks-calendar' controls
which dates to mark.

Meant to sit on these hooks:
- `calendar-today-invisible-hook'
- `calendar-today-visible-hook'"
  (calendar-unmark)
  (when org-node-series-that-marks-calendar
    (let* ((series (cdr (assoc org-node-series-that-marks-calendar
                               org-node--series)))
           (dates (mapcar #'car (plist-get series :sorted-items)))
           mdy)
      (dolist (date dates)
        (setq mdy (seq-let (_ _ _ d m y _ _ _) (org-parse-time-string date)
                    (list m d y)))
        (when (calendar-date-is-visible-p mdy)
          (calendar-mark-visible-date mdy))))))


;;;; Commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-node-creation-fn' to `org-node-new-via-roam-capture'."
  (interactive)
  (org-node-cache-ensure)
  (let* ((input (completing-read "Go to ID-node: " #'org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node--candidate<>node)))
    (if node
        (org-node--goto node)
      (org-node-create input (org-id-new)))))

;;;###autoload
(defun org-node-visit-random ()
  "Visit a random node."
  (interactive)
  (org-node-cache-ensure)
  (org-node--goto (nth (random (hash-table-count org-node--candidate<>node))
                       (hash-table-values org-node--candidate<>node))))

;;;###autoload
(defun org-node-insert-link (&optional region-as-initial-input)
  "Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*' and its docstring.

Optional argument REGION-AS-INITIAL-INPUT t means behave as
`org-node-insert-link*'."
  (interactive nil org-mode)
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
                            (try-completion region-text org-node--title<>id)))
                      region-text
                    nil))
         (input (completing-read "Node: " #'org-node-collection
                                 () () initial 'org-node-hist))
         (node (gethash input org-node--candidate<>node))
         (id (if node (org-node-get-id node) (org-id-new)))
         (link-desc (or region-text
                        (when (not org-node-alter-candidates) input)
                        (and node (--find (string-search it input)
                                          (org-node-get-aliases node)))
                        (and node (org-node-get-title node))
                        input)))
    (atomic-change-group
      (when region-text
        (delete-region beg end))
      (insert (org-link-make-string (concat "id:" id) link-desc))
      (run-hooks 'org-node-insert-link-hook))
    ;; TODO: Delete the link if a node was not created
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

The commands are the same, it is just a difference in
initial input."
  (interactive nil org-mode)
  (org-node-insert-link t))

;;;###autoload
(defun org-node-insert-transclusion ()
  "Insert a #+transclude: referring to a node."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (let ((node (gethash (completing-read "Node: " #'org-node-collection
                                        () () () 'org-node-hist)
                       org-node--candidate<>node)))
    (let ((id (org-node-get-id node))
          (title (org-node-get-title node))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id) title))
      (goto-char (pos-bol))
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level " (number-to-string (+ 1 level))))))

;;;###autoload
(defun org-node-insert-transclusion-as-subtree ()
  "Insert a link and a transclusion.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level 3

but adapt to the surrounding outline level.  I recommend
adding keywords to the things to exclude:

\(setq org-transclusion-exclude-elements
      \\='(property-drawer comment keyword))"
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (let ((node (gethash (completing-read "Node: " #'org-node-collection
                                        () () () 'org-node-hist)
                       org-node--candidate<>node)))
    (let ((id (org-node-get-id node))
          (title (org-node-get-title node))
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
      ;; If the target is a subtree rather than file-level node, I'd like to
      ;; cut out the initial heading because we already made an outer heading.
      ;; (We made the outer heading so that this transclusion will count as a
      ;; backlink, plus it makes more sense to me on export to HTML).
      ;;
      ;; Unfortunately cutting it out with the :lines trick would prevent
      ;; `org-transclusion-exclude-elements' from having an effect, and the
      ;; subtree's property drawer shows up!
      ;; TODO: Patch `org-transclusion-content-range-of-lines' to respect
      ;; `org-transclusion-exclude-elements', or (better) don't use :lines but
      ;; make a different argument like ":no-initial-heading"
      ;;
      ;; For now, just let it nest an extra heading. Looks odd, but doesn't
      ;; break things.
      (goto-char (marker-position m1))
      (set-marker m1 nil)
      (run-hooks 'org-node-insert-link-hook))))

;;;###autoload
(defun org-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created file.

You may find it a common situation that the subtree had not yet
been assigned an ID nor any other property that you normally
assign to a proper node.  Thus, this creates an ID for you if
there was no ID, copies over all inherited tags, and runs
`org-node-creation-hook'.

Adding to that, see below for an example advice that copies any
inherited \"CREATED\" property, if an ancestor has such a
property.  It is subjective whether you\\='d want this behavior,
but it can be desirable if you know the subtree had been part of
the source file for ages so that you see the ancestor\\='s
creation-date as more \"truthful\" than today\\='s date.

\(advice-add \\='org-node-extract-subtree :around
            (defun my-inherit-creation-date (orig-fn &rest args)
                   (let ((parent-creation (org-entry-get nil \"CREATED\" t)))
                     (apply orig-fn args)
                     ;; Now in the new buffer
                     (org-entry-put nil \"CREATED\"
                                    (or parent-creation
                                        (format-time-string \"[%F %a]\"))))))"
  (interactive nil org-mode)
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
           ;; Why is CATEGORY autocreated by `org-entry-properties'...  It's
           ;; an invisible property that's always present and usually not
           ;; interesting, unless user has entered some explicit value
           (explicit-category (save-excursion
                                (when (search-forward ":category:" boundary t)
                                  (org-entry-get nil "CATEGORY"))))
           (properties (--filter (not (equal "CATEGORY" (car it)))
                                 (org-entry-properties nil 'standard)))
           (path-to-write
            (file-name-concat
             dir (concat (format-time-string org-node-datestamp-format)
                         (funcall org-node-slug-fn title)
                         ".org"))))
      (if (file-exists-p path-to-write)
          (message "A file already exists named %s" path-to-write)
        (org-cut-subtree)
        ;; Try to leave a link at the end of parent entry, pointing to the
        ;; ID of subheading that was extracted.
        (unless (bound-and-true-p org-capture-mode)
          (widen)
          (org-up-heading-or-point-min)
          (goto-char (org-entry-end-position))
          (if (org-invisible-p)
              (message "Invisible area, not inserting link to extracted")
            (open-line 1)
            (insert "\n"
                    (format-time-string
                     (format "%s Created " (org-time-stamp-format nil t)))
                    (org-link-make-string (concat "id:" id) title)
                    "\n")
            (org-node--dirty-ensure-link-known id)))
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
           (string-join (--map (concat ":" (car it) ": " (cdr it))
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
        (org-node--dirty-ensure-node-known)
        (push (current-buffer) org-node--not-yet-saved)
        (run-hooks 'org-node-creation-hook)
        (when org-node-backlink-mode
          (org-node-backlink--fix-entry-here))))))

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
  (unless (equal format (car org-node--make-regexp-for-time-format))
    (setq org-node--make-regexp-for-time-format
          (cons format
                (let ((example (format-time-string format)))
                  (if (string-match-p (rx (any "^*+([\\")) example)
                      (error "org-node: Unable to safely rename with current `org-node-datestamp-format'.  This is not inherent in your choice of format, I am just not smart enough")
                    (concat "^"
                            (string-replace
                             "." "\\."
                             (replace-regexp-in-string
                              "[[:digit:]]+" "[[:digit:]]+"
                              (replace-regexp-in-string
                               "[[:alpha:]]+" "[[:alpha:]]+"
                               example t)))))))))
  ;; Memoize consecutive calls with same input
  (cdr org-node--make-regexp-for-time-format))

;; This function can be removed if one day we drop support for file-level
;; nodes, because then just (org-entry-get nil "ID" t) will suffice.  That
;; demonstrates the maintenance burden of file-level anything: `org-entry-get'
;; /can/ get the file-level ID but only sometimes.  Don't know where's the bug
;; in this case, but file-level property drawers were a mistake, they create
;; the need for special-case code all over the place, which leads to many new
;; bugs, and bring nothing to the table.  Even here, this function had a bug
;; earlier due to how I wrote it, but I shouldn't have had to write the
;; function at all.
(defun org-node-id-at-point ()
  "Get ID for current entry or up the outline tree."
  (save-excursion
    (without-restriction
      (or (org-entry-get nil "ID" t)
          (progn (goto-char (point-min))
                 (org-entry-get nil "ID" t))))))

(defcustom org-node-renames-allowed-dirs nil
  "Dirs in which files may be auto-renamed.
Used by `org-node-rename-file-by-title'.

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
`org-node-datestamp-format', and avoid overwriting it.

Suitable at the end of `after-save-hook'.  If called from a
hook, only operate on files in `org-node-renames-allowed-dirs'.
When called interactively, always prompt for confirmation.

Internal argument INTERACTIVE is automatically set."
  (interactive "p" org-mode)
  (let ((path buffer-file-truename)
        (buf (current-buffer))
        (title nil))
    (cond
     ((or (not (derived-mode-p 'org-mode))
          (not (equal "org" (file-name-extension path))))
      (when interactive
        (message "Will only rename Org files")))
     ((and (not interactive) (null org-node-renames-allowed-dirs))
      (message "User option `org-node-renames-allowed-dirs' should be configured"))
     ((or interactive
          (cl-loop
           for dir in (mapcar #'file-truename org-node-renames-allowed-dirs)
           if (string-match-p org-node-renames-exclude dir)
           do (user-error "Regexp `org-node-renames-exclude' would directly match a directory from `org-node-renames-allowed-dirs'")
           else if (and (string-prefix-p dir path)
                        (not (string-match-p org-node-renames-exclude path)))
           return t))
      (if (not (setq title (or (cadar (org-collect-keywords '("TITLE")))
                               ;; No #+title, so take first heading as title
                               ;; for this purpose
                               (save-excursion
                                 (without-restriction
                                   (goto-char 1)
                                   (or (org-at-heading-p)
                                       (outline-next-heading))
                                   (org-get-heading t t t t))))))
          (message "File has no title nor heading")
        (let* ((name (file-name-nondirectory path))
               (date-prefix (or (org-node-extract-file-name-datestamp path)
                                ;; Couldn't find date prefix, give a new one
                                (format-time-string org-node-datestamp-format)))
               (new-name (concat
                          date-prefix (funcall org-node-slug-fn title) ".org"))
               (new-path (file-name-concat (file-name-directory path) new-name)))

          (cond
           ((equal path new-path)
            (when interactive
              (message "Filename already correct: %s" path)))
           ((or (buffer-modified-p buf)
                (buffer-modified-p (buffer-base-buffer buf)))
            (when interactive
              (message "Unsaved file, letting it be: %s" path)))
           ((prog1 nil
              (when (get-file-buffer new-path)
                (user-error "Wanted to rename, but a buffer already visits target: %s"
                            new-path))
              (unless (file-writable-p path)
                (user-error "No permissions to rename file: %s"
                            path))
              (unless (file-writable-p new-path)
                (user-error "No permissions to write a new file at: %s"
                            new-path))
              ;; A bit unnecessary bc `rename-file' would error too,
              ;; but at least we didn't kill buffer yet
              (when (file-exists-p new-path)
                (user-error "Canceled because a file exists at: %s"
                            new-path))))
           ((or (not interactive)
                (y-or-n-p (format "Rename file %s to %s?" name new-name)))
            (let* ((pt (point))
                   (visible-window (get-buffer-window buf))
                   (window-start (window-start visible-window)))
              ;; Kill buffer before renaming, because it will not
              ;; follow the rename
              (kill-buffer buf)
              (rename-file path new-path)
              (let ((new-buf (find-file-noselect new-path)))
                ;; Don't let remaining hooks operate on some random buffer
                (set-buffer new-buf)
                ;; Helpfully go back to where point was
                (when visible-window
                  (set-window-buffer-start-and-point
                   visible-window new-buf window-start pt))
                (with-current-buffer new-buf
                  (goto-char pt)
                  (if (org-at-heading-p)
                      (org-show-entry)
                    (org-show-context)))))
            (message "File %s renamed to %s" name new-name)))))))))

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
    (dolist (file (or files (org-node-list-files t)))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (without-restriction
            (goto-char (point-min))
            (while-let ((end (re-search-forward org-link-bracket-re nil t)))
              (let* ((beg (match-beginning 0))
                     (link (substring-no-properties (match-string 0)))
                     (exact-link (rx (literal link)))
                     (parts (split-string link "]\\["))
                     (target (substring (car parts) 2))
                     (desc (when (cadr parts)
                             (substring (cadr parts) 0 -2)))
                     (id (when (string-prefix-p "id:" target)
                           (substring target 3)))
                     (node (gethash id org-node--id<>node))
                     (true-title (when node
                                   (org-node-get-title node)))
                     (answered-yes nil))
                (when (and id node desc
                           (not (string-equal-ignore-case desc true-title))
                           (not (member-ignore-case
                                 desc (org-node-get-aliases node))))
                  (switch-to-buffer (current-buffer))
                  (goto-char end)
                  (if (org-at-heading-p)
                      (org-show-entry)
                    (org-show-context))
                  (recenter)
                  (highlight-regexp exact-link 'org-node--rewrite-face)
                  (unwind-protect
                      (setq answered-yes
                            (y-or-n-p
                             (format "Rewrite link? Will become:  \"%s\""
                                     true-title)))
                    (unhighlight-regexp exact-link))
                  (when answered-yes
                    (goto-char beg)
                    (atomic-change-group
                      (delete-region beg end)
                      (insert (org-link-make-string target true-title)))
                    ;; Give user a moment to glimpse the result before hopping
                    ;; to the next link in case of a replacement gone wrong
                    (redisplay)
                    (sleep-for .15))
                  (goto-char end))))))))))

;;;###autoload
(defun org-node-rename-asset-and-rewrite-links ()
  "Helper for renaming images and all links that point to them.

Prompt for an asset such as an image file to be renamed, then
search recursively for Org files containing a link to that asset,
open a wgrep buffer of the search hits, and start an interactive
search-replace that updates the links.  After the user consents
to replacing all the links, finally rename the asset file itself."
  (interactive)
  (unless (require 'wgrep nil t)
    (user-error "This command requires the wgrep package"))
  (when (and (fboundp 'wgrep-change-to-wgrep-mode)
             (fboundp 'wgrep-finish-edit))
    ;; (user-error "This command requires the wgrep package")
    (let ((root (car (org-node--root-dirs (org-node-list-files))))
          (default-directory default-directory))
      (or (equal default-directory root)
          (if (y-or-n-p (format "Go to folder \"%s\"?" root))
              (setq default-directory root)
            (setq default-directory
                  (read-directory-name
                   "Directory with Org notes to operate on: "))))
      (when-let ((bufs (--filter (string-search "*grep*" (buffer-name it))
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
           (pop-to-buffer (--find (string-search "*grep*" (buffer-name it))
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
  (interactive nil org-mode)
  (org-insert-heading)
  (org-node-nodeify-entry))

;;;###autoload
(defun org-node-nodeify-entry ()
  "Add an ID to entry at point and run `org-node-creation-hook'."
  (interactive nil org-mode)
  (org-node-cache-ensure)
  (org-id-get-create)
  (run-hooks 'org-node-creation-hook))

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive nil org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED"
                   (format-time-string (org-time-stamp-format t t)))))

(defvar org-node--temp-extra-fns nil
  "Extra functions to run at the end of a full scan.
The list is emptied on each use.  Primarily exists to give the
interactive command `org-node-reset' a way to print the time
elapsed.")

;;;###autoload
(defun org-node-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (cl-pushnew #'org-node--print-elapsed org-node--temp-extra-fns)
  (org-node-cache-ensure nil t))

;;;###autoload
(defun org-node-forget-dir (dir)
  "Remove references in `org-id-locations' to files in DIR.

Note that if DIR can be found under `org-node-extra-id-dirs',
this action may make no practical impact unless you also add DIR
to `org-node-extra-id-dirs-exclude'.

In case of unsolvable problems, how to wipe org-id-locations:

\(progn
 (delete-file org-id-locations-file)
 (setq org-id-locations nil)
 (setq org-id--locations-checksum nil)
 (setq org-agenda-text-search-extra-files nil)
 (setq org-id-files nil)
 (setq org-id-extra-files nil))"
  (interactive "DForget all IDs in directory: ")
  (org-node-cache-ensure t)
  (let ((files (seq-intersection
                (mapcar #'abbreviate-file-name
                        (directory-files-recursively dir "."))
                (hash-table-values org-id-locations))))
    (if files
        (progn
          (message "Forgetting all IDs in directory... (%s)" dir)
          (org-node--forget-id-locations files)
          (org-id-locations-save)
          (org-node-reset))
      (message "No files in: %s" dir))))

;;;###autoload
(defun org-node-grep ()
  "Grep across all files known to org-node."
  (interactive)
  (unless (require 'consult nil t)
    (user-error "This command requires the consult package"))
  (org-node--init-ids)
  (when (and (fboundp 'consult--grep)
             (fboundp 'consult--grep-make-builder)
             (fboundp 'consult--ripgrep-make-builder)
             (boundp 'consult-ripgrep-args))
    (cl-letf (((symbol-function #'file-relative-name)
               (lambda (name &optional _dir) name)))
      (let ((consult-ripgrep-args (concat consult-ripgrep-args " --type=org")))
        (if (executable-find "rg")
            (consult--grep "Ripgrep in all known Org files: "
                           #'consult--ripgrep-make-builder
                           (org-node--root-dirs (org-node-list-files t))
                           nil)
          ;; Much slower, no --type=org means must target thousands of files
          ;; and not a handful of dirs
          (consult--grep "Grep in all known Org files: "
                         #'consult--grep-make-builder
                         (org-node-list-files)
                         nil))))))

(defvar org-node--linted nil
  "List of files linted so far.")

;;;###autoload
(defun org-node-lint-all-files (&optional restart)
  "Run `org-lint' on all known Org files, and report results.

If last run was interrupted, resume working through the file list
from where it stopped.  With prefix argument RESTART, start over
from the beginning."
  (interactive "P")
  (require 'org-lint)
  (org-node--init-ids)
  (let* ((warnings nil)
         (report-buffer (get-buffer-create "*org-node lint report*"))
         (files (-difference (org-node-list-files) org-node--linted))
         (ctr (length org-node--linted))
         (ctrmax (+ (length files) (length org-node--linted))))
    (with-current-buffer report-buffer
      (when (or (null files) restart)
        ;; Start over
        (when (y-or-n-p "Wipe the previous lint results? ")
          (setq files (org-node-list-files))
          (setq org-node--linted nil)
          (setq tabulated-list-entries nil)
          (let ((inhibit-read-only t))
            (erase-buffer))))
      (tabulated-list-mode)
      (add-hook 'tabulated-list-revert-hook #'org-node-lint-all-files nil t)
      (setq tabulated-list-format
            [("File" 35 t)
             ("Line" 5 t)
             ("Trust" 5 t)
             ("Explanation" 100 t)])
      (tabulated-list-init-header)
      (unwind-protect
          (dolist (file files)
            (message "Linting file... (you may quit and resume anytime) (%d/%d) %s"
                     (cl-incf ctr) ctrmax file)
            (org-node--with-quick-file-buffer file
              (setq warnings (org-lint)))
            (dolist (warning warnings)
              (let ((array (cadr warning)))
                (push (list warning
                            (vector
                             (list (file-name-nondirectory file)
                                   'face 'link
                                   'action `(lambda (_button)
                                              (find-file ,file)
                                              (goto-line
                                               ,(string-to-number (elt array 0))))
                                   'follow-link t)
                             (elt array 0)
                             (elt array 1)
                             (elt array 2)))
                      tabulated-list-entries)))
            (push file org-node--linted))
        (display-buffer report-buffer)
        (tabulated-list-print t t)
        (tabulated-list-sort 2))
      (when (null tabulated-list-entries)
        (message "All good, no lint warnings!")))))

(defun org-node-list-feedback-arcs ()
  "Show a feedback-arc-set of forward id-links.

Requires GNU R installed, with R packages tidyverse and igraph.

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
     "This command requires GNU R, with R packages tidyverse and igraph"))
  (let ((feedbacks nil)
        (r-code "library(stringr)
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

write_file(lisp_data, file.path(dirname(tsv), \"feedback-arcs.eld\"))")
        (script-file (org-node-parser--tmpfile "analyze_feedback_arcs.R"))
        (digraph-tsv (org-node-parser--tmpfile "id_node_digraph.tsv")))
    (write-region r-code nil script-file)
    (write-region (org-node--make-digraph-tsv-string) nil digraph-tsv)
    (with-current-buffer (get-buffer-create "*feedback arcs*")
      (fundamental-mode)
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (unless (= 0 (call-process "Rscript" nil t nil script-file digraph-tsv))
        (error "%s" (buffer-string)))
      (erase-buffer)
      (insert-file-contents (org-node-parser--tmpfile "feedback-arcs.eld"))
      (setq feedbacks (read (buffer-string)))
      (when (listp feedbacks)
        (erase-buffer)
        (tabulated-list-mode)
        (add-hook 'tabulated-list-revert-hook #'org-node-list-feedback-arcs
                  nil t)
        (setq tabulated-list-format
              [("Node containing link" 39 t)
               ("Target of link" 0 t)])
        (tabulated-list-init-header)
        (setq tabulated-list-entries
              (cl-loop
               for (origin . dest) in feedbacks
               as origin-node = (gethash origin org-node--id<>node)
               as dest-node = (gethash dest org-node--id<>node)
               collect
               (list (cons origin dest)
                     (vector (list (org-node-get-title origin-node)
                                   'face 'link
                                   'action `(lambda (_button)
                                              (org-node--goto ,origin-node))
                                   'follow-link t)
                             (list (org-node-get-title dest-node)
                                   'face 'link
                                   'action `(lambda (_button)
                                              (org-node--goto ,dest-node))
                                   'follow-link t)))))
        (tabulated-list-print))
      (display-buffer (current-buffer)))))

;; TODO: Temp merge all refs into corresponding ID
(defun org-node--make-digraph-tsv-string ()
  "Generate a string in Tab-Separated Values form.
The string is a 2-column table of destination-origin pairs, made
from ID links found in `org-node--dest<>links'."
  (concat
   "src\tdest\n"
   (string-join
    (seq-uniq (cl-loop
               for dest being the hash-keys of org-node--dest<>links
               using (hash-values links)
               append (cl-loop
                       for link in links
                       when (equal "id" (org-node-link-type link))
                       collect (concat dest "\t" (org-node-link-origin link)))))
    "\n")))

;; TODO: Command to list the coding systems of all files
;;       to help choose `org-node-perf-assume-coding-system'
;; (defvar org-node--found-systems nil)
;; (defun org-node-list-file-coding-systems ()
;;   (dolist (file (take 20 (org-node-list-files)))
;;     (org-node--with-quick-file-buffer file
;;       (push buffer-file-coding-system org-node--found-systems)))
;;   org-node--found-systems)

(defun org-node-list-dead-links ()
  "List links that lead to no known ID."
  (interactive)
  (let ((dead-links
         (cl-loop for dest being the hash-keys of org-node--dest<>links
                  using (hash-values links)
                  unless (gethash dest org-node--id<>node)
                  append (cl-loop for link in links
                                  when (equal "id" (org-node-link-type link))
                                  collect (cons dest link)))))
    (message "%d dead links found" (length dead-links))
    (pop-to-buffer (get-buffer-create "*Dead Links*"))
    (tabulated-list-mode)
    (add-hook 'tabulated-list-revert-hook #'org-node-list-dead-links nil t)
    (setq tabulated-list-format
          [("Location" 40 t)
           ("Unknown ID reference" 40 t)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries
          (cl-loop
           for (dest . link) in dead-links
           as origin-node = (gethash (org-node-link-origin link)
                                     org-node--id<>node)
           if (not (equal dest (org-node-link-dest link)))
           do (error "IDs not equal: %s, %s" dest (org-node-link-dest link))
           else if (not origin-node)
           do (error "Node not found for ID: %s" (org-node-link-origin link))
           else
           collect (list link
                         (vector
                          (list (org-node-get-title origin-node)
                                'face 'link
                                'action `(lambda (_button)
                                           (org-node--goto ,origin-node)
                                           (goto-char ,(org-node-link-pos link)))
                                'follow-link t)
                          dest))))
    (tabulated-list-print)))

(defun org-node-list-reflinks ()
  "List all reflinks and their locations.

Useful to see how many times you\\='ve inserted a link that is very
similar to another link, but not identical, so that likely only
one of them is associated with a ROAM_REFS."
  (interactive)
  (let ((plain-links (cl-loop
                      for list being the hash-values of org-node--dest<>links
                      append (cl-loop
                              for link in list
                              unless (equal "id" (org-node-link-type link))
                              collect link))))
    (with-current-buffer (get-buffer-create "*org-node reflinks*")
      (tabulated-list-mode)
      (add-hook 'tabulated-list-revert-hook #'org-node-list-reflinks nil t)
      (setq tabulated-list-format
            [("REF?" 4 t)
             ("Inside node" 30 t)
             ("Potential reflink" 0 t)])
      (tabulated-list-init-header)
      (setq tabulated-list-entries nil)
      (dolist (link plain-links)
        (let ((type (org-node-link-type link))
              (dest (org-node-link-dest link))
              (origin (org-node-link-origin link))
              (pos (org-node-link-pos link)))
          (let ((node (gethash origin org-node--id<>node)))
            (push (list link
                        (vector
                         (if (gethash dest org-node--ref<>id) "y" "")
                         (if node
                             (list (org-node-get-title node)
                                   'action `(lambda (_button)
                                              (org-id-goto ,origin)
                                              (goto-char ,pos)
                                              (if (org-at-heading-p)
                                                  (org-show-entry)
                                                (org-show-context)))
                                   'face 'link
                                   'follow-link t)
                           origin)
                         (if type (concat type ":" dest) dest)))
                  tabulated-list-entries))))
      (switch-to-buffer (current-buffer))
      (tabulated-list-print)
      (tabulated-list-sort 2))))

(defcustom org-node-warn-title-collisions t
  "Whether to print messages on finding duplicate node titles."
  :group 'org-node
  :type 'boolean)

(defvar org-node--collisions nil
  "Alist of node title collisions.")
(defun org-node-list-collisions ()
  "Pop up a buffer listing node title collisions."
  (interactive)
  (with-current-buffer (get-buffer-create "*org-node title collisions*")
    (tabulated-list-mode)
    (add-hook 'tabulated-list-revert-hook #'org-node-list-collisions nil t)
    (setq tabulated-list-format
          [("Non-unique name" 30 t)
           ("ID" 37 t)
           ("Other ID" 0 t)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries nil)
    (dolist (row org-node--collisions)
      (seq-let (msg id1 id2) row
        (push (list row
                    (vector
                     msg
                     (list id1
                           'action `(lambda (_button)
                                      (org-id-goto ,id1))
                           'face 'link
                           'follow-link t)
                     (list id2
                           'action `(lambda (_button)
                                      (org-id-goto ,id2))
                           'face 'link
                           'follow-link t)))
              tabulated-list-entries)))
    (if tabulated-list-entries
        (progn
          (tabulated-list-print)
          (switch-to-buffer (current-buffer)))
      (message "Congratulations, no title collisions! (in %d filtered nodes)"
               (hash-table-count org-node--title<>id)))))

(defvar org-node--problems nil
  "Alist of errors encountered by org-node-parser.")
(defun org-node-list-scan-problems ()
  "Pop up a buffer listing errors found by org-node-parser."
  (interactive)
  (with-current-buffer (get-buffer-create "*org-node scan problems*")
    (tabulated-list-mode)
    (add-hook 'tabulated-list-revert-hook #'org-node-list-scan-problems nil t)
    (setq tabulated-list-format
          [("Scan choked near position" 27 t)
           ("Issue (newest on top)" 0 t)])
    (tabulated-list-init-header)
    (setq tabulated-list-entries nil)
    (dolist (problem org-node--problems)
      (seq-let (file pos signal) problem
        (push (list problem
                    (vector
                     ;; Clickable link
                     (list (format "%s:%d"
                                   (file-name-nondirectory file) pos)
                           'face 'link
                           'action `(lambda (_button)
                                      (find-file ,file)
                                      (goto-char ,pos))
                           'follow-link t)
                     (format "%s" signal)))
              tabulated-list-entries)))
    (if tabulated-list-entries
        (progn
          (tabulated-list-print)
          (switch-to-buffer (current-buffer)))
      (message "Congratulations, no problems scanning %d nodes!"
               (hash-table-count org-node--id<>node)))))

;; REVIEW: Is this a sane logic for picking heading?  It does not mirror how
;;         org-set-tags picks heading to apply to, and maybe that is the
;;         behavior to model.
(defun org-node--call-at-nearest-node (function &rest args)
  "With point at the relevant heading, call FUNCTION with ARGS.

Prefer the closest ancestor heading that has an ID, else go to
the file-level property drawer if that contains an ID, else fall
back on the heading for the current entry.

Afterwards, maybe restore point to where it had been previously,
so long as the affected heading would still be visible in the
window."
  (let* ((where-i-was (point-marker))
         (id (org-node-id-at-point))
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

If the current entry has no ID, operate on the closest ancestor
with an ID.  If there\\='s no ID anywhere, operate on the current
entry.

Then behave like `org-entry-add-to-multivalued-property' but
preserve spaces: instead of percent-escaping each space character
as \"%20\", wrap the value in quotes if it has spaces."
  (org-node--call-at-nearest-node
   (lambda ()
     (let ((old (org-entry-get nil property)))
       (when old
         (setq old (split-string-and-unquote old)))
       (unless (member value old)
         (org-entry-put nil property (combine-and-quote-strings
                                      (cons value old))))))))

(defun org-node-alias-add ()
  "Add to ROAM_ALIASES in nearest relevant property drawer."
  (interactive nil org-mode)
  (org-node--add-to-property-keep-space
   "ROAM_ALIASES" (string-trim (read-string "Alias: "))))

(defun org-node-ref-add ()
  "Add to ROAM_REFS in nearest relevant property drawer.
Wrap the value in double-brackets if necessary."
  (interactive nil org-mode)
  (let ((ref (string-trim (read-string "Ref: "))))
    (when (and (string-match-p " " ref)
               (string-match-p org-link-plain-re ref))
      ;; If it is a link, it should not only be enclosed in quotes, but
      ;; brackets too.  (Technically, org-node-parser doesn't need quotes,
      ;; but it makes programming easier for everyone since they can use
      ;; `split-string-and-unquote'.)
      (setq ref (concat "[[" (string-trim ref (rx "[[") (rx "]]"))
                        "]]")))
    (org-node--add-to-property-keep-space "ROAM_REFS" ref)))

(defun org-node-tag-add (tag-or-tags)
  "Add TAG-OR-TAGS to the entry at point."
  (interactive
   (list (completing-read-multiple
          "Tag: "
          (cl-union
           (cl-loop for node being the hash-values of org-nodes
                    append (org-node-get-tags node))
           (cl-remove-if #'keywordp
                         (mapcar #'car (append
                                        ;; 200ms in big file w/o org elem cache
                                        ;; (org-get-buffer-tags)
                                        org-tag-persistent-alist
                                        org-tag-alist))))))
   org-mode)
  (if (= (org-outline-level) 0)
      ;; There's no builtin filetag setter yet
      (let* ((tags (cl-loop
                    for raw in (cdar (org-collect-keywords '("FILETAGS")))
                    append (split-string raw ":" t)))
             (new-tags (seq-uniq (append (ensure-list tag-or-tags) tags)))
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
              (if (re-search-forward "^[^:#]" nil t)
                  (progn
                    (backward-char 1)
                    (skip-chars-backward "\n\t\s"))
                (goto-char (point-max)))
              (newline)
              (insert "#+filetags: :" (string-join new-tags ":") ":")))))
    (org-set-tags (seq-uniq (append (ensure-list tag-or-tags)
                                    (org-get-tags))))))


;;;; CAPF (Completion-At-Point Function)

;; REVIEW: is it necessary to limit the effect to file-visiting buffers?
(define-minor-mode org-node-complete-at-point-mode
  "Use `org-node-complete-at-point' in all Org buffers.

-----"
  :global t
  :require 'org-node
  (if org-node-complete-at-point-mode
      (progn
        (add-hook 'org-mode-hook #'org-node--install-capf-in-buffer)
        (when (bound-and-true-p org-roam-completion-everywhere)
          (message "You may want to set `org-roam-completion-everywhere' nil"))
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and buffer-file-name (derived-mode-p 'org-mode))
              (add-hook 'completion-at-point-functions
                        #'org-node-complete-at-point nil t)))))
    (remove-hook 'org-mode-hook #'org-node--install-capf-in-buffer)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-hook 'completion-at-point-functions
                     #'org-node-complete-at-point t)))))

(defun org-node--install-capf-in-buffer ()
  "Let in-buffer completion try `org-node-complete-at-point'."
  (and buffer-file-name
       (derived-mode-p 'org-mode)
       (string-suffix-p "org" buffer-file-name)
       (add-hook 'completion-at-point-functions
                 #'org-node-complete-at-point nil t)))

(defun org-node-complete-at-point ()
  "Complete word at point to a known node title, and linkify.
Designed for `completion-at-point-functions', which see."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (and bounds
         ;; For some reason it runs in non-org buffers, like grep results?
         (derived-mode-p 'org-mode)
         ;; Any way to skip these checks?
         (not (org-in-src-block-p))
         (not (save-match-data
                (org-in-regexp org-link-any-re)))
         (list (car bounds)
               (cdr bounds)
               org-node--title<>id
               :exclusive 'no
               :exit-function
               (lambda (text _)
                 (when-let ((id (gethash text org-node--title<>id)))
                   (atomic-change-group
                     (delete-char (- (length text)))
                     (insert (org-link-make-string
                              (concat "id:" id) text)))))))))


;;;; Misc

(defun org-node-convert-link-to-super (&rest _)
  "Drop input and call `org-super-links-convert-link-to-super'."
  (require 'org-super-links)
  (when (fboundp 'org-super-links-convert-link-to-super)
    (org-super-links-convert-link-to-super nil)))

(defun org-node-try-visit-ref-node ()
  "Designed for `org-open-at-point-functions'.

For the link at point, if there exists an org-ID node that has
the link in its ROAM_REFS property, visit that node rather than
following the link normally.

If already visiting that node, then follow the link normally."
  (when-let ((url (thing-at-point 'url)))
    ;; Rarely more than one car
    (let* ((dest (car (org-node-parser--split-refs-field url)))
           (found (cl-loop for node being the hash-values of org-nodes
                           when (member dest (org-node-get-refs node))
                           return node)))
      (if (and found
               ;; Check that point is not already in said ref node (if so,
               ;; better to fallback to default `org-open-at-point' logic)
               (not (and (derived-mode-p 'org-mode)
                         (equal (org-node-id-at-point)
                                (org-node-get-id found)))))
          (progn (org-node--goto found)
                 t)
        nil))))


;;;; API not used inside this package

(defun org-node-at-point ()
  "Return the ID-node near point.

This may refer to the current Org heading, else an ancestor
heading, else the file-level node, whichever has an ID first."
  (gethash (org-node-id-at-point) org-node--id<>node))

(defun org-node-read ()
  "Prompt for a known ID-node."
  (gethash (completing-read "Node: " #'org-node-collection
                            () () () 'org-node-hist)
           org-node--candidate<>node))


;;;; Obsolete series functions (for :version 1)

(defvar org-node--obsolete-series-warned nil)
(defun org-node--obsolete-series-warn ()
  "Warn about usage of obsolete series helpers/versions."
  (unless org-node--obsolete-series-warned
    (setq org-node--obsolete-series-warned t)
    (display-warning
     'org-node "Your initfiles use a now-old version of series definitions. Still works, but port them when you can: https://github.com/meedstrom/org-node/wiki")))

(defun org-node--example-daily-creator (sortstr &rest _)
  "Create a daily-note using SORTSTR as the date."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (if (and (eq org-node-creation-fn 'org-node-new-via-roam-capture)
           (fboundp 'org-node-fakeroam-daily-create))
      ;; Assume this user wants to use their roam-dailies templates
      (progn
        (setq org-node-proposed-series-key "d")
        (unwind-protect
            (org-node-fakeroam-daily-create sortstr
                                            org-node-proposed-series-key)
          (setq org-node-proposed-series-key nil)))
    (let ((org-node-ask-directory
           (if (require 'org-roam-dailies nil t)
               (file-name-concat org-roam-directory org-roam-dailies-directory)
             (file-name-as-directory (file-name-concat org-directory "daily"))))
          (org-node-datestamp-format "")
          (org-node-slug-fn #'identity))
      (org-node-create sortstr (org-id-new) "d"))))

(defun org-node--series-standard-creator (sortstr &rest _)
  "Create a node with SORTSTR as the title."
  (declare (obsolete nil "2024-08-17"))
  (org-node--obsolete-series-warn)
  (org-node-create sortstr (org-id-new)))

(defun org-node--example-daily-classifier (node)
  "Classifier for dailies in Roam style.
If NODE is in a \"daily\" or \"dailies\" directory, return an
item using file name base as sort string."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (let ((path (org-node-get-file-path node)))
    (when (string-match-p "dail\\w+/" path)
      (cons (file-name-base path) path))))

(defun org-node--example-try-goto-file (item)
  "Assume cdr of ITEM is a filename and try to visit it."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (let ((buf (find-buffer-visiting (cdr item))))
    (if buf
        (switch-to-buffer buf)
      (when (file-readable-p (cdr item))
        (find-file (cdr item))))))

(defun org-node--example-try-goto-id (item)
  "Assume cdr of ITEM is an org-id and try to visit it."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (let* ((id (cdr item))
         (node (gethash id org-nodes)))
    (when node
      (org-node--goto node)
      t)))

(defun org-node--example-daily-whereami ()
  "Check the filename for a date and return it."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (when-let* ((path (buffer-file-name (buffer-base-buffer)))
              (clipped-name (file-name-base path)))
    (if (string-match-p
         (rx bol (= 4 digit) "-" (= 2 digit) "-" (= 2 digit) eol)
         clipped-name)
        clipped-name
      ;; Even in a non-daily file, pretend it is a daily if possible,
      ;; to allow entering the series at a more relevant date
      (when-let ((stamp (org-node-extract-file-name-datestamp path)))
        (org-node-extract-ymd stamp org-node-datestamp-format)))))

(defun org-node--example-daily-prompter (series &rest _)
  "Prompt for a date, return it in YYYY-MM-DD form.
Argument SERIES is the series that called this."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (let ((org-node-series-that-marks-calendar (plist-get series :key)))
    (org-read-date)))

(defun org-node--example-prompter (series)
  "Prompt to go to any of the sort-strings in SERIES."
  (declare (obsolete nil "2024-08-21"))
  (org-node--obsolete-series-warn)
  (completing-read "Go to: " (plist-get series :sorted-items)))

(provide 'org-node)

;;; org-node.el ends here
