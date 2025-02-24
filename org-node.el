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

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "28.1") (compat "30") (el-job "0.3.22") (llama "0.4.0"))
;; URL:              https://github.com/meedstrom/org-node

;; NOTE: Looking for Package-Version?
;;       Consult your package manager, or the Git tag.

;;; Commentary:

;; What is Org-node?

;; If you were the sort of person to prefer "id:" links over "file:" links
;; or radio-targets or any other type of link, you're in the right place!

;; Now you can worry less about mentally tracking your subtree hierarchies and
;; directory structures.  As long as you've assigned an ID to something, you
;; can find it later.

;; The philosophy is the same as org-roam: if you assign an ID every
;; time you make an entry that you know you might want to link to from
;; elsewhere, then it tends to work out that the `org-node-find' command
;; can jump to more or less every entry you'd ever want to jump to.

;; Anyway, that's just the core of it as described to someone not
;; familiar with zettelkasten-ish packages.  In fact, out of the
;; simplicity arises something powerful, more to be experienced than
;; explained.

;; Compared to org-roam:

;;   - Same idea, compatible disk format
;;   - Fast
;;   - Does not need SQLite
;;   - Does not support "roam:" links
;;   - Lets you opt out of those file-level property drawers
;;   - Tries to rely in a bare-metal way on upstream org-id and org-capture
;;   - Ships extra commands to e.g. auto-rename files and links

;;   As a drawback of relying on the org-id table, if a heading in some
;;   vendor README.org or whatever has an ID, it's considered part of
;;   your collection -- simply because if it's known to org-id, it's
;;   known to org-node.
;;   These headings can be filtered after-the-fact.

;; Compared to denote:

;;   - Org only, no Markdown nor other file types
;;   - Does not support "denote:" links
;;   - Filenames have no meaning (can match the Denote format if you like)
;;   - You can have as many "notes" as you want inside one file.  You
;;     could possibly use Denote to search files and org-node
;;     as a more granular search.

;;; Code:

;; Built-in
(require 'seq)
(require 'cl-lib)
(require 'subr-x)
(require 'bytecomp)
(require 'ucs-normalize)
(require 'org)
(require 'org-id)
(require 'org-macs)
(require 'org-element)

;; External
(require 'llama)
(require 'compat)
(require 'org-node-parser)
(require 'org-node-changes)
(require 'el-job)

;; Satisfy compiler
(defvar org-roam-directory)
(defvar org-roam-dailies-directory)
(defvar consult-ripgrep-args)
(defvar org-node-backlink-mode)
(declare-function org-node-backlink--fix-entry-here "org-node-backlink")
(declare-function profiler-report "profiler")
(declare-function profiler-stop "profiler")
(declare-function tramp-tramp-file-p "tramp")
(declare-function org-lint "org-lint")
(declare-function consult--grep "consult")
(declare-function consult--grep-make-builder "consult")
(declare-function consult--ripgrep-make-builder "consult")


;;;; Options

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

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

If you change your mind about this setting, you can
transition the files you already have with the Org-roam commands
`org-roam-promote-entire-buffer' and `org-roam-demote-entire-buffer'."
  :type 'boolean)

(defcustom org-node-link-types
  '("http" "https" "id")
  "Link types that may result in backlinks.

Of course, org-node is built around the \"id\" link type, which
corresponds to a target node\\='s ID property.  The ID property is
mandatory because it acts as a unique identifier.

However, the concept can be generalized.  Org-node also looks at the
ROAM_REFS property, which in another universe might have been called
\"EXTRA_IDS\", because in many ways it is just a list of additional IDs
for the same node.

For performance reasons, not just any string of text is accepted in the
ROAM_REFS property -- it must have valid links per Org syntax, such as
\"[[https://gnu.org]]\" or \"https://gnu.org\".
Use the command \\[org-node-add-refs] for convenience.

Finally, this variable controls which link types are permitted.
The fewer types, the faster your \\[org-node-reset].

What\\='s it actually used for: if you insert \"https://gnu.org\" in the
body text of another node, then that results in a new backlink, even
though no reference was made to a proper ID!

People often use this to write notes about a specific web-page or PDF
file, and call it a ref-node for that web-page.  See also
`org-node-try-visit-ref-node' and \\[org-node-list-reflinks].

Tip: eval `(org-link-types)' to see all built-in link types.

As a special case, citation keys such as \"@ioannidis2005\" also work in
ROAM_REFS, and correspond to citations like \"[cite:@ioannidis2005]\".
There is no need to add the \"cite\" type."
  :type '(repeat string)
  :package-version '(org-node . "0.7"))

(defvar org-node-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that org-node can scan for ID-nodes inside .org.gpg files.  Also,
`org-node-perf-keep-file-name-handlers' should include the EPG
handler.

I do not use EPG, so that is probably not enough to make it work.
Report an issue on https://github.com/meedstrom/org-node/issues
or drop me a line on Mastodon: @meedstrom@hachyderm.io"
  ;; Reverted to defvar for now
  ;; :type 'alist
  )

(defvar org-node-perf-keep-file-name-handlers nil
  "Which file handlers to respect while scanning for ID-nodes.

Normally, `file-name-handler-alist' changes the behavior of many Emacs
functions when passed some file names: TRAMP paths, compressed files or
.org.gpg files.

It slows down the access of very many files, since it is a series of
regexps applied to every file name passed.  The fewer items in this
list, the faster `org-node-reset'.

There is probably no point adding items for now, as org-node will
need other changes to support TRAMP and encryption."
  ;; Reverted to defvar for now
  ;; :type '(set
  ;;         (function-item jka-compr-handler)
  ;;         (function-item epa-file-handler)
  ;;         ;; REVIEW: Chesterton's Fence.  I don't understand why
  ;;         ;; `tramp-archive-autoload-file-name-handler' exists
  ;;         ;; (check emacs -Q), when these two already have autoloads?
  ;;         (function-item tramp-file-name-handler)
  ;;         (function-item tramp-archive-file-name-handler)
  ;;         (function-item file-name-non-special))
  )

(defcustom org-node-perf-assume-coding-system nil
  "Coding system to assume while scanning ID nodes.

Picking a specific coding system can speed up `org-node-reset'.
Set nil to let Emacs figure it out anew on every file.

This setting is likely only noticeable if `el-job--cores' is low \(1-3\)
or you have one giant Org file.

On MS Windows this probably should be nil.  Same if you access
your files from multiple platforms.

Modern GNU/Linux, BSD and MacOS systems almost always encode new
files as `utf-8-unix'.  You can verify with a helper command
\\[org-node-list-file-coding-systems]."
  :type '(choice coding-system (const nil)))

(defcustom org-node-perf-eagerly-update-link-tables t
  "Update backlink tables on every save.

A setting of t MAY slow down saving a big file containing
thousands of links on constrained devices.

Fortunately it is rarely needed, since the insert-link advices of
`org-node-cache-mode' will already record links added during
normal usage!

Other issues are corrected anyway when `org-node--idle-timer' fires.
These temporary issues are:

1. deleted links remain in the table, leading to undead backlinks
2. link positions can desync, which can affect the org-roam buffer

A user of `org-node-backlink-mode' is recommended to enable this as
well as `org-node-backlink-aggressive'."
  :type 'boolean)

(defun org-node--set-and-remind-reset (sym val)
  "Set SYM to VAL."
  (let ((caller (cadr (backtrace-frame 5))))
    (when (and (boundp 'org-node--first-init)
               (not org-node--first-init)
               ;; TIL: loading a theme calls ALL custom-setters?!
               (not (memq caller '(custom-theme-recalc-variable load-theme))))
      (lwarn 'org-node :debug
             "org-node--set-and-remind-reset called by %s" caller)
      (run-with-timer
       .1 nil #'message
       "Remember to run M-x org-node-reset after configuring %S" sym)))
  (custom-set-default sym val))

(defcustom org-node-filter-fn
  (lambda (node)
    (not (assoc "ROAM_EXCLUDE" (org-node-get-properties node))))
  "Predicate returning non-nil to include a node, or nil to exclude it.

The filtering only has an impact on the table
`org-node--candidate<>node', which forms the basis for
completions in the minibuffer, and `org-node--title<>id', used
by `org-node-complete-at-point-mode'.

In other words, passing nil means the user cannot autocomplete to the
node, but Lisp code can still find it in the \"main\" table
`org-node--id<>node', and backlinks are discovered normally.

This function is applied once for every ID-node found, and
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
                 (string-search \"/archive/\" (org-node-get-file node))
                 (member \"drill\" (org-node-get-tags-local node))))))"
  :type 'function
  :set #'org-node--set-and-remind-reset)

(defcustom org-node-insert-link-hook '()
  "Hook run after inserting a link to an Org-ID node.

Called with point in the new link."
  :type 'hook)

(defcustom org-node-creation-hook '(org-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied by `org-node-new-file', `org-node-capture-target',
`org-node-insert-heading', `org-node-nodeify-entry' and
`org-node-extract-subtree'.

NOT applied by `org-node-fakeroam-new-via-roam-capture' -- see
org-roam\\='s `org-roam-capture-new-node-hook' instead.

A good function for this hook is `org-node-put-created', since
the default `org-node-datestamp-format' is empty.

In the author\\='s experience, recording the creation-date somewhere may
prove useful later on, e.g. when publishing to a blog.

Filesystem creation-time cannot be relied on."
  :type 'hook)

(defcustom org-node-extra-id-dirs nil
  "Directories in which to search Org files for IDs.

Essentially like variable `org-id-extra-files', but take directories.

You could already do this by adding directories to variable
`org-agenda-files', but that only checks the directories once.  This
variable causes the directories to be checked again over time in order
to find new files that have appeared, e.g. files moved by terminal
commands or created by other instances of Emacs.

These directories are only checked as long as `org-node-cache-mode' is
active.  They are checked recursively (looking in subdirectories,
sub-subdirectories etc).

EXCEPTION: Subdirectories that start with a dot, such as \".emacs.d/\",
are not checked.  To check these, add them explicitly.

To avoid accidentally picking up duplicate files such as versioned
backups, causing org-id to complain about duplicate IDs, configure
`org-node-extra-id-dirs-exclude'."
  :type '(repeat directory)
  :set #'org-node--set-and-remind-reset)

;; TODO: Figure out how to permit .org.gpg and fail gracefully if
;;       the EPG settings are insufficient. easier to test with .org.gz first
(defcustom org-node-extra-id-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    "/node_modules/"
    ".sync-conflict-")
  "Path substrings of files that should not be searched for IDs.

This option only influences which files under `org-node-extra-id-dirs'
should be scanned.  It is meant as a way to avoid collecting IDs inside
versioned backup files and other noise.

For all other \"excludey\" purposes, you probably mean to configure
`org-node-filter-fn' instead.

If you have accidentally let org-id add a directory of backup files, try
\\[org-node-forget-dir].

It is not necessary to exclude backups or autosaves that end in ~ or #
or .bak, since the workhorse `org-node-list-files' only considers files
that end in precisely \".org\" anyway.

You can eke out a performance boost by excluding directories with a
humongous amount of files, such as the infamous \"node_modules\", even
if they contain no Org files.  However, directories that start with a
period are always ignored, so no need to specify e.g. \"~/.local/\" or
\".git/\" for that reason."
  :type '(repeat string))


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

\(Tip: users of the orderless library from July 2024 do not need this
setting, they can match against the prefix and suffix in any command via
`orderless-annotation', bound to the character \& by default.)

Another consequence: this setting can lift the uniqueness constraint on
note titles: you\\='ll be able to have two nodes with the same name, so
long as their prefix or suffix differ in some way.

After changing this setting, run \\[org-node-reset]."
  :type 'boolean
  :set #'org-node--set-and-remind-reset)

;; NOTE: For context see :affixation-function in `completion-extra-properties',
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

After changing this setting, run \\[org-node-reset].

------
Info for writing a custom function

The function receives two arguments: NODE and TITLE, and it must return
a list of three strings: title, prefix and suffix.  Of those three, the
title should be TITLE unmodified.

NODE is an object which form you can observe in examples from
\\[org-node-peek] and specified in type `org-node'
\(for docs, type \\[describe-symbol] org-node RET).

If a node has aliases, the same node is passed to this function
again for every alias, in which case TITLE is actually one of the
aliases."
  :type '(radio
          (function-item org-node-affix-bare)
          (function-item org-node-prefix-with-olp)
          (function-item org-node-prefix-with-tags)
          (function-item org-node-affix-with-olp-and-tags)
          (function :tag "Custom function"))
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
        (let ((tags (org-node-get-tags node)))
          (if tags
              (propertize (concat "(" (string-join tags ", ") ") ")
                          'face 'org-tag)
            ""))
        ""))

(defun org-node-prefix-with-olp (node title)
  "Prepend NODE's outline path to TITLE.
For use as `org-node-affixation-fn'."
  (list title
        (if-let ((fontified-ancestors
                  (cl-loop
                   for anc in (org-node-get-olp-full node t)
                   collect (propertize anc 'face 'completions-annotations))))
            (concat (string-join fontified-ancestors " > ") " > ")
          "")
        ""))

(defun org-node-affix-with-olp-and-tags (node title)
  "Prepend NODE's outline path to TITLE, and append NODE's tags.
For use as `org-node-affixation-fn'."
  (let ((prefix-len 0))
    (list title
          (if (org-node-get-is-subtree node)
              (let ((ancestors (org-node-get-olp-full node t))
                    (result nil))
                (dolist (anc ancestors)
                  (push (propertize anc 'face 'completions-annotations) result)
                  (push " > " result))
                (setq result (apply #'concat (nreverse result)))
                (setq prefix-len (length result))
                result)
            "")
          (let ((tags (org-node-get-tags node)))
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

(defvar org-node--title<>affixation-triplet (make-hash-table :test #'equal)
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
`org-node--title<>id', runs `org-node-affixation-fn' on each, and
depending on the user option `org-node-alter-candidates' it
either saves the affixed thing directly into
`org-node--candidate<>node' or into a secondary table
`org-node--title<>affixation-triplet'.  Finally, this function
then either simply reads candidates off the candidates table, or
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

;; Boost this completion hist to at least 1000 elements, unless user has nerfed
;; the global `history-length'.
(and (>= history-length (car (get 'history-length 'standard-value)))
     (< history-length 1000)
     (put 'org-node-hist 'history-length 1000))


;;;; The metadata struct

(cl-defstruct (org-node (:constructor org-node--make-obj)
                        (:copier nil)
                        (:conc-name org-node-get-))
  "An org-node object holds information about an Org ID node.
By the term 'Org ID node', we mean either a subtree with
an ID property, or a file with a file-level ID property.
The information is stored in fields listed above.

For each field, there exists a getter function \"org-node-get-FIELD\".
For example, field \"deadline\" has a getter `org-node-get-deadline'.
Given a NODE object, you would type this to get the deadline:

   (org-node-get-deadline NODE)

To list all existing node objects, you can eval:

   (hash-table-values org-nodes)

For examples of real-world usage, see
the documentation of `org-node-filter-fn'
or the README available as Info node `(org-node)'."
  (aliases    nil :read-only t :type list :documentation
              "List of ROAM_ALIASES registered on the node.")
  (deadline   nil :read-only t :type string :documentation
              "Node's DEADLINE state.")
  (file       nil :read-only t :type string :documentation
              "Truename of file where the node is.
Abbreviated per `abbreviate-file-name'.")
  (file-title nil :read-only t :type string :documentation
              "The #+title of the file where this node is. May be nil.
Rarely an useful value on its own, you may more often have use for
either `org-node-get-file-title-or-basename' or `org-node-get-title'.")
  (id         nil :read-only t :type string :documentation
              "Node's ID property.")
  (level      nil :read-only t :type integer :documentation
              "Amount of stars in the node heading. A file-level node has 0.
See also `org-node-get-is-subtree'.")
  (olp        nil :read-only t :type list :documentation
              "Outline path to this node, i.e. a list of ancestor headings.
Excludes file title.  To include it, try `org-node-get-olp-full'.")
  (pos        nil :read-only t :type integer :documentation
              "Char position of the node inside its file.
For a file-level node, always 1.
For a subtree node, the position of the first asterisk.")
  (priority   nil :read-only t :type string :documentation
              "Org priority state such as \"[#A]\".")
  (properties nil :read-only t :type alist :documentation
              "Alist of properties from the :PROPERTIES: drawer, verbatim.")
  (refs       nil :read-only t :type list :documentation
              "List of ROAM_REFS registered on the node.")
  (scheduled  nil :read-only t :type string :documentation
              "Node's SCHEDULED state.")
  (tags-local nil :read-only t :type list :documentation
              "List of tags local to the node.")
  (tags-inherited nil :read-only t :type list :documentation
                  "List of inherited tags.
See also `org-node-get-tags-with-inheritance'.")
  (title      nil :read-only t :type string :documentation
              "The node's heading, or #+title if it is a file-level node.
In the latter case, there is no difference from `file-title'.")
  (todo       nil :read-only t :type string :documentation
              "Node's TODO state."))

(defun org-node-get-tags (node)
  "Return NODE\\='s tags.

See also:
- `org-node-get-tags-local'
- `org-node-get-tags-with-inheritance'

This uses either of those two,
depending on the current value of `org-use-tag-inheritance'."
  (if org-use-tag-inheritance
      (org-node-get-tags-with-inheritance node)
    (org-node-get-tags-local node)))

(defun org-node-get-tags-with-inheritance (node)
  "Return all tags for NODE, local and inherited.
Also respect `org-tags-exclude-from-inheritance'."
  (delete-dups (append (org-node-get-tags-local node)
                       (org-node-get-tags-inherited node))))

(defun org-node-get-file-title-or-basename (node)
  "Return the #+title of file where NODE is, or file name if absent."
  (or (org-node-get-file-title node)
      (file-name-nondirectory (org-node-get-file node))))

(defun org-node-get-is-subtree (node)
  "Return t if NODE is a subtree instead of a file."
  (> (org-node-get-level node) 0))

(defun org-node-get-olp-full (node &optional filename-fallback)
  "Get outline path to NODE, and include the file title if present.
If FILENAME-FALLBACK is t, use the filename if title absent."
  (if (org-node-get-is-subtree node)
      (let ((top (if filename-fallback
                     (org-node-get-file-title-or-basename node)
                   (org-node-get-file-title node))))
        (if top
            (cons top (org-node-get-olp node))
          (org-node-get-olp node)))
    nil))

;; Should the names be shortened?
(defalias 'org-node-get-props #'org-node-get-properties)
;; (defalias 'org-node-get-prio #'org-node-get-priority)
;; (defalias 'org-node-get-sched #'org-node-get-scheduled)
;; (defalias 'org-node-get-lvl #'org-node-get-level)


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
  "1:1 table mapping ROAM_REFS members to the nearby ID property.
The exact form of such a member is determined by
`org-node-parser--split-refs-field'.")

(defvar org-node--ref-path<>ref-type (make-hash-table :test #'equal)
  "1:1 table mapping //paths to types:.

While the same path can be found with multiple types \(e.g. http and
https), this table will in that case store a random one of these, since
that is good enough to make completions look less outlandish.

This is a smaller table than you might think, since it only contains
entries for links found in a :ROAM_REFS: field, instead of all links
found anywhere.

To see all links found anywhere, try \\[org-node-list-reflinks].")

(defvar org-node--dest<>links (make-hash-table :test #'equal)
  "1:N table of links.

The table keys are destinations, i.e. values from a node's ID or
ROAM_REFS property.  In practice, that means a dest is an org-id, URI
path or a citekey.

For each table key, the corresponding table value is a list of
`org-node-link' records describing each link to that destination, with
info such as from which ID-node the link originates.

For more info see `org-node-get-id-links-to'.")

;; As of 2024-10-06, the MTIME is not used for anything except supporting
;; `org-node-fakeroam-db-feed-mode'.
;; However, `org-node-list-files' needs a hash table anyway for best perf,
;; else it could simply have reused `org-id-files'.
;; Additionally, the MTIME is often useful downstream.
(defvar org-node--file<>mtime (make-hash-table :test #'equal)
  "1:1 table mapping file paths to last-modification times.

The mtimes are expressed as integer Unix time.")

(defun org-node-get-id-links-to (node)
  "List all `org-node-link' objects of type \"id\" that point to NODE.
Each object has these fields:

origin - ID of origin node (where the link was found)
pos - buffer position where the link was found
dest - ID of destination node, or a ref that belongs to it
type - link type, such as \"https\", \"ftp\", \"info\" or
       \"man\".  For ID-links this is always \"id\".  For a
       citation this is always nil.

This function only returns ID-links, so you can always expect the dest
to equal the ID of the inputted NODE.  To return other link types, use
`org-node-get-reflinks-to'."
  (gethash (org-node-get-id node) org-node--dest<>links))

(defun org-node-get-reflinks-to (node)
  "Get list of reflink objects pointing to NODE.

Typical reflinks are URLs or @citekeys occurring in any document,
and they are considered to point to NODE when NODE has a
:ROAM_REFS: property that includes that same string.

The reflink object has the same shape as an ID-link object (see
`org-node-get-id-links-to'), but instead of an ID in the DEST field,
you have a ref string such an URL.  Common gotcha: for a web
address such as \"http://gnu.org\", the DEST field holds only
\"//gnu.org\", and the \"http\" part goes into the TYPE
field.  Colon is not stored anywhere.

Citations such as \"@gelman2001\" have TYPE nil, so you can
distinguish citations from other links this way."
  (cl-loop for ref in (org-node-get-refs node)
           append (gethash ref org-node--dest<>links)))

(defun org-node-peek (&optional ht)
  "Print some random values of table `org-nodes'.
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
  (remove-hook 'org-mode-hook #'org-node-cache-mode) ;; Old install instruction
  (if org-node-cache-mode
      (progn
        (add-hook 'org-node-creation-hook         #'org-node--dirty-ensure-node-known -50)
        (add-hook 'org-node-insert-link-hook      #'org-node--dirty-ensure-link-known -50)
        (add-hook 'org-roam-post-node-insert-hook #'org-node--dirty-ensure-link-known -50)
        (advice-add 'org-insert-link :after       #'org-node--dirty-ensure-link-known)
        (add-hook 'window-buffer-change-functions #'org-node--kill-blank-unsaved-buffers)
        (add-hook 'after-save-hook                #'org-node--handle-save)
        (advice-add 'rename-file :after           #'org-node--handle-rename)
        (advice-add 'delete-file :after           #'org-node--handle-delete)
        (org-node-cache-ensure 'must-async t)
        (org-node--maybe-adjust-idle-timer))
    (cancel-timer org-node--idle-timer)
    (remove-hook 'org-node-creation-hook          #'org-node--dirty-ensure-node-known)
    (remove-hook 'org-node-insert-link-hook       #'org-node--dirty-ensure-link-known)
    (remove-hook 'org-roam-post-node-insert-hook  #'org-node--dirty-ensure-link-known)
    (advice-remove 'org-insert-link               #'org-node--dirty-ensure-link-known)
    (remove-hook 'window-buffer-change-functions  #'org-node--kill-blank-unsaved-buffers)
    (remove-hook 'after-save-hook                 #'org-node--handle-save)
    (advice-remove 'rename-file                   #'org-node--handle-rename)
    (advice-remove 'delete-file                   #'org-node--handle-delete)))

(defun org-node--tramp-file-p (file)
  "Pass FILE to `tramp-tramp-file-p' if Tramp is loaded."
  (when (featurep 'tramp)
    (tramp-tramp-file-p file)))

(defun org-node--handle-rename (file newname &rest _)
  "Arrange to scan NEWNAME for nodes and links, and forget FILE."
  (org-node--scan-targeted
   (thread-last (list file newname)
                (seq-filter (##string-suffix-p ".org" %))
                (seq-remove #'backup-file-name-p)
                (seq-remove #'org-node--tramp-file-p)
                (mapcar #'file-truename)
                (org-node-abbrev-file-names))))

(defun org-node--handle-delete (file &rest _)
  "Arrange to forget nodes and links in FILE."
  (when (string-suffix-p ".org" file)
    (unless (org-node--tramp-file-p file)
      (org-node--scan-targeted
       (org-node-abbrev-file-names (file-truename file))))))

(defun org-node--handle-save ()
  "Arrange to re-scan nodes and links in current buffer."
  (when (and (string-suffix-p ".org" buffer-file-truename)
             (not (backup-file-name-p buffer-file-truename))
             (not (org-node--tramp-file-p buffer-file-truename)))
    (org-node--scan-targeted buffer-file-truename)))

(defvar org-node--idle-timer (timer-create)
  "Timer for intermittently checking `org-node-extra-id-dirs'.
for new, changed or deleted files, then resetting the cache.

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
              ;; Don't enter an infinite loop -- idle timers can be a footgun.
              (not (> (float-time (or (current-idle-time) 0))
                      new-delay)))
      (cancel-timer org-node--idle-timer)
      (setq org-node--idle-timer
            (run-with-idle-timer new-delay t #'org-node--scan-all)))))

;; FIXME: The idle timer will detect new files appearing, created by other
;;        emacsen, but won't run the hook `org-node-rescan-functions' on them,
;;        which would be good to do.  So check for new files and then try to
;;        use `org-node--scan-targeted', since that runs the hook, but it is
;;        easy to imagine a pitfall where the list of new files is just all
;;        files, and then we do NOT want to run the hook.  So use a heuristic
;;        cutoff like 10 files.
;; (defun org-node--catch-unknown-modifications ()
;;   (let ((new (-difference (org-node-list-files) (org-node-list-files t)))))
;;   (if (> 10 )
;;       (org-node--scan-all)
;;     (org-node--scan-targeted))
;;   )

(defvar org-node--new-unsaved-buffers nil
  "List of buffers created to hold a new node.")

(defun org-node--kill-blank-unsaved-buffers (&rest _)
  "Kill buffers created by org-node that have become blank.
Only applicable if the buffer\\='s file had not yet been written to
disk, and the buffer is unmodified.

This exists to allow you to create a node, especially a journal note for
today, change your mind, do an `undo' to empty the buffer, then browse
to the previous day\\='s note.  When later you want to create today\\='s
note after all, the sequence\\='s :creator function should be made to
run again, but will only do so if the buffer has been properly deleted
since, thus this hook."
  (unless (minibufferp)
    (dolist (buf org-node--new-unsaved-buffers)
      (if (or (not (buffer-live-p buf))
              (file-exists-p (buffer-file-name buf)))
          ;; Stop checking the buffer
          (setq org-node--new-unsaved-buffers
                (delq buf org-node--new-unsaved-buffers))
        (with-current-buffer buf
          (when (and (not (get-buffer-window buf t)) ;; Buffer is not visible
                     (string-blank-p (buffer-string))
                     (not (buffer-modified-p)))
            (when buffer-auto-save-file-name
              ;; Hopefully throw away a stale autosave
              (do-auto-save nil t))
            (org-node--dirty-forget-completions-in (list buffer-file-truename))
            (org-node--dirty-forget-files (list buffer-file-truename))
            (kill-buffer buf)))))))

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
  (when force
    ;; Launch the async processes
    (org-node--scan-all))
  (when (eq t synchronous)
    ;; Block until all processes finish
    (if org-node-cache-mode
        (el-job-await 'org-node 9 "org-node caching...")
      (el-job-await 'org-node 9 "org-node caching... (Hint: Avoid this hang by enabling org-node-cache-mode early)"))))

;; BUG: A heisenbug lurks inside (or is revealed by) org-id.
;; https://emacs.stackexchange.com/questions/81794/
;; When it appears, backtrace will show this, which makes no sense -- it's
;; clearly called on a list:
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
- Wipe `org-id-locations' if it appears afflicted by a known bug that
  makes the symbol value an indeterminate superposition of one of two
  possible values \(a hash table or an alist) depending on which code
  accesses it -- like Schrödinger\\='s cat -- and tell the user to
  rebuild the value, since even org-id\\='s internal functions are
  unable to fix it."
  (require 'org-id)
  (when (not org-id-track-globally)
    (user-error "Org-node requires `org-id-track-globally'"))
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
         (concat
          "No org-ids found.  If this was unexpected, try M-x `org-id-update-id-locations' or M-x `org-roam-update-org-id-locations'.
\tIf this is your first time using org-id, first assign an ID to some
\trandom heading with M-x `org-id-get-create', so that at least one exists
\ton disk, then do M-x `org-node-reset' and it should work from then on."))))))

(define-advice org-id-locations-load
    (:after () org-node--abbrev-org-id-locations)
  "Maybe abbreviate all filenames in `org-id-locations'.

Due to an oversight, org-id does not abbreviate after reconstructing
filenames if `org-id-locations-file-relative' is t.

https://lists.gnu.org/archive/html/emacs-orgmode/2024-09/msg00305.html"
  (when org-id-locations-file-relative
    (maphash (lambda (id file)
               (puthash id (org-node-abbrev-file-names file) org-id-locations))
             org-id-locations)))


;;;; Scanning

(defvar org-node--time-at-begin-full-scan nil)
(defun org-node--scan-all ()
  "Arrange a full scan."
  (unless (el-job-is-busy 'org-node)
    (setq org-node--time-at-begin-full-scan (time-convert nil t))
    (el-job-launch
     :id 'org-node
     :if-busy 'noop
     :inject-vars (append org-node-inject-variables (org-node--mk-work-vars))
     :load 'org-node-parser
     :funcall #'org-node-parser--collect-dangerously
     :inputs #'org-node-list-files
     :wrapup #'org-node--finalize-full)))

(defun org-node--scan-targeted (files)
  "Arrange to scan FILES."
  (when files
    (el-job-launch
     :id 'org-node-targeted
     :method 'reap
     :if-busy 'wait
     :skip-benchmark t
     :inject-vars (append org-node-inject-variables (org-node--mk-work-vars))
     :load 'org-node-parser
     :funcall #'org-node-parser--collect-dangerously
     :inputs (ensure-list files)
     :wrapup #'org-node--finalize-modified)))

(defun org-node--mk-work-vars ()
  "Return an alist of symbols and values to set in subprocesses."
  (let ((reduced-plain-re (org-node--mk-plain-re org-node-link-types)))
    (list
     ;; NOTE: The $sigil-prefixed names visually distinguish these
     ;; variables in the body of `org-node-parser--collect-dangerously'.
     (cons '$plain-re reduced-plain-re)
     (cons '$merged-re (concat org-link-bracket-re "\\|" reduced-plain-re))
     (cons '$assume-coding-system org-node-perf-assume-coding-system)
     (cons '$inlinetask-min-level (bound-and-true-p org-inlinetask-min-level))
     (cons '$nonheritable-tags org-tags-exclude-from-inheritance)
     (cons '$file-todo-option-re
           (rx bol (* space) (or "#+todo: " "#+seq_todo: " "#+typ_todo: ")))
     (cons '$global-todo-re
           (let ((default (default-value 'org-todo-keywords)))
             (org-node-parser--make-todo-regexp
              (string-join (if (stringp (car default))
                               default
                             (apply #'append (mapcar #'cdr default)))
                           " "))))
     (cons '$file-name-handler-alist
           (cl-remove-if-not (##memq % org-node-perf-keep-file-name-handlers)
                             file-name-handler-alist
                             :key #'cdr))
     (cons '$backlink-drawer-re
           (concat "^[\t\s]*:"
                   (or (and (require 'org-super-links nil t)
                            (boundp 'org-super-links-backlink-into-drawer)
                            (stringp org-super-links-backlink-into-drawer)
                            org-super-links-backlink-into-drawer)
                       "backlinks")
                   ":")))))

;; Copied from part of `org-link-make-regexps'
(defun org-node--mk-plain-re (link-types)
  "Build a moral equivalent to `org-link-plain-re'.
Make it target only LINK-TYPES instead of all the cars of
`org-link-parameters'."
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
	   (regexp ,(regexp-opt link-types t))
	   ":"
           (group
	    (1+ (or (regex ,non-space-bracket)
		    ,parenthesis))
	    (or (regexp "[^[:punct:][:space:]\n]")
                ?- ?/ ,parenthesis))))))


;;;; Scan-finalizers

(defvar org-node--first-init t
  "Non-nil until org-node has been initialized, then nil.
Mainly for muffling some messages.")

(defvar org-node-before-update-tables-hook nil
  "Hook run just before processing results from scan.")

(defun org-node--finalize-full (results job)
  "Wipe tables and repopulate from data in RESULTS.
JOB is the el-job object."
  (run-hooks 'org-node-before-update-tables-hook)
  (clrhash org-node--id<>node)
  (clrhash org-node--dest<>links)
  (clrhash org-node--candidate<>node)
  (clrhash org-node--title<>id)
  (clrhash org-node--ref<>id)
  (clrhash org-node--file<>mtime)
  (setq org-node--collisions nil) ;; To be populated by `org-node--record-nodes'
  (seq-let (missing-files file.mtime nodes path.type links problems) results
    (org-node--forget-id-locations missing-files)
    (dolist (link links)
      (push link (gethash (plist-get link :dest) org-node--dest<>links)))
    (cl-loop for (path . type) in path.type
             do (puthash path type org-node--ref-path<>ref-type))
    (cl-loop for (file . mtime) in file.mtime
             do (puthash file mtime org-node--file<>mtime))
    ;; Update `org-id-files' even though it makes no difference for org-id,
    ;; because downstream uses may assume that the variable reflects reality.
    (setq org-id-files (mapcar #'car file.mtime))
    (org-node--record-nodes nodes)
    (run-hooks 'org-node--mid-scan-hook)
    (setq org-node--time-elapsed
          ;; For more reproducible profiling: don't count time spent on
          ;; other sentinels, timers or I/O in between these periods
          (float-time
           (time-add
            (time-subtract (time-convert nil t)
                           (plist-get (el-job:timestamps job) :got-all-results))
            (time-subtract (plist-get (el-job:timestamps job) :children-done)
                           org-node--time-at-begin-full-scan))))
    (org-node--maybe-adjust-idle-timer)
    (while-let ((fn (pop org-node--temp-extra-fns)))
      (funcall fn))
    (when (and org-node--collisions org-node-warn-title-collisions)
      (message "Some nodes share title, see M-x org-node-list-collisions"))
    (when (setq org-node--problems problems)
      (message "Scan had problems, see M-x org-node-list-scan-problems"))
    (setq org-node--first-init nil)))

(defvar org-node--mid-scan-hook nil
  "Hook run after tables were updated but before the final timestamp.")

(defvar org-node--old-link-sets nil
  "For use by `org-node-backlink-aggressive'.

Alist of ((DEST . LINKS) (DEST . LINKS) ...), where LINKS is are sets of
links with destination DEST.  These reflect a past state of
`org-node--dest<>links', allowing for a diff operation against the
up-to-date set.")

(defun org-node--finalize-modified (results job)
  "Use RESULTS to update tables.
Argument JOB is the el-job object."
  (run-hooks 'org-node-before-update-tables-hook)
  (seq-let (missing-files file.mtime nodes path.type links problems) results
    (let ((found-files (mapcar #'car file.mtime)))
      (org-node--forget-id-locations missing-files)
      (dolist (file missing-files)
        (remhash file org-node--file<>mtime))
      (org-node--dirty-forget-files missing-files)
      (org-node--dirty-forget-completions-in missing-files)
      ;; In case a title was edited: don't persist old revisions of the title
      (org-node--dirty-forget-completions-in found-files)
      (setq org-node--old-link-sets nil)
      (when org-node-perf-eagerly-update-link-tables
        (cl-loop with ids-of-nodes-scanned = (cl-loop
                                              for node in nodes
                                              collect (org-node-get-id node))
                 with reduced-link-sets = nil
                 for dest being each hash-key of org-node--dest<>links
                 using (hash-values link-set)
                 do (cl-loop
                     with update-this-dest = nil
                     for link in link-set
                     if (member (plist-get link :origin)
                                ids-of-nodes-scanned)
                     do (setq update-this-dest t)
                     else collect link into reduced-link-set
                     finally do
                     (when update-this-dest
                       (push (cons dest reduced-link-set) reduced-link-sets)))
                 finally do
                 (cl-loop
                  for (dest . links) in reduced-link-sets do
                  (when (bound-and-true-p org-node-backlink-aggressive)
                    (push (cons dest (gethash dest org-node--dest<>links))
                          org-node--old-link-sets))
                  (puthash dest links org-node--dest<>links))))
      ;; Having discarded the links that were known to originate in the
      ;; re-scanned nodes, it's safe to record them (again).
      (dolist (link links)
        (push link (gethash (plist-get link :dest) org-node--dest<>links)))
      (cl-loop for (path . type) in path.type
               do (puthash path type org-node--ref-path<>ref-type))
      (cl-loop for (file . mtime) in file.mtime
               do (puthash file mtime org-node--file<>mtime))
      (org-node--record-nodes nodes)
      (while-let ((fn (pop org-node--temp-extra-fns)))
        (funcall fn job))
      (dolist (prob problems)
        (push prob org-node--problems))
      (when problems
        (message "Scan had problems, see M-x org-node-list-scan-problems"))
      (run-hook-with-args 'org-node-rescan-functions
                          (append missing-files found-files)))))

(defun org-node--record-nodes (nodes)
  "Add NODES to `org-nodes' and related info to other tables."
  (let ((affixator (org-node--try-ensure-compiled org-node-affixation-fn))
        (filterer (org-node--try-ensure-compiled org-node-filter-fn)))
    (dolist (node nodes)
      (let* ((id (org-node-get-id node))
             (path (org-node-get-file node))
             (refs (org-node-get-refs node)))
        ;; Share location with org-id & do so with manual `puthash'
        ;; because `org-id-add-location' would run logic we've already run
        (puthash id path org-id-locations)
        ;; Register the node
        (puthash id node org-node--id<>node)
        (dolist (ref refs)
          (puthash ref id org-node--ref<>id))
        ;; Setup completion candidates
        (when (funcall filterer node)
          ;; Let refs work as aliases
          (dolist (ref refs)
            (puthash ref node org-node--candidate<>node)
            (puthash ref
                     (let ((type (gethash ref org-node--ref-path<>ref-type)))
                       (list (propertize ref 'face 'org-cite)
                             (when type
                               (propertize (concat type ":")
                                           'face 'completions-annotations))
                             nil))
                     org-node--title<>affixation-triplet))
          (dolist (title (cons (org-node-get-title node)
                               (org-node-get-aliases node)))
            (let ((collision (gethash title org-node--title<>id)))
              (when (and collision (not (equal id collision)))
                (push (list title id collision) org-node--collisions)))
            (puthash title id org-node--title<>id)
            (let ((affx (funcall affixator node title)))
              (if org-node-alter-candidates
                  ;; Absorb the affixations into one candidate string
                  (puthash (concat (nth 1 affx) (nth 0 affx) (nth 2 affx))
                           node
                           org-node--candidate<>node)
                ;; Bare title as candidate, to be affixated in realtime by
                ;; `org-node-collection'
                (puthash title affx org-node--title<>affixation-triplet)
                (puthash title node org-node--candidate<>node)))))))))

(defvar org-node--time-elapsed 1.0
  "Duration of the last cache reset.")

(defun org-node--print-elapsed (&rest _)
  "Print time elapsed since start of `org-node--scan-all'.
Also report statistics about the nodes and links found.

Currently, the printed message implies that all of org-node\\='s
data were collected within the time elapsed, so you should not
run this function after only a partial scan, as the message would
be misleading."
  (if (not org-node-cache-mode)
      (message "Scan complete (Hint: Turn on org-node-cache-mode)")
    (let ((n-subtrees (cl-loop
                       for node being each hash-value of org-node--id<>node
                       count (org-node-get-is-subtree node)))
          (n-backlinks (cl-loop
                        for id being each hash-key of org-node--id<>node
                        sum (length (gethash id org-node--dest<>links))))
          (n-reflinks (cl-loop
                       for ref being each hash-key of org-node--ref<>id
                       sum (length (gethash ref org-node--dest<>links)))))
      (message "Saw %d file-nodes, %d subtree-nodes, %d ID-links, %d reflinks in %.2fs CPU-time (%.2fs wall-time)"
               (- (hash-table-count org-node--id<>node) n-subtrees)
               n-subtrees
               n-backlinks
               n-reflinks
               org-node--time-elapsed
               (float-time (time-since org-node--time-at-begin-full-scan))))))

(defvar org-node--compile-timers nil)
(defvar org-node--compiled-lambdas (make-hash-table :test #'equal)
  "1:1 table mapping lambda expressions to compiled bytecode.")

(defun org-node--try-ensure-compiled (fn)
  "Try to return FN as a compiled function.

- If FN is a symbol with uncompiled function definition, return
  the same symbol and arrange to natively compile it after some
  idle time.

  If this Emacs does not support native compilation, byte-compile the
  symbol right away.

- If FN is an anonymous lambda, compile it, cache the resulting
  bytecode, and return that bytecode."
  (cond ((compiled-function-p fn) fn)
        ((symbolp fn)
         (if (compiled-function-p (symbol-function fn))
             fn
           (if (native-comp-available-p)
               (unless (alist-get fn org-node--compile-timers)
                 (setf (alist-get fn org-node--compile-timers)
                       (run-with-idle-timer
                        (+ 5 (random 5)) nil #'native-compile fn)))
             (byte-compile fn))
           ;; May remain uncompiled until native comp is done
           fn))
        ((gethash fn org-node--compiled-lambdas))
        ((puthash fn (byte-compile fn) org-node--compiled-lambdas))))


;;;; "Dirty" functions
;; Help keep the cache reasonably in sync without having to do a full reset

;; See `org-node--finalize-modified' for forgetting links
(defun org-node--dirty-forget-files (files)
  "Remove from cache info about nodes/refs in FILES.
You should also run `org-node--dirty-forget-completions-in' for a
thorough cleanup."
  (when files
    (cl-loop
     for node being each hash-value of org-node--id<>node
     when (member (org-node-get-file node) files)
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
     for candidate being each hash-key of org-node--candidate<>node
     using (hash-values node)
     when (member (org-node-get-file node) files)
     do (remhash candidate org-node--candidate<>node))))

(defun org-node--dirty-ensure-link-known (&optional id &rest _)
  "Record the ID-link at point.
If optional argument ID is non-nil, do not check the link at
point but assume it is a link to ID."
  (when (derived-mode-p 'org-mode)
    (org-node--init-ids)
    (when-let ((origin (org-entry-get-with-inheritance "ID"))
               (dest (if (gethash id org-id-locations)
                         id
                       (let ((elm (org-element-context)))
                         (when (equal "id" (org-element-property :type elm))
                           (org-element-property :path elm))))))
      (push (list :origin origin
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
  (let ((id (org-entry-get-with-inheritance "ID"))
        (case-fold-search t))
    (unless (gethash id org-node--id<>node)
      (unless (gethash buffer-file-truename org-node--file<>mtime)
        (when (file-exists-p buffer-file-truename)
          (puthash buffer-file-truename (cons 0 0) org-node--file<>mtime)))
      (save-excursion
        (without-restriction
          (goto-char (point-min))
          (re-search-forward (concat "^[\t\s]*:id: +" id))
          (let ((props (org-entry-properties))
                (heading (org-get-heading t t t t))
                (fpath buffer-file-truename) ;; Abbreviated
                (ftitle (cadar (org-collect-keywords '("TITLE")))))
            (when heading
              (setq heading (substring-no-properties heading)))
            (org-node--record-nodes
             (list
              (org-node--make-obj
               :title (or heading ftitle)
               :id id
               :file fpath
               :file-title ftitle
               :aliases (split-string-and-unquote
                         (or (cdr (assoc "ROAM_ALIASES" props)) ""))
               :refs (org-node-parser--split-refs-field
                      (cdr (assoc "ROAM_REFS" props)))
               :pos (if heading (org-entry-beginning-position) 1)
               ;; NOTE: Don't use `org-reduced-level' since org-node-parser.el
               ;;       also does not correct for that
               :level (or (org-current-level) 0)
               :olp (org-get-outline-path)
               ;; Less important
               :properties props
               :tags-local (org-get-tags nil t)
               :tags-inherited (org-node--tags-at-point-inherited-only)
               :todo (when heading (org-get-todo-state))
               :deadline (cdr (assoc "DEADLINE" props))
               :scheduled (cdr (assoc "SCHEDULED" props)))))))))))


;;;; Etc

(defun org-node--tags-at-point-inherited-only ()
  "Like `org-get-tags', but get only the inherited tags.
Respects `org-tags-exclude-from-inheritance'."
  (let ((all-tags (if org-use-tag-inheritance
                      ;; NOTE: Above variable can have complex rules
                      (org-get-tags)
                    (let ((org-use-tag-inheritance t)
                          (org-trust-scanner-tags nil))
                      (org-get-tags)))))
    (cl-loop for tag in all-tags
             when (get-text-property 0 'inherited tag)
             collect (substring-no-properties tag))))

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
  "List Org files in `org-id-locations' and `org-node-extra-id-dirs'.

With optional argument INSTANT t, return already-known files
instead of checking the filesystem again.

When called interactively \(automatically making INTERACTIVE non-nil),
display the list of files in a new buffer."
  (interactive "i\np")
  (if interactive
      ;; TODO: Make something like a find-dired buffer instead, handy!
      (org-node--pop-to-tabulated-list
       :buffer "*org-node files*"
       :format [("File" 0 t)]
       :entries (cl-loop
                 for file in (org-node-list-files)
                 collect (list file (vector
                                     (list file
                                           'action `(lambda (_button)
                                                      (find-file ,file))
                                           'face 'link
                                           'follow-link t)))))
    (when (stringp org-node-extra-id-dirs)
      (setq org-node-extra-id-dirs (list org-node-extra-id-dirs))
      (message
       "Option `org-node-extra-id-dirs' must be a list, changed it for you"))

    (when (or (not instant)
              (hash-table-empty-p org-node--file<>mtime))
      (let* ((file-name-handler-alist nil)
             (dirs-to-scan (delete-dups
                            (mapcar #'file-truename org-node-extra-id-dirs))))
        (cl-loop for file in (org-node-abbrev-file-names
                              (cl-loop for dir in dirs-to-scan
                                       nconc (org-node--dir-files-recursively
                                              dir
                                              ".org"
                                              org-node-extra-id-dirs-exclude)))
                 do (or (gethash file org-node--file<>mtime)
                        (puthash file 0 org-node--file<>mtime))))
      (org-node--init-ids)
      (cl-loop for file being each hash-value of org-id-locations
               do (or (gethash file org-node--file<>mtime)
                      (puthash file 0 org-node--file<>mtime))))
    (hash-table-keys org-node--file<>mtime)))

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

(defvar org-node--userhome nil)
(defun org-node-abbrev-file-names (paths)
  "Abbreviate all file paths in PATHS.
Faster than `abbreviate-file-name', especially if you would have to call
it on many file paths at once.

May in some corner-cases give different results.  For instance, it
disregards file name handlers, affecting TRAMP.

PATHS can be a single path or a list, and are presumed to be absolute.

It is a good idea to abbreviate a path when you don\\='t know where it
came from.  That helps ensure that it is comparable to a path provided
in either `org-id-locations' or an `org-node' object.

If the wild path may be a symlink or not an absolute path, it would be
safer to process it first with `file-truename', then pass the result to
this function.

Tip: the inexactly named buffer-local variable `buffer-file-truename'
already contains an abbreviated truename."
  (unless org-node--userhome
    (setq org-node--userhome (file-name-as-directory (expand-file-name "~"))))
  ;; Assume a case-sensitive filesystem.
  ;; REVIEW: Not sure if it fails gracefully on NTFS/FAT/HFS+/APFS.
  (let ((case-fold-search nil))
    (if (listp paths)
        (cl-loop
         for path in paths
         do (setq path (directory-abbrev-apply path))
         if (string-prefix-p org-node--userhome path)
         ;; REVIEW: Sane in single-user mode Linux?
         collect (concat "~" (substring path (1- (length org-node--userhome))))
         else collect path)
      (setq paths (directory-abbrev-apply paths))
      (if (string-prefix-p org-node--userhome paths)
          (concat "~" (substring paths (1- (length org-node--userhome))))
        paths))))

(defun org-node--forget-id-locations (files)
  "Remove references to FILES in `org-id-locations'.
You might consider committing the effect to disk afterwards by calling
`org-id-locations-save', which this function will not do for you.

FILES are assumed to be abbreviated truenames."
  (when files
    (when (listp org-id-locations)
      (message "org-node--forget-id-locations: Surprised that `org-id-locations' is an alist at this time.  Converting to hash table.")
      (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
    (maphash (lambda (id file)
               (when (member file files)
                 (remhash id org-id-locations)))
             org-id-locations)))


;;;; Filename functions

;; To benchmark:
;; (progn (byte-compile #'org-node--root-dirs) (benchmark-run 10 (org-node--root-dirs (hash-table-values org-id-locations))))
;; REVIEW: 75% of compute is in `file-name-directory', can that be improved?
(defun org-node--root-dirs (file-list)
  "Infer root directories of FILE-LIST.

By 'root', we mean the longest directory path common to a set of files,
as long as that directory contains at least one member of
FILE-LIST itself.  For example, if you have the 3 members

- \"/home/me/Syncthing/foo.org\"
- \"/home/kept/archive/bar.org\"
- \"/home/kept/baz.org\"

the return value will not be \(\"/home/\"), even though
the substring \"/home/\" is common to all of them,
but \(\"/home/kept/\" \"/home/me/Syncthing/\").


On finding more than one root, sort by count of files they contain
recursively, so that the most populous root directory will be the first
element.

This function does not consult the filesystem,
so FILE-LIST must be a list of full paths that can be compared.


For Org users, it is pragmatic to know that if FILE-LIST was the
output of something like

   \(hash-table-values org-id-locations)

this function will in many cases spit out a list of exactly one item
because many people keep their Org files in one root directory \(with
various subdirectories)."
  (let* ((files (seq-uniq file-list))
         (dirs (sort (delete-consecutive-dups
                      (sort (mapcar #'file-name-directory files) #'string<))
                     (##length< %1 (length %2))))
         root-dirs)
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child.
    (while-let ((dir (car (last dirs))))
      ;; REVIEW: Maybe more elegant to use `nreverse' twice
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
  "Whether to ask the user where to save a new file node.

- Symbol nil: put file in the most populous root directory in
              `org-id-locations' without asking
- String: a directory path in which to put the file
- Symbol t: ask every time

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
Example from Denote: %Y%m%dT%H%M%S--

For the rest of the filename, configure `org-node-slug-fn'."
  :type 'string)

(defcustom org-node-slug-fn #'org-node-slugify-for-web
  "Function taking a node title and returning a filename component.
Receives one argument: the value of an Org #+TITLE keyword, or
the first heading in a file that has no #+TITLE.

Built-in choices:
- `org-node-slugify-for-web'
- `org-node-slugify-like-roam-default'
- `org-node-fakeroam-slugify-via-roam'

It is popular to also prefix filenames with a datestamp.  To do
that, configure `org-node-datestamp-format'."
  :type '(radio
          (function-item org-node-slugify-for-web)
          (function-item org-node-slugify-like-roam-default)
          (function-item org-node-fakeroam-slugify-via-roam)
          (function :tag "Custom function")))

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
               (ucs-normalize-NFD-string)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (ucs-normalize-NFC-string)
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
               (ucs-normalize-NFD-string)
               (seq-remove (lambda (char) (<= #x300 char #x331)))
               (concat)
               (ucs-normalize-NFC-string)
               (downcase)
               (string-trim)
               (replace-regexp-in-string "[[:space:]]+" "-")
               (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
               (replace-regexp-in-string "\\/" "-")
               (replace-regexp-in-string "--*" "-")
               (replace-regexp-in-string "^-" "")
               (replace-regexp-in-string "-$" "")))

;; Useful test cases if you want to hack on the above!

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


;;;; How to create new nodes

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-sequence nil
  "Key that identifies a node sequence about to be added-to.
Automatically set, should be nil most of the time.")

(defun org-node--goto (node)
  "Visit NODE."
  (if node
      (let ((file (org-node-get-file node)))
        (if (file-exists-p file)
            (let ((pos (org-node-get-pos node)))
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
              (if (org-node-get-is-subtree node)
                  (unless (and (pos-visible-in-window-p pos)
                               (not (org-invisible-p pos))
                               (equal (org-node-get-title node)
                                      (org-get-heading t t t t)))
                    (goto-char pos)
                    (org-show-entry)
                    (org-show-children)
                    (recenter 0))
                (unless (pos-visible-in-window-p pos)
                  (goto-char pos))))
          (message "org-node: Didn't find file, resetting...")
          (push (lambda ()
                  (message "org-node: Didn't find file, resetting... done"))
                org-node--temp-extra-fns)
          (org-node--scan-all)))
    (error "`org-node--goto' received a nil argument")))

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
  (setq org-node-proposed-sequence seq-key)
  (unwind-protect
      (funcall org-node-creation-fn)
    (setq org-node-proposed-title nil)
    (setq org-node-proposed-id nil)
    (setq org-node-proposed-sequence nil)))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called to create a node that does not yet exist.
Used by commands such as `org-node-find'.

Some choices:
- `org-node-new-file'
- `org-node-fakeroam-new-via-roam-capture'
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
          (function-item org-node-fakeroam-new-via-roam-capture)
          (function-item org-capture)
          (function :tag "Custom function")))

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
      (push (current-buffer) org-node--new-unsaved-buffers)
      (run-hooks 'org-node-creation-hook))))

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
          (setq node (gethash title org-node--candidate<>node)))
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
          (find-file (org-node-get-file node))
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
        (push (current-buffer) org-node--new-unsaved-buffers)
        (run-hooks 'org-node-creation-hook)))))


;;;; Commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-node-creation-fn' to `org-node-fakeroam-new-via-roam-capture'."
  (interactive)
  (org-node-cache-ensure)
  (let* ((input (completing-read "Go to ID-node: " #'org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node--candidate<>node)))
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
  (org-node--goto (nth (random (hash-table-count org-node--candidate<>node))
                       (hash-table-values org-node--candidate<>node))))

;;;###autoload
(defun org-node-insert-link (&optional region-as-initial-input immediate)
  "Insert a link to one of your ID nodes.

To behave exactly like org-roam\\='s `org-roam-node-insert',
see `org-node-insert-link*' and its docstring.

Optional argument REGION-AS-INITIAL-INPUT t means behave as
`org-node-insert-link*'."
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
                            (try-completion region-text org-node--title<>id)))
                      region-text
                    nil))
         (input (if (and immediate initial)
                    initial
                  (completing-read "Node: " #'org-node-collection
                                   () () initial 'org-node-hist)))
         (node (gethash input org-node--candidate<>node))
         (id (if node (org-node-get-id node) (org-id-new)))
         (link-desc (or region-text
                        (and (not org-node-alter-candidates) input)
                        (and node (seq-find (##string-search % input)
                                            (org-node-get-aliases node)))
                        (and node (org-node-get-title node))
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

;; TODO
(defun org-node-insert-link*-immediate ()
  "Insert a link to one of your ID nodes immediately,
without opening a window or buffer for that node, even
if it was not yet created. (Not-yet-created nodes are just
created according to the defaults for `org-node-creation-fn'.)
Behaves otherwise exactly like `org-node-insert-link*'."
  (interactive "*" org-mode)
  (if (boundp 'org-roam-capture-templates)
      (let ((org-roam-capture-templates
             (list (append (car org-roam-capture-templates)
                           '(:immediate-finish t)))))
        (org-node-insert-link t t))))

;;;###autoload
(defun org-node-insert-transclusion (&optional node)
  "Insert a #+transclude: referring to a node."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
  (unless node
    (setq node (gethash (completing-read "Node: " #'org-node-collection
                                         () () () 'org-node-hist)
                        org-node--candidate<>node)))
  (let ((id (org-node-get-id node))
        (title (org-node-get-title node))
        (level (or (org-current-level) 0)))
    (insert (org-link-make-string (concat "id:" id) title))
    (goto-char (pos-bol))
    (insert "#+transclude: ")
    (goto-char (pos-eol))
    (insert " :level " (number-to-string (+ 1 level)))))

;;;###autoload
(defun org-node-insert-transclusion-as-subtree (&optional node)
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
  (unless node
    (setq node (gethash (completing-read "Node: " #'org-node-collection
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
    (run-hooks 'org-node-insert-link-hook)))

(defun org-node-insert-raw-link ()
  (interactive)
  (insert (completing-read "Insert raw link: "
                           (org-node--list-known-raw-links)
                           nil nil nil 'org-node-link-hist)))

;;;###autoload
(defun org-node-refile ()
  "Experimental."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command expects an org-mode buffer"))
  (org-node-cache-ensure)
  (when (org-invisible-p)
    (user-error "Better not run this command in an invisible region"))
  (let* ((input (completing-read "Refile into ID-node: " #'org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node--candidate<>node)))
    (unless node
      (error "Node not found %s" input))
    (org-back-to-heading t)
    (when (org-invisible-p) ;; IDK...
      (user-error "Better not run this command in an invisible region"))
    (org-cut-subtree)
    (org-node--goto node)
    (widen)
    (when (outline-next-heading)
      (backward-char 1))
    (org-paste-subtree)
    (org-node--dirty-ensure-node-known)))

;;;###autoload
(defun org-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created file
as current buffer.

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
creation-date as more \"truthful\" than today\\='s date.

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
        (if (org-at-heading-p) (org-show-entry) (org-show-context))
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
        (org-node--dirty-ensure-node-known)
        (push (current-buffer) org-node--new-unsaved-buffers)
        (run-hooks 'org-node-creation-hook)
        (when (bound-and-true-p org-node-backlink-mode)
          (org-node-backlink--fix-entry-here))))))

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
`org-node-datestamp-format', and avoid overwriting it.

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
            (or (cadar (org-collect-keywords '("TITLE")))
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

;; FIXME: Kill opened buffers.  First make sure it can pick up where it left
;;        off.  Maybe use `org-node--in-files-do'.
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
      (dolist (file (or files (org-node-list-files t)))
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
    (let ((root (car (org-node--root-dirs (org-node-list-files))))
          (default-directory default-directory))
      (or (equal default-directory root)
          (if (y-or-n-p (format "Go to folder \"%s\"?" root))
              (setq default-directory root)
            (setq default-directory
                  (read-directory-name
                   "Directory with Org notes to operate on: "))))
      (when-let ((bufs (seq-filter (##string-search "*grep*" (buffer-name %))
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
  (org-node-cache-ensure)
  (org-id-get-create)
  (run-hooks 'org-node-creation-hook))

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive "*" org-mode)
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
  (let ((files
         (org-node-abbrev-file-names
          (nconc
           (org-node--dir-files-recursively (file-truename dir) ".org_exclude" nil)
           (org-node--dir-files-recursively (file-truename dir) ".org" nil)))))
    (when files
      (message "Forgetting all IDs in directory %s..." dir)
      (redisplay)
      (org-node--forget-id-locations files)
      (dolist (file files)
        (remhash file org-node--file<>mtime))
      (org-id-locations-save)
      (org-node-reset))))

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
          (consult--grep "Grep in all known Org files: "
                         #'consult--ripgrep-make-builder
                         (org-node--root-dirs (org-node-list-files t))
                         nil)
        ;; Much slower!  Vanilla grep does not have Ripgrep's --type=org, so
        ;; must target thousands of files and not a handful of dirs.
        (consult--grep "Grep in all known Org files: "
                       #'consult--grep-make-builder
                       (org-node-list-files)
                       nil)))))

(defvar org-node--unlinted nil)
(defvar org-node--lint-warnings nil)
(defun org-node-lint-all ()
  "Run `org-lint' on all known Org files, and report results.

If last run was interrupted, resume working through the file list
from where it stopped.  With prefix argument, start over
from the beginning."
  (interactive)
  (require 'org-lint)
  (org-node--init-ids)
  (when (or (equal current-prefix-arg '(4))
            (and (null org-node--unlinted)
                 (y-or-n-p (format "Lint %d files?"
                                   (length (org-node-list-files t))))))
    (setq org-node--unlinted (org-node-list-files t))
    (setq org-node--lint-warnings nil))
  (setq org-node--unlinted
        (org-node--in-files-do
          :files org-node--unlinted
          :msg "Running org-lint (you may quit and resume anytime)"
          :about-to-do "About to visit a file to run org-lint"
          :call (lambda ()
                  (when-let ((warning (org-lint)))
                    (push (cons buffer-file-name (car warning))
                          org-node--lint-warnings)))))
  (when org-node--lint-warnings
    (org-node--pop-to-tabulated-list
     :buffer "*org lint results*"
     :format [("File" 30 t) ("Line" 5 t) ("Trust" 5 t) ("Explanation" 0 t)]
     :reverter #'org-node-lint-all
     :entries (cl-loop
               for (file . warning) in org-node--lint-warnings
               collect (let ((array (cadr warning)))
                         (list warning
                               (vector
                                (list (file-name-nondirectory file)
                                      'face 'link
                                      'action `(lambda (_button)
                                                 (find-file ,file)
                                                 (goto-line ,(string-to-number
                                                              (elt array 0))))
                                      'follow-link t)
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
  (let* ((r-code "library(stringr)
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
      (org-node--pop-to-tabulated-list
       :buffer "*org-node feedback arcs*"
       :format [("Node containing link" 39 t) ("Target of link" 0 t)]
       :entries (cl-loop
                 for (origin . dest) in feedbacks
                 as origin-node = (gethash origin org-nodes)
                 as dest-node = (gethash dest org-nodes)
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
                                     'follow-link t))))))))

;; TODO: Temp merge all refs into corresponding ID
(defun org-node--make-digraph-tsv-string ()
  "Generate a string in Tab-Separated Values form.
The string is a 2-column table of destination-origin pairs, made
from ID links found in `org-node--dest<>links'."
  (concat
   "src\tdest\n"
   (string-join
    (seq-uniq (cl-loop
               for dest being each hash-key of org-node--dest<>links
               using (hash-values links)
               append (cl-loop
                       for link in links
                       when (equal "id" (plist-get link :type))
                       collect (concat dest "\t" (plist-get link :origin)))))
    "\n")))

(cl-defun org-node--pop-to-tabulated-list (&key buffer format entries reverter)
  "Create, populate and display a `tabulated-list-mode' buffer.

BUFFER is a buffer or buffer name where the list should be created.
FORMAT is the value to which `tabulated-list-format' should be set.
ENTRIES is the value to which `tabulated-list-entries' should be set.

Optional argument REVERTER is a function to add buffer-locally to
`tabulated-list-revert-hook'."
  (unless (and buffer format)
    (user-error
     "org-node--pop-to-tabulated-list: Mandatory arguments are buffer, format, entries"))
  (when (null entries)
    (message "No entries to tabulate"))
  (pop-to-buffer (get-buffer-create buffer))
  (tabulated-list-mode)
  (setq tabulated-list-format format)
  (tabulated-list-init-header)
  (setq tabulated-list-entries entries)
  (when reverter (add-hook 'tabulated-list-revert-hook reverter nil t))
  (tabulated-list-print t))

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
                                   (length (org-node-list-files t))))))
    (setq org-node--list-file-coding-systems-files (org-node-list-files t))
    (setq org-node--found-coding-systems nil))
  (setq org-node--list-file-coding-systems-files
        (org-node--in-files-do
          :files org-node--list-file-coding-systems-files
          :fundamental-mode t
          :msg "Checking file coding systems (quit and resume anytime)"
          :about-to-do "About to check file coding system"
          :call (lambda ()
                  (push (cons buffer-file-name buffer-file-coding-system)
                        org-node--found-coding-systems))))
  (org-node--pop-to-tabulated-list
   :buffer "*org file coding systems*"
   :format [("Coding system" 20 t) ("File" 40 t)]
   :entries (cl-loop for (file . sys) in org-node--found-coding-systems
                     collect (list file (vector (symbol-name sys) file)))
   :reverter #'org-node-list-file-coding-systems))

(defun org-node-list-dead-links ()
  "List links that lead to no known ID."
  (interactive)
  (let ((dead-links
         (cl-loop for dest being each hash-key of org-node--dest<>links
                  using (hash-values links)
                  unless (gethash dest org-nodes)
                  append (cl-loop for link in links
                                  when (equal "id" (plist-get link :type))
                                  collect (cons dest link)))))
    (message "%d dead links found" (length dead-links))
    (when dead-links
      (org-node--pop-to-tabulated-list
       :buffer "*dead links*"
       :format [("Location" 40 t) ("Unknown ID reference" 40 t)]
       :reverter #'org-node-list-dead-links
       :entries
       (cl-loop
        for (dest . link) in dead-links
        as origin-node = (gethash (plist-get link :origin)
                                  org-nodes)
        if (not (equal dest (plist-get link :dest)))
        do (error "IDs not equal: %s, %s" dest (plist-get link :dest))
        else if (not origin-node)
        do (error "Node not found for ID: %s" (plist-get link :origin))
        else collect
        (list link
              (vector
               (list (org-node-get-title origin-node)
                     'face 'link
                     'action `(lambda (_button)
                                (org-node--goto ,origin-node)
                                (goto-char ,(plist-get link :pos)))
                     'follow-link t)
               dest)))))))

(defun org-node-list-reflinks ()
  "List all reflinks and their locations.

Useful to see how many times you\\='ve inserted a link that is very
similar to another link, but not identical, so that likely only
one of them is associated with a ROAM_REFS property."
  (interactive)
  (let* ((link-objects-excluding-id-type
          (cl-loop
           for list being each hash-value of org-node--dest<>links
           append (cl-loop
                   for LINK in list
                   unless (equal "id" (plist-get LINK :type))
                   collect LINK)))
         (entries
          (cl-loop
           for LINK in link-objects-excluding-id-type
           collect (pcase-let (((map :origin :pos :type :dest) LINK))
                     (let ((node (gethash origin org-nodes)))
                       (list LINK
                             (vector
                              (if (gethash dest org-node--ref<>id) "*" "")
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
                              (if type (concat type ":" dest) dest))))))))
    (if entries
        (org-node--pop-to-tabulated-list
         :buffer "*org-node reflinks*"
         :format [("Ref" 4 t) ("Inside node" 30 t) ("Link" 0 t)]
         :reverter #'org-node-list-reflinks
         :entries entries)
      (message "No links found"))))

(defcustom org-node-warn-title-collisions t
  "Whether to print messages on finding duplicate node titles."
  :group 'org-node
  :type 'boolean)

(defvar org-node--collisions nil
  "Alist of node title collisions.")

(defun org-node-list-collisions ()
  "Pop up a buffer listing node title collisions."
  (interactive)
  (if org-node--collisions
      (org-node--pop-to-tabulated-list
       :buffer "*org-node title collisions*"
       :format [("Non-unique name" 30 t) ("ID" 37 t) ("Other ID" 0 t)]
       :reverter #'org-node-list-collisions
       :entries (cl-loop
                 for row in org-node--collisions
                 collect (seq-let (msg id1 id2) row
                           (list row
                                 (vector msg
                                         (list id1
                                               'action `(lambda (_button)
                                                          (org-id-goto ,id1))
                                               'face 'link
                                               'follow-link t)
                                         (list id2
                                               'action `(lambda (_button)
                                                          (org-id-goto ,id2))
                                               'face 'link
                                               'follow-link t))))))
    (message "Congratulations, no title collisions! (in %d filtered nodes)"
             (hash-table-count org-node--title<>id))))

(defvar org-node--problems nil
  "Alist of errors encountered by org-node-parser.")

(defun org-node-list-scan-problems ()
  "Pop up a buffer listing errors found by org-node-parser."
  (interactive)
  (if org-node--problems
      (org-node--pop-to-tabulated-list
       :buffer "*org-node scan problems*"
       :format [("Scan choked near position" 27 t) ("Issue (newest on top)" 0 t)]
       :reverter #'org-node-list-scan-problems
       :entries (cl-loop
                 for problem in org-node--problems
                 collect (seq-let (file pos signal) problem
                           (list problem
                                 (vector (list
                                          (format "%s:%d"
                                                  (file-name-nondirectory file) pos)
                                          'face 'link
                                          'action `(lambda (_button)
                                                     (find-file ,file)
                                                     (goto-char ,pos))
                                          'follow-link t)
                                         (format "%s" signal))))))
    (message "Congratulations, no problems scanning %d nodes!"
             (hash-table-count org-nodes))))

;; Very important macro for the backlink mode, because backlink insertion opens
;; the target Org file in the background, and if doing that is laggy, then
;; every link insertion is laggy.
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
                                (org-node-abbrev-file-names
                                 (file-truename ,file))
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
(cl-defun org-node--in-files-do
    (&key files fundamental-mode msg about-to-do call too-many-files-hack)
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

For explanation of TOO-MANY-FILES-HACK, see code comments."
  (declare (indent defun))
  (cl-assert (and msg files call about-to-do))
  (setq call (org-node--try-ensure-compiled call))
  (let ((enable-local-variables :safe)
        (org-inhibit-startup t) ;; Don't apply startup #+options
        (file-name-handler-alist nil)
        ;; (coding-system-for-read org-node-perf-assume-coding-system)
        (find-file-hook nil)
        (after-save-hook nil)
        (before-save-hook nil)
        (org-agenda-files nil)
        (kill-buffer-hook nil) ;; Inhibit save-place etc
        (kill-buffer-query-functions nil)
        (write-file-functions nil) ;; recentf-track-opened-file
        (buffer-list-update-hook nil))
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
                  ;; E.g. `org-node-lint-all' does not seem to need this hack,
                  ;; but `org-node-backlink-fix-all-files' does.
                  (signal 'org-node-must-retry nil)))
              (if interval
                  (when (zerop (% ctr interval))
                    (message "%s... %d files to go" msg (length files*)))
                ;; Set a reasonable interval between `message' calls, since they
                ;; can be surprisingly expensive.
                (when (> (float-time (time-since start-time)) 0.3)
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
           (cons file files*))
          (( error )
           (lwarn 'org-node :warning "%s: Loop interrupted by signal %S\n\tBuffer: %s\n\tFile: %s\n\tNext file: %s\n\tValue of ctr: %d"
                  (format-time-string "%T") err buf file (car files*) ctr)
           (unless (or was-open (not buf) (buffer-modified-p buf))
             (kill-buffer buf))
           files*)
          (:success
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
subroutine for `org-node--in-files-do' or any program that has
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

Prefer the closest ancestor heading that has an ID, else go to
the file-level property drawer if that contains an ID, else fall
back on the heading for the current entry.

Afterwards, maybe restore point to where it had been previously,
so long as the affected heading would still be visible in the
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
  "Add to ROAM_ALIASES in nearest relevant property drawer."
  (interactive nil org-mode)
  (org-node--add-to-property-keep-space
   "ROAM_ALIASES" (string-trim (read-string "Alias: "))))

;; FIXME: What if user yanks a [cite:... ... ...]?
(defun org-node-add-refs ()
  "Add a link to ROAM_REFS in nearest relevant property drawer.
Wrap the link in double-brackets if necessary."
  (interactive nil org-mode)
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
          ;; If it is a link, it should be enclosed in brackets
          (setq ref (concat "[["
                            (string-trim ref (rx "[[") (rx "]]"))
                            "]]"))
        (message "Spaces in ref, not sure how to format correctly: %s" ref)))
    (org-node--add-to-property-keep-space "ROAM_REFS" ref)))

;; TODO: Try to include all Firefox bookmarks and so on
(defun org-node--list-known-raw-links ()
  (let (result)
    (maphash
     (lambda (dest links)
       (let ((types (mapcar #'org-node-link-type links)))
         (when (memq nil types)
           ;; Type nil is a @citation
           (push dest result)
           (setq types (delq nil types)))
         (dolist (type (delete-dups (delete "id" types)))
           (push (concat type ":" dest) result))))
     org-node--dest<>links)
    result))

(defun org-node-add-tags (tags)
  "Add TAGS to the node at point or nearest ancestor that is a node.

Uses `org-node--call-at-nearest-node'.  To always operate on the current
entry, use `org-node-add-tags-here'."
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
                                (when org-element-use-cache
                                  (org-get-buffer-tags)))
                        (mapcar #'car)
                        (cl-remove-if #'keywordp)
                        (mapcar #'substring-no-properties))
           (cl-loop for node being each hash-value of org-nodes
                    append (org-node-get-tags-with-inheritance node))))))

(defun org-node--end-of-meta-data (&optional full)
  "Like `org-end-of-meta-data', but supports file-level metadata.

As in `org-end-of-meta-data', point always lands on a newline \(or the
end of buffer).  Since that newline may be the beginning of the next
heading, you should probably verify that `org-at-heading-p' is nil and
`point' has changed, else do `backward-char' or `open-line' prior to
inserting any text.

Argument FULL same as in `org-end-of-meta-data' when point is in a subtree,
meaningless otherwise.  When point is before the first heading, always jump
to a position after any file-level properties and keywords."
  (if (org-before-first-heading-p)
      (progn
        (goto-char (point-min))
        ;; Jump past top-level PROPERTIES drawer.
        (let ((case-fold-search t))
          (when (looking-at-p "[\t\s]*?:properties: *?$")
            (forward-line)
            (while (looking-at-p "[\t\s]*?:")
              (forward-line))))
        ;; Jump past #+keywords, comment lines and blank lines.
        (while (looking-at-p (rx (*? (any "\t\s")) (or "#+" "# " "\n")))
          (forward-line)))
    ;; PERF: Override a bottleneck in `org-end-of-meta-data'.
    (cl-letf (((symbol-function 'org-back-to-heading)
               #'org-node--back-to-heading-or-point-min))
      (org-end-of-meta-data full))))

(defun org-node--back-to-heading-or-point-min (&optional invisible-ok)
  "Alternative to `org-back-to-heading-or-point-min'.
Argument INVISIBLE-OK as in that function.

Like `org-back-to-heading-or-point-min' but should be faster in the case
that an org-element cache has not been built for the buffer.  This can
be the case in a buffer spawned by `org-roam-with-temp-buffer'.

As bonus, do not land on an inlinetask, seek a real heading."
  (let ((inlinetask-re (when (fboundp 'org-inlinetask-outline-regexp)
                         (org-inlinetask-outline-regexp))))
    (cl-loop until (and (org-at-heading-p (not invisible-ok))
                        (not (and inlinetask-re (looking-at-p inlinetask-re))))
             unless (re-search-backward org-outline-regexp-bol nil t)
             return (goto-char (point-min)))
    (point)))


;;;; CAPF (Completion-At-Point Function)

(defun org-node-complete-at-point ()
  "Complete word at point to a known node title, and linkify.
Designed for `completion-at-point-functions', which see."
  (when-let ((bounds (bounds-of-thing-at-point 'word)))
    (and (not (org-in-src-block-p))
         (not (save-match-data (org-in-regexp org-link-any-re)))
         (list (car bounds)
               (cdr bounds)
               org-node--title<>id
               :exclusive 'no
               :exit-function
               (lambda (text _)
                 (when-let ((id (gethash text org-node--title<>id)))
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

(define-globalized-minor-mode org-node-complete-at-point-mode
  org-node-complete-at-point-local-mode
  org-node-complete-at-point--try-enable)


;;;; Misc

(defun org-node-convert-link-to-super (&rest _)
  "Drop input and call `org-super-links-convert-link-to-super'."
  (require 'org-super-links)
  (when (fboundp 'org-super-links-convert-link-to-super)
    (org-super-links-convert-link-to-super nil)))

(defun org-node-try-visit-ref-node ()
  "Designed to be added to `org-open-at-point-functions'.

For the link at point, if there exists an org-ID node that has
the link in its ROAM_REFS property, visit that node rather than
following the link normally.

If already visiting that node, then follow the link normally."
  (when-let ((url (thing-at-point 'url)))
    ;; Rarely more than one car
    (let* ((dest (car (org-node-parser--split-refs-field url)))
           (found (cl-loop for node being each hash-value of org-nodes
                           when (member dest (org-node-get-refs node))
                           return node)))
      (if (and found
               ;; Check that point is not already in said ref node (if so,
               ;; better to fallback to default `org-open-at-point' logic)
               (not (and (derived-mode-p 'org-mode)
                         (equal (org-entry-get-with-inheritance "ID")
                                (org-node-get-id found)))))
          (progn (org-node--goto found)
                 t)
        nil))))


;;;; API not used inside this package

(defun org-node-id-at-point ()
  "Get the ID property in entry at point or some ancestor."
  (declare (obsolete org-entry-get-with-inheritance "2024-11-18"))
  (org-entry-get-with-inheritance "ID"))

(defun org-node-at-point ()
  "Return the ID-node near point.

This may refer to the current Org heading, else an ancestor
heading, else the file-level node, whichever has an ID first."
  (gethash (org-entry-get-with-inheritance "ID") org-nodes))

(defun org-node-by-id (id)
  (gethash id org-nodes))

(defun org-node-read ()
  "Prompt for a known ID-node."
  (gethash (completing-read "Node: " #'org-node-collection
                            () () () 'org-node-hist)
           org-node--candidate<>node))

(provide 'org-node)

;;; org-node.el ends here
