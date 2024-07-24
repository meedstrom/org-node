;;; org-node.el --- Like org-roam, treat org-id entries as linked notes -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.5") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-node

;;; Commentary:

;; If you were the sort of person to prefer "id:" links over "file:" links or
;; any other type of link, you're in the right place!  Now you can rely on IDs
;; and worry less about mentally tracking your subtree hierarchies and
;; directory structures.  As long as you've assigned an ID to something, you
;; can find it later.

;; In fact, you're likely to stop using any other method of browsing.  The
;; philosophy is the same as org-roam: if you assign an ID every time you make
;; an entry that you know you might want to link to from elsewhere, then it
;; tends to work out that the `org-node-find' command can find more or less
;; every entry you'd ever want to search for.  Pretty soon you've forgot that
;; your files have names.

;; Anyway, that's just the core of it as described to someone not familiar with
;; zettelkasten.  In fact, out of the simplicity arises something powerful,
;; more to be experienced than explained.

;; Compared to other systems:

;; - org-roam: Same idea, compatible disk format(!), but org-node lets you opt
;;   out of file-level property drawers, and it tries to rely in a bare-metal
;;   way on upstream org-id and org-capture.  For example, headings with IDs in
;;   some Git README are considered part of your collection -- if it's known to
;;   org-id, it's known to org-node.

;; - denote: Org-node is Org only, no support for "denote:" links or Markdown.
;;   Filenames have no meaning (and can be automatically managed), and you can
;;   have as many "notes" as you want inside one file.

;;; Code:

;; TODO: After 8 August, externalize org-node-fakeroam.el so it can pull its own
;;       dependencies

;; TODO: A workflow to allow pseudo-untitled (numeric-titled) nodes
;;       - Need a bunch of commands for that, like select node by fulltext
;;         search

;; TODO: Maybe rename object getters -get- to just :

(require 'cl-lib)
(require 'seq)
(require 'subr-x)
(require 'dash)
(require 'compat)
(require 'org)
(require 'org-id)
(require 'org-node-worker)
(require 'org-node-obsolete)

(declare-function #'org-element-property "org-element")
(declare-function #'org-super-links-convert-link-to-super "org-super-links")
(declare-function #'consult--grep "consult")
(declare-function #'consult--grep-make-builder "consult")
(declare-function #'wgrep-finish-edit "wgrep")
(declare-function #'wgrep-change-to-wgrep-mode "wgrep")
(declare-function #'org-roam-capture- "org-roam-capture")
(declare-function #'org-roam-node-create "org-roam-node")
(declare-function #'org-roam-node-slug "org-roam-node")


;;;; Options

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

(defcustom org-node-rescan-hook nil
  "Hook run after scanning specific files.
It is not run after a full cache reset, only after a file is
saved or renamed causing an incremental update to the cache.

Called with one argument: a list of files re-scanned."
  :group 'org-node
  :type 'hook)

(defcustom org-node-prefer-with-heading nil
  "Make a heading even when creating isolated file nodes.
If nil, write a #+TITLE and a file-level property-drawer instead.
In other words:

- if nil, use outline level 0
- if t, use outline level 1

This affects the behavior of `org-node-new-file',
`org-node-extract-subtree', and `org-node-capture-target'.

If you change your mind about this setting, the Org-roam commands
`org-roam-promote-entire-buffer' and
`org-roam-demote-entire-buffer' can help you transition the
files you already have."
  :group 'org-node
  :type 'boolean)

(defcustom org-node-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that org-node can scan for ID nodes inside .org.gpg files.  Also,
`org-node-perf-keep-file-name-handlers', should include the EPA
handler.

I do not use EPG, so that is probably not enough to make it work.
Report an issue on https://github.com/meedstrom/org-node/issues
or drop me a line on Mastodon: @meedstrom@emacs.ch"
  :group 'org-node
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
conversion or downloaded, it's very possible that there's a mix
of coding systems among them.  In that case, setting this
variable may cause org-node to fail to scan some of them, or
display their titles with strange glyphs."
  :group 'org-node
  :type '(choice coding-system (const nil)))

(defcustom org-node-perf-keep-file-name-handlers nil
  "Which file handlers to respect while scanning for ID nodes.

Normally, `file-name-handler-alist' reacts specially to seeing
some file names: TRAMP paths, compressed files or .org.gpg files.

It's infamous for (somewhat) slowing down the access of very many
files, since it is a series of regexps applied to every file name
visited.  The smaller this list, the faster `org-node-reset'."
  :group 'org-node
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

(setq org-node-filter-fn
      (lambda (node)
        (not (or (assoc \"ROAM_EXCLUDE\" (org-node-get-properties node))
                 (org-node-get-todo node)
                 (string-search \"/archive/\" (org-node-get-file-path node))
                 (member \"drill\" (org-node-get-tags node))))))"
  :group 'org-node
  :type 'function)

(defcustom org-node-insert-link-hook '()
  "Hook run after inserting a link to an Org-ID node.

Called with two arguments: the ID and the link description, with
point in the new link."
  :group 'org-node
  :type 'hook)

(defcustom org-node-creation-hook '(org-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied only by `org-node-new-file', `org-node-capture-target',
`org-node-insert-heading', `org-node-nodeify-entry' and
`org-node-extract-subtree'.

NOT applied by `org-node-new-via-roam-capture' -- see org-roam's
`org-roam-capture-new-node-hook' instead.

A good member to put in this hook is `org-node-put-created',
since the default `org-node-datestamp-format' is null."
  :group 'org-node
  :type 'hook)

(defcustom org-node-extra-id-dirs (list)
  "Directories in which to search Org files for IDs.

Unlike other variables such as `org-id-extra-files' and
`org-agenda-files', directories listed here are scanned again
over time in order to find new files that have appeared.

These directories are only scanned as long as
`org-node-cache-mode' is active.  They are scanned
recursively (looking in subdirectories, sub-subdirectories etc).

To avoid accidentally picking up duplicate files such as
versioned backups, causing org-id to complain about duplicate
IDs, configure `org-node-extra-id-dirs-exclude'.

Tip: If it happened anyway, try \\[org-node-forget-dir], because
merely removing a directory from this list does not forget the
IDs already found."
  :group 'org-node
  :type '(repeat directory))

;; TODO: figure out how to permit .org.gpg and fail gracefully if
;;       the EPG settings are insufficient. easier to test with .org.gz first
(defcustom org-node-extra-id-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    ".sync-conflict-")
  "Path substrings of files that should not be searched for IDs.

This option only influences how the function `org-node-files'
should seek files found in `org-node-extra-id-dirs'.  It is meant
as a way to avoid collecting IDs inside versioned backup files
causing org-id to complain about duplicate IDs.

For all other \"excludey\" purposes, you probably mean to
configure `org-node-filter-fn' instead.

If you have accidentally added a directory of backup files, try
\\[org-node-forget-dir].

It is not necessary to exclude backups or autosaves that end in ~
or # or .bak since `org-node-files' only considers files that end
in precisely \".org\" or \".org_archive\" anyway."
  :group 'org-node
  :type '(repeat string))


;;;; Pretty completion

(defcustom org-node-alter-candidates nil
  "Whether to alter completion candidates instead of affixating.

This means that org-node will concatenate the result of
`org-node-affixation-fn' into a single string, so what the
user types in the minibuffer can match against the prefix and
suffix as well as against the node title.

(Tip: users of the orderless library on versions from 2024-07-11
can match the prefix and suffix via `orderless-annotation',
without need for this setting.)

Another consequence is it lifts the uniqueness constraint on note
titles: you'll be able to have two headings with the same name so
long as their prefix or suffix differ.

After changing this setting, please run \\[org-node-reset]."
  :type 'boolean
  :group 'org-node)

(defcustom org-node-affixation-fn #'org-node-affix-with-olp
  "Function to give prefix and suffix to completion candidates.

The results will be indirectly used by the affixation function
associated with `org-node-collection', i.e. to style the
appearance of completions during \\[org-node-find] et al.

To read more about affixations, see docstring
`completion-extra-properties', however this function operates on
one candidate at a time, not the whole collection.

It receives two arguments: NODE and TITLE, and it must return a
list of three strings: title, prefix and suffix.

NODE is an object which form you can observe in examples from
\\[org-node-peek] and specified in type `org-node'
(C-h o org-node RET).

If a node has aliases, it is passed to this function again for
every alias, in which case TITLE is actually one of the aliases."
  :type '(radio
          (function-item org-node-affix-bare)
          (function-item org-node-affix-with-olp)
          function)
  :group 'org-node)

(defun org-node-affix-bare (_node title)
  "Use TITLE as-is.
For use as `org-node-affixation-fn'."
  (list title "" ""))

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
          "")
        ""))

(defvar org-node--title<>affixation-triplet (make-hash-table :test #'equal)
  "1:1 table mapping titles or aliases to affixation triplets.")

(defun org-node--affixate-collection (coll)
  "Take COLL and return precomputed affixations for each member."
  (cl-loop for title in coll
           collect (gethash title org-node--title<>affixation-triplet)))

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
`completing-read' to exit with user-entered input that didn't
match anything.

Arguments STR, PRED and ACTION are handled behind the scenes,
read more in the manual at (elisp)Programmed Completion."
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

(cl-defstruct (org-node-link (:constructor org-node-link--make-obj)
                             (:copier nil))
  origin
  pos
  type
  dest
  properties)


;;;; Tables

(defvaralias 'org-nodes 'org-node--id<>node)

(defvar org-node--id<>node (make-hash-table :test #'equal)
  "1:1 table mapping ids to nodes.
To peek on the contents, try \\[org-node-peek] a few times, which
can demonstrate the data format.  See also the type `org-node'.")

(defvar org-node--candidate<>node (make-hash-table :test #'equal)
  "1:1 table mapping completion candidates to nodes.")

(defvar org-node--title<>id (make-hash-table :test #'equal)
  "1:1 table mapping raw titles (and ROAM_ALIASES) to IDs.")

(defvar org-node--ref<>id (make-hash-table :test #'equal)
  "1:1 table mapping ROAM_REFS members to the adjacent ID.")

(defvar org-node--uri-path<>uri-type (make-hash-table :test #'equal)
  "1:1 table mapping //paths to types:.")

(defvar org-node--dest<>links (make-hash-table :test #'equal)
  "1:N table of links.

The table keys are destination IDs, and the corresponding table
value is a list of `org-node-link' records describing each link,
with info such as the ID-node where the link originated.")

(defun org-node-get-id-links (node)
  "Get list of ID-link objects pointing to NODE.
Each object is of type `org-node-link' with these fields:

:origin - ID of origin node (where the link was found)
:pos - buffer position where the link was found
:dest - ID of destination node, or a ref that belongs to it
:type - link type, such as \"https\", \"ftp\", \"info\" or
        \"man\".  For ID-links this is always \"id\".  For a
        citation this is always nil.

This function only returns ID-links, so you can expect the DEST
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
you have a ref string such an URL.  Also, citations have TYPE
nil, so you can distinguish citations from other links this way."
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
  "Instruct on-save hooks and such things to update the cache.

Without this mode active, commands such as `org-node-find' may
present out-of-date completions, and `org-node-backlink-mode' may
delete too many backlinks on cleanup.

-----"
  :global t
  :group 'org-node
  (remove-hook 'org-mode-hook #'org-node-cache-mode)
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node--handle-save)
        (add-hook 'org-node-creation-hook #'org-node--dirty-ensure-node-known)
        (add-hook 'org-node-insert-link-hook #'org-node--dirty-ensure-link-known)
        (add-hook 'org-roam-post-node-insert-hook #'org-node--dirty-ensure-link-known)
        (advice-add 'org-insert-link :after #'org-node--dirty-ensure-link-known)
        (advice-add 'rename-file :after #'org-node--handle-save)
        (advice-add 'delete-file :after #'org-node--handle-save)
        (org-node-cache-ensure 'must-async t)
        (org-node--maybe-adjust-idle-timer))
    (cancel-timer org-node--idle-timer)
    (remove-hook 'after-save-hook #'org-node--handle-save)
    (remove-hook 'org-node-creation-hook #'org-node--dirty-ensure-node-known)
    (remove-hook 'org-node-insert-link-hook #'org-node--dirty-ensure-link-known)
    (remove-hook 'org-roam-post-node-insert-hook #'org-node--dirty-ensure-link-known)
    (advice-remove 'org-insert-link #'org-node--dirty-ensure-link-known)
    (advice-remove 'rename-file #'org-node--handle-save)
    (advice-remove 'delete-file #'org-node--handle-save)))

(defun org-node-cache-ensure (&optional synchronous force)
  "Ensure that `org-node--id<>node' and other tables are ready for use.
Specifically, do the following:

- Initialize `org-id-locations' if it is not already.
- Ensure `org-id-locations' is a hash table and not an alist.
- (Re-)build the cache if it is empty, or if FORCE t.

The primary use case is at the start of autoloaded commands.

Optional argument SYNCHRONOUS t means that if a cache build is
needed or already ongoing, block Emacs until it is done.

When SYNCHRONOUS is nil, return immediately and let the caching
proceed in the background.  As that may take a few seconds, that
would mean that the `org-node--id<>node' table is probably still outdated
by the time you query it, but that is acceptable in many
situations such as in an user command since the table is mostly
correct - and fully correct by the time of the next invocation.

If the `org-node--id<>node' table is currently empty, behave as if
SYNCHRONOUS t, unless SYNCHRONOUS is the symbol `must-async'."
  (unless (eq synchronous 'must-async)
    ;; HACK The warn-function becomes a no-op after the first run, so gotta run
    ;; it as late as possible in case of late variable settings.  By running it
    ;; here, we've waited until the user runs a command.
    (org-node--warn-obsolete-variables))
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
        (message "org-node caching...")
      (message "org-node caching... (Hint: Turn on org-node-cache-mode)"))
    (mapc #'accept-process-output org-node--processes)
    ;; Just in case... see docstring of `org-node--create'.
    ;; Not super happy about this edge-case, it's a wart of the current design
    ;; of `org-node--try-launch-scan'.
    (while (member org-node--retry-timer timer-list)
      (cancel-timer org-node--retry-timer)
      (funcall (timer--function org-node--retry-timer))
      (mapc #'accept-process-output org-node--processes))))

(defun org-node--init-ids ()
  (require 'org-id)
  (when (not org-id-track-globally)
    (user-error "Org-node requires `org-id-track-globally'"))
  (when (null org-id-locations)
    (when (file-exists-p org-id-locations-file)
      (org-id-locations-load)))
  (when (listp org-id-locations)
    (setq org-id-locations (org-id-alist-to-hash org-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-id-locations-load)
    (when (and (hash-table-empty-p org-id-locations)
               (null org-node-extra-id-dirs))
      (org-node--die "org-id-locations empty, try `org-id-update-id-locations' or `org-roam-update-org-id-locations'"))))

(defun org-node--handle-save (&optional arg1 arg2 &rest _)
  "Scan nodes and links in a single file, or forget them if the
file is gone.

Either operate on ARG2 if it seems to be a file name, else ARG1,
else the current buffer file.  Meant for `after-save-hook' or as
advice on `rename-file' or `delete-file'."
  (let ((file (cond ((and (stringp arg2) (file-exists-p arg2)) ;; rename-file
                     arg2)
                    ((stringp arg1) ;; delete-file
                     arg1)
                    ((stringp buffer-file-name) ;; after-save-hook
                     buffer-file-name))))
    (when (--any-p (string-suffix-p it file)
                   '(".org" ".org_archive" ".org.gpg"))
      (org-node--scan-targeted (list file)))))

(defun org-node--maybe-adjust-idle-timer ()
  "Adjust `org-node--idle-timer' based on duration of last scan.
If not running, start it.

This function mainly exists to give the user something to
override if they disagree with the timer delay or even having a
timer."
  (let ((new-delay (* 25 (1+ org-node--time-elapsed))))
    (when (or (not (member org-node--idle-timer timer-idle-list))
              ;; Disarm a footgun (gh:meedstrom/org-node#21)
              (not (> (float-time (or (current-idle-time) 0))
                      (- new-delay 1))))
      (cancel-timer org-node--idle-timer)
      (setq org-node--idle-timer
            (run-with-idle-timer new-delay t #'org-node--scan-all)))))


;;;; Scanning

(defun org-node--scan-all ()
  (org-node--try-launch-scan t))

(defun org-node--scan-targeted (files)
  (org-node--try-launch-scan (ensure-list files)))

(defvar org-node--retry-timer (timer-create))
(defvar org-node--known-files nil)
(defvar org-node--file-queue nil)
(defvar org-node--wait-start nil)
(defvar org-node--full-scan-requested nil)

;; TODO: Shorten.  How?  At the moment, we line up a specific file for scan even
;; if a "full" scan will happen or has just happened, for (IIRC) reasons:
;;
;; 1. Ongoing full scan may have already gone past the targeted
;;    file by the time the order comes in (unlikely)
;; 2. Targeting a deleted file will clean it out of org-id-locations (full scan
;;    will not be aware of deleted files... actually yes, because it tries to
;;    scan every file recorded in org-id-locations)
;; 3. Only a targeted scan will execute `org-node-rescan-hook', for good reason
;;
;; Hm, point 3 could be taken care of at full scan by comparing to a
;; table of previously known file<>mtime.
(defun org-node--try-launch-scan (&optional files)
  "Ensure that multiple calls occurring in a short time (like when
multiple files are being renamed) will be handled
eventually and not dropped."
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
                (message "org-node: processes worked longer than 30 sec, killing")
                (while-let ((old-process (pop org-node--processes)))
                  (delete-process old-process)))
            (setq must-retry t)))
      ;; All clear, scan now
      (setq org-node--wait-start nil)
      (if org-node--full-scan-requested
          (progn
            (setq org-node--full-scan-requested nil)
            (org-node--scan (org-node-files) #'org-node--finalize-full)
            (when org-node--file-queue
              (setq must-retry t)))
        ;; Targeted scan of specific files
        (org-node--scan org-node--file-queue #'org-node--finalize-modified)
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
  "Name of buffer for the subprocesses stderr.")

;; NOTE: On most systems it does not matter how many subprocesses go to
;; work, since it will only block Emacs on first autoload.
(defcustom org-node-perf-max-jobs 0
  "Number of subprocesses to run.
If left at 0, will be set at runtime to the result of
`org-node--count-logical-cores'."
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
               (ignore-errors
                 (with-temp-buffer
                   (call-process "echo" nil t nil "%NUMBER_OF_PROCESSORS%")
                   (buffer-string)))))))
       1))

(defun org-node--ensure-compiled-lib ()
  "Compile org-node-worker.el, in case the user's package manager
didn't do so already, or local changes have been made."
  (let* ((file-name-handler-alist nil)
         ;; FIXME When working on a checked-out repo, this will still just find
         ;;       elpaca/straight's clone.  So the developer has to paste in
         ;;       the true library path here.
         (lib (find-library-name "org-node-worker"))
         (native-path (and (featurep 'native-compile)
                           (native-comp-available-p)
                           (require 'comp)
                           (comp-el-to-eln-filename lib)))
         (elc-path (org-node-worker--tmpfile "worker.elc"))
         (byte-compile-warnings '(not free-vars)))
    (mkdir (org-node-worker--tmpfile) t)
    (if native-path
        (unless (file-newer-than-file-p native-path lib)
          (native-compile lib))
      ;; No native-comp facility, so make an .elc
      (unless (file-newer-than-file-p elc-path lib)
        ;; Ensure the .elc won't clutter some source directory
        (let ((byte-compile-dest-file-function `(lambda (&rest _) ,elc-path)))
          (byte-compile-file lib))))
    (or native-path elc-path)))

(defmacro org-node--ensure-compiled-fn (var)
  "Ensure that the value of variable VAR is a compiled function."
  `(or (compiled-function-p (if (symbolp ,var) (symbol-function ,var) ,var))
       ;; Would use native-comp here, but it wrecks Customize
       (setq ,var (byte-compile ,var))))

(defun org-node--scan (files finalizer)
  "Begin async scanning FILES for id-nodes and links.

When finished, pass a list of scan results to the FINALIZER
function to update current tables."
  (when (= 0 org-node-perf-max-jobs)
    (setq org-node-perf-max-jobs (org-node--count-logical-cores)))
  (org-node--ensure-compiled-fn org-node-filter-fn)
  (org-node--ensure-compiled-fn org-node-affixation-fn)
  (let ((compiled-lib (org-node--ensure-compiled-lib))
        (file-name-handler-alist nil)
        (coding-system-for-read org-node-perf-assume-coding-system)
        (coding-system-for-write org-node-perf-assume-coding-system))
    (when org-node--debug (garbage-collect))
    (setq org-node--time-at-scan-begin (current-time))
    (setq org-node--done-ctr 0)
    (when (-any-p #'process-live-p org-node--processes)
      ;; We should never end up here, but just in case
      (mapc #'delete-process org-node--processes)
      (message "org-node processes alive, bug report would be appreciated"))
    (setq org-node--processes nil)
    (with-current-buffer (get-buffer-create org-node--stderr-name)
      (erase-buffer))
    (with-temp-file (org-node-worker--tmpfile "work-variables.eld")
      (let ((standard-output (current-buffer))
            (print-length nil)
            (print-level nil))
        (prin1
         ;; NOTE The $sigils in the names are to visually distinguish these
         ;;      "external" variables in the body of
         ;;      `org-node-worker--collect-dangerously'.
         (append
          org-node-inject-variables
          `(($plain-re . ,org-link-plain-re)
            ($merged-re . ,(concat org-link-bracket-re "\\|" org-link-plain-re))
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
                  (org-node-worker--make-todo-regexp
                   (string-join (-mapcat #'cdr
                                         (default-value 'org-todo-keywords))
                                " "))))
            ($backlink-drawer-re
             . ,(concat "^[[:space:]]*:"
                        (or (and (fboundp 'org-super-links-link)
                                 (require 'org-super-links)
                                 (stringp org-super-links-backlink-into-drawer)
                                 org-super-links-backlink-into-drawer)
                            "backlinks")
                        ":")))))))

    (if org-node--debug
        ;; Special case for debugging; run single-threaded so we can step
        ;; through the org-node-worker.el functions with edebug
        (progn
          (delete-file (org-node-worker--tmpfile "results-0.eld"))
          (let ((print-length nil))
            (write-region (prin1-to-string files)
                          nil
                          (org-node-worker--tmpfile "file-list-0.eld")))
          (setq i 0)
          (setq org-node-worker--result:found-links nil)
          (setq org-node-worker--result:problems nil)
          (setq org-node-worker--result:paths-types nil)
          (when (bound-and-true-p editorconfig-mode)
            (message "Maybe disable editorconfig-mode while debugging"))
          (load-file compiled-lib)
          (garbage-collect)
          (setq org-node--time-at-scan-begin (current-time))
          (with-current-buffer (get-buffer-create "*org-node debug*")
            (when (eq 'show org-node--debug)
              (pop-to-buffer (current-buffer)))
            (erase-buffer)
            (org-node-worker--collect-dangerously)
            (org-node--handle-finished-job 1 finalizer)))

      ;; If not debugging, split the work over many child processes
      (let* ((file-lists
              (org-node--split-into-n-sublists files org-node-perf-max-jobs))
             (n-jobs (length file-lists)))
        (dotimes (i n-jobs)
          (delete-file (org-node-worker--tmpfile "results-%d.eld" i))
          (let ((write-region-inhibit-fsync nil) ;; Default t in emacs30
                (print-length nil)
                (print-level nil))
            (write-region (prin1-to-string (pop file-lists))
                          nil
                          (org-node-worker--tmpfile "file-list-%d.eld" i)))
          (push (make-process
                 :name (format "org-node-%d" i)
                 :noquery t
                 :stderr (get-buffer-create org-node--stderr-name)
                 :command
                 ;; Ensure the children run the same binary executable as
                 ;; this Emacs, so the compiled-lib fits
                 (list (expand-file-name invocation-name invocation-directory)
                       "--quick"
                       "--batch"
                       "--eval" "(setq gc-cons-threshold most-positive-fixnum)"
                       "--eval" (format "(setq i %d)" i)
                       "--eval" (format "(setq temporary-file-directory \"%s\")"
                                        temporary-file-directory)
                       "--load" compiled-lib
                       "--funcall" "org-node-worker--collect-dangerously")
                 :sentinel (lambda (_process _event)
                             (org-node--handle-finished-job n-jobs finalizer)))
                org-node--processes))))))

(defvar org-node--first-init t
  "True if org-node has not been initialized yet.
Muffles some messages.")

(defun org-node--handle-finished-job (n-jobs finalizer)
  "Check if this was the last process to return (by counting up
to N-JOBS), then if so, wrap-up and call FINALIZER."
  (when (eq n-jobs (cl-incf org-node--done-ctr))
    (when org-node--debug
      (setq org-node--first-init nil)
      (garbage-collect))
    (setq org-node--time-at-finalize (current-time))
    (let ((file-name-handler-alist nil)
          (coding-system-for-read org-node-perf-assume-coding-system)
          (coding-system-for-write org-node-perf-assume-coding-system)
          result-sets)
      (with-temp-buffer
        (dotimes (i n-jobs)
          (let ((results-file (org-node-worker--tmpfile "results-%d.eld" i)))
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
      (setq org-node--time-at-last-child-done (-last-item (car result-sets)))
      ;; Merge N result-sets into one result-set, to run FINALIZER once
      (funcall finalizer (--reduce (-zip-with #'nconc it acc) result-sets)))))


;;;; Scan finalizers

(defun org-node--finalize-full (results)
  (clrhash org-node--id<>node)
  (clrhash org-node--dest<>links)
  (clrhash org-node--candidate<>node)
  (clrhash org-node--title<>id)
  (clrhash org-node--ref<>id)
  (setq org-node--problems nil)
  (setq org-node--collisions nil)
  (seq-let (missing-files _ nodes path<>type links errors) results
    (org-node--forget-id-locations missing-files)
    (dolist (link links)
      (push link (gethash (org-node-link-dest link) org-node--dest<>links)))
    (dolist (pair path<>type)
      (puthash (car pair) (cdr pair) org-node--uri-path<>uri-type))
    (dolist (node nodes)
      (org-node--record-node node))
    (dolist (err errors)
      (push err org-node--problems))
    (org-id-locations-save)
    (setq org-node--time-elapsed
          ;; For reproducible profiling: don't count time taken by
          ;; other sentinels or timers or I/O in between these periods
          (+ (float-time
              (time-subtract (current-time)
                             org-node--time-at-finalize))
             (float-time
              (time-subtract org-node--time-at-last-child-done
                             org-node--time-at-scan-begin))))
    (org-node--maybe-adjust-idle-timer)
    (when org-node--first-init
      (setq org-node--first-init nil))
    (while-let ((fn (pop org-node--temp-extra-fns)))
      (funcall fn))
    (when (and org-node--collisions org-node-warn-title-collisions)
      (message "Some nodes share title, see M-x org-node-list-collisions"))
    (when errors
      (message "Scan had problems, see M-x org-node-list-scan-problems"))))

(defun org-node--finalize-modified (results)
  (seq-let (missing-files found-files nodes path<>type links errors) results
    (org-node--forget-id-locations missing-files)
    (org-node--dirty-forget-files missing-files)
    ;; In case a title was edited: don't persist old revisions of the title
    (org-node--dirty-forget-completions-in found-files)
    (when org-node-eagerly-update-link-tables
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
      ;; Having erased the links that were known to originate in the re-scanned
      ;; nodes, it's safe to add them (again).
      (dolist (link links)
        (push link (gethash (org-node-link-dest link) org-node--dest<>links))))
    (dolist (pair path<>type)
      (puthash (car pair) (cdr pair) org-node--uri-path<>uri-type))
    (dolist (node nodes)
      (org-node--record-node node))
    (dolist (err errors)
      (push err org-node--problems))
    (when errors
      (message "org-node found issues, see M-x org-node-list-scan-problems"))
    (run-hook-with-args 'org-node-rescan-hook found-files)))

(defcustom org-node-eagerly-update-link-tables nil
  "Update backlink tables on every save.

By default, we do not bother to do this on every save (only after
`org-node--idle-timer'), because it can slow down saving a big
file containing thousands of links, on constrained devices.

Fortunately it is rarely needed, since the insert-link advices of
`org-node-cache-mode' will already record links added during
normal usage!  What's left undone til idle:
1. deleted links remain in the table --> undead backlinks
2. :pos values can desync, which can affect org-roam-buffer"
  :group 'org-node
  :type 'boolean)

(defun org-node--record-node (node)
  "Add NODE to `org-node--id<>node' and related to other tables."
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
    (when (funcall org-node-filter-fn node)
      ;; Let refs work as aliases
      (dolist (ref refs)
        (puthash ref node org-node--candidate<>node)
        (puthash ref
                 (list (propertize ref 'face 'org-cite)
                       (if-let (type (gethash ref org-node--uri-path<>uri-type))
                           (propertize (concat type ":")
                                       'face 'completions-annotations))
                       nil)
                 org-node--title<>affixation-triplet))
      (dolist (title (cons (org-node-get-title node)
                           (org-node-get-aliases node)))
        (let ((collision (gethash title org-node--title<>id)))
          (when (and collision (not (equal id collision)))
            (push (list title id collision) org-node--collisions)))
        (puthash title id org-node--title<>id)
        (let ((affx (funcall org-node-affixation-fn node title)))
          (if org-node-alter-candidates
              ;; Absorb the affixations into one candidate string
              (puthash (concat (nth 1 affx) (nth 0 affx) (nth 2 affx))
                       node
                       org-node--candidate<>node)
            ;; Just title as candidate, to be affixated by `org-node-collection'
            (puthash title node org-node--candidate<>node)
            (puthash title affx org-node--title<>affixation-triplet)))))))


;;;; "Dirty" functions
;; Help keep the cache reasonably in sync without having to do a full reset

(defun org-node--dirty-forget-files (files)
  "Remove from cache all ROAM_REFS and IDs that existed within
FILES, and remove the corresponding completion candidates."
  (when files
    (org-node--dirty-forget-completions-in files)
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
  "Record the ID-link at point."
  (when (derived-mode-p 'org-mode)
    (require 'org-element)
    (org-node--init-ids)
    (when-let ((origin (org-entry-get nil "ID" t))
               (dest (if (gethash id org-id-locations)
                         id
                       (let ((elm (org-element-context)))
                         (when (equal "id" (org-element-property :type elm))
                           (org-element-property :path elm))))))
      (push (org-node-link--make-obj
             :origin origin
             :pos (point)
             :type "id"
             :dest dest
             :properties (list :outline (ignore-errors
                                          (org-get-outline-path t))))
            (gethash dest org-node--dest<>links)))))

(defun org-node--dirty-ensure-node-known ()
  "Record the node at point.

Not meant to be perfect, but good enough to:

1. ensure that the node at point will show up among completion
candidates right away, without having to save the buffer.

2. ensure that `org-node-backlink-mode' won't autoclean backlinks
to this node on account of it \"not existing yet\".  Actually,
also necessary to do is `org-node--dirty-ensure-link-known'."
  (let ((id (org-entry-get nil "ID" t))
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
              :refs (org-node-worker--split-refs-field
                     (cdr (assoc "ROAM_REFS" props)))
              :pos (if heading (org-entry-beginning-position) 1)
              :level (or (org-current-level) 0)
              :olp (org-get-outline-path)
              ;; Less important
              :properties props
              :tags (org-get-tags(replace-regexp-in-string "[[:space:]]+" "-" title))
              :todo (if heading (org-get-todo-state))
              :deadline (cdr (assoc "DEADLINE" props))
              :scheduled (cdr (assoc "SCHEDULED" props))))))))))


;;;; Etc

(defvar org-node--debug nil
  "Whether to run in a way suitable for debugging.")

(defvar org-node--idle-timer (timer-create)
  "Timer for intermittently checking `org-node-extra-id-dirs'
for new, changed or deleted files.

This redundant behavior helps detect changes made by something
other than the current instance of Emacs, such as an user typing
rm on the command line instead of using \\[delete-file].")

(defvar org-node--time-elapsed 1
  "Duration of the last cache reset.")

(defvar org-node--time-at-scan-begin nil)
(defvar org-node--time-at-last-child-done nil)
(defvar org-node--time-at-finalize nil)

(defun org-node--print-elapsed ()
  "Print time elapsed since `org-node--time-at-scan-begin'.
Also report statistics about the nodes and links found.

Currently, the printed message implies that all of org-node's
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

;; I wish that `find-file-noselect' when called from Lisp would take an
;; optional argument that explains why the file is about to be opened, amending
;; the autosave recovery prompt.  Hack up something like that.
(defvar org-node--imminent-recovery-msg
  "Org-node about to look inside or edit a file, but it's likely you will first get a prompt to recover an auto-save file, ready? "
  "For use by `org-node--with-quick-file-buffer'.")

;; NOTE Very important macro for the backlink mode, because backlink insertion
;;      opens the target Org file in the background, and if doing that is
;;      laggy, then every link insertion is laggy.
(defmacro org-node--with-quick-file-buffer (file &rest body)
  "Pseudo-backport of Emacs 29 `org-with-file-buffer'.
Also integrates `org-with-wide-buffer' behavior, some magic
around saving, and tries to open FILE quickly i.e. with
minimal hooks.

In short, if a buffer was visiting FILE, go to that buffer, else
visit FILE in a new buffer, in which case ignore the usual Org
startup checks.  Temporarily `widen' the buffer, execute BODY,
then restore point.  Finally:

- If a new buffer had to be opened: save and kill it.  Mandatory
  because buffers opened in the quick way look \"wrong\" as many
  hooks did not run, e.g. no indent-mode, no visual wrap etc.
- If a buffer had been open: leave it open and leave it unsaved."
  (declare (indent 1) (debug t))
  `(let ((find-file-hook nil)
         (after-save-hook nil)
         (before-save-hook nil)
         (org-element-use-cache nil) ;; generally prevent bugs
         (org-inhibit-startup t) ;; don't use org startup options
         (org-agenda-files nil)
         (kill-buffer-hook nil) ;; inhibit save-place etc
         (kill-buffer-query-functions nil)
         (buffer-list-update-hook nil))
     (let ((was-open (find-buffer-visiting ,file)))
       (when (or was-open
                 (if (file-newer-than-file-p (let ((buffer-file-name ,file))
                                               (make-auto-save-file-name))
                                             ,file)
                     (y-or-n-p org-node--imminent-recovery-msg)
                   t))
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
             (kill-buffer)))))))

(defun org-node--die (format-string &rest args)
  "Like `error' but make sure the user sees it.
Useful because not everyone has `debug-on-error' t, and then
errors are very easy to miss."
  (let ((err-string (apply #'format-message format-string args)))
    (display-warning 'org-node err-string :error)
    (error "%s" err-string)))

(defun org-node--consent-to-bothersome-modes-for-mass-edit ()
  (cl-loop for mode in '(auto-save-visited-mode
                         git-auto-commit-mode)
           when (and (boundp mode)
                     (symbol-value mode)
                     (not (y-or-n-p
                           (format "%S is active - proceed anyway?" mode))))
           return nil
           finally return t))

;; (progn (byte-compile #'org-node-files) (garbage-collect) (benchmark-run 10 (org-node-files)))
(defun org-node-files (&optional _)
  "List files in `org-id-locations' or `org-node-extra-id-dirs'."
  (declare (advertised-calling-convention nil "2024-07-21"))
  (-union ;; 10x faster than `seq-union'
   (hash-table-values org-id-locations)
   (let ((file-name-handler-alist nil)) ;; Cuts 200 ms to 70 ms
     (cl-loop
      for dir in org-node-extra-id-dirs
      append (cl-loop
              for file in (directory-files-recursively
                           dir (rx (or ".org" ".org_archive") eos)
                           nil t)
              unless (cl-loop
                      for exclude in org-node-extra-id-dirs-exclude
                      when (string-search exclude file)
                      return t)
              ;; Abbreviating because org-id does too
              collect (abbreviate-file-name file))))))

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

;; (progn (byte-compile #'org-node--root-dirs) (garbage-collect) (benchmark-run 100 (org-node--root-dirs (hash-table-values org-id-locations))))
(defun org-node--root-dirs (file-list)
  "Infer root directories of FILE-LIST.

If FILE-LIST is the `hash-table-values' of `org-id-locations',
this function will in many cases spit out a list of one item
because many people keep their Org files in one root
directory (with various subdirectories).

If it finds more than one root, it sorts by count of files they
contain, so that the most populous root directory will be the
first element.

Note also that the only directories that may qualify are those
that directly contain a member of FILE-LIST, so that if you have
the 3 members

- \"/home/me/Syncthing/foo.org\"
- \"/home/kept/bar.org\"
- \"/home/kept/archive/baz.org\"

the return value will not be (\"/home/\"), but
(\"/home/kept/\" \"/home/me/Syncthing/\"), because \"/home\"
contains no members of FILE-LIST itself.

FILE-LIST must be a list of full paths.  This function does not
consult the filesystem, just compares substrings to each other."
  (let* ((files (seq-uniq file-list))
         (dirs (seq-uniq (mapcar #'file-name-directory files))))
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child
    (cl-loop for dir in dirs
             as dirs-aside-from-this-one = (remove dir dirs)
             when (--any-p (string-prefix-p it dir) dirs-aside-from-this-one)
             do (setq dirs (delete dir dirs)))
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
basename is controlled by `org-node-filename-fn'."
  :group 'org-node
  :type '(choice boolean string))

(defun org-node-guess-or-ask-dir (prompt)
  "Maybe prompt for a directory, and if so, show string PROMPT.
Behavior depends on the user option `org-node-ask-directory'."
  (if (eq t org-node-ask-directory)
      (read-directory-name prompt)
    (if (stringp org-node-ask-directory)
        org-node-ask-directory
      (car (org-node--root-dirs (org-node-files t))))))

(defvar org-node-filename-fn nil
  "Deprecated. Please set these variables

- `org-node-datestamp-format'
- `org-node-slug-fn'

and then set this variable to nil (or remove from initfiles and
restart).")

(defcustom org-node-datestamp-format ""
  "Passed to `format-time-string' to prepend to filenames.

Example from Org-roam: \"%Y%m%d%H%M%S-\"
Example from Denote: \"%Y%m%dT%H%M%S--\""
  :type 'string)

(defcustom org-node-slug-fn #'org-node-slugify-for-web
  "Function taking a node title and returning a filename.
Receives one argument: the value of an Org #+TITLE keyword, or
the first heading if there is no #+TITLE.

Built-in choices:
- `org-node-slugify-for-web'
- `org-node-slugify-like-roam-default'
- `org-node-slugify-like-roam-actual'"
  :type '(radio
          (function-item org-node-slugify-for-web)
          (function-item org-node-slugify-like-roam-default)
          (function-item org-node-slugify-like-roam-actual)
          function))

;; Useful test cases if you want to hack on this!

;; (org-node-slugify-for-web "A/B testing")
;; (org-node-slugify-for-web "\"But there's still a chance, right?\"")
;; (org-node-slugify-for-web "Löb's Theorem")
;; (org-node-slugify-for-web "How to convince me that 2 + 2 = 3")
;; (org-node-slugify-for-web "E. T. Jaynes")
;; (org-node-slugify-for-web "Amnesic recentf, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (org-node-slugify-for-web "Slimline/\"pizza box\" computer chassis")
;; (org-node-slugify-for-web "#emacs")
;; (org-node-slugify-for-web "칹え🐛")

;; https://irreal.org/blog/?p=11896
(defun org-node--emacs25-strip-diacritics (string)
  (let ((diacritics-alist
         (seq-mapn (lambda (a b) (cons a b))
                   "ÀÁÂÃÄÅàáâãäåÒÓÔÕÕÖØòóôõöøÈÉÊËèéêëðÇçÐÌÍÎÏìíîïÙÚÛÜùúûüÑñŠšŸÿýŽža"
                   "AAAAAAaaaaaaOOOOOOOooooooEEEEeeeeeCcDIIIIiiiiUUUUuuuuNnSsYyyZz")))
    (concat (seq-map (lambda (char)
                       (or (alist-get char diacritics-alist)
                           char))
                     string))))

(defun org-node-slugify-for-web (title)
  "From TITLE, make a filename that looks nice as URL component.

A title like \"Löb's Theorem\" becomes \"lobs-theorem\".  Note
that while diacritical marks are stripped, it retains most
symbols that belong to the alphabet category in Unicode,
preserving for example kanji and Greek letters."
  (if (version<= "29" emacs-version)
      (thread-last title
                   (string-glyph-decompose)
                   (string-to-list)
                   (--reject (< 767 it 818)) ;; Remove diacritics
                   (concat)
                   (string-glyph-compose)
                   (downcase)
                   (string-trim)
                   (replace-regexp-in-string "[[:space:]]+" "-")
                   (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
                   (replace-regexp-in-string "\\/" "-")
                   (replace-regexp-in-string "--*" "-")
                   (replace-regexp-in-string "^-" "")
                   (replace-regexp-in-string "-$" ""))
    (thread-last title
                 (org-node--emacs25-strip-diacritics)
                 (downcase)
                 (string-trim)
                 (replace-regexp-in-string "[[:space:]]+" "-")
                 (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
                 (replace-regexp-in-string "\\/" "-")
                 (replace-regexp-in-string "--*" "-")
                 (replace-regexp-in-string "^-" "")
                 (replace-regexp-in-string "-$" ""))))

(defun org-node-slugify-like-roam-default (title)
  "From TITLE, make a filename in the default org-roam style.
Unlike `org-node-slugify-like-roam-actual', does not load
org-roam."
  (if (version<= "29" emacs-version)
      (thread-last title
                   (string-glyph-decompose)
                   (string-to-list)
                   (--reject (< 767 it 818)) ;; Remove diacritics
                   (concat)
                   (string-glyph-compose)
                   (downcase)
                   (string-trim)
                   (replace-regexp-in-string "[^[:alnum:][:digit:]]" "_")
                   (replace-regexp-in-string "__*" "_")
                   (replace-regexp-in-string "^_" "")
                   (replace-regexp-in-string "_$" ""))
    (error "Emacs 29 required for `org-node-slugify-like-roam-default'")))

(defun org-node-slugify-like-roam-actual (title)
  "Call on `org-roam-node-slug' to transform TITLE."
  (unless (fboundp #'org-roam-node-slug)
    (user-error "org-roam required to run `org-node-slugify-like-roam-actual'"))
  (require 'org-roam)
  (org-roam-node-slug (org-roam-node-create :title title)))

;; DEPRECATED
(defun org-node-slugify-like-roam (title)
  "From TITLE, make a filename in the default org-roam style."
  (unless (fboundp #'org-roam-node-slug)
    (user-error "org-roam required to run `org-node-slugify-like-roam'"))
  (require 'org-roam)
  (message "Variable `org-node-filename-fn' deprecated, please update config")
  (concat (format-time-string "%Y%m%d%H%M%S-")
          (org-roam-node-slug (org-roam-node-create :title title))
          ".org"))


;;;; How to create new nodes

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.")

(defun org-node--goto (node)
  "Visit NODE."
  (if node
      (let ((file (org-node-get-file-path node)))
        (if (file-exists-p file)
            (let ((pos (org-node-get-pos node)))
              (find-file file)
              (widen)
              ;; Don't move point if node pos is already inside
              ;; visible part of buffer and point is in its entry
              (if (org-node-get-is-subtree node)
                  (progn
                    (unless (and (pos-visible-in-window-p pos)
                                 (not (org-invisible-p pos))
                                 (equal (org-node-get-title node)
                                        (org-get-heading t t t t)))
                      (goto-char pos)
                      (org-show-context)
                      (org-show-entry)
                      (recenter 0)))
                (unless (pos-visible-in-window-p pos)
                  (goto-char pos))))
          (message "org-node: Didn't find a file, resetting cache...")
          (org-node--scan-all)))
    (error "`org-node--goto' received a nil argument")))

(defun org-node--create (title id)
  "Call `org-node-creation-fn' with necessary variables set.

When calling from Lisp, you should not assume anything about
which buffer will be current afterwards, since it depends on
`org-node-creation-fn' and whether TITLE or ID already existed.
To visit a node after creating it, either let-bind
`org-node-creation-fn' so you know what you get, or write:

    (org-node--create TITLE ID)
    (org-node-cache-ensure t)
    (let ((node (gethash ID org-node--id<>node)))
      (if node (org-node--goto node)))"
  (setq org-node-proposed-title title)
  (setq org-node-proposed-id id)
  (condition-case err
      (funcall org-node-creation-fn)
    (( t debug error )
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil)
     (signal (car err) (cdr err)))
    (:success
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil))))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called by `org-node-find' and `org-node-insert-link' to
create a node that does not yet exist.

Built-in choices:
- `org-node-new-file'
- `org-node-new-via-roam-capture'
- `org-capture'

If you choose `org-capture' here, configure
`org-capture-templates' such that some capture templates use
`org-node-capture-target' as their target.

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
           (path-to-write (file-name-concat
                           dir (org-node--name-file org-node-proposed-title))))
      (if (or (file-exists-p path-to-write)
              (find-buffer-visiting path-to-write))
          (message "A file or buffer already exists for path %s"
                   (file-name-nondirectory path-to-write))
        (mkdir (file-name-directory path-to-write) t)
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
        (unwind-protect
            (run-hooks 'org-node-creation-hook)
          (save-buffer)
          ;; REVIEW Redundant?
          (org-id-add-location org-node-proposed-id path-to-write))))))

(defun org-node-new-via-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn', at which
time some necessary variables are set."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "`org-node-new-via-roam-capture' is meant to be called indirectly via `org-node--create'")
    (unless (fboundp #'org-roam-capture-)
      (org-node--die "Didn't create node! Either install org-roam or %s"
                     "configure `org-node-creation-fn'"))
    (require 'org-roam)
    (org-roam-capture- :node (org-roam-node-create
                              :title org-node-proposed-title
                              :id    org-node-proposed-id))))

;; TODO: write a template to capture into an org-journal file
;; (defun org-node-capture-target-day ()
;;   )

(defun org-node-capture-target ()
  "Can be used as target in a capture template.
See `org-capture-templates' for more info about targets.

In simple terms, let's say you have configured
`org-capture-templates' so it has a template that
targets `(function org-node-capture-target)'.  Now here's a
possible workflow:

1. Run M-x org-capture
2. Select your template
3. Type name of known or unknown node
4a. If it was known, it will capture into that node.
4b. If it was unknown, it will create a file-level node and then capture
    into there.

Additionally, if you've set (setq org-node-creation-fn #'org-capture),
commands like `org-node-find' will outsource to org-capture when you
type the name of a node that does not exist.  That enables this
\"inverted\" workflow:

1. Run M-x org-node-find
2. Type name of an unknown node
3. Select your template
4. Same as 4b earlier."
  (org-node-cache-ensure)
  (let (title node id)
    (if org-node-proposed-title
        ;; Was called from `org-node--create', so the user already typed the
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
             (path-to-write (file-name-concat
                             dir (org-node--name-file title))))
        (if (or (file-exists-p path-to-write)
                (find-buffer-visiting path-to-write))
            (error "File or buffer already exists: %s" path-to-write)
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
          (unwind-protect
              (run-hooks 'org-node-creation-hook)
            (save-buffer)
            (org-node--scan-targeted (list path-to-write))))))))


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
      (org-node--create input (org-id-new)))))

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

To behave exactly like org-roam's `org-roam-node-insert',
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
                        ;; HACK input is already what the user selected, but
                        ;; when `org-node-alter-candidates' = t, we dont want
                        ;; to use the whole completion candidate
                        (and node (--find (string-search it input)
                                          (org-node-get-aliases node)))
                        (and node (org-node-get-title node))
                        input)))
    (atomic-change-group
      (when region-text
        (delete-region beg end))
      (insert (org-link-make-string (concat "id:" id) link-desc))
      (run-hook-with-args 'org-node-insert-link-hook id link-desc))
    ;; TODO: Delete the link if a node was not created
    (unless node
      (org-node--create input id))))

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
which the region should be linkified, you'll prefer
`org-node-insert-link'.

The commands are actually the same, it is just a difference in
initial input.

On the topic of Org-roam emulation, bonus tips:

- To behave like Org-roam on node creation, set
  `org-node-creation-fn' to `org-node-new-via-roam-capture'.

- If you still find the behavior different, perhaps you had
  something in `org-roam-post-node-insert-hook'.  Configure
  `org-node-insert-link-hook' the same way."
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

(setq org-transclusion-exclude-elements
      '(property-drawer comment keyword))"
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
      ;; cut out the initial heading because we already made a heading.  (And
      ;; we made the heading so that this transclusion will count as a
      ;; backlink, plus it makes more sense to me on export to HTML).
      ;;
      ;; Unfortunately the :lines trick would prevent
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
      (run-hook-with-args 'org-node-insert-link-hook id title))))

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
property.  It is subjective whether you'd want this behavior, but
it can be desirable if you know the subtree had been part of the
source file for ages so that you see the ancestor's creation-date
as more \"truthful\" than today's date.


(defun my-inherit-creation-date (orig-fn &rest args)
       (let ((parent-creation (org-entry-get nil \"CREATED\" t)))
         (apply orig-fn args)
         ;; Now in the new buffer
         (org-entry-put nil \"CREATED\"
                        (or parent-creation
                            (format-time-string \"[%F %a]\")))))"
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
           (path-to-write (file-name-concat
                           dir (org-node--name-file title)))
           (source-path buffer-file-name))
      (if (file-exists-p path-to-write)
          (message "A file already exists named %s" path-to-write)
        (org-cut-subtree)
        ;; Leave a link under the parent heading pointing to the subheading
        ;; that was extracted.
        (save-excursion
          (without-restriction
            (org-up-heading-or-point-min)
            (goto-char (org-entry-end-position))
            (if (org-invisible-p)
                (message "Invisible area, not inserting link to extracted")
              (open-line 1)
              (insert "\n"
                      (format-time-string
                       (format "[%s] Created " (car org-timestamp-formats)))
                      (org-link-make-string (concat "id:" id) title)
                      "\n"))))
        (save-buffer)
        (find-file path-to-write)
        (org-paste-subtree)
        (save-buffer)
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
        (run-hooks 'org-node-creation-hook)
        (save-buffer)
        ;; TODO: arrange so the backlink-mode backlink appears
        (org-node--scan-targeted (list path-to-write source-path))))))

;; Transitional wrapper
(defun org-node--name-file (title)
  (if org-node-filename-fn
      (concat (string-remove-suffix ".org"
                                    (funcall org-node-filename-fn title))
              ".org")
    (if org-node-slug-fn
        (concat
         (and org-node-datestamp-format
              (format-time-string org-node-datestamp-format))
         (funcall org-node-slug-fn title)
         ".org")
      (error "`org-node-slug-fn' not set"))))

(defun org-node--make-regexp-for-time-format (format)
  "Make regexp to match a (format-time-string FORMAT) result."
  (let ((example (format-time-string format)))
    (if (string-match-p (rx (any "^*+([\\")) example)
        (error "org-node: Unable to safely rename with current `org-node-datestamp-format'.  This is not inherent in your choice of format, I am just not smart enough")
      (concat "^"
              (replace-regexp-in-string
               "[[:digit:]]+" "[[:digit:]]+"
               (replace-regexp-in-string
                "[[:alpha:]]+" "[[:alpha:]]+"
                example t))))))

;; FIXME (buffer-modified-p) returns t on after-save-hook
(defun org-node-rename-file-by-title-maybe ()
  "Rename current file according to TITLE.
In addition to the checks described in
`org-node-rename-file-by-title', will refuse to rename files that
are outside `org-roam-directory'.  Suitable as a save hook:

    (add-hook 'after-save-hook #'org-node-rename-file-by-title-maybe)"
  (when (derived-mode-p 'org-mode)
    (unless (bound-and-true-p org-roam-directory)
      (user-error "org-node-rename-file-by-title-maybe: Variable `org-roam-directory' must be set (org-roam not needed)"))
    (when (string-prefix-p org-roam-directory (buffer-file-name))
      (org-node-rename-file-by-title))))

;;;###autoload
(defun org-node-rename-file-by-title (&optional interactive)
  "Rename the current file according to `org-node-slug-fn'.

Attempt to check for a prefix in the style of
`org-node-datestamp-format', and preserve it.  However, if the
deprecated option `org-node-filename-fn' is set, then overwrite
the entire filename anyway.

When called from a hook (or from Lisp in general), will also
check if the file appears to be under
`org-roam-dailies-directory' or `org-journal-dir', and do nothing
in these cases.  Used interactively, it is still possible to
act on files in these directories (or if Lisp code passes
INTERACTIVE t).

Note that it is rarely a good idea to run this on a save-hook.
See the alternative `org-node-rename-file-by-title-maybe',
which wraps this function."
  (interactive "p" org-mode)
  (when (stringp interactive)
    (user-error "org-node-rename-file-by-title: PATH argument deprecated"))
  (if (not (derived-mode-p 'org-mode))
      (when interactive
        (user-error "Only works in org-mode buffers"))
    (let ((path (buffer-file-name)))
      (unless (equal "org" (file-name-extension path))
        (error "File doesn't end in .org: %s" path))
      (when (or interactive
                ;; Be aware of "dailies" and don't touch
                (and (or (not (bound-and-true-p org-journal-dir))
                         (not (string-prefix-p org-journal-dir path)))
                     (or (not (bound-and-true-p org-roam-dailies-directory))
                         (not (string-prefix-p
                               (expand-file-name org-roam-dailies-directory
                                                 org-roam-directory)
                               path)))))
        (let* ((title (or (cadar (org-collect-keywords '("TITLE")))
                          (save-excursion
                            (without-restriction
                              ;; No title, use first heading in file
                              (goto-char 1)
                              (or (org-at-heading-p)
                                  (outline-next-heading))
                              (org-get-heading t t t t)))))
               (basename (file-name-nondirectory path))
               (date-prefix (if (and org-node-datestamp-format
                                     (string-match
                                      (org-node--make-regexp-for-time-format
                                       org-node-datestamp-format)
                                      basename))
                                (match-string 0 basename)
                              ""))
               (new-path (file-name-concat
                          (file-name-directory path)
                          ;; HACK 2024-07-21
                          ;; Use old behavior if old option set
                          (if org-node-filename-fn
                              (funcall org-node-filename-fn title)
                            (concat date-prefix
                                    (funcall org-node-slug-fn title)
                                    ".org"))))
               (visiting (find-buffer-visiting path))
               (visiting-on-window (and visiting (get-buffer-window visiting))))

          (if (equal path new-path)
              (when interactive
                (message "Filename already correct: %s" path))
            (if (and visiting (buffer-modified-p visiting))
                (when interactive
                  (message "Unsaved file, letting it be: %s" path))
              (if (get-file-buffer new-path)
                  (if interactive
                      (message "A buffer is already visiting the would-be new filename")
                    (user-error "A buffer is already visiting the would-be new filename"))
                (unless (file-writable-p path)
                  (user-error "No permissions to rename file: %s" path))
                (unless (file-writable-p new-path)
                  (user-error "No permissions to write a new file at: %s" new-path))
                ;; Unnecessary b/c `rename-file' will already warn, but hey
                (when (file-exists-p new-path)
                  (user-error "Canceled because a file exists at: %s" new-path))
                ;; Kill buffer before renaming, because it will not follow the rename
                (when visiting
                  (kill-buffer visiting))
                (rename-file path new-path)
                ;; Visit the file again if you had it open
                (when visiting
                  (let ((buf (find-file-noselect new-path)))
                    (when visiting-on-window
                      (set-window-buffer visiting-on-window buf))))
                (message "File %s renamed to %s"
                         (file-name-nondirectory path)
                         (file-name-nondirectory new-path))))))))))

;;;###autoload
(defun org-node-rewrite-links-ask (&optional files)
  "Search all files for ID-links where the link description has
gotten out of sync from the destination's current title.

At each link, prompt for user consent, then auto-update the link
so it matches the destination's current title."
  (interactive)
  (require 'ol)
  (require 'org-faces)
  (defface org-node--rewrite-face
    `((t :inherit 'org-link
         :inverse-video ,(not (face-inverse-video-p 'org-link))))
    "Face for use in `org-node-rewrite-links-ask'.")
  (org-node-cache-ensure)
  (when (org-node--consent-to-bothersome-modes-for-mass-edit)
    (dolist (file (or files (org-node-files)))
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
                  (org-show-context)
                  (org-show-entry)
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
  "Prompt for an asset such as an image file to be renamed, then
search recursively for Org files containing a link to that asset,
open a wgrep buffer of the search hits, and start an interactive
search-replace that updates the links.  After the user consents
to replacing all the links, finally rename the asset file itself."
  (interactive)
  (unless (fboundp 'wgrep-change-to-wgrep-mode)
    (user-error "This command requires the wgrep package"))
  (require 'wgrep)
  (let ((root (car (org-node--root-dirs (org-node-files))))
        (default-directory default-directory))
    (or (equal default-directory root)
        (if (y-or-n-p (format "Go to this folder? %s" root))
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
      (message "Waiting for rgrep to populate buffer..."))))

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
                   (concat "["
                           (format-time-string (car org-timestamp-formats))
                           "]"))))

(defvar org-node--temp-extra-fns nil
  "Extra functions to run at the end of a full scan.
The list is emptied on each use.  Primarily exists to give the
interactive command `org-node-reset' a way to print the time
elapsed.")

;;;###autoload
(defun org-node-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (push #'org-node--print-elapsed org-node--temp-extra-fns)
  (org-node-cache-ensure nil t))

;;;###autoload
(defun org-node-forget-dir (dir)
  "Remove references in `org-id-locations' to files in DIR.

Note that if DIR can be found under `org-node-extra-id-dirs',
this action may make no practical impact unless you also add DIR
to `org-node-extra-id-dirs-exclude'."
  (interactive "DForget all IDs in directory: ")
  (org-node-cache-ensure t)
  (let ((files (seq-intersection
                (mapcar #'abbreviate-file-name
                        (directory-files-recursively dir "\\.org$"))
                (hash-table-values org-id-locations))))
    (when files
      (message "Forgetting all IDs in directory... (%s)" dir)
      (org-node--forget-id-locations files)
      (org-id-locations-save)
      (org-node-reset))))

;;;###autoload
(defun org-node-grep ()
  (interactive)
  (unless (fboundp #'consult--grep)
    (user-error "This command requires the consult package"))
  (require 'consult)
  (org-node--init-ids)
  (consult--grep "Grep across all files known to org-node"
                 #'consult--grep-make-builder
                 (org-node-files)
                 nil))

(defvar org-node--linted nil
  "List of files linted so far.")

;;;###autoload
(defun org-node-lint-all-files (&optional C-u)
  "Run `org-lint' on all known Org files, and report results."
  (interactive "P")
  (org-node--init-ids)
  (let* ((warnings nil)
         (report-buffer (get-buffer-create "*org-node lint report*"))
         (files (-difference (org-node-files) org-node--linted))
         (ctr (length org-node--linted))
         (ctrmax (+ (length files) (length org-node--linted))))
    (with-current-buffer report-buffer
      (when (or (null files) C-u)
        ;; Start over
        (when (y-or-n-p "Wipe the previous lint results? ")
          (setq files (org-node-files))
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
        (script-file (org-node-worker--tmpfile "analyze_feedback_arcs.R"))
        (digraph-tsv (org-node-worker--tmpfile "id_node_digraph.tsv")))
    (write-region r-code nil script-file)
    (write-region (org-node--make-digraph-tsv-string) nil digraph-tsv)
    (with-current-buffer (get-buffer-create "*feedback arcs*")
      (fundamental-mode)
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (unless (= 0 (call-process "Rscript" nil t nil script-file digraph-tsv))
        (error "%s" (buffer-string)))
      (erase-buffer)
      (insert-file-contents (org-node-worker--tmpfile "feedback-arcs.eld"))
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
  "From `org-node--dest<>links', generate a list of
destination-origin pairs, expressed as Tab-Separated Values."
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
;;       to help validate `org-node-perf-assume-coding-system'
;; (defvar org-node--found-systems nil)
;; (defun org-node-list-file-coding-systems ()
;;   (dolist (file (take 20 (org-node-files)))
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

Useful to see how many times you've inserted a link that is very
similar to another, but not identical, preventing its association
to ROAM_REFS."
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
                                              (org-show-context)
                                              (org-show-entry))
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

(defvar org-node--collisions nil)
(defun org-node-list-collisions ()
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

(defvar org-node--problems nil)
(defun org-node-list-scan-problems ()
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


;;;; CAPF (Completion-At-Point Function)

(defvar org-node--roam-settings nil)
(define-minor-mode org-node-complete-at-point-mode
  "Use `org-node-complete-at-point' in all Org buffers.
Also turn off Org-roam's equivalent, if active.

-----"
  :global t
  :require 'org-node
  (when (featurep 'org-roam)
    (unless (assoc 'org-roam-completion-everywhere org-node--roam-settings)
      (push (cons 'org-roam-completion-everywhere
                  org-roam-completion-everywhere)
            org-node--roam-settings)))
  (if org-node-complete-at-point-mode
      ;; Turn on
      (progn
        (setq org-roam-completion-everywhere nil)
        (add-hook 'org-mode-hook #'org-node--install-capf-in-buffer)
        ;; Add to already-open buffers
        (dolist (buf (buffer-list))
          (with-current-buffer buf
            (when (and buffer-file-name (derived-mode-p 'org-mode))
              (add-hook 'completion-at-point-functions
                        #'org-node-complete-at-point nil t)))))
    ;; Turn off
    (remove-hook 'org-mode-hook #'org-node--install-capf-in-buffer)
    ;; Remove from already-open buffers
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (remove-hook 'completion-at-point-functions
                     #'org-node-complete-at-point t)))
    ;; Maybe reenable Org-roam's capf
    (setq org-roam-completion-everywhere
          (alist-get 'org-roam-completion-everywhere
                     org-node--roam-settings))))

(defun org-node--install-capf-in-buffer ()
  "Conf in-buffer completion to try `org-node-complete-at-point'."
  (and buffer-file-name
       (derived-mode-p 'org-mode)
       (equal "org" (file-name-extension buffer-file-name))
       (add-hook 'completion-at-point-functions
                 #'org-node-complete-at-point nil t)))

(defun org-node-complete-at-point ()
  "Complete word at point to any known node title, and linkify.
Designed for `completion-at-point-functions', which see."
  (let ((bounds (bounds-of-thing-at-point 'word)))
    (and bounds
         ;; For some reason it runs in non-org buffers like grep results?
         (derived-mode-p 'org-mode)
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
  (org-super-links-convert-link-to-super nil))

(defun org-node-try-visit-ref-node ()
  "Designed for `org-open-at-point-functions'.

For the link at point, if there exists an org-ID node that has
the link in its ROAM_REFS property, visit that node rather than
following the link normally.

If already visiting that node, then follow the link normally."
  (let* ((url (thing-at-point 'url))
         (found (cl-loop for node being the hash-values of org-nodes
                         when (member url (org-node-get-refs node))
                         return node)))
    (if (and found
             ;; check that point is not already in the ref node (if so, better
             ;; to fallback to default `org-open-at-point' logic)
             (not (and (derived-mode-p 'org-mode)
                       (equal (org-entry-get nil "ID" t)
                              (org-node-get-id found)))))
        (progn (org-node--goto found)
               t)
      nil)))


;;;; API not used inside this package

(defun org-node-at-point ()
  "Return the ID-node near point.

This may refer to the current Org heading, else an ancestor
heading, else the file-level node, whichever has an ID first."
  (gethash (org-entry-get nil "ID" t) org-node--id<>node))

(defun org-node-read ()
  "Prompt for a known ID-node."
  (gethash (completing-read "Node: " #'org-node-collection
                            () () () 'org-node-hist)
           org-node--candidate<>node))

(provide 'org-node)

;;; org-node.el ends here
