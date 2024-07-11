;;; org-node-common.el -*- lexical-binding: t; -*-

;; TODO The perf-* user options now seem to have almost no effect, there must
;;      be significant overhead on the child processes - eliminate it

(require 'compat)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'org-id)
(require 'org)

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

(defcustom org-node-rescan-hook nil
  "Hook run after scanning specific files.
It is not run after a full cache reset, only after a file is
saved or renamed, causing an incremental update to the cache.

Called with one argument: the list of files re-scanned."
  :group 'org-node
  :type 'hook)

(defcustom org-node-ask-directory nil
  "Whether to ask the user where to save a new file node.

- Symbol nil: put file in the most populous root directory in
       `org-id-locations' without asking
- String: a directory path in which to put the file
- Symbol t: ask every time, starting from the current directory"
  :group 'org-node
  :type 'boolean)

(defcustom org-node-warn-title-collisions t
  ""
  :group 'org-node
  :type 'boolean)

(defcustom org-node-make-file-level-nodes t
  "If t, write a file-level title and property drawer when
making a new file, otherwise write a more traditional top-level
heading.

This affects the behavior of `org-node-new-file',
`org-node-extract-subtree', and `org-node-capture-target'.

If you change your mind about this setting, the Org-roam commands
`org-roam-promote-entire-buffer' and
`org-roam-demote-entire-buffer' can help you detransition the
files you have made along the way."
  :group 'org-node
  :type 'boolean)

(defcustom org-node-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that org-node can scan for ID nodes inside .org.gpg files.

I don't use EPG so I don't know if that's enough to make it work.
Probably not.  Get me working on it by dropping me a line on
https://github.com/meedstrom/org-node/issues or Mastodon:
@meedstrom@emacs.ch."
  :group 'org-node
  :type 'alist)

;; TODO Maybe suggest `utf-8-auto-unix', but first find out if the
;; coding-system-for-write infers from coding-system-for-read
(defcustom org-node-perf-assume-coding-system nil
  "Coding system to assume while scanning ID nodes.

Picking a specific coding system can speed up `org-node-reset',
sometimes significantly.  Set nil to let Emacs figure it out anew
on every file.

Modern GNU/Linux, BSD and Mac systems almost always encode new
files as `utf-8-unix'.

On Windows this probably should be nil.  Same if you access your
files from multiple platforms.

Note that if your Org collection is old and has survived several
system migrations, or some of it was generated via Pandoc
conversion or downloaded, it's very possible that there's a mix
of coding systems among them.  In that case, setting this
variable non-nil may cause org-node to fail to scan some of them."
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

(defcustom org-node-perf-gc-cons-threshold most-positive-fixnum
  "Temporary override for `gc-cons-threshold'.
Tweak to maybe speed up `org-node-reset'.  Set nil to use the
actual value of `gc-cons-threshold'.

It can be surprising which value works best.  It is possible that
80 kB is performant, and 16 MB is performant, but something in
between such as 1 MB is twice as slow.  Experiment to find a good
setting."
  :group 'org-node
  :type '(choice integer (const nil)))

(defcustom org-node-filename-fn #'org-node-slugify-as-url
  "Function taking a #+TITLE and returning a filename.

This is used when you type a new node title, and a file is
created for you.

Built-in choices:
- `org-node-slugify-for-web'
- `org-node-slugify-like-roam'"
  :group 'org-node
  :type '(radio
          (function-item org-node-slugify-like-roam)
          (function-item org-node-slugify-as-url)
          function))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called by `org-node-find' and `org-node-insert-link' to
create a node that does not yet exist.

Built-in choices:
- `org-node-new-file'
- `org-node-new-by-roam-capture'
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
          (function-item org-node-new-by-roam-capture)
          (function-item org-capture)
          function))

(defcustom org-node-format-candidate-fn
  #'org-node-format-bare
  "Function to return a string to represent a given node.
Affects how selections are displayed during e.g. `org-node-find'.

Called with two arguments: the node data and the title.

The title may in fact be one of the aliases and not the canonical
title, because the completion builder calls this again for every
alias.

The node data is an object which form you can observe in examples
from \\[org-node-cache-peek] and specified in the type
`org-node-get'."
  :group 'org-node
  :type '(radio
          (function-item org-node-format-with-olp)
          (function-item org-node-format-bare)
          function))

(defcustom org-node-filter-fn
  (lambda (node)
    (not (assoc "ROAM_EXCLUDE" (org-node-get-properties node))))
  "Predicate returning t to include a node, or nil to exclude it.

The filtering only has an impact on `org-node--node-by-candidate', which
forms the basis for completions in the minibuffer, and
`org-node--id-by-title', used by the in-buffer
`org-node-complete-at-point'.  In other words, passing nil means
the user cannot autocomplete to the node, but Lisp code can still
find it in `org-node--node-by-id'.

This function is applied once for every org-id node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-cache-peek] and
specified in the type `org-node-get' (search with C-h o).

See the following example for a way to filter out nodes with a
ROAM_EXCLUDE property, or that have any kind of TODO state, or
are tagged :drill:, or where the full file path contains the word
\"archive\".

(setq org-node-filter-fn
      (lambda (node)
        (and (not (assoc \"ROAM_EXCLUDE\" (org-node-get-properties node)))
             (not (org-node-get-todo node))
             (not (string-search \"archive\" (org-node-get-file-path node)))
             (not (member \"drill\" (org-node-get-tags node))))))"
  :group 'org-node
  :type 'function)

(defcustom org-node-insert-link-hook '()
  "Hook run after inserting a link to an Org-ID node.

Called with two arguments: the ID and the link description.

Point will be positioned at the link."
  :group 'org-node
  :type 'hook)

(defcustom org-node-creation-hook '(org-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied only by `org-node-new-file', `org-node-capture-target',
`org-node-insert-heading', `org-node-nodeify-entry' and
`org-node-extract-subtree'.

NOT applied by `org-node-new-by-roam-capture' -- see org-roam's
`org-roam-capture-new-node-hook' instead.

A good member to put in this hook is `org-node-put-created',
especially since the default `org-node-filename-fn' does not put
a timestamp in the filename."
  :group 'org-node
  :type 'hook)

(defcustom org-node-extra-id-dirs (list)
  "Directories in which to search Org files for IDs.

Same idea as `org-id-extra-files', but specifies only directories
to achieve a similar convenience to `org-agenda-files'.

Unlike with `org-agenda-files', directories listed here may be
scanned again in order to find new files that have appeared.

These directories are only scanned as long as
`org-node-cache-mode' is active.  They are scanned
recursively (looking in subdirectories, sub-subdirectories etc).

To avoid accidentally picking up duplicate files such as
versioned backups, causing org-id to complain about \"duplicate
IDs\", configure `org-node-extra-id-dirs-exclude'.

Tip: If it happened anyway, try \\[org-node-forget-dir], because
merely removing a directory from this list does not forget the
IDs already found.

Warning: If you have custom elements in `directory-abbrev-alist',
none of them should match the part of a file path that comes
after one of the dirs listed here.  This is because upstream
Org-id applies `abbreviate-file-name' to all files, but that is
an expensive operation which Org-node opted against - it will
only do it up to the directory."
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
causing org-id to complain about \"duplicate IDs\".

For all other \"excludey\" purposes, you probably want to
configure `org-node-filter-fn' instead.

If you have accidentally added a directory of backup files, try
\\[org-node-forget-dir].

It is not necessary to exclude backups or autosaves that end in ~
or # or .bak since `org-node-files' only considers files that end
in precisely \".org\" or \".org_archive\" anyway."
  :group 'org-node
  :type '(repeat string))


;;; Functions & variables

(defun org-node-format-with-olp (node title)
  "Prepend TITLE with NODE's outline path."
  (if (org-node-get-is-subtree node)
      (let ((ancestors (cons (org-node-get-file-title-or-basename node)
                             (org-node-get-olp node)))
            (result nil))
        (dolist (anc ancestors)
          (push (propertize anc 'face 'completions-annotations) result)
          (push " > " result))
        (push title result)
        (string-join (nreverse result)))
    title))

(defun org-node-format-bare (_node title)
  "Return TITLE as-is."
  title)

(defun org-node--record-link-at-point (id _desc)
  (when (derived-mode-p 'org-mode)
    (when-let ((origin (org-entry-get nil "ID" t)))
      (push (list :origin origin
                  :pos (point)
                  :type "id"
                  :dest id
                  :properties (list :outline (ignore-errors
                                               (org-get-outline-path t))))
            (gethash id org-node--backlinks-by-id)))))

;; Wishlist: that `find-file-noselect' (and maybe also `find-file') when called
;; from Lisp would take an optional argument that explains why the file is
;; about to be opened, changing the autosave recovery prompt.  In lieu of that,
;; hack up a gatekeeper prompt.
(defvar org-node--imminent-recovery-msg
  "Org-node about to look inside or edit a file, but it's likely you will first get a prompt to recover an auto-save file, ready? "
  "For use by `org-node--with-quick-file-buffer'.")

;; NOTE Very important macro for the backlink mode, because backlink insertion
;;      opens an Org file, and if doing that is laggy, then every link
;;      insertion is laggy.
(defmacro org-node--with-quick-file-buffer (file &rest body)
  "Pseudo-backport of Emacs 29 `org-with-file-buffer'.
Also integrates `org-with-wide-buffer' behavior, some magic
around saving, and tries to open FILE quickly i.e. with
minimal hooks.

In short, if a buffer was visiting FILE, go to that existing
buffer, else visit FILE in a new buffer, in which case ignore the
usual Org startup checks.  With that as the current buffer,
temporarly `widen' it, execute BODY, then restore point.
Finally:

- If a new buffer had to be opened: save and kill it.  Mandatory
  because buffers opened in the quick way look \"wrong\" as many
  hooks did not run, e.g. no indent-mode, no visual wrap etc.
- If a buffer had been open: leave it open and leave it unsaved."
  (declare (indent 1) (debug t))
  `(let ((find-file-hook nil)
         (org-element-use-cache nil) ;; generally prevents bugs
         (after-save-hook nil)
         (kill-buffer-hook nil) ;; inhibit save-place etc
         (before-save-hook nil)
         (org-agenda-files nil)
         (org-inhibit-startup t))
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

;; REVIEW Maybe save 50ms (of total 200ms) by recording only
;; unabbreviated file names in mem and the mtimes table, and deferring
;; abbreviation until the ..scan fn.  FWIW, most of the other 150ms is
;; `directory-files-recursively', hard to improve.
(let (mem)
  (defun org-node-files (&optional memoized)
    "List files in `org-id-locations' or `org-node-extra-id-dirs'.

With argument MEMOIZED t, reuse the list from the last time
something called this function, returning instantly."
    (if (and memoized mem)
        mem
      (setq mem
            (-union
             (hash-table-values org-id-locations)
             (let ((file-name-handler-alist nil))
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
                        collect (abbreviate-file-name file)))))))))

(defun org-node--split-into-n-sublists (big-list n)
  "Split BIG-LIST into a list of N sublists.

In the special case where BIG-LIST contains fewer than N
elements, the return value is just like BIG-LIST except that each
element is wrapped in its own list."
  (let ((sublist-length (max 1 (/ (length big-list) n)))
        result)
    (dotimes (i n)
      (if (= i (1- n))
          ;; Let the last iteration just take what's left
          (push big-list result)
        (push (take sublist-length big-list) result)
        (setq big-list (nthcdr sublist-length big-list))))
    (delq nil result)))

(defvar org-node--node-by-id (make-hash-table :test #'equal)
  "1:1 table mapping ids to nodes.
To peek on the contents, try \\[org-node-cache-peek] a few times, which
can demonstrate the data format.  See also the type `org-node-get'.")

(defvaralias 'org-nodes 'org-node--node-by-id)

(defvar org-node--node-by-candidate (make-hash-table :test #'equal)
  "1:1 table mapping completion candidates to nodes.")

(defvar org-node--mtime-by-file (make-hash-table :test #'equal)
  "1:1 table mapping files to modification times.")

(defvar org-node--id-by-title (make-hash-table :test #'equal)
  "1:1 table mapping raw titles (and ROAM_ALIASES) to IDs.")

(defvar org-node--id-by-ref (make-hash-table :test #'equal)
  "1:1 table mapping ROAM_REFS members to the adjacent ID.")

(defvar org-node--backlinks-by-id (make-hash-table :test #'equal)
  "1:N table of ID-links.

(defvar org-node--affixation-by-candidate (make-hash-table :test #'equal))

The table keys are destination IDs, and the corresponding table
value is a list of plists describing each link, including naming
the ID-node where the link originated.")

;; REVIEW May have to look over the naming... "ref" has come to mean any link
;; matching `org-link-plain-re' that isn't of type "id", end of story.  Under
;; that definition, a citation @key is not a ref, which is surprising.
(defvar org-node--reflinks-by-ref (make-hash-table :test #'equal)
  "1:N table mapping refs to latent reflinks.

The table keys are \"refs\" i.e. plain URI strings such as web
addresses or \"file:\" links, and the table values are lists
holding a plist for each place where that URI was found.

To see how these plists look, see source of
`org-node-worker--collect-links-until'.

In truth, passing this table to `hash-table-keys' will get you a
grand list of all (non-ID) links found in any node anywhere,
whether or not a node exists with a corresponding ROAM_REFS
property.

To know whether one of these \"latent\" reflinks has a node
actually referencing it, check for the the same ref in
`org-node--id-by-ref'.  Or use the convenience wrapper
`org-node-get-reflinks' if suitable.")

(defvar org-node--cites-by-citekey (make-hash-table :test #'equal)
  "1:N table mapping @citekeys to latent citations.
For explanation of \"latent\", see `org-node--reflinks-by-ref'.")

(defun org-node-get-reflinks (node)
  "Get list of reflinks pointing to NODE."
  (cl-loop for ref in (org-node-get-refs node)
           append (gethash ref org-node--reflinks-by-ref)))

(defun org-node-get-backlinks (node)
  "Get list of ID-links pointing to NODE."
  (gethash (org-node-get-id node) org-node--backlinks-by-id))

(defun org-node--forget-id-locations (files)
  "Remove references to FILES in `org-id-locations'.
You might consider \"committing\" the effect afterwards by
calling `org-id-locations-save', which this function does not do."
  (when files
    ;; (setq org-id-files (-difference org-id-files files)) ;; Redundant
    (let ((alist (org-id-hash-to-alist org-id-locations)))
      (cl-loop for file in files
               do (assoc-delete-all file alist))
      (setq org-id-locations (org-id-alist-to-hash alist)))))

(defun org-node--die (format-string &rest args)
  "Like `error' but make sure the user sees it.
Useful because not everyone has `debug-on-error' t, and then
errors are very easy to miss."
  (let ((err-string (apply #'format-message format-string args)))
    (display-warning 'org-node err-string :error)
    (error "%s" err-string)))

;; Test by evalling:
;; (org-node--root-dirs (hash-table-values org-id-locations))
(defun org-node--root-dirs (file-list)
  "Given FILE-LIST, infer the most base root directories.

If FILE-LIST is the `hash-table-values' of `org-id-locations',
this function will in many cases spit out a list of one item
because many people keep their Org files in one root
directory (with various subdirectories).

If it finds more than one root, it sorts by count of files they
contain, so that the most populous root directory will be the
first element.

FILE-LIST must be a list of full paths.  This function does not
consult the filesystem, just compares substrings to each other."
  (let* ((files (-uniq file-list))
         (directories (-uniq (mapcar #'file-name-directory files))))
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child
    (cl-loop for dir in directories
             as dirs-aside-from-this-one = (remove dir directories)
             when (--any-p (string-prefix-p it dir) dirs-aside-from-this-one)
             do (setq directories (delete dir directories)))
    ;; Now sort by count of items inside if we found 2 or more roots
    (if (= (length directories) 1)
        directories
      (cl-loop
       with dir-counters = (--map (cons it 0) directories)
       for file in files
       as the-root = (--find (string-prefix-p it file) directories)
       do (cl-incf (cdr (assoc the-root dir-counters)))
       finally return (mapcar #'car (cl-sort dir-counters #'> :key #'cdr))))))

;; REVIEW deprecate?
(defun org-node--consent-to-problematic-modes-for-mass-edit ()
  (--all-p (if (and (boundp it) (symbol-value it))
               (y-or-n-p
                (format "%S is active - proceed anyway?" it))
             t)
           '(auto-save-visited-mode
             git-auto-commit-mode)))

(defvar org-node--debug nil
  "Whether to run in a way suitable for debugging.")

(defun org-node--debug-scan-this-file ()
  (interactive)
  (when (derived-mode-p 'org-mode)
    (org-node-cache--scan-targeted (list buffer-file-name))))

(defun org-node--debug-toggle ()
  (interactive)
  (setq org-node--debug (not org-node--debug)))


;;; Data structure

(cl-defstruct (org-node-get (:constructor org-node--make-obj)
                            (:copier nil))
  "An org-node object holds information about an Org ID node.
By the term \"Org ID node\", we mean either a subtree with
an ID property, or a file with a file-level ID property.  The
information is stored in slots listed below.

For each slot, there exists a getter function
\"org-node-get-FIELD\".

For example, the field \"deadline\" has a getter
`org-node-get-deadline'.  So you would type
\"(org-node-get-deadline NODE)\", where NODE is one of the
elements of the `hash-table-values' of `org-node--node-by-id'.

For real-world usage of these accessors, see examples in the
documentation of `org-node-filter-fn', the documentation of
`org-node-format-candidate-fn', or the package README.

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
  (properties nil :read-only t :type alist :documentation
              "Returns alist of properties from the :PROPERTIES: drawer.")
  (priority   nil :read-only t :type string :documentation
              "Returns priority such as [#A], as a string.")
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


;;; Obsoletions

;; 2024-07-11
(defun org-node-aliases    (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-aliases,     update to org-node-get-aliases   " 78)))
(defun org-node-deadline   (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-deadline,    update to org-node-get-deadline  " 78)))
(defun org-node-file-path  (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-file-path,   update to org-node-get-file-path " 78)))
(defun org-node-file-title (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-file-title,  update to org-node-get-file-title" 78)))
(defun org-node-is-subtree (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-is-subtree,  update to org-node-get-is-subtree" 78)))
(defun org-node-id         (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-id,          update to org-node-get-id        " 78)))
(defun org-node-level      (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-level,       update to org-node-get-level     " 78)))
(defun org-node-olp        (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-olp,         update to org-node-get-olp       " 78)))
(defun org-node-pos        (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-pos,         update to org-node-get-pos       " 78)))
(defun org-node-properties (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-properties,  update to org-node-get-properties" 78)))
(defun org-node-refs       (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-refs,        update to org-node-get-refs      " 78)))
(defun org-node-scheduled  (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-scheduled,   update to org-node-get-scheduled " 78)))
(defun org-node-tags       (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-tags,        update to org-node-get-tags      " 78)))
(defun org-node-title      (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-title,       update to org-node-get-title     " 78)))
(defun org-node-todo       (node) (org-node--die "%s" (string-fill "\nYour config uses deprecated accessor org-node-todo,        update to org-node-get-todo      " 78)))

(define-obsolete-variable-alias
  'org-node-collection 'org-node--node-by-candidate "2024-07-11")

(define-obsolete-variable-alias
  'org-node-slug-fn 'org-node-filename-fn "2024-07-11")

(define-obsolete-variable-alias
  'org-node-cache-rescan-file-hook 'org-node-rescan-hook "2024-07-11")

;; 2024-07-11
(add-hook 'org-node-rescan-hook #'org-node--deprecate-rescan-hook-old-usage)
(defun org-node--deprecate-rescan-hook-old-usage (&optional files)
  "Emit a warning for deprecated usage."
  (when (null files)
    (display-warning 'org-node (string-fill "\nBreaking change: The hook `org-node-cache-rescan-file-hook' is renamed to `org-node-rescan-hook', and is no longer called with any specific buffer current, and instead passes the list of files scanned (one argument, not zero)" 78))))

(provide 'org-node-common)

;;; org-node-common.el ends here
