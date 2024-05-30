;;; org-node-common.el -*- lexical-binding: t; -*-

(require 'compat)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'org-id)
(require 'org)

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

(defcustom org-node-ask-directory nil
  "Whether to ask the user where to save a new file node.

Set nil to assume that the most populous root directory in
`org-id-locations' is always the correct directory."
  :group 'org-node
  :type 'boolean)

(defcustom org-node-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that `org-node-cache-mode' can scan for ID nodes inside .org.gpg
files.

I don't use EPG so I don't know if that's enough to make it work."
  :group 'org-node
  :type 'alist)

;; TODO: Maybe suggest `utf-8-auto-unix', but first find out if the
;; coding-system-for-write infers from coding-system-for-read
(defcustom org-node-perf-assume-coding-system nil
  "Coding system to assume while scanning ID nodes.

Picking a specific coding system can speed up `org-node-reset',
sometimes significantly.  Set nil to let Emacs figure it out anew
on every file.

Modern GNU/Linux and BSD systems almost always encode new files
as `utf-8-unix'.  On Mac it would be `utf-8-mac'.

On Windows this probably should be nil.  Same if you access your
files from multiple platforms.

Note that if your Org collection is old and has survived several
system migrations, or some of it was generated via Pandoc
conversion or downloaded, it's very possible that there's a mix
of coding systems among them.  In that case, setting this
variable non-nil may cause org-node to fail to scan some of them."
  :group 'org-node
  :type '(choice coding-system (const nil)))

(defcustom org-node-perf-keep-file-name-handlers '(epa-file-handler)
  "Which file handlers to respect while scanning for ID nodes.

Normally, `file-name-handler-alist' reacts specially to seeing
some file names: TRAMP paths, compressed files or .org.gpg files.

It's infamous for (somewhat) slowing down the access of very many
files, since it is a series of regexps applied to every file name
encountered.  With fewer members in this list, `org-node-reset'
can work faster."
  :group 'org-node
  :type '(choice (const :tag "Keep all" t)
          (set
           (function-item jka-compr-handler)
           (function-item epa-file-handler)
           (function-item tramp-file-name-handler)
           (function-item tramp-completion-file-name-handler)
           (function-item tramp-archive-file-name-handler)
           (function-item file-name-non-special))))

(defcustom org-node-perf-gc-cons-threshold nil
  "Temporary override for `gc-cons-threshold'.
Tweak to maybe speed up `org-node-reset'.  Set nil to use the
actual value of `gc-cons-threshold'.

It can be surprising which value works best.  It is possible that
80 kB is performant, and 16 MB is performant, but something in
between such as 1 MB is twice as slow.  Experiment to find a good
setting."
  :group 'org-node
  :type '(choice integer (const nil)))

(defcustom org-node-slug-fn #'org-node-slugify-as-url
  "Function to transform title into a filename.

Built-in choices:
- `org-node-slugify-as-url'
- `org-node-slugify-like-roam'"
  :group 'org-node
  :type '(choice
          (function-item org-node-slugify-like-roam)
          (function-item org-node-slugify-as-url)
          function))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called by `org-node-find' and `org-node-insert-link' to
create a node.

Some built-in options
- `org-node-new-file'
- `org-node-new-by-roam-capture'
- `org-capture'

The option `org-capture' requires some assembly, see
`org-node-capture-target'.

If you wish to write a custom function, know that during
execution, two variables are available: `org-node-proposed-title'
and `org-node-proposed-id'.  Use them."
  :group 'org-node
  :type '(choice
          (function-item org-node-new-file)
          (function-item org-node-new-by-roam-capture)
          (function-item org-capture)
          function))

(defcustom org-node-insert-link-hook ()
  "Hook run after inserting a link to an Org-ID node.

Two arguments provided: the ID and the link description.

Functions here should not leave point outside the link."
  :group 'org-node
  :type 'hook)

(defcustom org-node-creation-hook '(org-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied only by `org-node-new-file', `org-node-create-subtree',
`org-node-nodeify-entry' and `org-node-extract-subtree'.

NOT applied by `org-node-new-by-roam-capture' -- see org-roam's
`org-roam-capture-new-node-hook' instead.

A good member for this hook is `org-node-put-created', especially
since the default `org-node-slug-fn' does not put a timestamp in
the filename."
  :group 'org-node
  :type 'hook)

(defcustom org-node-format-candidate-fn
  (lambda (_node title)
    title)
  "Function to return a string to represent a given node.
Affects how selections are displayed during e.g. `org-node-find'.

Called with two arguments: the node data and the title.

The title may in fact be one of the aliases and not the canonical
title, because the function runs again for every alias.

The node data is an object which form you can observe in examples
from \\[org-node-cache-peek] and specified in the type
`org-node-data' (use C-h o to search for it).

The following example will make the completions display the
ancestors (outline path) to each node:

(setq org-node-format-candidate-fn
      (lambda (node title)
        (if-let ((olp (org-node-get-olp node)))
            (concat (string-join olp \" > \") \" > \" title)
          title)))"
  :group 'org-node
  :type 'function)

(defcustom org-node-filter-fn
  (lambda (node)
    (not (assoc "ROAM_EXCLUDE" (org-node-get-properties node))))
  "Predicate returning t to include a node, or nil to exclude it.

This function is applied once for every org-id node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-cache-peek] and
specified in the type `org-node-data' (search with C-h o).

This function is called after fully building the `org-nodes'
table, so you can even query it for other nodes.  The filtering
only has an impact on `org-node-collection', which forms the
basis for completions in the minibuffer.

See the following example for a way to filter out nodes with a
ROAM_EXCLUDE property, or that have any kind of TODO state, or
are tagged :drill:, or where the full file path contains the word
\"archive\".

(setq org-node-filter-fn
      (lambda (node)
        (and (not (assoc \"ROAM_EXCLUDE\" (org-node-get-properties node)))
             (not (org-node-get-todo node))
             (not (member \"drill\" (org-node-get-tags node)))
             (not (string-search \"archive\" (org-node-get-file-path node))))))"
  :group 'org-node
  :type 'function)

;; TODO: maybe permit .org.gpg and .org.gz
(defcustom org-node-extra-id-dirs (list)
  "Like `org-id-extra-files', but expressed as directories.

Intended to have the same convenience as setting
`org-agenda-files', informing org-id about every Org file that
exists within these directories (and subdirectories and
sub-subdirectories and so on).

For it to have an effect, `org-node-cache-mode' must be active.

To avoid accidentally picking up versioned backups, causing
org-id to complain about \"duplicate\" IDs, configure
`org-node-extra-id-dirs-exclude'."
  :group 'org-node
  :type '(repeat directory))

(defcustom org-node-extra-id-dirs-exclude
  '("/logseq/bak/"
    "/logseq/version-files/"
    ".sync-conflict-")
  "Substrings of file paths that say a file should be ignored.

This option has to do only with how to find files within
`org-node-extra-id-dirs', so that you have a way to prevent
org-id from looking inside versioned backup files and then
complain about \"duplicate\" IDs.

For all other purposes, you probably want to configure
`org-node-filter-fn' instead.

It is not necessary to exclude backups or autosaves that end in ~
or # since `org-node-files' already only considers files that end
in exactly \".org\"."
  :group 'org-node
  :type '(repeat string))



(defmacro org-node--with-file (file &rest body)
  "Backport of the `org-with-file-buffer' concept.
Also integrates `org-with-wide-buffer' behavior.

If a buffer was visiting FILE, go to that buffer, else visit it
in a new buffer.  With that as the current buffer, execute BODY.
Finally:

- If a new buffer had to be opened, save and kill it.
- If a buffer had been open, but it was unmodified before running
  BODY, keep it open and save any changes.
- If a buffer had been open, and modified, keep it open and leave
  it unsaved."
  (declare (indent 1))
  ;; REVIEW: do these perf hacks or don't?
  `(let ((find-file-hook nil)
         (after-save-hook nil)
         (before-save-hook nil)
         (org-agenda-files nil)
         (org-inhibit-startup t))
     (let* ((was-open (find-buffer-visiting ,file))
            (was-modified (and was-open (buffer-modified-p was-open))))
       (with-current-buffer (or was-open
                                (delay-mode-hooks
                                  (find-file-noselect ,file)))
         (save-excursion
           (without-restriction
             ,@body))
         (unless (and was-open was-modified)
           (let ((save-silently t)
                 (inhibit-message t))
             (save-buffer)))
         (unless was-open
           (kill-buffer))))))

;; TODO: Diff the old value with the new value and schedule a targeted caching
;;       of any new files that appeared.
(let (mem)
  (defun org-node-files (&optional instant)
    "List files in `org-id-locations' or `org-node-extra-id-dirs'.

With argument INSTANT t, reuse a result from the last time
something called this function.  Else you may get a momentary
delay when thousands of files are involved, which may not be
desirable in an user-facing command."
    (if (and instant mem)
        mem
      (setq mem
            (-union
             (hash-table-values org-id-locations)
             (cl-loop
              for dir in org-node-extra-id-dirs
              append (cl-loop
                      for file in (directory-files-recursively dir "\\.org$")
                      unless (cl-loop
                              for exclude in org-node-extra-id-dirs-exclude
                              when (string-search exclude file)
                              return t)
                      ;; Abbreviating because org-id does too
                      collect (abbreviate-file-name file))))))))

(defvar org-nodes (make-hash-table :test #'equal :size 5000)
  "Table associating ids with file/subtree data.
To peek on the contents, try \\[org-node-cache-peek] a few times, which
can demonstrate the data format.  See also the type `org-node-data'.")

(defvar org-node-collection (make-hash-table :test #'equal :size 5000)
  "Filtered `org-nodes', keyed not on ids but formatted titles.
This allows use with `completing-read'.")

(defun org-node--forget-id-location (file)
  "Remove references to FILE in `org-id-locations'.
You might consider \"committing\" the effect afterwards by
calling `org-id-locations-save'."
  (let ((alist (org-id-hash-to-alist org-id-locations)))
    (assoc-delete-all file alist)
    (setq org-id-locations (org-id-alist-to-hash alist))))

;; REVIEW deprecate?
(defun org-node-die (format-string &rest args)
  "Like `error' but make sure the user sees it.
Because not everyone has `debug-on-error' t."
  (let ((err-string (apply #'format format-string args)))
    (display-warning 'org-node err-string :error)
    (error "%s" err-string)))

;; REVIEW deprecate?
(defconst org-node--standard-tip
  ", try `org-id-update-id-locations' or `org-roam-update-org-id-locations'")

(defun org-node--root-dirs (file-list)
  "Given FILE-LIST, infer the most base root directories.

In many cases, if FILE-LIST is the `hash-table-values' of
`org-id-locations', this function will spit out a list of one item
because many people keep their Org files in one root directory.

If it finds more than one root, it sorts by count of files they
contain, so that the most populous root directory will be the
first element."
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
(defun org-node--consent-to-problematic-modes-for-mass-op ()
  (--all-p (if (and (boundp it) (symbol-value it))
               (y-or-n-p
                (format "%S is active - proceed anyway?" it))
             t)
           '(auto-save-mode
             auto-save-visited-mode
             git-auto-commit-mode)))

;; FIXME The hash table effectively imposes an uniqueness
;; constraint... probably need to deprecate this variable, make a temporary
;; alist during `org-node--add-link-to-tables' and then just iterate thru
;; `org-node--refs-table' to enrich it or something.
(defvar org-node--reflinks-table (make-hash-table :test #'equal)
  "Table of potential reflinks.
The table keys are just URIs such as web addresses, and the
values a plist describing the ID-node where the link was found.

The fact that a link is in this table does not mean there is a
node with a ROAM_REFS matching that same link.  To find that out,
you'd have to cross-reference with `org-node--refs-table'.")

(defvar org-node--links-table (make-hash-table :test #'equal))
(defvar org-node--refs-table (make-hash-table :test #'equal))
(defvar org-node--dbg nil)


;;; Data structure

;; A struct was pointless while I developed the package for my own use, plists
;; did fine, but now that it has users... the problem with `plist-get' is I can
;; never rename any of the data fields.
;;
;; If you use `plist-get' to fetch a key that doesn't exist, it just quietly
;; returns nil, no error, no warning.  (Bad for an API!)  Let's say I want to
;; deprecate or rename a plist field such as :roam-exclude, then I must hope
;; everyone reads the news.  By contrast if the getter is a function
;; `org-node-get-roam-exclude', I can override it so it emits a warning.

(cl-defstruct org-node-data
  "To get e.g. a node's title, use `(org-node-get-title NODE)'."
  (aliases    nil :read-only t :type list    :documentation
              "List of ROAM_ALIASES.")
  (deadline   nil :read-only t :type string  :documentation
              "The DEADLINE state.")
  (file-path  nil :read-only t :type string  :documentation
              "Full file path.")
  (file-title nil :read-only t :type string  :documentation
              "The title of the file where this node is.")
  (file-title-or-basename nil :read-only t :type string  :documentation
                          "The title of the file where this node is, or its filename if untitled.")
  (id         nil :read-only t :type string  :documentation
              "The ID property.")
  (is-subtree nil :read-only t :type boolean :documentation
              "Valued t if it is a subtree, nil if it is a file-level node.")
  (level      nil :read-only t :type number  :documentation
              "Number of stars in the heading. File-level node always 0.")
  (olp        nil :read-only t :type list    :documentation
              "List of ancestor headings to this node.")
  (pos        nil :read-only t :type number  :documentation
              "Char position of the node. File-level node always 1.")
  (properties nil :read-only t :type list    :documentation
              "Alist of properties from the :PROPERTIES: drawer.")
  (refs       nil :read-only t :type list    :documentation
              "List of ROAM_REFS.")
  (scheduled  nil :read-only t :type string  :documentation
              "The SCHEDULED state.")
  (tags       nil :read-only t :type list    :documentation
              "List of tags.")
  (title      nil :read-only t :type string  :documentation
              "The node's heading, or #+title if it is not a heading.")
  (todo       nil :read-only t :type string  :documentation
              "The TODO state."))

;; Make getters called "org-node-get-..." instead of "org-node-data-...".
;;
;; It's one letter shorter, and a verb.  Function names read better as verbs,
;; and while it may not be necessary for a struct accessor, then consider an
;; accessor like "org-node-data-title": what does this mean?  The title of the
;; data, or the title of the node?  Much less confusion with
;; "org-node-get-title"!
;;
;; Of course it could've been just "org-node-title", which is free of the above
;; confusion, but has its own issues.  First (1) the package itself is also
;; called "org-node", so this pollutes the namespace.  When you search for
;; functions, you're unsure what's a struct accessor and what may be commands
;; that have nothing to do with the struct.
;;
;; Additionally (2), it's good to distinguish the concept of an ID node
;; (meaning an Org file, or a heading in an Org file) from the concept of a
;; metadata object about that ID node.  The sentence "snow is white" is true if
;; and only if snow is white, and all that.
(defalias 'org-node-get-aliases    #'org-node-data-aliases)
(defalias 'org-node-get-deadline   #'org-node-data-deadline)
(defalias 'org-node-get-file-path  #'org-node-data-file-path)
(defalias 'org-node-get-file-title #'org-node-data-file-title)
(defalias 'org-node-get-file-title-or-basename #'org-node-data-file-title-or-basename)
(defalias 'org-node-get-id         #'org-node-data-id)
(defalias 'org-node-get-is-subtree #'org-node-data-is-subtree)
(defalias 'org-node-get-level      #'org-node-data-level)
(defalias 'org-node-get-olp        #'org-node-data-olp)
(defalias 'org-node-get-pos        #'org-node-data-pos)
(defalias 'org-node-get-properties #'org-node-data-properties)
(defalias 'org-node-get-refs       #'org-node-data-refs)
(defalias 'org-node-get-scheduled  #'org-node-data-scheduled)
(defalias 'org-node-get-tags       #'org-node-data-tags)
(defalias 'org-node-get-title      #'org-node-data-title)
(defalias 'org-node-get-todo       #'org-node-data-todo)



;;; Obsoletions

;; 2024-05-05
(let (warned-once)
  (defun org-node-aliases    (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-aliases    node))
  (defun org-node-deadline   (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-deadline   node))
  (defun org-node-file-path  (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-file-path  node))
  (defun org-node-file-title (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-file-title node))
  (defun org-node-id         (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-id         node))
  (defun org-node-is-subtree (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-is-subtree node))
  (defun org-node-level      (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-level      node))
  (defun org-node-olp        (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-olp        node))
  (defun org-node-pos        (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-pos        node))
  (defun org-node-properties (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-properties node))
  (defun org-node-refs       (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-refs       node))
  (defun org-node-scheduled  (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-scheduled  node))
  (defun org-node-tags       (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-tags       node))
  (defun org-node-title      (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-title      node))
  (defun org-node-todo       (node) (unless warned-once (display-warning 'org-node (string-fill "\nYour config uses deprecated accessors org-node-..., update to org-node-get-..." 79)) (setq warned-once t)) (org-node-get-todo       node)))

;;;###autoload
(let (warned-once)
  (defun org-node-create-subtree (&rest args)
    (interactive)
    (unless warned-once
      (setq warned-once t)
      (lwarn 'org-node :warning
             "Command renamed on 2024-05-01: org-node-create-subtree to org-node-insert-heading"))
    (apply #'org-node-insert-heading args)))

;; Not technically an obsoletion...  just fundamentally uninteresting
;;;###autoload
(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer
       .1 nil #'lwarn 'org-node :warning
       "Someone misspelled `org-node-backlink-mode', but I ran it for you"))
    (apply #'org-node-backlink-mode args)))

(provide 'org-node-common)

;;; org-node-common.el ends here
