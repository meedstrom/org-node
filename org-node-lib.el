;;; org-node-lib.el -*- lexical-binding: t; -*-

(require 'compat)
(require 'cl-lib)
(require 'subr-x)
(require 'dash)
(require 'org-id)

;; Prevent "invalid function org-with-file-buffer". Someone explains at
;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=46958
(require 'org-macs)

(defgroup org-node nil
  "Support a zettelkasten of org-id files and subtrees."
  :group 'org)

(defcustom org-node-ask-directory nil
  "Whether to ask the user where to save a new file node.

Set nil to assume that the most populous root directory in
`org-id-locations' is always the correct choice."
  :group 'org-node
  :type 'boolean)

(defcustom org-node-perf-assume-coding-system nil
  "Coding system to use while scanning for metadata.
This speeds up `org-node-reset' a bit.  Set nil to let Emacs
figure it out anew on every file.

On modern Linux systems, the correct assumption is almost always
the symbol `utf-8-unix'.  If you access your files from several
different systems, consider keeping this at nil.

Some example choices:
utf-8-unix utf-8-dos utf-8-mac utf-16-le-dos utf-16-be-dos"
  :group 'org-node
  :type '(choice symbol (const nil)))

(defcustom org-node-async-inject-variables (list)
  "Alist of variable-value pairs that child processes should set.

May be useful for injecting your authinfo and EasyPG settings so
that even with `org-node-perf-multicore', the child processes can
scan for metadata inside .org.gpg files.

I don't use EPG so I don't know if that's enough to make it work."
  :group 'org-node
  :type 'alist)

(defcustom org-node-perf-keep-file-name-handlers '(epa-file-handler)
  "Which file handlers to not ignore while scanning for metadata.

Normally, `file-name-handler-alist' reacts specially to seeing
some file names: TRAMP paths, compressed files or
.org.gpg files.

It's infamous for (somewhat) slowing down the access of very many
files, since it is a series of regexps applied to every file name
encountered.  Temporarily eliminating members will speed up
`org-node-reset' a bit."
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
  "Temporary setting for `gc-cons-threshold'.
Tweak to maybe speed up `org-node-reset'.  Set nil to use the
actual value of `gc-cons-threshold'.

It can be surprising which value works best.  It is possible that
80 kB is performant, and 16 MB is performant, but something in
between such as 1 MB is very slow."
  :group 'org-node
  :type '(choice number (const nil)))

(defcustom org-node-perf-multicore t
  "Whether to use the async, multi-threaded cacher."
  :group 'org-node
  :type 'boolean)

(defcustom org-node-slug-fn #'org-node-slugify-as-url
  "Function to transform title into a filename.

Built-in choices:
- `org-node-slugify-as-url'
- `org-node-slugify-like-roam'
"
  :group 'org-node
  :type '(choice
          (function-item org-node-slugify-like-roam)
          (function-item org-node-slugify-as-url)
          function))

(defcustom org-node-creation-fn #'org-node-new-file
  "Function called by `org-node-find', and `org-node-insert-link' to
create a node.

If you wish to write a new function, know that during execution, two
variables are available: `org-node-proposed-title' and
`org-node-proposed-id'.  Use them.

Some options
- `org-node-new-file'
- `org-node-new-by-roam-capture'
- `org-capture'

Using `org-capture' requires some assembly.  See `org-node-capture-target'."
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

NOT run by `org-node-new-by-roam-capture' - see that package's hook
`org-roam-capture-new-node-hook' instead.

A good member for this hook is `org-node-put-created', especially
since the default `org-node-slug-fn' does not put a timestamp in
the filename."
  :type 'hook
  :group 'org-node)

(defcustom org-node-only-show-subtrees-with-id t
  "Set nil to include all subtrees as completion candidates."
  :group 'org-node
  :type 'boolean)

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
          title)))
"
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
             (not (string-search \"archive\" (org-node-get-file-path node))))))
"
  :type 'function
  :group 'org-node)



(defvar org-nodes (make-hash-table :test #'equal :size 4000)
  "Table associating ids with file/subtree data.
To peek on the contents, try \\[org-node-cache-peek] a few times, which
can demonstrate the data format.  See also the type `org-node-data'.")

(defvar org-node-collection (make-hash-table :test #'equal :size 4000)
  "Filtered `org-nodes', keyed not on ids but formatted titles.
This allows use with `completing-read'.")

(defun org-node--forget-id-location (file)
  (org-node--init-org-id-locations-or-die)
  (cl-loop for id being the hash-keys of org-id-locations
           using (hash-values file-on-record)
           when (file-equal-p file file-on-record)
           do (remhash id org-id-locations)))

(defun org-node-die (format-string &rest args)
  "Like `error' but make sure the user sees it.
Because not everyone has `debug-on-error' t."
  (let ((err-string (apply #'format format-string args)))
    (display-warning 'org-node err-string :error)
    (error "%s" err-string)))

(defconst org-node--standard-tip
  ", try `org-id-update-id-locations' or `org-roam-update-org-id-locations'")

(defun org-node--init-org-id-locations-or-die ()
  (require 'org-id)
  ;; Sometimes `org-id-locations' decides to be an alist instead of a hash
  ;; table...  and interestingly, when it's an alist, the filename is car, but
  ;; when it's hash table, the filename is not the key but the value...
  (if (or (null org-id-locations)
          (if (hash-table-p org-id-locations)
              (hash-table-empty-p org-id-locations)))
      ;; Load, and guarantee a hash-table from now on
      (org-id-locations-load)
    (when (listp org-id-locations)
      ;; This /should/ make it a hash table...
      (org-id-update-id-locations)))
  (when (hash-table-empty-p org-id-locations)
    (org-node-die "org-id-locations empty%s" org-node--standard-tip)))

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

(defun org-node--consent-to-problematic-modes-for-mass-op ()
  (--all-p (if (and (boundp it) (symbol-value it))
               (y-or-n-p
                (format "%S is active - proceed anyway?" it))
             t)
           '(auto-save-mode
             auto-save-visited-mode
             git-auto-commit-mode)))

(defvar org-node--reflinks-table (make-hash-table :test #'equal))
(defvar org-node--links-table (make-hash-table :test #'equal))
(defvar org-node--refs-table (make-hash-table :test #'equal))

;; I feel like this could be easier to read...
(defun org-node--add-node-to-tables (node-as-plist)
  "Add a node to `org-nodes' and maybe `org-node-collection'."
  (let ((node (apply #'make-org-node-data node-as-plist)))
    ;; Record the node even if it has no ID
    (puthash (or (org-node-get-id node) (format-time-string "%N"))
             node
             org-nodes)
    (when (or (not org-node-only-show-subtrees-with-id) (org-node-get-id node))
      ;; Populate `org-node--refs-table'
      (dolist (ref (org-node-get-refs node))
        (puthash ref (org-node-get-id node) org-node--refs-table))
      (when (funcall org-node-filter-fn node)
        ;; Populate `org-node-collection'
        (dolist (title (cons (org-node-get-title node)
                             (org-node-get-aliases node)))
          (puthash (funcall org-node-format-candidate-fn node title)
                   node
                   org-node-collection))
        ;; Let refs work as aliases
        (dolist (ref (org-node-get-refs node))
          (puthash ref node org-node-collection))))))

(defun org-node--add-link-to-tables (link-plist path type)
  (push link-plist
        (gethash path (if (equal type "id")
                          org-node--links-table
                        org-node--reflinks-table))))


;; A struct was pointless while I developed the package for my own use, but
;; now that it has users... the problem with `plist-get' is I can never rename
;; any of the data fields.
;;
;; If you use `plist-get' to fetch a key that doesn't exist, it just quietly
;; returns nil, no error, no warning.  (Bad for an API!)  Let's say I want to
;; deprecate or rename a plist field such :roam-exclude, then I must hope
;; everyone reads the news.  By contrast if the getter is a function
;; `org-node-get-roam-exclude', I can override it so it emits a warning.
(eval `(cl-defstruct org-node-data
         "To get a node's title, use e.g. `(org-node-get-title NODE)'."
         (aliases    nil :read-only t :type list    :documentation ,(string-fill "List of ROAM_ALIASES." 70))
         (deadline   nil :read-only t :type string  :documentation ,(string-fill "The DEADLINE state." 70))
         (file-path  nil :read-only t :type string  :documentation ,(string-fill "Full file path." 70))
         (file-title nil :read-only t :type string  :documentation ,(string-fill "The title of the file where this node is.  If this node is itself a file-level node, then it is the same as the title." 70))
         (id         nil :read-only t :type string  :documentation ,(string-fill "The ID property." 70))
         (is-subtree nil :read-only t :type boolean :documentation ,(string-fill "Valued t if it is a subtree, nil if it is a file-level node." 70))
         (level      nil :read-only t :type number  :documentation ,(string-fill "Outline level, i.e. the number of stars in the heading.  A file-level node has level 0." 70))
         (olp        nil :read-only t :type list    :documentation ,(string-fill "List of ancestor headings to this node.  Naturally, this is empty if the node is a file-level node or a top-level heading." 70))
         (pos        nil :read-only t :type number  :documentation ,(string-fill "Char position of the node.  File-level nodes always have position 1." 70))
         (properties nil :read-only t :type list    :documentation ,(string-fill "Alist of properties from the :PROPERTIES: drawer." 70))
         (refs       nil :read-only t :type list    :documentation ,(string-fill "List of ROAM_REFS." 70))
         (scheduled  nil :read-only t :type string  :documentation ,(string-fill "The SCHEDULED state." 70))
         (tags       nil :read-only t :type list    :documentation ,(string-fill "List of tags." 70))
         (title      nil :read-only t :type string  :documentation ,(string-fill "The node's heading, or #+title if it is not a heading." 70))
         (todo       nil :read-only t :type string  :documentation ,(string-fill "The TODO state." 70))))

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

;; 2024-05-01 Deprecate the old "org-node" struct, badly named
(define-obsolete-function-alias 'org-node-aliases    #'org-node-get-aliases    "2024-05-01")
(define-obsolete-function-alias 'org-node-deadline   #'org-node-get-deadline   "2024-05-01")
(define-obsolete-function-alias 'org-node-file-path  #'org-node-get-file-path  "2024-05-01")
(define-obsolete-function-alias 'org-node-file-title #'org-node-get-file-title "2024-05-01")
(define-obsolete-function-alias 'org-node-id         #'org-node-get-id         "2024-05-01")
(define-obsolete-function-alias 'org-node-is-subtree #'org-node-get-is-subtree "2024-05-01")
(define-obsolete-function-alias 'org-node-level      #'org-node-get-level      "2024-05-01")
(define-obsolete-function-alias 'org-node-olp        #'org-node-get-olp        "2024-05-01")
(define-obsolete-function-alias 'org-node-pos        #'org-node-get-pos        "2024-05-01")
(define-obsolete-function-alias 'org-node-properties #'org-node-get-properties "2024-05-01")
(define-obsolete-function-alias 'org-node-refs       #'org-node-get-refs       "2024-05-01")
(define-obsolete-function-alias 'org-node-scheduled  #'org-node-get-scheduled  "2024-05-01")
(define-obsolete-function-alias 'org-node-tags       #'org-node-get-tags       "2024-05-01")
(define-obsolete-function-alias 'org-node-title      #'org-node-get-title      "2024-05-01")
(define-obsolete-function-alias 'org-node-todo       #'org-node-get-todo       "2024-05-01")


;;; Other obsoletions

(let (warned-once)
  (defun org-node-cache-scan-file (&rest args)
    (unless warned-once
      (setq warned-once t)
      (lwarn 'org-node :warning
             "Function renamed on 2024-05-01: org-node-cache-scan-file to org-node-cache-rescan-file"))
    (apply #'org-node-cache-rescan-file args)))

;;;###autoload
(let (warned-once)
  (defun org-node-create-subtree (&rest args)
    (interactive)
    (unless warned-once
      (setq warned-once t)
      (lwarn 'org-node :warning
             "Command renamed on 2024-05-01: org-node-create-subtree to org-node-insert-heading"))
    (apply #'org-node-insert-heading args)))

;;;###autoload
(let (warned-once)
  (defun org-node-insert-heading-node (&rest args)
    (interactive)
    (unless warned-once
      (setq warned-once t)
      (lwarn 'org-node :warning
             "Command renamed on 2024-05-02: org-node-insert-heading-node to org-node-insert-heading"))
    (apply #'org-node-insert-heading args)))


(provide 'org-node-lib)

;;; org-node-lib.el ends here
