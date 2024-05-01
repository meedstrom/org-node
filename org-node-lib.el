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
  "Whether to ask the user where to save a new node.

Set nil to assume that the most populous root directory in
`org-id-locations' is always the correct choice."
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
          (const org-node-slugify-like-roam)
          (const org-node-slugify-as-url)
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
          (const org-node-new-file)
          (const org-node-new-by-roam-capture)
          (const org-capture)
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
  "Function to return what string should represent this node.
Affects how selections are displayed during e.g. `org-node-find'.

Called with two arguments: the node data and the title.

The title may in fact be one of the aliases and not the canonical title,
because the function runs again for every alias.

The node data is an object which form you can observe in
examples from \\[org-node-cache-peek].

This example shows the ancestor entries to each node:

(setq org-node-format-candidate-fn
      (defun my-format-with-olp (node title)
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

This function is applied once for every Org-ID node found, and
receives the node data as a single argument: an object which form
you can observe in examples from \\[org-node-cache-peek].

This function is called after fully building the `org-nodes'
table, so you may query it as needed.

See the following example for a way to filter out nodes tagged
:drill: and all files with a substring \"archive\" in the name.

(setq org-node-filter-fn
      (lambda (node)
        (and (not (assoc \"ROAM_EXCLUDE\" (org-node-get-properties node)))
             (not (org-node-get-todo node))
             (not (member \"drill\" (org-node-get-tags node)))
             (not (string-search \"archive\" (org-node-get-file-path node))))))

If you have an expensive filter slowing things down, a tip is
make a defun, not a lambda, and byte-compile that init file:

(setq org-node-filter-fn #'my-filter)
(defun my-filter (node)
  (some expensive calculations)
  (phew!))
"
  :type 'function
  :group 'org-node)



(defvar org-nodes (make-hash-table :test #'equal :size 4000)
  "Table associating ids with file/subtree data.
To peek on the contents, try \\[org-node-cache-peek] a few times, which
should suffice to demonstrate the data format.")

(defvar org-node-collection (make-hash-table :test #'equal :size 4000)
  "Filtered `org-nodes', keyed not on ids but formatted titles.
This allows use with `completing-read'.")

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

;; A struct was pointless while I developed the package for my own use, but
;; now that it has users... the problem with `plist-get' is I can never rename
;; any of the data fields.
;;
;; If you use `plist-get' to fetch a key that doesn't exist, it just quietly
;; returns nil, no error, no warning.  (Bad for an API!)  Let's say I want to
;; deprecate or rename a field such :roam-exclude, then I must hope everyone
;; reads the news.  By contrast if the getter is a function
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
         (pos        nil :read-only t :type number  :documentation ,(string-fill "" 70))
         (properties nil :read-only t :type list    :documentation ,(string-fill "" 70))
         (refs       nil :read-only t :type list    :documentation ,(string-fill "" 70))
         (scheduled  nil :read-only t :type string  :documentation ,(string-fill "" 70))
         (tags       nil :read-only t :type list    :documentation ,(string-fill "" 70))
         (title      nil :read-only t :type string  :documentation ,(string-fill "" 70))
         (todo       nil :read-only t :type string  :documentation ,(string-fill "" 70))))

;; Make getters called "org-node-get-..." instead of "org-node-data-..."
;;
;; It's one letter shorter, and a verb.  Function names generally read better
;; as verbs, and while that may not necessary for lisp struct accessors, then
;; consider a getter like "org-node-data-title": what does this mean?  The
;; title of the data, or the title of the node?  Much less confusion with
;; "org-node-get-title"!
;;
;; Of course it could've been just "org-node-title", which is free of the above
;; confusion, but has its own issues.  First (1) the package itself is also
;; called "org-node", so this pollutes the namespace.  When you search for
;; functions, you're unsure what's a getter for a node object and what may be
;; commands that have nothing to do with node objects.  Additionally (2) it's
;; good to distinguish the concept of an ID node (which is an Org file, or a
;; heading in an Org file) from the concept of a data object that just holds
;; metadata about that ID node.
;;
;; The sentence "snow is white" is true if and only if snow is white, and all
;; that.
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

(provide 'org-node-lib)

;;; org-node-lib.el ends here
