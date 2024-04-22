;;; org-node-common.el -*- lexical-binding: t; -*-

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

(defcustom org-node-format-candidate-fn
  (lambda (_node title) title)
  "Function to return what string should represent this node.
Affects how selections are displayed during e.g. `org-node-find'.

Called with two arguments: the node data and the title.

The node data is a plist which form you can observe in examples
from \\[org-node-cache-peek].  The title may in fact be one of
the aliases and not the real title, because the function runs
again for every alias.

This example shows the file name in addition to the title:

(setq org-node-format-candidate-fn
      (lambda (node title)
        (concat (file-name-nondirectory (plist-get node :file-path))
                \" -- \"
                title)))
"
  :group 'org-node
  :type 'function)

(defcustom org-node-filter-fn
  (lambda (node)
    (and (not (plist-get node :roam-exclude))
         (not (plist-get node :todo))))
  "Predicate returning t to include a node, or nil to exclude it.

This function is applied once for every Org-ID node found, and
receives the node data as a single argument: a plist which form
you can observe in examples from \\[org-node-cache-peek].

This function is called after fully building the `org-nodes'
table, so you may query it as needed.

See the following example for a way to filter out nodes tagged
:drill: and all files with a substring \"archive\" in the name.

(setq org-node-filter-fn
      (lambda (node)
        (and (not (plist-get node :roam-exclude))
             (not (plist-get node :todo))
             (not (member \"drill\" (plist-get node :tags)))
             (not (string-search \"archive\" (plist-get node :file-path))))))

(setq org-node-filter-fn
      (lambda (node)
       (and (not (plist-get node :roam-exclude))
            (not (plist-get node :todo)))))

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
  "Table associating ids with cached file/subtree data.
To peek on the contents, try \\[org-node-cache-peek] a few times, which
should suffice to demonstrate the data format.")

(defvar org-node-collection (make-hash-table :test #'equal :size 4000)
  "Filtered `org-nodes', keyed not on ids but formatted titles.
This allows direct use with `completing-read'.")

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
  (and (or (null org-id-locations)
           (if (hash-table-p org-id-locations)
               (hash-table-empty-p org-id-locations)))
       ;; Load, and guarantee a hash-table from now on
       (org-id-locations-load)
       (hash-table-empty-p org-id-locations)
       (org-node-die "org-id-locations empty%s" org-node--standard-tip)))

(defun org-node--root-dirs (file-list)
  "Given FILE-LIST, infer the most base root directories.

In many cases, if passed the `hash-table-values' of
`org-id-locations', this function will spit out a list of one
item because many people keep their Org files in one root
directory.

If it finds more than one root, it sorts by count of files they
contain, so that the most populous root directory will be the
first element."
  (let* ((files (-uniq file-list))
         (directories (-uniq (mapcar #'file-name-directory files))))
    ;; Example: if there is /home/roam/courses/Math1A/, but ancestor dir
    ;; /home/roam/ is also a member of the set, throw out the child because
    ;; ripgrep works recursively anyway
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
       as the-root = (--find (string-prefix-p file it) directories)
       do (cl-incf (cdr (assoc the-root dir-counters)))
       finally return (mapcar #'car (cl-sort dir-counters #'> :key #'cdr))))))


;;; Visit-getters

(defun org-node--visit-get-pos (node)
  "Visit NODE and return the char position where it starts."
  (with-temp-buffer
    (insert-file-contents (plist-get node :file-path))
    (forward-line (1- (plist-get node :line-number)))
    (point)))

(defun org-node--visit-get-file-title (node)
  (org-with-file-buffer (plist-get node :file)
    (org-get-title)))

(defun org-node--visit-get-properties (node)
  (org-with-file-buffer (plist-get node :file)
    (save-excursion
      (without-restriction
        (goto-char 1)
        (forward-line (plist-get node :line-number))
        (org-entry-properties)))))

(provide 'org-node-common)

;;; org-node-common.el ends here
