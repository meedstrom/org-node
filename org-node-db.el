;;; org-node-db.el --- Bridge to org-roam -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Maintain a diskless SQLite database,
;; that other packages can rely on.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'sqlite)
(require 'sqlite-mode)
(require 'llama)
(require 'org-node)

;; TODO: See `org-node-db--update' about perf hotspot in sqlite.c.
;;       Maybe can workaround by changing table indices or something.

;; TODO: 50% of CPU spent in `prin1-to-string'. how improve?
;;       - Less to print.  Do that pullreq mentioned elsewhere.
;;       - Propose org-roam change data type of mtime/atime to integer.

;;;###autoload
(define-minor-mode org-node-db-mode
  "Maintain a SQLite database in memory.
Contents are copied from `org-node-cache-mode' tables.

This allows you to safely stop using `org-roam-db-autosync-mode',
if `org-node-db-share' is t.

Alternatively, instead of `org-node-db-share',
you can load the \"org-node-shims\" library at init,
before anything would load \"org-roam\".

Tip: With this mode disabled, you can still use
\\[org-node-db-sync-to-roam] rebuild org-roam.db, as a faster
alternative to \\[org-roam-db-sync].

------"
  :global t
  :group 'org-node
  (if org-node-db-mode
      (progn
        (org-node-db--check-emacs)
        ;; TODO: Integrate with the regular time-elapsed message
        (add-hook 'org-node--mid-scan-hook #'org-node-db--reset)
        (add-hook 'org-node-rescan-functions #'org-node-db--update)
        ;; NOTE: Superfluous when shims loaded and roam not loaded,
        ;;       just necessary after roam loads.
        (advice-add 'org-roam-db-sync :override 'org-node-db-sync-to-roam)
        (advice-add 'org-roam-db :override 'org-node-db))
    (remove-hook 'org-node-rescan-functions 'org-node-db--update)
    (advice-remove 'org-roam-db 'org-node-db)
    (advice-remove 'org-roam-db-sync 'org-node-db-sync-to-roam)))

(defun org-node-db--check-emacs ()
  "Do sanity checks."
  (unless (sqlite-available-p)
    (error "Emacs built using --without-sqlite3, so org-node-db won't work"))
  (when org-node-db-mode
    (when (and (bound-and-true-p org-roam-db-update-on-save)
               (bound-and-true-p org-roam-db-autosync-mode))
      (message "You may want to set `%S' to nil when using `%S'"
               'org-roam-db-update-on-save 'org-node-db-mode))
    (when (not org-node-cache-mode)
      (message "You may want to enable `%S' when using `%S'"
               'org-node-cache-mode 'org-node-db-mode))))

(defun org-node-db--init-pragmas-and-schemata (db)
  "Set up pragmas and table schemata in DB."
  (sqlite-execute db "PRAGMA user_version = 18;") ; See `org-roam-db-version'
  (sqlite-execute db "PRAGMA foreign_keys = on;")
  ;; Note to devs: try M-x `org-node-db--insert-roam-schemata-atpt'
  (mapc
   (##sqlite-execute db %)
   '("CREATE TABLE files (
	file UNIQUE PRIMARY KEY,
	title,
	hash NOT NULL,
	atime NOT NULL,
	mtime NOT NULL
);"
     "CREATE TABLE nodes (
	id NOT NULL PRIMARY KEY,
	file NOT NULL,
	level NOT NULL,
	pos NOT NULL,
	todo,
	priority,
	scheduled text,
	deadline text,
	title,
	properties,
	olp,
	FOREIGN KEY (file) REFERENCES files (file) ON DELETE CASCADE
);"
     "CREATE TABLE aliases (
	node_id NOT NULL,
	alias,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE citations (
	node_id NOT NULL,
	cite_key NOT NULL,
	pos NOT NULL,
	properties,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE refs (
	node_id NOT NULL,
	ref NOT NULL,
	type NOT NULL,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE tags (
	node_id NOT NULL,
	tag,
	FOREIGN KEY (node_id) REFERENCES nodes (id) ON DELETE CASCADE
);"
     "CREATE TABLE links (
	pos NOT NULL,
	source NOT NULL,
	dest NOT NULL,
	type NOT NULL,
	properties NOT NULL,
	FOREIGN KEY (source) REFERENCES nodes (id) ON DELETE CASCADE
);"))

  ;; That's it for full compatibility.
  ;; Now play with perf settings.
  ;; Reference:  https://www.sqlite.org/pragma.html
  ;; In-memory DB defaults:  https://www.sqlite.org/inmemorydb.html

  (sqlite-execute db "CREATE INDEX refs_node_id  ON refs    (node_id);")
  (sqlite-execute db "CREATE INDEX tags_node_id  ON tags    (node_id);")
  (sqlite-execute db "CREATE INDEX alias_node_id ON aliases (node_id);")
  (sqlite-execute db "PRAGMA cache_size = -20000;") ;; 20,480,000 bytes

  ;; I have no idea what I'm doing
  (sqlite-execute db "PRAGMA mmap_size = 20480000;")
  (sqlite-execute db "PRAGMA temp_store = memory;")
  (sqlite-execute db "PRAGMA synchronous = off;"))

(defvar org-node-db--connection nil
  "A SQLite handle.")

(defcustom org-node-db-share t
  "Force org-roam to use our diskless DB.
A side effect is reducing the amount of info in it, restricting it
to files in `org-roam-directory'."
  :type 'boolean
  :group 'org-node)

;; Considering naming it `org-node-db-get-create' so it's less magical
(defun org-node-db ()
  "More or less a counterpart to the `org-roam-db' function.
Overrides `org-roam-db' while `org-node-db-mode' is enabled."
  (if (ignore-errors (sqlite-pragma org-node-db--connection "im_still_here"))
      org-node-db--connection
    (setq org-node-db--connection (org-node-db--write-new-db))))

;;;###autoload
(defun org-node-db-sync-to-roam (&optional _)
  "Like `org-roam-db-sync' with FORCE t."
  (interactive)
  (cl-assert org-node-db-mode)
  (cl-assert (featurep 'org-roam))
  (when (and (boundp 'org-roam-db-location)
             (fboundp 'org-roam-db--close))
    ;; INFO: People who use separate org-roam-directories apparently must set
    ;; both `org-roam-directory' and `org-roam-db-location', and the calling
    ;; conventions don't make this obvious but `org-roam-db--close' will check
    ;; that and then `org-roam-db' would have checked both to init a new db
    ;; (here we use `org-node-db--write-new-db' instead).
    (org-roam-db--close)
    (delete-file org-roam-db-location)
    (let ((db (org-node-db--write-new-db org-roam-db-location)))
      (when (and org-node-db-share
                 (boundp 'org-roam-directory)
                 (boundp 'org-roam-db--connection)
                 (fboundp 'org-roam-db--close)
                 (fboundp 'org-roam-db--get-connection))
        (org-roam-db--close (org-roam-db--get-connection))
        (puthash (expand-file-name (file-name-as-directory org-roam-directory))
                 db
                 org-roam-db--connection)))))

(defun org-node-db--write-new-db (&optional loc)
  "Generate a new database and return a connection-handle to it.
Shape it according to org-roam schemata and pre-populate it with data.

Normally, this creates a diskless database.  With optional file path
LOC, write the database as a file to LOC."
  (org-node-cache-ensure)
  (when loc
    (cl-assert (file-name-absolute-p loc))
    (when (file-exists-p loc) (delete-file loc)))
  (let ((T (current-time))
        (name (or loc "diskless DB"))
        (db (sqlite-open loc)))
    (message "org-node-db: Re-creating %s..." name)
    (org-node-db--init-pragmas-and-schemata db)
    (org-node-db--populate db (org-node-db--mk-rows))
    (message "org-node-db: Re-creating %s... done \(%.2fs\)"
             name
             (float-time (time-since T)))
    db))

(defmacro org-node-db--insert-en-masse (db table-sym n-cols)
  (let ((template-row (concat "(" (string-join (make-list n-cols "?")
                                               ", ")
                              ")")))
    `(sqlite-execute ,db
                     (concat ,(format "INSERT INTO %S VALUES " table-sym)
                             (string-join (make-list (length ,table-sym) ,template-row)
                                          ", "))
                     ;; TODO: Instead of this nconc, build the lists the right
                     ;;       way the first time.
                     (apply #'nconc ,table-sym))))

(defun org-node-db--populate (db row-sets)
  (seq-let (files nodes aliases citations refs tags links) row-sets
    (with-sqlite-transaction db
      (org-node-db--insert-en-masse db files 5)
      (org-node-db--insert-en-masse db nodes 11)
      (org-node-db--insert-en-masse db aliases 2)
      (org-node-db--insert-en-masse db citations 4)
      (org-node-db--insert-en-masse db refs 3)
      (org-node-db--insert-en-masse db tags 2)
      (org-node-db--insert-en-masse db links 5))))

;; Compare! This is the impact of the `unnecessary-compat-data'.
;; (setq org-node-db-share nil)
;; (setq org-node-db-share t)
;; (benchmark-call #'org-node-db--mk-rows)

(defun org-node-db--mk-rows (&optional specific-files)
  "Return everything org-node knows, that org-roam can consume.

Specifically, return seven lists of rows, one for each SQL table
defined in `org-node-db--init-pragmas-and-schemata'.

With SPECIFIC-FILES, only return data that involves those files."
  (let (file-rows
        node-rows
        alias-rows
        citation-rows
        ref-rows
        tag-rows
        link-rows
        (dummy-props (prin1-to-string (list :outline nil)))
        (seen-files (make-hash-table :test 'equal))
        (roam-dir (and org-node-db-share
                       (boundp 'org-roam-directory)
                       (org-node-abbrev-file-names
                        (file-truename org-roam-directory)))))
    (when (and org-node-db-share
               (or (not roam-dir) (not (file-exists-p roam-dir))))
      (error "Value `org-roam-directory' set to nonexistent place: %s (%s)"
             roam-dir
             "Error because `org-node-db-share' is t"))
    (cl-loop
     for node being each hash-value of org-node--id<>node
     as file = (org-node-get-file node)
     when (or (not specific-files) (member file specific-files))
     when (or (not org-node-db-share) (string-prefix-p roam-dir file))
     do
     (unless (gethash file seen-files)
       (puthash file t seen-files)
       (push (org-node-db--mk-file-row node) file-rows))
     (cl-symbol-macrolet ((..aliases    (org-node-get-aliases node))
                          (..deadline   (org-node-get-deadline node))
                          (..file       (org-node-get-file node))
                          (..id         (org-node-get-id node))
                          (..level      (org-node-get-level node))
                          (..olp        (org-node-get-olp node))
                          (..pos        (org-node-get-pos node))
                          (..priority   (org-node-get-priority node))
                          (..properties (org-node-get-properties node))
                          (..roam-refs  (org-node-get-refs node))
                          (..scheduled  (org-node-get-scheduled node))
                          (..tags       (org-node-get-tags node))
                          (..title      (org-node-get-title node))
                          (..todo       (org-node-get-todo node)))
       ;; See `org-roam-db-insert-aliases'
       (cl-loop for alias in ..aliases do
                (push (list ..id alias) alias-rows))
       ;; See `org-roam-db-insert-tags'
       (cl-loop for tag in ..tags do
                (push (list ..id tag) tag-rows))
       ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
       (push (list ..id
                   ..file
                   ..level
                   ..pos
                   ..todo
                   ..priority
                   (and ..scheduled
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string ..scheduled))))
                   (and ..deadline
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string ..deadline))))
                   ..title
                   (prin1-to-string ..properties)
                   (prin1-to-string ..olp))
             node-rows)
       ;; See `org-roam-db-insert-refs'
       (cl-loop for ref in ..roam-refs do
                (let ((type (gethash ref org-node--ref-path<>ref-type)))
                  (push (list ..id
                              ref
                              (or type "cite"))
                        ref-rows))))
     (dolist (link (append (org-node-get-id-links-to node)
                           (org-node-get-reflinks-to node)))
       (let* ((origin-node (gethash (plist-get link :origin) org-nodes))
              ;; TODO: Pullreq org-roam to drop this thing.
              ;; (It's used for `org-roam-node-insert-section', which also
              ;; receives a SOURCE-NODE argument, with which this data is
              ;; trivial to reconstruct.  Pre-constructing leads to GC churn.)
              (unnecessary-compat-data
               (if org-node-db-share
                   (prin1-to-string
                    (list :outline
                          (and origin-node
                               (org-node-get-olp-with-self origin-node))))
                 dummy-props)))
         (unless origin-node
           (error "Unknown ID: %s" (plist-get link :origin)))
         (when (or (not org-node-db-share)
                   (string-prefix-p roam-dir (org-node-get-file origin-node)))
           (if (plist-get link :type)
               ;; See `org-roam-db-insert-link'
               (push (list (plist-get link :pos)
                           (plist-get link :origin)
                           (plist-get link :dest)
                           (plist-get link :type)
                           unnecessary-compat-data)
                     link-rows)
             ;; See `org-roam-db-insert-citation'
             (push (list (plist-get link :origin)
                         (substring (plist-get link :dest) 1)
                         (plist-get link :pos)
                         unnecessary-compat-data)
                   citation-rows))))))
    (list
     file-rows node-rows alias-rows citation-rows ref-rows tag-rows link-rows)))

(defun org-node-db--mk-file-row (node)
  (let* ((file (org-node-get-file node))
         (lisp-mtime
          (prin1-to-string
           (seconds-to-time
            (1+ (gethash file org-node--file<>mtime))))))
    (list file
          (org-node-get-file-title node)
          ""         ; HACK: Hashing is slow, skip
          lisp-mtime ; HACK: org-roam doesn't use atime anyway
          lisp-mtime)))

(defun org-node-db--update (files)
  "Update current DB about nodes and links involving FILES."
  (cl-assert (or (featurep 'org-roam) org-node-db-mode))
  (let ((db (if (and org-node-db-share (fboundp 'org-roam-db))
                (org-roam-db)
              (org-node-db)))
        (rows (org-node-db--mk-rows files)))
    ;; NOTE: There's a likely performance bug in Emacs sqlite.c.
    ;;       I have a yuge file, which takes <0.01 seconds to delete on the
    ;;       sqlite3 command line.
    ;;       Doing it with `sqlite-execute' takes 0.53 seconds.
    (dolist (file files)
      (sqlite-execute db "DELETE FROM files WHERE file LIKE ?" (list file)))
    (org-node-db--populate db rows)))

(defun org-node-db-open (&optional db)
  "Explore contents of currently used SQLite DB.

With optional argument DB, use that database connection
instead of `org-node-db--connection'."
  (interactive)
  (cl-assert (sqlite-available-p))
  (pop-to-buffer
   (get-buffer-create (format "*SQLite %.50s*"
                              (if db (prin1-to-string db)
                                "org-node diskless DB"))))
  (sqlite-mode)
  (setq-local sqlite--db (or db (org-node-db)))
  (unless (ignore-errors (sqlite-pragma sqlite--db "im_still_here"))
    (error "org-node-db-open: Not a live db connection: %s" sqlite--db))
  (sqlite-mode-list-tables))


;;; Dev tools

(defun org-node-db--reset ()
  "Close current `org-node-db--connection' and set to a new one."
  (interactive)
  (ignore-errors (sqlite-close org-node-db--connection))
  (setq org-node-db--connection (org-node-db--write-new-db)))

(defvar emacsql-type-map)
(defun org-node-db--insert-roam-schemata-atpt ()
  "Dev tool for printing `org-roam-db--table-schemata' as raw SQL.
Requires loading org-roam."
  (interactive)
  (require 'org-roam)
  (require 'emacsql)
  ;; Sir Compiler doth worry.
  (when (and (boundp 'org-roam-db--table-schemata)
             (fboundp 'emacsql-types)
             (fboundp 'emacsql-sqlite-open)
             (fboundp 'emacsql-close)
             (fboundp 'emacsql-format)
             (fboundp 'emacsql-prepare))
    (cl-loop
     with dummy = (emacsql-sqlite-open nil)
     with emacsql-type-map = (emacsql-types dummy)
     with exp = (let ((exp* (emacsql-prepare [:create-table $i1 $S2])))
                  (cons (thread-last (car exp*)
                                     (string-replace "("  "(\n\t")
                                     (string-replace ")"  "\n)"))
                        (cdr exp*)))
     for (table schema) in org-roam-db--table-schemata
     do (insert " \n\""
                (string-replace ", " ",\n\t"
                                (emacsql-format exp table schema))
                ";\"")
     finally do (emacsql-close dummy))))

(provide 'org-node-db)

;;; org-node-db.el ends here
