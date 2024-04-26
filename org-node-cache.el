;;; org-node-cache.el --- The heart -*- lexical-binding: t; -*-

(require 'pcre2el)
(require 'org-node-common)

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Instruct on-save hooks and such things to update the cache.
While the mode is active, commands such as `org-node-find' and
`org-node-insert-link' do not need to update the cache every
time."
  :global t
  :group 'org-node
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node-cache-file)
        (advice-add #'rename-file :after #'org-node-cache-file)
        (advice-add #'delete-file :before #'org-node-cache--schedule-reset-maybe))
    (remove-hook 'after-save-hook #'org-node-cache-file)
    (advice-remove #'rename-file #'org-node-cache-file)
    (advice-remove #'delete-file #'org-node-cache--schedule-reset-maybe)))

(defcustom org-node-cache-extra-rg-args
  '("--glob" "**/*.org"
    "--glob" "!**/logseq/bak/**"
    "--glob" "!**/logseq/version-files/**"
    "--glob" "!**/*.sync-conflict-*") ;; ignore Syncthing dups
  "Extra arguments to ripgrep - useful for filtering paths.
Read about glob syntax in the Ripgrep guide:

  https://github.com/BurntSushi/ripgrep/blob/master/GUIDE.md

Rather than add more arguments here, it's probably easier to rely
on local .ignore or .gitignore rules.

On an exotic system such as Windows, you probably have to edit
these paths.

These arguments are NOT passed through a shell, so there's no
need to shell-escape characters.  If you have a filename with a
space, it's fine (I think).  Do not pass several arguments in a
single string, it will not work."
  :type '(repeat string)
  :group 'org-node)

(defun org-node-cache-peek (&optional ht keys)
  "For debugging: peek on some values of `org-nodes'.
When called from Lisp, peek on any hash table HT.  With KEYS t,
peek on keys instead."
  (interactive)
  (let ((rows (if keys (hash-table-keys (or ht org-nodes))
                (hash-table-values (or ht org-nodes)))))
    (dotimes (_ 4)
      (print (nth (random (length rows)) rows)))))

(defun org-node-cache-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (unless (executable-find "rg")
    (user-error "Install ripgrep to use org-node"))
  (let ((sum-files 0)
        (sum-subtrees 0)
        (then (current-time)))
    ;; Wipe
    (clrhash org-nodes)
    (clrhash org-node-collection)
    (clrhash org-node-cache--refs-table)
    (clrhash org-node-cache--files)
    ;; Rebuild `org-nodes'
    (org-node--init-org-id-locations-or-die)
    (dolist (dir (org-node--root-dirs (hash-table-values org-id-locations)))
      (org-node-cache--scan dir))
    (dolist (node (hash-table-values org-nodes))
      (cl-incf (if (plist-get node :is-subtree) sum-subtrees sum-files))
      ;; Rebuild `org-node-cache--refs-table'
      (dolist (ref (plist-get node :roam-refs))
        (puthash ref (plist-get node :id) org-node-cache--refs-table)
        ;; Let refs work as aliases
        (puthash ref node org-node-collection))
      ;; Rebuild `org-node-collection'
      (if (funcall org-node-filter-fn node)
          (dolist (title (cons (plist-get node :title)
                               (plist-get node :aliases)))
            (puthash (funcall org-node-format-candidate-fn node title)
                     node
                     org-node-collection))
        (plist-put node :is-excluded t)))
    (message "org-node: recorded %d files and %d subtrees in %.2fs"
             sum-files
             sum-subtrees
             (float-time (time-since then)))
    (run-hooks 'org-node-cache-hook)))

(defun org-node-cache--scan (target)
  (org-node-cache--collect-file-level-nodes target)
  (org-node-cache--calc-file-outlines target)
  (org-node-cache--collect-subtree-nodes target))

(defun org-node-cache-file (&rest args)
  "Grep for nodes in a single file."
  ;; If triggered by `rename-file' advice, the second argument is NEWNAME.  The
  ;; new name may not be that of the current buffer, e.g. it may be called from
  ;; a Dired buffer.
  (let* ((arg2 (cadr args))
         (file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                   arg2
                 (buffer-file-name))))
    (org-node-cache--scan file))
  (run-hooks 'org-node-cache-file-hook))

(defvar org-node-cache-hook (list))
(defvar org-node-cache-file-hook (list))

(defun org-node-cache-ensure-fresh ()
  (when (or (not org-node-cache-mode) (not (hash-table-p org-nodes)) (hash-table-empty-p org-node-collection))
    (org-node-cache-reset)
    ;; In practice, this message delivered once per emacs session
    (message "To speed up this command, turn on `org-node-cache-mode'")))

(let ((this-timer (timer-create)))
  (defun org-node-cache--schedule-reset-maybe (&rest _)
    "If inside an org file now, reset cache after a few seconds.

This is intended to trigger prior to file deletion, and the delay
avoids bothering the user who may be trying to delete many files."
    (when (derived-mode-p 'org-mode)
      (cancel-timer this-timer)
      (setq this-timer (run-with-idle-timer 6 nil #'org-node-cache-reset)))))


;;; Plumbing

(defvar org-node-cache--refs-table (make-hash-table :size 1000 :test #'equal))

(defun org-node-cache--make-todo-regexp ()
  "Make a regexp based on global value of `org-todo-keywords',
that will match any of the keywords."
  (require 'org)
  (->> (string-join (-mapcat #'cdr (default-value 'org-todo-keywords)) " ")
       (replace-regexp-in-string "(.*?)" "")
       (replace-regexp-in-string "[^ [:alpha:]]" "")
       (string-trim)
       (string-split)
       (regexp-opt)))

;; Optimized for one purpose, do not reuse
(let ((regexp (rx "[[id:" (group (+? nonl)) "][" (+? nonl) "]]")))
  (defun org-node-cache--backlinks->list (backlinks-string)
    "Take substrings looking like [[id:abcd][Description]] in
BACKLINKS-STRING, and collect the \"abcd\" parts into a list.

At least if its assumptions about STR are correct, which it does
not check."
    (declare (pure t) (side-effect-free t))
    (if backlinks-string
        (string-split (replace-regexp-in-string regexp "\\1" backlinks-string)
                      "  " t))))

(defun org-node-cache--program-output (program &rest args)
  "Like `shell-command-to-string', but skip the shell intermediary.

Arguments PROGRAM and ARGS as in `call-process'.  Each argument
is a string usually without spaces, and needs not
backslash-escape characters such as asterisks.  On the other
hand, you get no shell magic such as globs or envvars."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))


;;; More fun plumbing

;; TODO: make the entire property drawer optional
(defconst org-node-cache--file-level-re
  (rxt-elisp-to-pcre
   (rx bol ":PROPERTIES:" (* space)
       (group (+? "\n:" (not space) (* nonl)))
       "\n:END:" (* space)
       (*? "\n" (* nonl))
       (group (+ "\n#+" (* nonl)))))
  "Regexp to match file-level nodes.")

;; How do people live without `rx'?
(defun org-node-cache--calc-subtree-re ()
  "Construct new regexp for the variable `org-node-cache--subtree-re'."
  (rxt-elisp-to-pcre
   (rx bol (group (+ "*")) (+ space)
       (? (group (regexp (org-node-cache--make-todo-regexp))) (+ space)) ; TODO
       (group (+ nonl))                                     ; Heading title
       (? (+ space) (group ":" (+? nonl) ":")) (* space)     ; :tags:
       ;; TODO formulate elisp to process the whole line
       ;; (? "\n" (* space) (group "DEADLINE: " (+ (not (any ">]"))))  (not (any " *")) (* nonl))
       (group (? "\n" (* space) (not (any " *")) (* nonl)))          ; CLOSED/SCHEDULED
       (? "\n" (* space) (or ":PROPERTIES:" ":properties:")
          (group (+? "\n" (* space) ":" (not space) (+ nonl)))
          "\n" (* space) (or ":END:" ":end:")))))

(defun org-node-cache--collect-file-level-nodes (target)
  "Scan TARGET (a file or directory) for file-level ID nodes."
  (let ((case-fold-search t)
        (rg-result
         (apply #'org-node-cache--program-output "rg"
                `("--multiline"
                  "--ignore-case"
                  "--with-filename"
                  "--line-number"
                  "--max-count" "1"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2--sophisticatedSeparator--"
                  ,@org-node-cache-extra-rg-args
                  ,org-node-cache--file-level-re
                  ,target))))
    (with-temp-buffer
      (dolist (file-head (string-split rg-result "--sophisticatedSeparator--\n" t))
        (let ((splits (string-split file-head "\f")))
          (let ((file:lnum (string-split (nth 0 splits) ":" t))
                ($1 (nth 1 splits))
                ($2 (nth 2 splits))
                props id title tags)
            (erase-buffer)
            (insert $1)
            (goto-char (point-min))
            (while (not (eobp))
              (dotimes (_ 3) (search-forward ":"))
              (push (cons (upcase (buffer-substring-no-properties
                                   (point) (1- (search-forward ":"))))
                          (string-trim (buffer-substring-no-properties
                                        (point) (line-end-position))))
                    props)
              (forward-line 1))
            (let ((beg (point)))
              (insert $2)
              (goto-char beg)
              (when (search-forward "#+title: " nil t)
                (setq title (string-trim (buffer-substring-no-properties
                                          (point) (line-end-position))))
                (goto-char beg))
              (when (search-forward "#+filetags: " nil t)
                (setq tags (string-trim (buffer-substring-no-properties
                                         (point) (line-end-position))))))
            (setq id (cdr (assoc "ID" props)))
            (when (and id title)
              (puthash id (list
                           :title title
                           :level 0
                           :line-number 1
                           :file-path (car file:lnum)
                           :file-title title
                           :id id
                           :tags (and tags (string-split tags ":" t))
                           :properties props
                           :exclude
                           (equal "t" (cdr (assoc "ROAM_EXCLUDE" props)))
                           :aliases
                           (let ((aliases (cdr (assoc "ROAM_ALIASES" props))))
                             (and aliases (split-string-and-unquote aliases)))
                           :roam-refs
                           (let ((refs (cdr (assoc "ROAM_REFS" props))))
                             (and refs (split-string-and-unquote refs)))
                           :backlink-origins
                           (org-node-cache--backlinks->list
                            (cdr (assoc "CACHED_BACKLINKS" props))))
                       org-nodes)
              (puthash (car file:lnum) (list title) org-node-cache--files))
            ))))))

(defun org-node-cache--calc-file-outlines (target)
  "Record outline level at each line num with a heading."
  (let ((rg-result (apply #'org-node-cache--program-output "rg"
                          `("--with-filename"
                            "--line-number"
                            "--only-matching"
                            ,@org-node-cache-extra-rg-args
                            "^\\*+ "
                            ,target))))
    (dolist (line (split-string rg-result "\n" t))
      ;; A match looks like /home/file.org:91:***
      (let ((parts (split-string line ":")))
        (let ((lnum (string-to-number (nth 1 parts)))
              (level (length (substring (nth 2 parts) 0 -1))))
          (push (list lnum level)
                (gethash (nth 0 parts) org-node-cache--files)))))))

(defun org-node-cache--collect-subtree-nodes (target)
  "Scan TARGET (a file or directory) for subtree ID nodes."
  (let ((workaround-counter 0)
        (rg-result
         (apply #'org-node-cache--program-output "rg"
                `("--multiline"
                  "--with-filename"
                  "--line-number"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2\f$3\f$4\f$5\f$6--sophisticatedSeparator--"
                  ,@org-node-cache-extra-rg-args
                  ,(org-node-cache--calc-subtree-re)
                  ,target))))
    (with-temp-buffer
      ;; TODO: boost perf by grouping some operations by file
      (dolist (subtree (string-split rg-result "--sophisticatedSeparator--\n" t))
        (let ((splits (string-split subtree "\f")))
          (let* ((file:lnum (string-split (nth 0 splits) ":" t))
                 ($1 (nth 1 splits))
                 ($2 (nth 2 splits))
                 ($3 (nth 3 splits))
                 ($4 (nth 4 splits))
                 ($5 (nth 5 splits))
                 ($6 (nth 6 splits))
                 (file (car file:lnum))
                 (lnum (string-to-number (cadr file:lnum)))
                 (file-data (gethash file org-node-cache--files))
                 (file-title (car (last file-data)))
                 (outline-data (butlast file-data))
                 props id scheduled deadline)
            ;; Workaround https://github.com/BurntSushi/ripgrep/issues/2779
            ;; Bug will still occur if you have headings separated by just 2+
            ;; newlines (and no property drawer)

            (if (and (assoc lnum outline-data)
                     (not (stringp (car (alist-get lnum outline-data)))))
                (setq workaround-counter 0)
              (cl-incf lnum workaround-counter))
            (while (or (null (assoc lnum outline-data))
                       (stringp (car (alist-get lnum outline-data))))
              (cl-incf lnum)
              (cl-incf workaround-counter))

            ;; (if (stringp (car (alist-get lnum outline-data)))
            ;;     (cl-incf lnum (cl-incf workaround-counter))
            ;;   (setq workaround-counter 0))
            ;; add this heading title to the outline data, for use next iter.
            (push $3 (alist-get lnum (gethash file org-node-cache--files)))
            ;; (let )
            ;; (setq foo '(a b c))
            ;; (push nil foo)
            (erase-buffer)
            (insert $6)
            (goto-char (point-min))
            (while (not (eobp))
              (dotimes (_ 3) (search-forward ":"))
              (push (cons (upcase (buffer-substring-no-properties
                                   (point) (1- (search-forward ":"))))
                          (string-trim (buffer-substring-no-properties
                                        (point) (line-end-position))))
                    props)
              (forward-line 1))
            (setq id (cdr (assoc "ID" props)))
            (let ((beg (point)))
              (insert $5)
              (goto-char beg)
              (when (search-forward "SCHEDULED: " nil t)
                (setq scheduled
                      (string-trim (buffer-substring-no-properties
                                    (point) (1+ (re-search-forward "[^]>]*")))))
                (goto-char beg))
              (when (search-forward "DEADLINE: " nil t)
                (setq deadline
                      (string-trim (buffer-substring-no-properties
                                    (point) (1+ (re-search-forward "[^]>]*")))))))
            (puthash
             ;; Record it even if it hasn't an ID
             (or id (format-time-string "%N"))
             (list :title $3
                   :is-subtree t
                   :level (length $1)
                   :line-number lnum
                   :tags (string-split $4 ":" t)
                   :todo (unless (string-blank-p $2) $2)
                   :file-path file
                   :scheduled scheduled
                   :deadline deadline
                   :file-title file-title
                   :olp (org-node-cache--line->olp outline-data lnum)
                   :properties props
                   :id (cdr (assoc "ID" props))
                   :exclude (equal "t" (cdr (assoc "ROAM_EXCLUDE" props)))
                   :aliases (split-string-and-unquote
                             (or (cdr (assoc "ROAM_ALIASES" props)) ""))
                   :roam-refs (when-let ((refs (cdr (assoc "ROAM_REFS" props))))
                                (split-string-and-unquote refs))
                   :backlink-origins (org-node-cache--backlinks->list
                                      (cdr (assoc "CACHED_BACKLINKS" props))))
             org-nodes)))))))

(defvar org-node-cache--files
  (make-hash-table :size 3000 :test #'equal)
  "Table keyed on filepaths, where the value is a plist:
:file-level-node t/nil
:lnums-ids
((LINE-NUM . ID)
 (LINE-NUM . ID)
 ...)
:lnums-levels
((LINE-NUM . OUTLINE-LEVEL)
 (LINE-NUM . OUTLINE-LEVEL)
 ...)
")

(defun org-node-cache--line->olp (outline-data line)
  "Given LINE number, return a list of line numbers corresponding to
where the ancestor subtrees are.  The current subtree is included.

Argument OUTLINE-DATA must be of the form
 ((1 \"File title\" 0)
  (23 \"Heading title\" 3)
  (50 \"Another heading\" 1)
  (73 \"Heading\" 2)
  ...)

where the car of each element represents a line number, the cadr the
heading title and the caddr the outline depth i.e. the number of
asterisks in the heading at that line.

LINE itself needs not to correspond to a subtree."
  (let* (reversed
         ;; Drop all the lines below LINE
         (relevant (cl-loop for cell in (reverse outline-data)
                            if (> line (car cell))
                            do (push cell reversed)
                            else return t))

         (curr-level (caddr (car reversed)))
         ;; Bootstrap
         ;; (olp (list (cadr (car reversed))))
         (olp nil))
    ;; (print reversed)
    ;; Work backwards towards the top of the file
    (cl-loop for cell in reversed
             when (> curr-level (caddr cell))
             do (setq curr-level (caddr cell))
             (push (cadr cell) olp))
    ;; (print olp)
    olp))


;; (cadr (hash-table-keys org-node-cache--files))
;; (cadr (hash-table-values org-node-cache--files))
;; (clrhash org-node-cache--files)
;; (gethash (org-id-get nil nil nil t) org-nodes)

(provide 'org-node-cache)

;;; org-node-cache.el ends here
