;;; org-id-node-cache.el --- The heart -*- lexical-binding: t; -*-

(require 'pcre2el)
(require 'org-id-node-common)

;;;###autoload
(define-minor-mode org-id-node-cache-mode
  "Instruct on-save hooks and such things to update the cache.
While the mode is active, commands such as `org-id-node-find' and
`org-id-node-insert-link' do not need to update the cache every
time."
  :global t
  (if org-id-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-id-node-cache-file)
        (advice-add #'rename-file :after #'org-id-node-cache-file)
        (advice-add #'delete-file :before #'org-id-node-cache--schedule-reset-maybe))
    (remove-hook 'after-save-hook #'org-id-node-cache-file)
    (advice-remove #'rename-file #'org-id-node-cache-file)
    (advice-remove #'delete-file #'org-id-node-cache--schedule-reset-maybe)))

(defcustom org-id-node-cache-extra-rg-args
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
  :group 'org-id-node)

(defun org-id-node-cache-peek (&optional ht keys)
  "For debugging: peek on some values of `org-id-nodes'.
When called from Lisp, peek on any hash table HT.  With KEYS t,
peek on keys instead."
  (interactive)
  (let ((rows (if keys (hash-table-keys (or ht org-id-nodes))
                (hash-table-values (or ht org-id-nodes)))))
    (dotimes (_ 4)
      (print (nth (random (length rows)) rows)))))

(defun org-id-node-cache-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (unless (executable-find "rg")
    (user-error "Install ripgrep to use org-id-node"))
  (let ((sum-files 0)
        (sum-subtrees 0)
        (then (current-time)))
    ;; Wipe
    (clrhash org-id-nodes)
    (clrhash org-id-node-collection)
    ;; Rebuild `org-id-nodes'
    (org-id-node--init-org-id-locations-or-die)
    (dolist (dir (org-id-node--root-dirs (hash-table-values org-id-locations)))
      (org-id-node-cache--scan dir))
    ;; Rebuild `org-id-node-collection'
    (dolist (node (hash-table-values org-id-nodes))
      (cl-incf (if (plist-get node :is-subtree) sum-subtrees sum-files))
      (if (funcall org-id-node-filter-fn node)
          (dolist (title (cons (plist-get node :title)
                               (plist-get node :aliases)))
            (puthash (funcall org-id-node-format-candidate-fn node title)
                     node
                     org-id-node-collection))
        (plist-put node :exclude t)))
    (message "org-id-node: Recorded %d files and %d subtrees in %.2fs"
             sum-files
             sum-subtrees
             (float-time (time-since then)))
    (run-hooks 'org-id-node-cache-hook)))

;; Overridden by a thing in org-id-node-experimental.el
(defun org-id-node-cache--scan (target)
  (org-id-node-cache--collect-file-level-nodes target)
  (org-id-node-cache--collect-subtree-nodes target))

(defun org-id-node-cache-file (&rest args)
  "Grep for nodes in a single file."
  ;; If triggered by `rename-file' advice, the second argument is NEWNAME.  The
  ;; new name may not be that of the current buffer, e.g. it may be called from
  ;; a Dired buffer.
  (let* ((arg2 (cadr args))
         (file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                   arg2
                 (buffer-file-name))))
    (org-id-node-cache--scan file))
  (run-hooks 'org-id-node-cache-hook))

(defvar org-id-node-cache-hook (list))

(defun org-id-node-cache-ensure-fresh ()
  (when (or (not org-id-node-cache-mode) (not (hash-table-p org-id-nodes)) (hash-table-empty-p org-id-node-collection))
    (org-id-node-cache-reset)
    ;; In practice, this message delivered once per emacs session
    (message "To speed up this command, turn on `org-id-node-cache-mode'")))

(let ((this-timer (timer-create)))
  (defun org-id-node-cache--schedule-reset-maybe (&rest _)
    "If inside an org file now, reset cache after a few seconds.

This is intended to trigger prior to file deletion, and the delay
avoids bothering the user who may be trying to delete many files."
    (when (derived-mode-p 'org-mode)
      (cancel-timer this-timer)
      (setq this-timer (run-with-idle-timer 6 nil #'org-id-node-cache-reset)))))


;;; Plumbing

(defun org-id-node-cache--make-todo-regexp ()
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
  (defun org-id-node-cache--backlinks->list (backlinks-string)
    "Take substrings looking like [[id:abcd][Description]] in
BACKLINKS-STRING, and collect the \"abcd\" parts into a list.

At least if its assumptions about STR are correct, which it does
not check."
    (declare (pure t) (side-effect-free t))
    (string-split (replace-regexp-in-string regexp "\\1" backlinks-string)
                  "  " t)))

(defun org-id-node-cache--aliases->list (aliases-string)
  "Turn ALIASES-STRING into a list.
Assumes that each alias comes wrapped in double-quotes."
  (declare (pure t) (side-effect-free t))
  (string-split (string-replace "\" \"" "\"\f\"" aliases-string)
                "\f" t))

(defun org-id-node-cache--program-output (program &rest args)
  "Like `shell-command-to-string', but skip the shell intermediary.

Arguments PROGRAM and ARGS as in `call-process'.  Each argument
is a string usually without spaces, and needs not
backslash-escape characters such as asterisks.  On the other
hand, you get no shell magic such as globs or envvars."
  (with-temp-buffer
    (apply #'call-process program nil t nil args)
    (buffer-string)))


;;; More fun plumbing

(defconst org-id-node-cache--file-level-re
  (rxt-elisp-to-pcre
   (rx bol ":PROPERTIES:" (* space)
       (*? "\n:" (not space) (* nonl))
       (?  "\n:CACHED_BACKLINKS:" (+ space) (group (+ nonl)))
       (*? "\n:" (not space) (* nonl))
       "\n:ID:"                   (+ space) (group (+? nonl))
       (*? "\n:" (not space) (* nonl))
       (?  "\n:ROAM_ALIASES:"     (+ space) (group (+ nonl)))
       (*? "\n:" (not space) (* nonl))
       (?  "\n:ROAM_EXCLUDE:"     (+ space) (group (+ nonl)))
       (*? "\n:" (not space) (* nonl))
       (?  "\n:ROAM_REFS:"        (+ space) (group (+ nonl)))
       (*? "\n:" (not space) (* nonl))
       "\n:END:" (* space)
       (*? "\n" (* nonl))
       (? "\n#+filetags:" (* space) (group (+ nonl)))
       (*? "\n" (* nonl))
       "\n#+title:" (* space) (group (+ nonl))))
  "Regexp to match file-level nodes.")

;; How do people live without `rx'?
(defun org-id-node-cache--calc-subtree-re ()
  "Construct new regexp for the variable `org-id-node-cache--subtree-re'."
  (rxt-elisp-to-pcre
   (rx bol (group (+ "*")) (+ space)
       (? (group (regexp (org-id-node-cache--make-todo-regexp))) (+ space)) ; TODO
       (group (+? nonl))                                     ; Heading title
       (? (+ space) (group ":" (+? nonl) ":")) (* space)     ; :tags:
       (? "\n" (* space) (not (any " *")) (* nonl))          ; CLOSED/SCHEDULED
       ;; We needed to set case-sensitivity to catch todo keywords, so now we
       ;; fake insensitivity
       "\n" (* space) (or ":PROPERTIES:" ":properties:")
       (*? "\n" (* space) ":" (not space) (+ nonl))
       (?  "\n" (* space) (or ":CACHED_BACKLINKS:" ":cached_backlinks:") (+ space) (group (+ nonl)))
       (*? "\n" (* space) ":" (not space) (+ nonl))
       ;; Other properties are optional, but ID is mandatory
       "\n" (* space) (or ":ID:" ":id:")                                 (+ space) (group (+ nonl))
       (*? "\n" (* space) ":" (not space) (+ nonl))
       (?  "\n" (* space) (or ":ROAM_ALIASES:" ":roam_aliases:")         (+ space) (group (+ nonl)))
       (*? "\n" (* space) ":" (not space) (+ nonl))
       (?  "\n" (* space) (or ":ROAM_EXCLUDE:" ":roam_exclude:")         (+ space) (group (+ nonl)))
       (*? "\n" (* space) ":" (not space) (+ nonl))
       (?  "\n" (* space) (or ":ROAM_REFS:" ":roam_refs:")               (+ space) (group (+ nonl)))
       (*? "\n" (* space) ":" (not space) (+ nonl))
       "\n" (* space) (or ":END:" ":end:"))))

(defun org-id-node-cache--collect-file-level-nodes (target)
  "Scan TARGET (a file or directory) for file-level ID nodes."
  (let ((rg-result
         (apply #'org-id-node-cache--program-output "rg"
                `("--multiline"
                  "--ignore-case"
                  "--with-filename"
                  "--line-number"
                  "--max-count" "1"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2\f$3\f$4\f$5\f$6\f$7--veryIntelligentSeparator--"
                  ,@org-id-node-cache-extra-rg-args
                  ,org-id-node-cache--file-level-re
                  ,target))))
    (dolist (file-head (string-split rg-result "--veryIntelligentSeparator--\n" t))
      (let ((splits (string-split file-head "\f")))
        (let ((file:lnum (string-split (nth 0 splits) ":" t))
              ($1 (nth 1 splits))
              ($2 (nth 2 splits))
              ($3 (nth 3 splits))
              ($4 (nth 4 splits))
              ($5 (nth 5 splits))
              ($6 (nth 6 splits))
              ($7 (nth 7 splits)))
          (puthash $2 (list :title $7
                            :level 0
                            :line-number 1
                            :exclude (not (string-blank-p $4))
                            :tags (string-split $6 ":" t)
                            :file-path (car file:lnum)
                            :id $2
                            :aliases (org-id-node-cache--aliases->list $3)
                            :roam-refs (string-split $5 " " t)
                            :backlink-ids (org-id-node-cache--backlinks->list $1))
                   org-id-nodes))))))

(defun org-id-node-cache--collect-subtree-nodes (target)
  "Scan TARGET (a file or directory) for subtree ID nodes."
  (let ((rg-result
         (apply #'org-id-node-cache--program-output "rg"
                `("--multiline"
                  "--with-filename"
                  "--line-number"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2\f$3\f$4\f$5\f$6\f$7\f$8\f$9--veryIntelligentSeparator--"
                  ,@org-id-node-cache-extra-rg-args
                  ,(org-id-node-cache--calc-subtree-re)
                  ,target))))
    (dolist (subtree (string-split rg-result "--veryIntelligentSeparator--\n" t))
      (let ((splits (string-split subtree "\f")))
        (let ((file:lnum (string-split (nth 0 splits) ":" t))
              ($1 (nth 1 splits))
              ($2 (nth 2 splits))
              ($3 (nth 3 splits))
              ($4 (nth 4 splits))
              ($5 (nth 5 splits))
              ($6 (nth 6 splits))
              ($7 (nth 7 splits))
              ($8 (nth 8 splits))
              ($9 (nth 9 splits)))
          (puthash $6 (list :title $3
                            :is-subtree t
                            :level (length $1)
                            :line-number (string-to-number (cadr file:lnum))
                            :exclude (not (string-blank-p $8))
                            :tags (string-split $4 ":" t)
                            :todo (unless (string-blank-p $2) $2)
                            :file-path (car file:lnum)
                            :id $6
                            :aliases (org-id-node-cache--aliases->list $7)
                            :roam-refs (string-split $9 " " t)
                            :backlink-ids (org-id-node-cache--backlinks->list $5))
                   org-id-nodes))))))

(provide 'org-id-node-cache)

;;; org-id-node-cache.el ends here
