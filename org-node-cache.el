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
        (advice-add #'rename-file :before #'org-node-cache--handle-delete)
        (advice-add #'delete-file :before #'org-node-cache--handle-delete))
    (remove-hook 'after-save-hook #'org-node-cache-file)
    (advice-remove #'rename-file #'org-node-cache-file)
    (advice-remove #'rename-file #'org-node-cache--handle-delete)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

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
  (let ((then (current-time)))
    (clrhash org-nodes)
    (clrhash org-node-collection)
    (clrhash org-node--refs-table)
    (org-node--init-org-id-locations-or-die)
    (let ((files (-uniq (hash-table-values org-id-locations))))
      (org-node-cache--collect-nodes files)
      (message "org-node: scanned %d files and %d subtrees in %.2fs"
               (length files)
               (cl-loop for node being the hash-values of org-nodes
                        count (plist-get node :is-subtree))
               (float-time (time-since then))))
    (run-hooks 'org-node-cache-hook)))

(defun org-node-cache-file (&rest args)
  "Seek nodes in a single file."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it applies to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let* ((arg2 (cadr args))
         (file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                   arg2
                 (buffer-file-name))))
    (org-node-cache--collect-nodes (list file)))
  (run-hooks 'org-node-cache-file-hook))

(defvar org-node-cache-hook (list))
(defvar org-node-cache-file-hook (list))

(defun org-node-cache-ensure-fresh ()
  (when (or (not org-node-cache-mode)
            (not (hash-table-p org-nodes))
            (hash-table-empty-p org-node-collection))
    (org-node-cache-reset)
    ;; This message delivers once per emacs session
    (message "To speed up this command, turn on `org-node-cache-mode'")))

(let ((this-timer (timer-create)))
  (defun org-node-cache--handle-delete (arg1 &rest args)
    "Update org-id and org-node after an Org file is deleted.

First remove references in `org-id-locations', as oddly, upstream does
not do that.

Then rebuild the org-node caches after a few idle seconds.  The delay
avoids bothering the user who may be trying to delete many files in a
short time."
    (let ((file-being-deleted (if (and arg1 (stringp arg1) (file-exists-p arg1))
                                  arg1
                                (buffer-file-name))))
      (when (member (file-name-extension file-being-deleted)
                    '("org" "org_archive" "gpg"))
        (org-node--init-org-id-locations-or-die)
        (org-node-cache--forget-id-location file-being-deleted)
        (cancel-timer this-timer)
        (setq this-timer
              (run-with-idle-timer 6 nil #'org-node-cache-reset))))))


;;; Plumbing

(defun org-node-cache--forget-id-location (file)
  (cl-loop for id being the hash-keys of org-id-locations
           using (hash-values file-on-record)
           when (file-equal-p file file-on-record)
           do (remhash id org-id-locations)))

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

;; Optimized for one purpose, do NOT reuse.  Bungles the match data despite
;; claiming not to!
(let ((regexp (rx "[[id:" (group (+? nonl)) "][" (+? nonl) "]]")))
  (defun org-node-cache--backlinks->list (backlinks-string)
    "Out of BACKLINKS-STRING, take substrings looking like
[[id:abcd][Description]], and collect the \"abcd\" parts into a list.

At least if its assumptions about the input are correct, which
it does not check."
    (declare (pure t) (side-effect-free t))
    (if backlinks-string
        (split-string (replace-regexp-in-string regexp "\\1" backlinks-string)
                      "  " t))))

(defun org-node-cache--collect-properties (beg end)
  (let (res)
    (goto-char beg)
    (with-restriction beg end
      (while (not (eobp))
        (search-forward ":")
        (push (cons (upcase (decode-coding-string
                             (buffer-substring
                              (point) (1- (search-forward ":")))
                             'utf-8))
                    (string-trim (decode-coding-string
                                  (buffer-substring
                                   (point) (line-end-position))
                                  'utf-8)))
              res)
        (forward-line 1)))
    res))

(defun org-node-cache--collect-nodes (files)
  "Scan FILES for id-nodes, adding them to `org-nodes'."
  (with-temp-buffer
    (let ((todo-re (org-node-cache--make-todo-regexp))
          (not-a-full-reset (not (hash-table-empty-p org-nodes)))
          (case-fold-search t)
          (gc-cons-threshold (* 1000 1000 1000)))
      (dolist (file files)
        (if (not (file-exists-p file))
            ;; Example situation: user renamed/deleted a file using shell
            ;; commands, outside Emacs.  So org-id references a file that
            ;; doesn't exist.  Our solution: just skip.  If the file was
            ;; renamed to a new location, it'll have to be picked up by org-id
            ;; in its usual ways.
            ;;
            ;; This is a good time to do `org-id-update-id-locations' just
            ;; because, but it won't pick up the new file path.
            (progn
              (org-node-cache--forget-id-location file)
              (org-id-update-id-locations))
          (erase-buffer)
          (insert-file-contents-literally file)
          (let (;; Position of first "content": a line not starting with # or :
                (far (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
                props file-title file-tags outline-data)
            (goto-char 1)
            (when (re-search-forward "^ *:properties:" far t)
              (forward-line 1)
              (setq props (org-node-cache--collect-properties
                           (point) (and (re-search-forward "^ *:end:")
                                        (line-beginning-position)))))
            (goto-char 1)
            (when (re-search-forward "^#\\+filetags: " far t)
              (setq file-tags (split-string
                               (decode-coding-string
                                (buffer-substring (point) (line-end-position))
                                'utf-8)
                               ":" t)))
            (goto-char 1)
            (if (re-search-forward "^#\\+title: " far t)
                (setq file-title (decode-coding-string
                                  (buffer-substring (point) (line-end-position))
                                  'utf-8))
              ;; File nodes dont strictly need #+title, fall back on filename
              (setq file-title (file-name-nondirectory file)))
            (let ((id (cdr (assoc "ID" props))))
              (when not-a-full-reset
                (when id (org-id-add-location id file)))
              (org-node-cache--add-node-to-tables
               (list
                :title file-title
                :level 0
                :tags file-tags
                :file-path file
                :pos 1
                :file-title file-title
                :properties props
                :id id
                :exclude (cdr (assoc "ROAM_EXCLUDE" props))
                :aliases (split-string-and-unquote
                          (or (cdr (assoc "ROAM_ALIASES" props)) ""))
                :roam-refs (let ((refs (cdr (assoc "ROAM_REFS" props))))
                             (and refs (split-string-and-unquote refs)))
                :backlink-origins (org-node-cache--backlinks->list
                                   (cdr (assoc "CACHED_BACKLINKS" props))))))
            ;; Loop over the file's subtrees
            (while (re-search-forward "^\\*+ " nil t)
              (let ((pos (goto-char (line-beginning-position)))
                    (level (skip-chars-forward "*"))
                    here todo title line+2 tags todo sched deadline props id)
                (skip-chars-forward " ")
                (setq here (point))
                ;; TODO Allow unicode todo keywords
                (when (looking-at todo-re)
                  (setq todo (buffer-substring (point) (match-end 0)))
                  (goto-char (1+ (match-end 0)))
                  (setq here (point)))
                (if (re-search-forward " +\\(:.+:\\) *$" (line-end-position) t)
                    (progn
                      (setq title (decode-coding-string
                                   (buffer-substring
                                    here (match-beginning 0))
                                   'utf-8))
                      (setq tags (string-split
                                  (decode-coding-string (match-string 1)
                                                        'utf-8)
                                  ":" t)))
                  (setq title (decode-coding-string (buffer-substring
                                                     here (line-end-position))
                                                    'utf-8)))
                (setq here (point))
                (setq line+2 (and (forward-line 2) (point)))
                (goto-char here)
                (when (re-search-forward "[\n\s]SCHEDULED: " line+2 t)
                  (setq sched
                        (decode-coding-string
                         (buffer-substring
                          ;; \n just there for safety
                          (point) (+ 1 (point) (skip-chars-forward "^]>\n")))
                         'utf-8))
                  (goto-char here))
                (when (re-search-forward "[\n\s]DEADLINE: " line+2 t)
                  (setq deadline
                        (decode-coding-string
                         (buffer-substring
                          (point) (+ 1 (point) (skip-chars-forward "^]>\n")))
                         'utf-8)))
                (setq here (point))
                (setq line+2 (and (forward-line 2) (point)))
                (goto-char here)
                (when (re-search-forward "^ *:properties:" line+2 t)
                  (forward-line 1)
                  (setq props (org-node-cache--collect-properties
                               (point) (and (re-search-forward "^ *:end:")
                                            (line-beginning-position)))))
                (setq id (cdr (assoc "ID" props)))
                (when not-a-full-reset
                  (when id (org-id-add-location id file)))
                (push (list pos title level) outline-data)
                (org-node-cache--add-node-to-tables
                 (list
                  :title title
                  :is-subtree t
                  :level level
                  :pos pos
                  :tags tags
                  :todo todo
                  :file-path file
                  :scheduled sched
                  :deadline deadline
                  :file-title file-title
                  :olp (org-node-cache--pos->olp outline-data pos)
                  :properties props
                  :id id
                  :exclude (equal "t" (cdr (assoc "ROAM_EXCLUDE" props)))
                  :aliases (split-string-and-unquote
                            (or (cdr (assoc "ROAM_ALIASES" props)) ""))
                  :roam-refs (let ((refs (cdr (assoc "ROAM_REFS" props))))
                               (and refs (split-string-and-unquote refs)))
                  :backlink-origins (org-node-cache--backlinks->list
                                     (cdr (assoc "CACHED_BACKLINKS" props)))))))))))))

;; I feel like this could be easier to read...
(defun org-node-cache--add-node-to-tables (node)
  "Add NODE to `org-nodes' and maybe `org-node-collection'."
  ;; Record to `org-nodes' even if it has no ID
  (puthash (or (plist-get node :id) (format-time-string "%N"))
           node
           org-nodes)
  (when (or (not org-node-only-show-subtrees-with-id) (plist-get node :id))
    ;; Add to `org-node--refs-table'
    (dolist (ref (plist-get node :roam-refs))
      (puthash ref (plist-get node :id) org-node--refs-table))
    (if (funcall org-node-filter-fn node)
        (progn
          ;; Add to `org-node-collection'
          (dolist (title (cons (plist-get node :title)
                               (plist-get node :aliases)))
            (puthash (funcall org-node-format-candidate-fn node title)
                     node
                     org-node-collection))
          ;; Let refs work as aliases
          (dolist (ref (plist-get node :roam-refs))
            (puthash ref node org-node-collection)))
      (plist-put node :is-excluded t))))

(defun org-node-cache--pos->olp (oldata pos)
  "Given buffer position POS, return the Org outline path.
Result should be like that from `org-get-outline-path'.

Argument OLDATA must be of the form
 ((373 \"Heading\" 2)
  (250 \"Another heading\" 1)
  (123 \"Misplaced third-level heading\" 3)
  ...)

where the car of each element represents a buffer position, the cadr the
heading title and the caddr the outline depth i.e. the number of
asterisks in the heading at that location.

OLDATA must be in reverse order, so the last heading in the file is
represented as the first element.  It must also contain an element
corresponding to POS."
  (declare (pure t) (side-effect-free t))
  (let (olp
        ;; Drop all the data about positions below POS
        (reversed (-drop (-elem-index (assoc pos oldata) oldata) oldata)))
    (let ((curr-level (caddr (car reversed))))
      ;; Work backwards towards the top of the file
      (cl-loop for cell in reversed
               when (> curr-level (caddr cell))
               do (setq curr-level (caddr cell))
               (push (cadr cell) olp)
               and if (= 1 curr-level)
               ;; Stop
               return nil))
    olp))

(provide 'org-node-cache)

;;; org-node-cache.el ends here
