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
  (unless (executable-find "rg")
    (user-error "Install ripgrep to use org-node"))
  (let ((then (current-time)))
    ;; Wipe
    (clrhash org-nodes)
    (clrhash org-node-collection)
    (clrhash org-node--refs-table)
    ;; Rebuild `org-nodes'
    (org-node--init-org-id-locations-or-die)
    (let ((files (-uniq (hash-table-values org-id-locations))))
      (org-node-cache--collect-nodes files)
      (message "org-node: scanned %d files and %d subtrees from scratch in %.2fs"
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
  (defun org-node-cache--handle-delete (&rest _)
    "Update org-id and org-node after an Org file is deleted.

First remove references in `org-id-locations', as oddly, upstream does
not do that.

Then rebuild the org-node caches after a few idle seconds.  The delay
avoids bothering the user who may be trying to delete many files in a
short time."
    (when (derived-mode-p 'org-mode)
      (cl-loop for id being the hash-keys of org-id-locations
               using (hash-values file)
               when (file-equal-p file (buffer-file-name))
               do (remhash id org-id-locations))
      ;; (let ((ids-in-file
      ;;        (save-excursion
      ;;          (without-restriction
      ;;            (goto-char 1)
      ;;            (cl-loop while (re-search-forward "^[\s\t]*:ID: *\\(.+?\\) *$"
      ;;                                              nil t)
      ;;                     collect (match-string 1))))))
      ;;   (dolist (id ids-in-file)
      ;;     (remhash id org-id-locations)))
      (cancel-timer this-timer)
      (setq this-timer (run-with-idle-timer 6 nil #'org-node-cache-reset)))))


;;; Plumbing

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
    ;; Shorthand to avoid juggling its extra arg during refactor
    (cl-flet ((unicode (str) (decode-coding-string str 'utf-8)))
      (cl-loop
       with todo-re = (org-node-cache--make-todo-regexp)
       with single-file-being-scanned = (not (hash-table-empty-p org-nodes))
       with case-fold-search = t
       with gc-cons-threshold = (* 3 1000 1000 1000)
       for file in files
       when (--none-p (string-search it file) '("/logseq/"))
       do
       (erase-buffer)
       (insert-file-contents-literally file)
       (let (;; Position of first content (a line not starting with # or :)
             (far (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
             ;; (far (or (re-search-forward "^\\*" nil t) (point-max)))
             props file-title file-tags
             outline-data)
         (goto-char 1)
         (when (re-search-forward "^ *:properties:" far t)
           (forward-line 1)
           (setq props (org-node-cache--collect-properties
                        (point) (and (re-search-forward "^ *:end:")
                                     (line-beginning-position)))))
         (goto-char 1)
         (when (re-search-forward "^#\\+filetags: " far t)
           (setq file-tags (split-string
                            (unicode (buffer-substring
                                      (point) (line-end-position)))
                            ":" t)))
         (goto-char 1)
         ;; this also means unlike roam, file nodes dont need #+title
         (if (re-search-forward "^#\\+title: " far t)
             (setq file-title (unicode (buffer-substring
                                        (point) (line-end-position))))
           (setq file-title (file-name-nondirectory file)))
         (let* ((id (cdr (assoc "ID" props)))
                (node (list
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
           (puthash id node org-nodes)
           (org-node-cache--populate-other-tables node)
           (when single-file-being-scanned
             (when id (org-id-add-location id file))))
         ;; Loop over the file's subtrees
         (while (re-search-forward "^\\*+ " nil t)
           (let (here todo title line2-end level pos tags todo scheduled deadline props id)
             (setq pos (goto-char (line-beginning-position)))
             (setq level (skip-chars-forward "*"))
             (skip-chars-forward " ")
             (setq here (point))
             ;; NOTE: The todo-regexp prolly won't match unicode todo keywords
             (when (looking-at todo-re)
               (setq todo (buffer-substring (point) (match-end 0)))
               (goto-char (1+ (match-end 0)))
               (setq here (point)))
             (if (re-search-forward " +\\(:.+:\\) *$" (line-end-position) t)
                 (progn
                   (setq title (unicode (buffer-substring
                                         here (match-beginning 0))))
                   (setq tags (string-split (unicode (match-string 1))
                                            ":" t)))
               (setq title (unicode (buffer-substring
                                     here (line-end-position)))))
             (setq here (point))
             (setq line2-end (and (forward-line 2) (point)))
             (goto-char here)
             (when (re-search-forward "[\n\s]SCHEDULED: " line2-end t)
               (setq scheduled
                     (unicode
                      (buffer-substring
                       ;; \n just there for safety
                       (point) (+ 1 (point) (skip-chars-forward "^]>\n")))))
               (goto-char here))
             (when (re-search-forward "[\n\s]DEADLINE: " line2-end t)
               (setq deadline
                     (unicode
                      (buffer-substring
                       (point) (+ 1 (point) (skip-chars-forward "^]>\n"))))))
             (setq here (point))
             (setq line2-end (and (forward-line 2) (point)))
             (goto-char here)
             (when (re-search-forward "^ *:properties:" line2-end t)
               (forward-line 1)
               (setq props (org-node-cache--collect-properties
                            (point) (and (re-search-forward "^ *:end:")
                                         (line-beginning-position)))))
             (setq id (cdr (assoc "ID" props)))
             (push (list pos title level) outline-data)
             (let ((node
                    (list
                     :title title
                     :is-subtree t
                     :level level
                     :pos pos
                     :tags tags
                     :todo todo
                     :file-path file
                     :scheduled scheduled
                     :deadline deadline
                     :file-title file-title
                     :olp (org-node-cache--pos->olp outline-data pos)
                     :properties props
                     :id id
                     :exclude (equal "t" (cdr (assoc "ROAM_EXCLUDE" props)))
                     :aliases (split-string-and-unquote
                               (or (cdr (assoc "ROAM_ALIASES" props)) ""))
                     :roam-refs (when-let ((refs (cdr (assoc "ROAM_REFS" props))))
                                  (split-string-and-unquote refs))
                     :backlink-origins (org-node-cache--backlinks->list
                                        (cdr (assoc "CACHED_BACKLINKS" props))))))
               (when single-file-being-scanned
                 (when id (org-id-add-location id file)))
               ;; Record subtree even if it has no ID
               (puthash (or id (format-time-string "%N")) node org-nodes)
               (org-node-cache--populate-other-tables node)))))))))

(defun org-node-cache--populate-other-tables (node)
  ;; Add to  `org-node--refs-table' and `org-node-collection'
  (when (or (not org-node-only-show-subtrees-with-id)
            (plist-get node :id))
    (dolist (ref (plist-get node :roam-refs))
      (puthash ref (plist-get node :id) org-node--refs-table))
    (if (funcall org-node-filter-fn node)
        (progn
          (dolist (title (cons (plist-get node :title)
                               (plist-get node :aliases)))
            (puthash (funcall org-node-format-candidate-fn
                              node title)
                     node
                     org-node-collection))
          ;; Let refs work as aliases
          (dolist (ref (plist-get node :roam-refs))
            (puthash ref node org-node-collection)))
      (plist-put node :is-excluded t))))

;; (cl-loop for node in (hash-table-values org-nodes)
;;          for todo = (plist-get node :todo)
;;          when todo do (message todo))

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
represented as the first element.

OLDATA must contain an element containing POS."
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
