;;; org-node-cache.el --- The beating heart -*- lexical-binding: t; -*-

(require 'org-node-lib)

;;;###autoload
(define-minor-mode org-node-cache-mode
  "Instruct on-save hooks and such things to update the cache.
While the mode is active, commands such as `org-node-find' and
`org-node-insert-link' do not need to update the cache every
time."
  :global t
  :group 'org-node
  (remove-hook 'org-mode-hook #'org-node-cache-mode)
  ;; Take this opportunity to check for deprecated usage
  (when (or (string-search "plist-get" (prin1-to-string org-node-filter-fn))
            (string-search "plist-get" (prin1-to-string org-node-format-candidate-fn)))
    (display-warning 'org-node (string-fill "\n2024-04-30 breaking change: node metadata comes in objects now, not plists!  This change was made because plist-get fails silently, which makes debugging more difficult.  See updated examples for org-node-filter-fn etc." 79)))
  (if org-node-cache-mode
      (progn
        (add-hook 'after-save-hook #'org-node-cache-rescan-file)
        (advice-add #'rename-file :after #'org-node-cache-rescan-file)
        (advice-add #'rename-file :before #'org-node-cache--handle-delete)
        (advice-add #'delete-file :before #'org-node-cache--handle-delete))
    (remove-hook 'after-save-hook #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache-rescan-file)
    (advice-remove #'rename-file #'org-node-cache--handle-delete)
    (advice-remove #'delete-file #'org-node-cache--handle-delete)))

(defun org-node-cache-peek ()
  "For debugging: peek on a random member of `org-nodes'."
  (interactive)
  (require 'map)
  (require 'seq)
  (let ((fields (--map (intern (concat "org-node-" (symbol-name it)))
                       (map-keys (cdr (cl-struct-slot-info 'org-node)))))
        (random-node (seq-random-elt
                      (-filter #'org-node-id (hash-table-values org-nodes)))))
    (message "%s"
             (--zip-with
              (format "(%s X) => %s\n" it other)
              fields
              (cl-loop for field in fields
                       collect (funcall field random-node))))))

(defvar org-node-cache-reset-hook nil)
(defvar org-node-cache-rescan-file-hook nil)

(defun org-node-cache-reset ()
  "Wipe and rebuild the cache.
For an user-facing command, see \\[org-node-reset]."
  (clrhash org-nodes)
  (clrhash org-node-collection)
  (clrhash org-node--refs-table)
  (clrhash org-node--reflinks-table)
  (clrhash org-node--links-table)
  (org-node--init-org-id-locations-or-die)
  (org-node-cache--collect (-uniq (hash-table-values org-id-locations)))
  (run-hooks 'org-node-cache-reset-hook))

(defun org-node-cache-rescan-file (&optional _arg1 arg2 &rest _)
  "Seek nodes and links in a single file."
  ;; If triggered as advice on `rename-file', the second argument is the new
  ;; name.  Do not assume it is being done to the current buffer; it may be
  ;; called from a Dired buffer, for example.
  (let ((file (if (and arg2 (stringp arg2) (file-exists-p arg2))
                  arg2
                (buffer-file-name))))
    (org-node-cache--collect (list file))
    (when (boundp 'org-node-cache-scan-file-hook)
      (lwarn 'org-node :warning "Hook renamed: org-node-cache-scan-file-hook to org-node-cache-rescan-file-hook"))
    (run-hooks 'org-node-cache-rescan-file-hook)))

(define-obsolete-function-alias 'org-node-cache-scan-file 'org-node-cache-rescan-file "2024-05-01")

(defun org-node-cache-ensure-fresh ()
  (org-node--init-org-id-locations-or-die)
  ;; Once-per-session tip
  (when (and (hash-table-empty-p org-node-collection)
             (not (member 'org-node-cache-mode org-mode-hook)))
    (message "To speed up this command, turn on `org-node-cache-mode'"))
  (when (or (not org-node-cache-mode)
            (hash-table-empty-p org-node-collection))
    (org-node-cache-reset)))

(let ((timer (timer-create)))
  (defun org-node-cache--handle-delete (&optional arg1 &rest _)
    "Update org-id and org-node after an Org file is deleted.

First remove any references to the file in `org-id-locations'.

Then schedule a rebuild of org-node caches after a few idle
seconds.  The delay avoids bothering the user who may be trying
to delete several files in a row."
    (let ((file-being-deleted (if (and arg1 (file-exists-p arg1))
                                  arg1
                                (buffer-file-name))))
      (when (member (file-name-extension file-being-deleted)
                    '("org" "org_archive" "gpg"))
        (org-node-cache--forget-id-location file-being-deleted)
        (cancel-timer timer)
        (setq timer (run-with-idle-timer 6 nil #'org-node-cache-reset))))))

(defun org-node-cache--forget-id-location (file)
  (org-node--init-org-id-locations-or-die)
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

(defun org-node-cache--collect-properties (beg end)
  (let (res)
    (goto-char beg)
    (with-restriction beg end
      (while (not (eobp))
        (search-forward ":")
        (push (cons (upcase
                     (buffer-substring
                      (point) (1- (search-forward ":")))
                     )
                    (string-trim
                     (buffer-substring
                      (point) (line-end-position))
                     ))
              res)
        (forward-line 1)))
    res))

(defun org-node-cache--collect (files)
  "Scan FILES for id-nodes, adding them to `org-nodes'.
Also scan for links."
  (with-temp-buffer
    (let ((outline-regexp org-outline-regexp)
          (backlinks-drawer-re
           (concat "^[[:space:]]*:"
                   (or (and (boundp 'org-super-links-backlink-into-drawer)
                            (stringp org-super-links-backlink-into-drawer)
                            (downcase org-super-links-backlink-into-drawer))
                       "backlinks")
                   ":"))
          (todo-re (org-node-cache--make-todo-regexp))
          (not-a-full-reset (not (hash-table-empty-p org-nodes)))
          (case-fold-search t)
          (gc-cons-threshold (* 1000 1000 1000))
          (please-update-id nil))
      (dolist (file files)
        (if (not (file-exists-p file))
            ;; Example situation: user renamed/deleted a file using shell
            ;; commands, outside Emacs.  Now org-id references a file that
            ;; doesn't exist.  Our solution: just skip.  If the file was
            ;; renamed to a new location, it'll have to be picked up by org-id
            ;; in its usual ways.
            ;;
            ;; This is a good time to do `org-id-update-id-locations' just
            ;; because, but it won't pick up the new file path.  A heuristic we
            ;; could've used:
            ;;
            ;; (org-id-update-id-locations
            ;;  (--mapcat
            ;;   (directory-files-recursively it "\\.org$")
            ;;   (org-node--root-dirs (hash-table-values org-id-locations))))
            ;;
            ;; but it doesn't take into account what the user may want to
            ;; exclude, like versioned backup files, so we shouldn't do that.
            ;; Starting to think we need an org-id2.el.
            (progn
              (org-node-cache--forget-id-location file)
              (setq please-update-id t))
          (erase-buffer)
          (insert-file-contents file)
          (let (;; Position of first "content": a line not starting with # or :
                (far (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
                props file-title file-tags file-id outline-data)
            (goto-char 1)
            (when (re-search-forward "^ *:properties:" far t)
              (forward-line 1)
              (setq props (org-node-cache--collect-properties
                           (point) (and (re-search-forward "^ *:end:")
                                        (line-beginning-position)))))
            (goto-char 1)
            (when (re-search-forward "^#\\+filetags: " far t)
              (setq file-tags (split-string

                               (buffer-substring (point) (line-end-position))

                               ":" t)))
            (goto-char 1)
            (if (re-search-forward "^#\\+title: " far t)
                (setq file-title
                      (org-link-display-format

                       (buffer-substring (point) (line-end-position))
                       ))
              ;; File nodes dont strictly need #+title, fall back on filename
              (setq file-title (file-name-nondirectory file)))
            (setq file-id (cdr (assoc "ID" props)))
            (when not-a-full-reset
              (when file-id (org-id-add-location file-id file)))
            ;; Collect links
            (let ((end (save-excursion
                         (outline-next-heading)
                         (1- (point)))))
              (when file-id
                ;; Don't count org-super-links backlinks as forward links
                (when (re-search-forward backlinks-drawer-re end t)
                  (search-forward ":end:"))
                (org-node-cache--collect-links-until end file-id nil)))
            (org-node-cache--add-node-to-tables
             (make-org-node
              :title file-title
              :level 0
              :tags file-tags
              :file-path file
              :pos 1
              :file-title file-title
              :properties props
              :id file-id
              :aliases (split-string-and-unquote
                        (or (cdr (assoc "ROAM_ALIASES" props)) ""))
              :refs (split-string-and-unquote
                     (or (cdr (assoc "ROAM_REFS" props)) ""))))
            ;; Loop over the file's subtrees
            (while (outline-next-heading)
              (let ((pos (point))
                    (level (skip-chars-forward "*"))
                    here todo title line+2 tags todo sched deadline props id olp)
                (skip-chars-forward " ")
                (setq here (point))
                ;; TODO Allow unicode todo keywords
                (when (looking-at todo-re)
                  (setq todo (buffer-substring (point) (match-end 0)))
                  (goto-char (1+ (match-end 0)))
                  (setq here (point)))
                (if (re-search-forward " +\\(:.+:\\) *$" (line-end-position) t)
                    (progn
                      (setq title (org-link-display-format

                                   (buffer-substring
                                    here (match-beginning 0))
                                   ))
                      (setq tags (string-split
                                  (match-string 1)

                                  ":" t)))
                  (setq title (org-link-display-format
                               (buffer-substring
                                here (line-end-position))
                               )))
                (setq here (point))
                (setq line+2 (and (forward-line 2) (point)))
                (goto-char here)
                (when (re-search-forward "[\n\s]SCHEDULED: " line+2 t)
                  (setq sched

                        (buffer-substring
                         ;; \n just there for safety
                         (point) (+ 1 (point) (skip-chars-forward "^]>\n")))
                        )
                  (goto-char here))
                (when (re-search-forward "[\n\s]DEADLINE: " line+2 t)
                  (setq deadline

                        (buffer-substring
                         (point) (+ 1 (point) (skip-chars-forward "^]>\n")))
                        ))
                (setq here (point))
                (setq line+2 (and (forward-line 2) (point)))
                (goto-char here)
                (when (re-search-forward "^ *:properties:" line+2 t)
                  (forward-line 1)
                  (setq props (org-node-cache--collect-properties
                               (point) (progn
                                         (re-search-forward "^ *:end:")
                                         (line-beginning-position)))))
                (setq id (cdr (assoc "ID" props)))
                (when not-a-full-reset
                  (when id (org-id-add-location id file)))
                (push (list pos title level id) outline-data)
                (setq olp (org-node-cache--pos->olp outline-data pos))
                ;; Now collect links!
                (let ((id-here (or id (org-node-cache--pos->parent-id
                                       outline-data pos file-id)))
                      (end (save-excursion
                             (outline-next-heading)
                             (1- (point))))
                      (olp-with-self (append olp (list title))))
                  (when id-here
                    ;; Don't count org-super-links backlinks as forward links
                    (when (re-search-forward backlinks-drawer-re end t)
                      (search-forward ":end:"))
                    (org-node-cache--collect-links-until end id-here olp-with-self)
                    ;; Gotcha... also collect links inside the heading
                    (goto-char pos)
                    ;; (forward-char 1) ;; Do not match org-outline-regexp-bol
                    (org-node-cache--collect-links-until
                     (line-end-position) id-here olp-with-self)))
                (org-node-cache--add-node-to-tables
                 (make-org-node
                  :title title
                  :is-subtree t
                  :level level
                  :id id
                  :pos pos
                  :tags tags
                  :todo todo
                  :file-path file
                  :scheduled sched
                  :deadline deadline
                  :file-title file-title
                  :olp olp
                  :properties props
                  :aliases (split-string-and-unquote
                            (or (cdr (assoc "ROAM_ALIASES" props)) ""))
                  :refs (split-string-and-unquote
                         (or (cdr (assoc "ROAM_REFS" props)) "")))))))))
      (when please-update-id
        (org-id-update-id-locations)
        (org-id-locations-save)
        ;; REVIEW Ensure this can't lead to infinite recursion
        (org-node-cache-reset)))))

(defun org-node-cache--collect-links-until (end id-here olp-with-self)
  "From here to position END, look for forward-links.
Use these links to populate tables `org-node--links-table' and
`org-node--reflinks-table'.

Argument ID-HERE is the ID of the subtree where this function
will presumably be executed (or that of an ancestor subtree).  It
is important for correctness that END is also within the
boundaries of that subtree.

Argument OLP-WITH-SELF is simply the outline path to the current
subtree, including its own heading.  This is data that org-roam
wants for some reason."
  (while (re-search-forward org-link-plain-re end t)
    (let ((type (match-string 1))
          (path (match-string 2)))
      (if (save-excursion
            (goto-char (line-beginning-position))
            (or (looking-at "[[:space:]]*# " t)
                (looking-at "[[:space:]]*#\\+" t)))
          ;; On a # comment or #+keyword, skip whole line
          (goto-char (line-end-position))
        (push (list :src id-here
                    :pos (point)
                    :type type
                    ;; Because org-roam asks for it
                    :properties (list :outline olp-with-self))
              (gethash path (if (equal type "id")
                                org-node--links-table
                              org-node--reflinks-table)))))))

;; I feel like this could be easier to read...
(defun org-node-cache--add-node-to-tables (node)
  "Add NODE to `org-nodes' and maybe `org-node-collection'."
  ;; Record the node even if it has no ID
  (puthash (or (org-node-id node) (format-time-string "%N"))
           node
           org-nodes)
  (when (or (not org-node-only-show-subtrees-with-id) (org-node-id node))
    ;; Populate `org-node--refs-table'
    (dolist (ref (org-node-refs node))
      (puthash ref (org-node-id node) org-node--refs-table))
    (when (funcall org-node-filter-fn node)
      ;; Populate `org-node-collection'
      (dolist (title (cons (org-node-title node)
                           (org-node-aliases node)))
        (puthash (funcall org-node-format-candidate-fn node title)
                 node
                 org-node-collection))
      ;; Let refs work as aliases
      (dolist (ref (org-node-refs node))
        (puthash ref node org-node-collection)))))

(defun org-node-cache--pos->parent-id (oldata pos file-id)
  "Return ID of the closest ancestor heading that has an ID.
See `org-node-cache--pos->olp' for explanation of arguments.

Extra argument FILE-ID is the file-level id, used as a fallback
if no ancestor heading has an ID.  It can be nil."
  (declare (pure t) (side-effect-free t))
  (let (;; Drop all the data about positions below POS
        (reversed (-drop (-elem-index (assoc pos oldata) oldata) oldata)))
    (let ((curr-level (caddr (car reversed))))
      ;; Work backwards towards the top of the file
      ;; Sorry about the `cl-loop' brain teaser.
      (cl-loop for cell in reversed
               as id = (nth 3 cell)
               if (> curr-level (caddr cell))
               do (setq curr-level (caddr cell))
               and if id return id

               if (= 1 curr-level) return file-id))))

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
