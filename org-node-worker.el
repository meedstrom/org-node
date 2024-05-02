;;; org-node-worker.el --- Gotta go fast -*- lexical-binding: t; -*-

(eval-when-compile
  ;; (require 'subr-x)
  (require 'cl-macs)
  (require 'compat))

;; copy pasta
(defun org-node--make-todo-regexp (todo-string)
  "Make a regexp based on global value of `org-todo-keywords',
that will match any of the keywords."
  (declare (pure t) (side-effect-free t))
  (save-match-data
    (thread-last todo-string
                 (replace-regexp-in-string "(.*?)" "")
                 (replace-regexp-in-string "[^ [:alpha:]]" "")
                 (string-trim)
                 (string-split)
                 (regexp-opt))))

(defun org-node-worker--org-link-display-format (s)
  "Like `org-link-display-format'."
  (save-match-data
    (replace-regexp-in-string
     ;; Pasted from `org-link-bracket-re'
     "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
     (lambda (m) (or (match-string 2 m) (match-string 1 m)))
     s nil t)))

(defun org-node-worker--next-heading ()
  "Like `outline-next-heading'."
  ;; Prevent matching the same line forever
  (when (looking-at-p "^\\*+ ")
    (forward-char))
  (if (re-search-forward "^\\*+ " nil t)
      (goto-char (pos-bol))
    (goto-char (point-max))
    nil))

(defun org-node-worker--collect-properties (beg end)
  (let (res)
    (goto-char beg)
    (with-restriction beg end
      (while (not (eobp))
        (search-forward ":")
        (push (cons (upcase
                     (buffer-substring-no-properties
                      (point) (1- (search-forward ":"))))
                    (string-trim
                     (buffer-substring-no-properties
                      (point) (pos-eol))))
              res)
        (forward-line 1)))
    res))

(defun org-node-worker--elem-index (elem list)
  "Like `-elem-index'."
  (declare (pure t) (side-effect-free t))
  (let ((list list)
        (i 0))
    (while (and list (not (equal elem (car-safe list))))
      (setq i (1+ i)
            list (cdr list)))
    (unless (and (= 0 i) (null list))
      i)))

(defun org-node-worker--pos->parent-id (oldata pos file-id)
  "Return ID of the closest ancestor heading that has an ID.
See `org-node-worker--pos->olp' for explanation of arguments.

Extra argument FILE-ID is the file-level id, used as a fallback
if no ancestor heading has an ID.  It can be nil."
  (declare (pure t) (side-effect-free t))
  (let (;; Drop all the data about positions below POS
        (reversed (nthcdr (org-node-worker--elem-index (assoc pos oldata)
                                                       oldata)
                          oldata)))
    (let ((curr-level (caddr (car reversed))))
      ;; Work backwards towards the top of the file
      ;; Sorry about the `cl-loop' brain teaser.
      (cl-loop for cell in reversed
               as id = (nth 3 cell)
               if (> curr-level (caddr cell))
               do (setq curr-level (caddr cell))
               and if id return id

               if (= 1 curr-level) return file-id))))

(defun org-node-worker--pos->olp (oldata pos)
  "Given buffer position POS, return the Org outline path.
Result should be like that from `org-get-outline-path'.

Argument OLDATA must be of the form
 ((373 \"A subheading\" 2)
  (250 \"A top heading\" 1)
  (199 \"Another top heading\" 1)
  (123 \"Poorly placed third-level heading\" 3))

where the car of each element represents a buffer position, the cadr the
heading title, and the caddr the outline depth i.e. the number of
asterisks in the heading at that location.

OLDATA must be in \"reverse\" order, such the last heading in the
file is represented as the first element.  It must also contain
an element corresponding to POS exactly."
  (declare (pure t) (side-effect-free t))
  (let (olp
        ;; Drop all the data about positions below POS
        (reversed (nthcdr (org-node-worker--elem-index (assoc pos oldata)
                                                       oldata)
                          oldata)))
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

(defvar org-node-worker--queued-writes nil)

(defun org-node-worker--collect-links-until (end id-here olp-with-self)
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
  (while (re-search-forward
          ;; Pasted from `org-link-plain-re'
          "\\(?:\\<\\(?:\\(attachment\\|el\\(?:feed\\|isp\\)\\|f\\(?:ile\\(?:\\+\\(?:\\(?:emac\\|sy\\)s\\)\\)?\\|tp\\)\\|h\\(?:elp\\|ttps?\\)\\|id\\|mailto\\|news\\|roam\\|shell\\)\\):\\(\\(?:[^][
()<>]\\|[(<[]\\(?:[^][
()<>]\\|[(<[][^][
()<>]*[])>]\\)*[])>]\\)+\\(?:[^[:punct:]
]\\|/\\|[(<[]\\(?:[^][
()<>]\\|[(<[][^][
()<>]*[])>]\\)*[])>]\\)\\)\\)" end t)
    (let ((type (match-string 1))
          (path (match-string 2)))
      (if (save-excursion
            (goto-char (pos-bol))
            (or (looking-at-p "[[:space:]]*# ")
                (looking-at-p "[[:space:]]*#\\+")))
          ;; On a # comment or #+keyword, skip whole line
          (goto-char (pos-eol))
        (push
         `(org-node-async--add-link-to-tables
           ,(list :src id-here
                  :pos (point)
                  :type type
                  ;; Because org-roam asks for it
                  :properties (list :outline olp-with-self))
           ,path ,type)
         org-node-worker--queued-writes)))))

;; TODO Let it run in a single Emacs
;; TODO Get rid of the uglier perf attempts that didn't help after all (and
;;      profile)
;; TODO Consider what to do if org-id-locations stored the same file under
;;      different names
(defun org-node-worker-function ()
  "Scan FILES for id-nodes, adding them to `org-nodes'.
Also scan for links."
  (with-temp-buffer
    (insert-file-contents (file-name-concat (temporary-file-directory)
                                            "org-node-worker-variables.eld"))
    (dolist (variable (car (read-from-string (buffer-string))))
      (set (car variable) (cdr variable)))
    (let ((case-fold-search t)
          (i (pop files))
          (format-alist nil)
          ;; Keep only the EasyPG handler, not the TRAMP handlers nor the
          ;; compressed-archive handler
          (file-name-handler-alist
           (if $bungle-file-name-handler
               (let ((epa (rassoc 'epa-file-handler file-name-handler-alist)))
                 (if epa
                     (list epa)
                   ()))
             file-name-handler-alist)))
      (dolist (file files)
        (if (not (file-exists-p file))
            ;; TODO: Move this explanation somewhere else
            ;;
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
            ;; exclude, like versioned backup org-node-worker--files, so we shouldn't do that.
            ;; Starting to think we need an org-id2.el.
            (push `(org-node-async--forget-id-location ,forget)
                  org-node-worker--queued-writes)
          (erase-buffer)
          ;; NOTE: Used `insert-file-contents-literally' in the past, converting
          ;; each captured substring afterwards with `decode-coding-string', but
          ;; it still made us record the wrong value for :pos when there was any
          ;; Unicode in the file.  So instead, the let-bindings above reproduce
          ;; much of what it did.
          (insert-file-contents file)
          (let (;; Good-enough substitute for `org-end-of-meta-data'
                (FAR (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
                (TODO-RE $global-todo-re)
                PROPS FILE-TITLE FILE-TAGS FILE-ID OUTLINE-DATA
                POS LEVEL HERE TITLE LINE+2)
            (goto-char 1)
            (when (re-search-forward "^ *:properties:" FAR t)
              (forward-line 1)
              (setq PROPS (org-node-worker--collect-properties
                           (point) (and (re-search-forward "^ *:end:")
                                        (pos-bol))))
              (goto-char 1))
            (when (re-search-forward "^#\\+filetags: " FAR t)
              (setq FILE-TAGS
                    (split-string
                     (buffer-substring-no-properties (point) (pos-eol))
                     ":" t))
              (goto-char 1))
            (when (re-search-forward $file-todo-option-re FAR t)
              (setq TODO-RE
                    (org-node--make-todo-regexp
                     (buffer-substring-no-properties (point) (pos-eol))))
              (goto-char 1))
            (if (re-search-forward "^#\\+title: " FAR t)
                (setq FILE-TITLE
                      (org-node-worker--org-link-display-format
                       (buffer-substring-no-properties (point) (pos-eol))))
              ;; File nodes dont strictly need #+title, fall back on filename
              (setq FILE-TITLE (file-name-nondirectory file)))
            (setq FILE-ID (cdr (assoc "ID" PROPS)))
            (when $not-a-full-reset
              (when FILE-ID
                (push `(org-id-add-location ,FILE-ID ,file)
                      org-node-worker--queued-writes)))
            ;; Collect links
            (let ((END (save-excursion
                         (org-node-worker--next-heading)
                         (1- (point)))))
              (when FILE-ID
                ;; Don't count org-super-links backlinks as forward links
                (when (re-search-forward $backlink-drawer-re END t)
                  (search-forward ":end:"))
                (org-node-worker--collect-links-until END FILE-ID nil)))
            (push `(org-node-async--add-node-to-tables
                    ,(list :title FILE-TITLE
                           :level 0
                           :tags FILE-TAGS
                           :file-path file
                           :pos 1
                           :file-title FILE-TITLE
                           :properties PROPS
                           :id FILE-ID
                           :aliases
                           (split-string-and-unquote
                            (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                           :refs
                           (split-string-and-unquote
                            (or (cdr (assoc "ROAM_REFS" PROPS)) ""))))
                  org-node-worker--queued-writes)
            ;; Loop over the file's subtrees
            (while (org-node-worker--next-heading)
              ;; These bindings must be reinitialized to nil on each subtree,
              ;; because a nil value is meaningful and we may not set them to
              ;; anything non-nil.
              (let (TODO-STATE TAGS SCHED DEADLINE PROPS ID OLP)
                (skip-chars-forward " ")
                (setq POS (point))
                (setq LEVEL (skip-chars-forward "*"))
                (setq HERE (point))
                (when (looking-at TODO-RE)
                  (setq TODO-STATE (buffer-substring-no-properties
                                    (point) (match-end 0)))
                  (goto-char (1+ (match-end 0)))
                  (setq HERE (point)))
                (if (re-search-forward " +\\(:.+:\\) *$" (pos-eol) t)
                    (progn
                      (setq TITLE
                            (org-node-worker--org-link-display-format
                             (buffer-substring-no-properties
                              HERE (match-beginning 0))))
                      (setq TAGS (split-string (match-string 1) ":" t)))
                  (setq TITLE
                        (org-node-worker--org-link-display-format
                         (buffer-substring-no-properties HERE (pos-eol)))))
                (setq HERE (point))
                (setq LINE+2 (and (forward-line 2) (point)))
                (goto-char HERE)
                (when (re-search-forward "[\n\s]SCHEDULED: " LINE+2 t)
                  (setq SCHED
                        (buffer-substring-no-properties
                         ;; \n just there for safety
                         (point) (+ 1 (point) (skip-chars-forward "^]>\n"))))
                  (goto-char HERE))
                (when (re-search-forward "[\n\s]DEADLINE: " LINE+2 t)
                  (setq DEADLINE
                        (buffer-substring-no-properties
                         (point) (+ 1 (point) (skip-chars-forward "^]>\n")))))
                (setq HERE (point))
                (setq LINE+2 (and (forward-line 2) (point)))
                (goto-char HERE)
                (when (re-search-forward "^ *:properties:" LINE+2 t)
                  (forward-line 1)
                  (setq PROPS (org-node-worker--collect-properties
                               (point) (progn
                                         (re-search-forward "^ *:end:")
                                         (pos-bol)))))
                (setq ID (cdr (assoc "ID" PROPS)))
                (when $not-a-full-reset
                  (when ID
                    (push `(org-id-add-location ,ID ,file)
                          org-node-worker--queued-writes)))
                (push (list POS TITLE LEVEL ID) OUTLINE-DATA)
                (setq OLP (org-node-worker--pos->olp OUTLINE-DATA POS))
                ;; Now collect links!
                (let ((ID-HERE (or ID (org-node-worker--pos->parent-id
                                       OUTLINE-DATA POS FILE-ID)))
                      (END (save-excursion
                             (org-node-worker--next-heading)
                             (1- (point))))
                      (OLP-WITH-SELF (append OLP (list TITLE))))
                  (when ID-HERE
                    ;; Don't count org-super-links backlinks
                    (when (re-search-forward $backlink-drawer-re END t)
                      (search-forward ":end:"))
                    (org-node-worker--collect-links-until
                     END ID-HERE OLP-WITH-SELF)
                    ;; Gotcha... also collect links inside the heading
                    (goto-char POS)
                    (org-node-worker--collect-links-until
                     (pos-eol) ID-HERE OLP-WITH-SELF)))
                (push `(org-node-async--add-node-to-tables
                        ,(list :title TITLE
                               :is-subtree t
                               :level LEVEL
                               :id ID
                               :pos POS
                               :tags TAGS
                               :todo TODO-STATE
                               :file-path file
                               :scheduled SCHED
                               :deadline DEADLINE
                               :file-title FILE-TITLE
                               :olp OLP
                               :properties PROPS
                               :aliases
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                               :refs
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_REFS" PROPS)) ""))))
                      org-node-worker--queued-writes))))))
      (with-temp-file (format "/tmp/org-node-result-%d.eld" i)
        (insert (prin1-to-string org-node-worker--queued-writes))))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
