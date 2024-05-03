;;; org-node-worker.el --- Gotta go fast -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs))

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
        (data-until-pos (nthcdr (org-node-worker--elem-index (assoc pos oldata)
                                                             oldata)
                                oldata)))
    (let ((previous-level (nth 2 (car data-until-pos))))
      ;; Work backwards towards the top of the file
      ;; New version without `cl-loop' (untested):
      ;; (catch 'id
      ;;   (while (let* ((row (pop (data-until-pos)))
      ;;                 (curr-level (nth 2 row))
      ;;                 (id (nth 3 row)))
      ;;            (when (> previous-level curr-level)
      ;;              (setq previous-level curr-level)
      ;;              (if id (throw 'id id)))
      ;;            (if (= 1 previous-level) (throw 'id file-id)))))
      (cl-loop for row in data-until-pos
               as id = (nth 3 row)
               as curr-level = (nth 2 row)
               if (> previous-level curr-level)
               do (setq previous-level curr-level)
               and if id return id
               ;; Even the top-level heading had no id
               if (= 1 previous-level) return file-id))))

(defun org-node-worker--pos->olp (oldata pos)
  "Given buffer position POS, return the Org outline path.
Result should be like that from `org-get-outline-path'.

Argument OLDATA must be of a form looking like
 ((373 \"A subheading\" 2)
  (250 \"A top heading\" 1)
  (199 \"Another top heading\" 1)
  (123 \"First heading in the file is apparently third-level\" 3))

where the car of each element represents a buffer position, the cadr the
heading title, and the caddr the outline depth i.e. the number of
asterisks in the heading at that location.

OLDATA must be in \"reverse\" order, such the last heading in the
file is represented as the first element.  POS itself must be
included in one of the elements."
  (declare (pure t) (side-effect-free t))
  (let (olp
        ;; Drop all the data about positions below POS (using `nthcdr' because
        ;; oldata is in reverse order)
        (data-until-pos (nthcdr (org-node-worker--elem-index (assoc pos oldata)
                                                             oldata)
                                oldata)))
    (let ((previous-level (caddr (car data-until-pos))))
      ;; Work backwards towards the top of the file
      (cl-loop for row in data-until-pos
               when (> previous-level (caddr row))
               do (setq previous-level (caddr row))
               (push (cadr row) olp)
               and if (= 1 previous-level)
               ;; Stop
               return nil))
    olp))

(defun org-node-worker--make-todo-regexp (todo-string)
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
  "Copy-pasted from `org-link-display-format'."
  (save-match-data
    (replace-regexp-in-string
     ;; Pasted from `org-link-bracket-re'
     "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
     (lambda (m) (or (match-string 2 m) (match-string 1 m)))
     s nil t)))

(defun org-node-worker--next-heading ()
  "Like `org-node-worker--next-heading'."
  ;; Prevent matching the same line forever
  (if (and (bolp) (not (eobp)))
      (forward-char))
  (if (re-search-forward "^\\*+ " nil 'move)
      (goto-char (pos-bol))))

(defvar org-node-worker--demands nil
  "Alist of functions and arguments to execute.

With `org-node-perf-multicore' non-nil, each subprocess builds
its own instance of this variable and then writes it to a file
for reading by the mother Emacs process.")

(defun org-node-worker--collect-links-until (end id-here olp-with-self)
  "From here to buffer position END, look for forward-links.
Use these links to populate tables `org-node--links-table' and
`org-node--reflinks-table'.

Argument ID-HERE is the ID of the subtree where this function
will presumably be executed (or that of an ancestor subtree, if
the current subtree has none).

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own.

Argument OLP-WITH-SELF is the outline path to the current
subtree, with its own heading tacked onto the end.  This is data
that org-roam expects to have."
  (while (re-search-forward
          ;; Pasted from `org-link-plain-re'
          "\\(?:\\<\\(?:\\(attachment\\|el\\(?:feed\\|isp\\)\\|f\\(?:ile\\(?:\\+\\(?:\\(?:emac\\|sy\\)s\\)\\)?\\|tp\\)\\|h\\(?:elp\\|ttps?\\)\\|id\\|mailto\\|news\\|roam\\|shell\\)\\):\\(\\(?:[^][
()<>]\\|[(<[]\\(?:[^][
()<>]\\|[(<[][^][
()<>]*[])>]\\)*[])>]\\)+\\(?:[^[:punct:]
]\\|/\\|[(<[]\\(?:[^][
()<>]\\|[(<[][^][
()<>]*[])>]\\)*[])>]\\)\\)\\)"
          end t)
    (let ((type (match-string 1))
          (path (match-string 2)))
      (if (save-excursion
            (goto-char (pos-bol))
            (or (looking-at-p "[[:space:]]*# ")
                (looking-at-p "[[:space:]]*#\\+")))
          ;; On a # comment or #+keyword, skip whole line
          (goto-char (pos-eol))
        (push `(org-node--add-link-to-tables
                ,(list :src id-here
                       :pos (point)
                       :type type
                       ;; Because org-roam asks for it
                       :properties (list :outline olp-with-self))
                ,path
                ,type)
              org-node-worker--demands)))))

(defun org-node-worker--collect-properties (beg end)
  "Assuming BEG and END mark the region in between a
:PROPERTIES:...:END: drawer, collect the properties into an
alist."
  (let (res)
    (goto-char beg)
    (with-restriction beg end
      (while (not (eobp))
        (search-forward ":")
        (push (cons (upcase
                     (buffer-substring
                      (point) (1- (search-forward ":"))))
                    (string-trim
                     (buffer-substring
                      (point) (pos-eol))))
              res)
        (forward-line 1)))
    res))

;; TODO Consider what to do if org-id-locations stored the same file under
;;      different names
(defun org-node-worker--collect (&optional synchronous variables)
  "Scan for id-nodes across all files, adding them to `org-nodes'.

The argument SYNCHRONOUS, if provided, should be a list of files
to scan.  If not provided, assume we are in a child emacs spawned
by `org-node-async--collect' and do what it expects."
  (with-temp-buffer
    (setq vars (if synchronous
                   variables
                 (insert-file-contents "/tmp/org-node-worker-variables.eld")
                 (car (read-from-string (buffer-string)))))
    (dolist (var vars)
      (set (car var) (cdr var)))
    (if synchronous
        (setq files synchronous)
      ;; When async, `files' is already set by
      ;; `org-node-async--collect'... with an extra morsel of data sent along
      (setq i (pop files)))
    (let ((case-fold-search t)
          ;; Perf
          (format-alist nil) ;; REVIEW: profile
          (file-name-handler-alist
           (delq nil (mapcar (lambda (handler)
                               (rassoc handler file-name-handler-alist))
                             $keep-file-name-handlers)))
          (gc-cons-threshold $gc-cons-threshold)
          (coding-system-for-read $assume-coding-system)
          ;; Always assigned on every iteration, so may as well reuse the
          ;; memory locations (hopefully producing less garbage)
          TITLE FILE-TITLE POS LEVEL HERE LINE+2)
      (dolist (FILE files)
        (if (not (file-exists-p FILE))
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
            (push `(org-node--forget-id-location ,forget)
                  org-node-worker--demands)
          (erase-buffer)
          ;; NOTE: Used `insert-file-contents-literally' in the past,
          ;; converting each captured substring afterwards with
          ;; `decode-coding-string', but it still made us record the wrong
          ;; value for :pos when there was any Unicode in the file.  So
          ;; instead, the let-bindings above reproduce much of what it did.
          (insert-file-contents FILE)
          (let (;; Roughly like `org-end-of-meta-data' for file level
                (FAR (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
                (TODO-RE $global-todo-re)
                PROPS FILE-TAGS FILE-ID)
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
                     (buffer-substring (point) (pos-eol))
                     ":" t))
              (goto-char 1))
            (when (re-search-forward $file-todo-option-re FAR t)
              (setq TODO-RE
                    (org-node-worker--make-todo-regexp
                     (buffer-substring (point) (pos-eol))))
              (goto-char 1))
            (if (re-search-forward "^#\\+title: " FAR t)
                (setq FILE-TITLE
                      (org-node-worker--org-link-display-format
                       (buffer-substring (point) (pos-eol))))
              ;; File nodes dont strictly need #+title, fall back on filename
              (setq FILE-TITLE (file-name-nondirectory FILE)))
            (setq FILE-ID (cdr (assoc "ID" PROPS)))
            (when $not-a-full-reset
              ;; This was probably called by a rename-file advice, so...
              ;; REVIEW: Maybe rename the boolean to $explicit?
              (when FILE-ID
                (push `(org-id-add-location ,FILE-ID ,FILE)
                      org-node-worker--demands)))
            ;; Collect links
            (let ((END (save-excursion
                         (org-node-worker--next-heading)
                         (1- (point)))))
              (when FILE-ID
                ;; Don't count org-super-links backlinks as forward links
                (when (re-search-forward $backlink-drawer-re END t)
                  (search-forward ":end:"))
                (org-node-worker--collect-links-until END FILE-ID nil)))
            (push `(org-node--add-node-to-tables
                    ,(list :title FILE-TITLE
                           :level 0
                           :tags FILE-TAGS
                           :file-path FILE
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
                  org-node-worker--demands)
            ;; Loop over the file's subtrees
            (while (org-node-worker--next-heading)
              ;; These bindings must be reinitialized to nil on each subtree,
              ;; because a nil value is also meaningful
              (let (TODO-STATE TAGS SCHED DEADLINE PROPS ID OLP OUTLINE-DATA)
                (skip-chars-forward " ")
                (setq POS (point))
                (setq LEVEL (skip-chars-forward "*"))
                (setq HERE (point))
                (when (looking-at TODO-RE)
                  (setq TODO-STATE (buffer-substring
                                    (point) (match-end 0)))
                  (goto-char (1+ (match-end 0)))
                  (setq HERE (point)))
                (if (re-search-forward " +\\(:.+:\\) *$" (pos-eol) t)
                    (progn
                      (setq TITLE
                            (org-node-worker--org-link-display-format
                             (buffer-substring
                              HERE (match-beginning 0))))
                      (setq TAGS (split-string (match-string 1) ":" t)))
                  (setq TITLE
                        (org-node-worker--org-link-display-format
                         (buffer-substring HERE (pos-eol)))))
                (setq HERE (point))
                (setq LINE+2 (and (forward-line 2) (point)))
                (goto-char HERE)
                (when (re-search-forward "[\n\s]SCHEDULED: " LINE+2 t)
                  (setq SCHED
                        (buffer-substring
                         ;; \n just there for safety
                         (point) (+ 1 (point) (skip-chars-forward "^]>\n"))))
                  (goto-char HERE))
                (when (re-search-forward "[\n\s]DEADLINE: " LINE+2 t)
                  (setq DEADLINE
                        (buffer-substring
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
                  ;; Called by a rename-file advice
                  (when ID
                    (push `(org-id-add-location ,ID ,FILE)
                          org-node-worker--demands)))
                (push (list POS TITLE LEVEL ID) OUTLINE-DATA)
                (setq OLP (org-node-worker--pos->olp OUTLINE-DATA POS))
                ;; Now collect links while we're here!
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
                    ;; Gotcha... also collect links inside the heading, not
                    ;; just the body text
                    (goto-char POS)
                    (org-node-worker--collect-links-until
                     (pos-eol) ID-HERE OLP-WITH-SELF)))
                (push `(org-node--add-node-to-tables
                        ,(list :title TITLE
                               :is-subtree t
                               :level LEVEL
                               :id ID
                               :pos POS
                               :tags TAGS
                               :todo TODO-STATE
                               :file-path FILE
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
                      org-node-worker--demands))))))
      (if synchronous
          (let ((please-update-id-locations nil))
            (while-let (demand (pop org-node-worker--demands))
              (apply (car demand) (cdr demand))
              (when (eq 'org-id-add-location (car demand))
                (setq please-update-id-locations t)))
            (when please-update-id-locations
              (org-id-update-id-locations)
              (org-id-locations-save)
              ;; in case
              (when (listp org-id-locations)
                (setq org-id-locations (org-id-alist-to-hash org-id-locations)))))
        ;; Write down the demands so `org-node-async--handle-finished-job' will
        ;; do the equivalent of above in the main Emacs process
        (with-temp-file (format "/tmp/org-node-result-%d.eld" i)
          (insert (prin1-to-string org-node-worker--demands)))))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
