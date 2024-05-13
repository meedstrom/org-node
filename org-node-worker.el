;;; org-node-worker.el --- Gotta go fast -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))

(defun org-node-worker--tmpfile (&optional basename &rest args)
  "Return a path that puts BASENAME in a temporary directory.
Usually it will be in /tmp/org-node/.  Also format BASENAME with
ARGS like `format', which see."
  (expand-file-name (if basename
                        (apply #'format basename args)
                      "")
                    (expand-file-name "org-node" (temporary-file-directory))))

(defun org-node-worker--elem-index (elem list)
  "Like `-elem-index', return first index of ELEM in LIST."
  (declare (pure t) (side-effect-free t))
  (when list
    (let ((list list)
          (i 0))
      (while (and list (not (equal elem (car-safe list))))
        (setq i (1+ i)
              list (cdr list)))
      i)))

(defun org-node-worker--pos->parent-id (oldata pos file-id)
  "Return ID of the closest ancestor heading that has an ID.
See `org-node-worker--pos->olp' for explanation of OLDATA and POS.

Extra argument FILE-ID is the file-level id, used as a fallback
if no ancestor heading has an ID.  It can be nil."
  (declare (pure t) (side-effect-free t))
  (let (;; Drop all the data about positions below POS
        (data-until-pos (nthcdr (org-node-worker--elem-index (assoc pos oldata)
                                                             oldata)
                                oldata)))
    (let ((previous-level (nth 2 (car data-until-pos))))
      ;; Work backwards towards the top of the file
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
      ;; NOTE: Tried catch-throw and dolist, but `cl-loop' wins perf
      (cl-loop for row in data-until-pos
               when (> previous-level (caddr row))
               do (setq previous-level (caddr row))
               (push (cadr row) olp)
               and if (= 1 previous-level)
               ;; Stop
               return nil))
    olp))

(defun org-node-worker--make-todo-regexp (todo-string)
  "Make a regexp based on TODO-STRING,
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
  "Like `outline-next-heading'."
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

(defun org-node-worker--collect-links-until (end id-here olp-with-self link-re)
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
          ;; NOTE: There was a hair-pulling bug here because I pasted the
          ;; evalled value of `org-link-plain-re', but whitespace cleaners
          ;; subtly changed it upon save!  So now we just pass in the variable.
          ;; And a lesson: set your editor to always highlight trailing spaces,
          ;; at least in the regions you have modified (PR ws-butler?)
          link-re end t)
    (let ((link-type (match-string 1))
          (path (match-string 2)))
      (if (save-excursion
            (goto-char (pos-bol))
            (or (looking-at-p "[[:space:]]*# ")
                (looking-at-p "[[:space:]]*#\\+")))
          ;; On a # comment or #+keyword, skip whole line
          (goto-char (pos-eol))
        (push `(org-node-cache--add-link-to-tables
                ,(list :src id-here
                       :pos (point)
                       :type link-type
                       ;; Because org-roam asks for it
                       :properties (list :outline olp-with-self))
                ,path
                ,link-type)
              org-node-worker--demands)))))

(defun org-node-worker--collect-properties (beg end file)
  "Assuming BEG and END delimit the region in between
:PROPERTIES:...:END:, collect the properties into an alist."
  (let (res)
    (goto-char beg)
    (while (not (>= (point) end))
      (skip-chars-forward "[:space:]")
      (unless (looking-at-p ":")
        (error "Possibly malformed property drawer in %s at position %d"
               file (point)))
      (forward-char)
      (push (cons (upcase
                   (buffer-substring
                    (point)
                    (1- (or (search-forward ":" (pos-eol) t)
                            (error "Possibly malformed property drawer in file %s at position %d"
                                   file (point))))))
                  (string-trim
                   (buffer-substring
                    (point) (pos-eol))))
            res)
      (forward-line 1))
    res))

(defun org-node-worker--collect ()
  "Scan for ID-nodes across files."
  (with-temp-buffer
    (insert-file-contents (org-node-worker--tmpfile "work-variables.eld"))
    (dolist (var (car (read-from-string (buffer-string))))
      (set (car var) (cdr var)))
    (erase-buffer)
    ;; The variable `i' was set via the command line that launched this process
    (insert-file-contents (org-node-worker--tmpfile "file-list-%d.eld" i))
    (setq $files (car (read-from-string (buffer-string))))
    (let ((case-fold-search t)
          ;; Perf
          (file-name-handler-alist $file-name-handler-alist)
          (gc-cons-threshold $gc-cons-threshold)
          ;; REVIEW: reading source for `recover-file', it sounds like the coding
          ;; system for read can affect the system for write?
          (coding-system-for-read $assume-coding-system)
          ;; Reassigned on every iteration, so may as well re-use the memory
          ;; locations (hopefully producing less garbage) instead of making a
          ;; new let-binding every time.  Not sure how elisp works... but
          ;; profiling shows a speedup.
          TITLE FILE-TITLE POS LEVEL HERE FAR
          TODO-STATE TAGS SCHED DEADLINE ID OLP FILE-TITLE-OR-BASENAME
          PROPS FILE-TAGS FILE-ID OUTLINE-DATA TODO-RE)
      (dolist (FILE $files)
        (if (not (file-exists-p FILE))
            ;; We got here because user deleted a file in a way that we didn't
            ;; notice.  If it was actually a rename, it'll get picked up on
            ;; next reset.
            ;; TODO: Schedule a targeted caching of any new files that appeared
            ;; in `org-node-files' output
            (push `(org-node--forget-id-location ,FILE)
                  org-node-worker--demands)
          (erase-buffer)
          ;; NOTE: Used `insert-file-contents-literally' in the past,
          ;; converting each captured substring afterwards with
          ;; `decode-coding-string', but it still made us record the wrong
          ;; value for POS when there was any Unicode in the file.  So
          ;; instead, the let-bindings above reproduce much of the perf.
          (insert-file-contents FILE)
          ;; Verify there is at least one ID-node, otherwise skip file
          (when (re-search-forward "^[[:space:]]*:id: " nil t)
            (goto-char (point-min))
            (setq OUTLINE-DATA nil)
            ;; Roughly like `org-end-of-meta-data' for file level
            (setq FAR (or (re-search-forward "^ *?[^#:]" nil t) (point-max)))
            (goto-char 1)
            (setq PROPS
                  (if (re-search-forward "^ *:properties:" FAR t)
                      (progn
                        (forward-line 1)
                        (prog1 (org-node-worker--collect-properties
                                (point)
                                (if (re-search-forward "^ *:end:" nil t)
                                    (pos-bol)
                                  (error "Couldn't find matching :END: drawer in file %s at position %d"
                                         FILE (point)))
                                FILE)
                          (goto-char 1)))
                    nil))
            (setq FILE-TAGS
                  (if (re-search-forward "^#\\+filetags: " FAR t)
                      (prog1 (split-string
                              (buffer-substring (point) (pos-eol))
                              ":" t)
                        (goto-char 1))
                    nil))
            (setq TODO-RE
                  (if (re-search-forward $file-option-todo-re FAR t)
                      (prog1
                          (org-node-worker--make-todo-regexp
                           (buffer-substring (point) (pos-eol)))
                        (goto-char 1))
                    $global-todo-re))
            (setq FILE-TITLE (when (re-search-forward "^#\\+title: " FAR t)
                               (org-node-worker--org-link-display-format
                                (buffer-substring (point) (pos-eol)))))
            (setq FILE-TITLE-OR-BASENAME
                  (or FILE-TITLE (file-name-nondirectory FILE)))
            (when (setq FILE-ID (cdr (assoc "ID" PROPS)))
              (when $targeted
                ;; This was probably called by a rename-file advice, i.e. this
                ;; is not a full reset of all files, just a scan of 1 file
                (push `(org-id-add-location ,FILE-ID ,FILE)
                      org-node-worker--demands))
              ;; Collect links
              (let ((END (save-excursion
                           (when (org-node-worker--next-heading)
                             (1- (point))))))
                ;; Don't count org-super-links backlinks as forward links
                (when (re-search-forward $backlink-drawer-re END t)
                  (unless (search-forward ":end:" END t)
                    (error "Couldn't find matching :END: drawer in file %s" FILE)))
                (org-node-worker--collect-links-until END FILE-ID nil $link-re))
              (push `(org-node-cache--add-node-to-tables
                      ,(list :title FILE-TITLE-OR-BASENAME ;; Uhm
                             :level 0
                             :tags FILE-TAGS
                             :file-path FILE
                             :pos 1
                             :file-title FILE-TITLE
                             :file-title-or-basename FILE-TITLE-OR-BASENAME
                             :properties PROPS
                             :id FILE-ID
                             :aliases
                             (split-string-and-unquote
                              (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                             :refs
                             (split-string-and-unquote
                              (or (cdr (assoc "ROAM_REFS" PROPS)) ""))))
                    org-node-worker--demands))
            ;; Loop over the file's subtrees
            (while (org-node-worker--next-heading)
              (setq POS (point))
              (setq LEVEL (skip-chars-forward "*"))
              (skip-chars-forward " ")
              (setq HERE (point))
              (if (looking-at TODO-RE)
                  (progn
                    (setq TODO-STATE (buffer-substring (point) (match-end 0)))
                    (goto-char (1+ (match-end 0)))
                    (setq HERE (point)))
                (setq TODO-STATE nil))
              (if (re-search-forward " +\\(:.+:\\) *$" (pos-eol) t)
                  (progn
                    (setq TITLE (org-node-worker--org-link-display-format
                                 (buffer-substring HERE (match-beginning 0))))
                    (setq TAGS (split-string (match-string 1) ":" t)))
                (setq TITLE
                      (org-node-worker--org-link-display-format
                       (buffer-substring HERE (pos-eol))))
                (setq TAGS nil))
              ;; Now we must be careful.  Imagine this subtree is just a
              ;; heading, empty of content, and the very next line is another
              ;; heading.  Gotta go forward 1 line, see if it is a
              ;; planning-line (i.e. not a heading), and if it is, then go
              ;; forward 1 more line, and if that is a :PROPERTIES: line, then
              ;; we know it belongs to the current subtree.  If we had just
              ;; allowed the search for :PROPERTIES: to cross 2 lines, we could
              ;; have matched a property drawer for the wrong heading.  Of
              ;; course `with-restriction' could guard us against this kind of
              ;; thing, but with this algorithm as solid as it is, that'd be a
              ;; superfluous instruction.
              (forward-line 1)
              (setq HERE (point))
              (setq FAR (pos-eol))
              (setq SCHED
                    (if (re-search-forward "[[:space:]]*SCHEDULED:" FAR t)
                        (prog1 (buffer-substring
                                ;; \n just there for safety
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (setq DEADLINE
                    (if (re-search-forward "[[:space:]]*DEADLINE:" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (when (or SCHED
                        DEADLINE
                        (re-search-forward "[[:space:]]*CLOSED:" FAR t))
                ;; Alright, so there was a planning-line, meaning any
                ;; :PROPERTIES: must be on the next line.
                (forward-line 1)
                (setq FAR (pos-eol)))
              (setq PROPS
                    (if (re-search-forward "^[[:space:]]*:properties:" FAR t)
                        (progn
                          (forward-line 1)
                          (org-node-worker--collect-properties
                           (point)
                           ;; TODO: Can we better handle a missing :END:?
                           ;; Thinking the function above can do verification.
                           (if (re-search-forward "^[[:space:]]*:end:" nil t)
                               (prog1 (pos-bol)
                                 ;; For safety in case seeking :END: landed us
                                 ;; way down the file.  Some error will hopefully
                                 ;; be printed about this subtree, but we can
                                 ;; keep going sanely from here on.
                                 (goto-char FAR))
                             (error "Couldn't find matching :END: drawer in file %s at position %d"
                                    FILE (point)))
                           FILE))
                      nil))
              (setq ID (cdr (assoc "ID" PROPS)))
              (push (list POS TITLE LEVEL ID) OUTLINE-DATA) ;; nil ID allowed
              (when ID
                (when $targeted
                  ;; Called by a rename-file advice
                  (push `(org-id-add-location ,ID ,FILE)
                        org-node-worker--demands))
                (setq OLP (org-node-worker--pos->olp OUTLINE-DATA POS))
                (push `(org-node-cache--add-node-to-tables
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
                               :file-title-or-basename FILE-TITLE-OR-BASENAME
                               :olp OLP
                               :properties PROPS
                               :aliases
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                               :refs
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_REFS" PROPS)) ""))))
                      org-node-worker--demands))
              ;; Now collect links while we're here!
              ;; REVIEW: Oddly, the number of ID-links changes when we do a
              ;; save-restriction and narrow to a subtree at a time
              (let ((ID-HERE (or ID (org-node-worker--pos->parent-id
                                     OUTLINE-DATA POS FILE-ID)))
                    (END (save-excursion
                           (when (org-node-worker--next-heading)
                             (1- (point)))))
                    (OLP-WITH-SELF (append OLP (list TITLE))))
                (when ID-HERE
                  ;; Don't count org-super-links backlinks
                  (when (re-search-forward $backlink-drawer-re END t)
                    (unless (search-forward ":end:" END t)
                      (error "Couldn't find matching :END: drawer in file %s at position %d"
                             FILE (point))))
                  (org-node-worker--collect-links-until
                   END ID-HERE OLP-WITH-SELF $link-re)
                  ;; Gotcha... also collect links inside the heading, not
                  ;; just the body text
                  (goto-char POS)
                  (org-node-worker--collect-links-until
                   (pos-eol) ID-HERE OLP-WITH-SELF $link-re)))))))
      (with-temp-file (org-node-worker--tmpfile "demands-%d.eld" i)
        (let ((print-length nil))
          (insert (prin1-to-string org-node-worker--demands)))))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
