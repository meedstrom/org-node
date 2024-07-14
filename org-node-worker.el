;;; org-node-worker.el --- Gotta go fast -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))

(defun org-node-worker--tmpfile (&optional basename &rest args)
  "Return a path that puts BASENAME in a temporary directory.
As a nicety, `format' BASENAME with ARGS too.

On most systems, the resulting string will be
/tmp/org-node/BASENAME, but it depends on
`temporary-file-directory'."
  (file-name-concat temporary-file-directory
                    "org-node"
                    (if basename (apply #'format basename args) "")))

(defun org-node-worker--make-todo-regexp (keywords-string)
  "Make a regexp based on KEYWORDS-STRING,
that will match any of the TODO keywords within."
  (thread-last keywords-string
               (replace-regexp-in-string "(.*?)" "")
               (string-replace "|" "")
               (string-trim)
               (split-string)
               (regexp-opt)))

(defun org-node-worker--split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly."
  (when roam-refs
    (with-temp-buffer
      (insert roam-refs)
      (goto-char 1)
      (let (refs beg end)
        (while (search-forward "[cite:" nil t)
          (setq beg (match-beginning 0))
          (setq end (search-forward "]"))
          (goto-char beg)
          ;; The regexp is `org-element-citation-key-re'
          (while (re-search-forward "@\\([!#-+./:<>-@^-`{-~[:word:]-]+\\)"
                                    end t)
            (push (match-string 0) refs))
          (delete-region beg end))
        (goto-char 1)
        (while (search-forward "[[" nil t)
          (setq beg (match-beginning 0))
          (setq end (search-forward "]]"))
          (goto-char beg)
          (push (buffer-substring (+ 2 beg) (1- (search-forward "]")))
                refs)
          (delete-region beg end))
        (append refs (split-string-and-unquote (buffer-string)))))))

(defun org-node-worker--elem-index (elem list)
  "Like `-elem-index', return first index of ELEM in LIST."
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
  (let (;; Drop all the data about positions below HEADING-POS
        (data-until-pos
         (nthcdr (org-node-worker--elem-index (assoc pos oldata) oldata)
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
  "Given buffer position HEADING-POS, return the Org outline path.
Result should look like a result from `org-get-outline-path'.

Argument OLDATA must be of a form looking like
 ((373 \"A subheading\" 2)
  (250 \"A top heading\" 1)
  (199 \"Another top heading\" 1)
  (123 \"First heading in the file is apparently third-level\" 3))

where the car of each element represents a buffer position, the cadr the
heading title, and the caddr the outline depth i.e. the number of
asterisks in the heading at that location.

As apparent in the example, OLDATA is expected in \"reverse\"
order, such that the last heading in the file is represented in
the first element.  An exact match for HEADING-POS must also be included
in one of the elements."
  (let* (olp
         (pos-data (or (assoc pos oldata)
                       (error "Broken algo: HEADING-POS %s not found in OLDATA %s"
                              pos oldata)))
         ;; Drop all the data about positions below HEADING-POS (using `nthcdr'
         ;; because oldata is in reverse order)
         (data-until-pos (nthcdr (org-node-worker--elem-index pos-data oldata)
                                 oldata)))
    (let ((previous-level (caddr (car data-until-pos))))
      ;; Work backwards towards the top of the file
      ;; NOTE: Tried catch-while-throw and dolist, but `cl-loop' wins at perf
      (cl-loop for row in data-until-pos
               when (> previous-level (caddr row))
               do (setq previous-level (caddr row))
               (push (cadr row) olp)
               and if (= 1 previous-level)
               ;; Stop
               return nil))
    olp))

(defun org-node-worker--org-link-display-format (s)
  "Copypasta from `org-link-display-format'."
  (save-match-data
    (replace-regexp-in-string
     ;; The regexp is `org-link-bracket-re'
     "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
     (lambda (m) (or (match-string 2 m) (match-string 1 m)))
     s nil t)))

(defun org-node-worker--next-heading ()
  "Similar to `outline-next-heading'."
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward "^\\*+ " nil 'move)
      (goto-char (pos-bol))))

(defun org-node-worker--collect-links-until
    (end id-here olp-with-self plain-re merged-re)
  "From here to buffer position END, look for forward-links.
Argument ID-HERE is the ID of the subtree where this function is
being executed (or that of an ancestor heading, if the current
subtree has none), and will be put in each link's metadata.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own.

Argument OLP-WITH-SELF is the outline path to the current
subtree, with its own heading tacked onto the end.  This is data
that org-roam expects to have.

Argument PLAIN-RE is expected to be the value of
`org-link-plain-re', passed in this way only so that the child
process does not have to load org.el."
  (let ((beg (point))
        link-type path)
    (while (re-search-forward merged-re end t)
      (if (match-string 1)
          ;; Link is the bracketed kind
          (progn
            (setq link-type ""
                  path (match-string 1))
            ;; Is there an URI: style link inside?
            ;; Here is the magic that allows links to have spaces, it is not
            ;; possible without brackets.
            (when (string-match plain-re path)
              (setq link-type (match-string 1 path)
                    path (string-trim-left path ".*?:"))))
        ;; Link is the unbracketed kind
        (setq link-type (match-string 3)
              path (match-string 4)))
      (if (save-excursion
            (goto-char (pos-bol))
            (or (looking-at-p "[[:space:]]*# ")
                (looking-at-p "[[:space:]]*#\\+")))
          ;; On a # comment or #+keyword, skip whole line
          (goto-char (pos-eol))
        (and link-type path
             (push (list :origin id-here
                         :pos (point)
                         :type link-type
                         :dest path
                         ;; Because org-roam asks for it
                         :properties (list :outline olp-with-self))
                   (if (equal "id" link-type)
                       org-node-worker--result-found-id-links
                     org-node-worker--result-found-reflinks)))))
    ;; Start over and look for @citekeys
    (goto-char beg)
    ;; NOTE Should ideally search for `org-element-citation-prefix-re', but
    ;; hoping this is good enough.
    (while (search-forward "[cite" end t)
      (let ((closing-bracket (save-excursion (search-forward "]" end t))))
        (when closing-bracket
          ;; The regexp is `org-element-citation-key-re'
          (while (re-search-forward "@\\([!#-+./:<>-@^-`{-~[:word:]-]+\\)"
                                    closing-bracket t)
            (if (save-excursion
                  (goto-char (pos-bol))
                  (or (looking-at-p "[[:space:]]*# ")
                      (looking-at-p "[[:space:]]*#\\+")))
                ;; On a # comment or #+keyword, skip citation
                ;; (NOTE: don't skip whole line as in the other fn)
                (goto-char closing-bracket)
              (push (list :origin id-here
                          :pos (point)
                          :key (match-string 0)
                          ;; Because org-roam asks for it
                          :properties (list :outline olp-with-self))
                    org-node-worker--result-found-citations)))))))
  ;; FIXME Surprisingly, fail to scan many nodes... whY?
  ;; (goto-char (1- end))
  )

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


;;; Main

(defvar org-node-worker--result-found-id-links nil)
(defvar org-node-worker--result-found-reflinks nil)
(defvar org-node-worker--result-found-citations nil)

(defun org-node-worker--collect-dangerously ()
  "Dangerous!  Assumes the current buffer is a temp buffer!

Using info in the temp files prepared by `org-node-cache--scan',
look for ID-nodes and links in all Org files given in the file
list, and write results to another temp file."
  (let ((file-name-handler-alist nil))
    (insert-file-contents (org-node-worker--tmpfile "work-variables.eld"))
    (dolist (var (read (buffer-string)))
      (set (car var) (cdr var)))
    (erase-buffer)
    ;; The variable `i' was set via the command line that launched this process
    (insert-file-contents (org-node-worker--tmpfile "file-list-%d.eld" i)))
  (setq $files (read (buffer-string)))
  (let ((case-fold-search t)
        result-missing-files
        result-found-nodes
        result-found-files
        ;; Perf
        (file-name-handler-alist $file-name-handler-alist)
        (coding-system-for-read $assume-coding-system)
        (coding-system-for-write $assume-coding-system) ;; same as mother emacs
        ;; Assigned on every iteration, so let-bind once to produce less
        ;; garbage.  Not sure how elisp works... but profiling shows a speedup
        HEADING-POS HERE FAR END OUTLINE-DATA OLP-WITH-SELF ID-HERE
        TITLE FILE-TITLE FILE-TITLE-OR-BASENAME
        TODO-STATE TODO-RE FILE-TODO-SETTINGS
        TAGS FILE-TAGS ID FILE-ID SCHED DEADLINE OLP PRIORITY LEVEL PROPS)
    (dolist (FILE $files)
      (condition-case err
          (catch 'file-done
            (unless (file-exists-p FILE)
              ;; We got here because user deleted a file in a way that we
              ;; didn't notice.  If it was actually a rename file-done outside
              ;; Emacs, the new name will get picked up on next reset.
              (push FILE result-missing-files)
              (throw 'file-done t))
            (push FILE result-found-files)
            (erase-buffer)
            ;; NOTE: Here I used `insert-file-contents-literally' in the past,
            ;; converting each captured substring afterwards with
            ;; `decode-coding-string', but it still made us record wrong values
            ;; for HEADING-POS when there was any Unicode in the file.
            ;; Instead, overriding `coding-system-for-read' and
            ;; `file-name-handler-alist' recoups much of the performance.
            (insert-file-contents FILE)
            ;; Verify there is at least one ID-node, otherwise skip file
            (unless (re-search-forward "^[[:space:]]*:id: " nil t)
              (throw 'file-done t))
            (goto-char 1)
            (setq OUTLINE-DATA nil)
            ;; Rough equivalent of `org-end-of-meta-data' for the file
            ;; level, can jump somewhat too far but that's ok
            (if (re-search-forward "^ *?[^#:]" nil t)
                ;; NOTE Observe that if first char in file is an Org heading
                ;;      star, this sets FAR=1 exactly
                (setq FAR (1- (point)))
              (setq FAR (point-max)))
            (goto-char 1)
            (setq PROPS
                  (if (re-search-forward "^[[:space:]]*:properties:" FAR t)
                      (progn
                        (forward-line 1)
                        (prog1 (org-node-worker--collect-properties
                                (point)
                                (if (re-search-forward "^[[:space:]]*:end:" nil t)
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
                  (if (re-search-forward $file-todo-option-re FAR t)
                      (progn
                        (setq FILE-TODO-SETTINGS nil)
                        ;; Because you can have multiple #+todo: lines...
                        (while (progn
                                 (push (buffer-substring (point) (pos-eol)) FILE-TODO-SETTINGS)
                                 (re-search-forward $file-todo-option-re FAR t)))
                        (goto-char 1)
                        (org-node-worker--make-todo-regexp
                         (string-join FILE-TODO-SETTINGS " ")))
                    $global-todo-re))
            (setq FILE-TITLE (when (re-search-forward "^#\\+title: " FAR t)
                               (org-node-worker--org-link-display-format
                                (buffer-substring (point) (pos-eol)))))
            (setq FILE-TITLE-OR-BASENAME
                  (or FILE-TITLE (file-name-nondirectory FILE)))
            (when (setq FILE-ID (cdr (assoc "ID" PROPS)))
              ;; Collect links & citations before first heading
              (setq END (save-excursion
                          (when (org-node-worker--next-heading)
                            (1- (point)))))
              (setq HERE (point))
              ;; Don't count org-super-links backlinks as forward links
              (when (re-search-forward $backlink-drawer-re END t)
                (let ((drawer-beg (point)))
                  (unless (search-forward ":end:" END t)
                    (error "Couldn't find matching :END: drawer in file %s"
                           FILE))
                  (org-node-worker--collect-links-until
                   END FILE-ID nil $plain-re $merged-re)
                  (setq END drawer-beg)))
              (goto-char HERE)
              (org-node-worker--collect-links-until
               END FILE-ID nil $plain-re $merged-re)
              (push (list
                     :title FILE-TITLE-OR-BASENAME ;; Title mandatory?
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
                     :refs (org-node-worker--split-refs-field
                            (cdr (assoc "ROAM_REFS" PROPS))))
                    result-found-nodes))

            ;; This initial condition supports the special case where the very
            ;; first line of a file is a heading, as would be typical for
            ;; people who nix `org-node-prefer-file-level-nodes'.
            (unless (or (looking-at-p "\\*")
                        (org-node-worker--next-heading))
              (throw 'file-done t))
            ;; Loop over the file's subtrees
            (while (not (eobp))
              (narrow-to-region (point)
                                (save-excursion
                                  (or (org-node-worker--next-heading)
                                      (point-max))))
              (setq HEADING-POS (point))
              (setq LEVEL (skip-chars-forward "*"))
              (skip-chars-forward " ")
              (setq HERE (point))
              (let ((case-fold-search nil))
                (if (looking-at TODO-RE)
                    (progn
                      (setq TODO-STATE (buffer-substring (point) (match-end 0)))
                      (goto-char (match-end 0))
                      (skip-chars-forward " ")
                      (setq HERE (point)))
                  (setq TODO-STATE nil))
                (if (looking-at "\\[#[A-Z0-9]+\\]")
                    (progn
                      (setq PRIORITY (match-string 0))
                      (goto-char (match-end 0))
                      (skip-chars-forward " ")
                      (setq HERE (point)))
                  (setq PRIORITY nil)))
              ;; Skip statistics-cookie such as "[2/10]"
              (when (looking-at "\\[[0-9]*/[0-9]*\\]")
                (goto-char (match-end 0))
                (skip-chars-forward " ")
                (setq HERE (point)))
              ;; Tags in heading
              (if (re-search-forward " +\\(:.+:\\) *$" (pos-eol) t)
                  (progn
                    (setq TITLE (org-node-worker--org-link-display-format
                                 (buffer-substring HERE (match-beginning 0))))
                    (setq TAGS (split-string (match-string 1) ":" t)))
                (setq TITLE
                      (org-node-worker--org-link-display-format
                       (buffer-substring HERE (pos-eol))))
                (setq TAGS nil))
              ;; Gotta go forward 1 line, see if it is a planning-line, and
              ;; if it is, then go forward 1 more line, and if that is a
              ;; :PROPERTIES: line, then we're good
              ;; actually--just check at the start if there is an id
              (forward-line 1)
              (setq HERE (point))
              (setq FAR (pos-eol))
              (setq SCHED
                    (if (re-search-forward "[[:space:]]*SCHEDULED: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                ;; \n just there for safety
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (setq DEADLINE
                    (if (re-search-forward "[[:space:]]*DEADLINE: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (if (or SCHED
                      DEADLINE
                      (re-search-forward "[[:space:]]*CLOSED: +" FAR t))
                  ;; Alright, so there was a planning-line, meaning any
                  ;; :PROPERTIES: must be on the next line.
                  (progn
                    (forward-line 1)
                    (setq FAR (pos-eol)))
                ;; No planning-line, who knows what it is (a new
                ;; heading?), return to safety
                (goto-char (1+ HEADING-POS)))
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
              ;; nil ID allowed
              (push (list HEADING-POS TITLE LEVEL ID) OUTLINE-DATA)
              (when ID
                (setq OLP (org-node-worker--pos->olp OUTLINE-DATA HEADING-POS))
                (push (list
                       :title TITLE
                       :is-subtree t
                       :level LEVEL
                       :id ID
                       :pos HEADING-POS
                       :tags TAGS
                       :todo TODO-STATE
                       :file-path FILE
                       :scheduled SCHED
                       :deadline DEADLINE
                       :file-title FILE-TITLE
                       :file-title-or-basename FILE-TITLE-OR-BASENAME
                       :olp OLP
                       :properties PROPS
                       :priority PRIORITY
                       :aliases
                       (split-string-and-unquote
                        (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                       :refs (org-node-worker--split-refs-field
                              (cdr (assoc "ROAM_REFS" PROPS))))
                      result-found-nodes))
              ;; Now collect links while we're here!
              (setq ID-HERE (or ID (org-node-worker--pos->parent-id
                                    OUTLINE-DATA HEADING-POS FILE-ID)))
              (when ID-HERE
                (setq END (save-excursion
                            (when (org-node-worker--next-heading)
                              (1- (point)))))
                (setq OLP-WITH-SELF (append OLP (list TITLE)))
                ;; Don't count org-super-links backlinks
                (when (re-search-forward $backlink-drawer-re END t)
                  (or (search-forward ":end:" END t)
                      (error "Couldn't find matching :END: drawer in file %s at position %d"
                             FILE (point))))
                (setq HERE (point))
                ;; Gotcha... collect links inside the heading, not
                ;; just the body text
                (goto-char HEADING-POS)
                (org-node-worker--collect-links-until
                 (pos-eol) ID-HERE OLP-WITH-SELF $plain-re $merged-re)
                (goto-char HERE)
                (org-node-worker--collect-links-until
                 END ID-HERE OLP-WITH-SELF $plain-re $merged-re))
              (goto-char (point-max))
              (widen)))

        (( t error )
         ;; Don't crash the whole process when there is a problem scanning one
         ;; file, report the problem and continue to the next file
         (let ((print-length nil)
               (print-level nil))
           (write-region (concat "\n\nProblems scanning " FILE ":"
                                 "\n" (prin1-to-string err))
                         nil
                         (org-node-worker--tmpfile "errors-%d.txt" i)
                         'append)))))

    (let ((print-length nil)
          (print-level nil))
      (write-region
       (prin1-to-string (list result-missing-files
                              result-found-files
                              result-found-nodes
                              org-node-worker--result-found-id-links
                              org-node-worker--result-found-reflinks
                              org-node-worker--result-found-citations
                              (current-time)))
       nil
       (org-node-worker--tmpfile "results-%d.eld" i)))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
