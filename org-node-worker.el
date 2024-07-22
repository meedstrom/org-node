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

(defvar org-node-worker--result:paths-types nil)

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

(defconst org-node-worker--citation-key-re
  "@\\([!#-+./:<>-@^-`{-~[:word:]-]+\\)"
  "Copy of `org-element-citation-key-re'.")

;; REVIEW I don't know for sure what people put in ROAM_REFS... check my
;; assumptions
(defun org-node-worker--split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly.
What this means?   See org-node-test.el."
  (when roam-refs
    (with-temp-buffer
      (insert roam-refs)
      (goto-char 1)
      (let (links beg end)
        ;; Extract all [[bracketed links]]
        (while (search-forward "[[" nil t)
          (setq beg (match-beginning 0))
          ;; TODO warn close-bracket missing in ROAM_REFS property
          (when (setq end (search-forward "]]" nil 'move))
            (goto-char beg)
            (push (buffer-substring (+ 2 beg) (1- (search-forward "]")))
                  links)
            (delete-region beg end)))
        ;; Return merged list
        (cl-loop
         for link? in (append links (split-string-and-unquote (buffer-string)))
         ;; @citekey
         if (string-prefix-p "@" link?)
         collect link?
         ;; Some sort of uri://path
         else when (string-match "^\\(.*?\\):" link?)
         collect (let ((path (substring link? (match-end 0))))
                   ;; Remember the uri: prefix for completions later
                   (push (cons path (match-string 1 link?))
                         org-node-worker--result:paths-types)
                   ;; .. but the actual ref is just the //path
                   path))))))

;; TODO: Extract org-ref v3 &citekeys too
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
      (if (setq path (match-string 1))
          ;; Link is the [[bracketed]] kind.  Is there an URI: style link
          ;; inside?  Here is the magic that allows links to have spaces, it is
          ;; not possible with plain-re alone.
          (if (string-match plain-re path)
              (setq link-type (match-string 1 path)
                    path (string-trim-left path ".*?:"))
            ;; Nothing of interest between the brackets
            nil)
        ;; Link is the unbracketed kind
        (setq link-type (match-string 3)
              path (match-string 4)))
      (when link-type
        (unless (save-excursion
                  ;; On a # comment or #+keyword, skip
                  (goto-char (pos-bol))
                  (or (looking-at-p "[[:space:]]*# ")
                      (looking-at-p "[[:space:]]*#\\+")))
          (push (record 'org-node-link
                        id-here
                        (point)
                        link-type
                        path
                        ;; Because org-roam asks for it
                        (list :outline olp-with-self))
                org-node-worker--result:found-links))))
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
              (push (record 'org-node-link
                            id-here
                            (point)
                            nil
                            (match-string 0)
                            ;; Because org-roam asks for it
                            (list :outline olp-with-self))
                    org-node-worker--result:found-links)))))))
  (goto-char (or end (point-max))))

(defun org-node-worker--collect-properties (beg end)
  "Assuming BEG and END delimit the region in between
:PROPERTIES:...:END:, collect the properties into an alist."
  (catch 'break
    (let (result)
      (goto-char beg)
      (while (not (>= (point) end))
        (skip-chars-forward "[:space:]")
        (unless (looking-at-p ":")
          (push (list org-node-worker--curr-file (point)
                      "Possibly malformed property drawer")
                org-node-worker--result:problems)
          (throw 'break nil))
        (forward-char)
        (push (cons (upcase
                     (buffer-substring
                      (point)
                      (1- (or (search-forward ":" (pos-eol) t)
                              (progn
                                (push (list org-node-worker--curr-file (point)
                                            "Possibly malformed property drawer")
                                      org-node-worker--result:problems)
                                (throw 'break nil))))))
                    (string-trim
                     (buffer-substring
                      (point) (pos-eol))))
              result)
        (forward-line 1))
      result)))


;;; Main

(defvar org-node-worker--result:found-links nil)
(defvar org-node-worker--result:problems nil)
(defvar org-node-worker--curr-file nil)

;; (defvar org-node-worker--temp-buf nil
;;   "One extra buffer.")

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
  ;; (setq org-node-worker--temp-buf (get-buffer-create " *org-node temp*" t))
  (let ((case-fold-search t)
        result:missing-files
        result:found-nodes
        result:found-files
        ;; Perf
        (file-name-handler-alist $file-name-handler-alist)
        (coding-system-for-read $assume-coding-system)
        (coding-system-for-write $assume-coding-system)
        ;; Assigned on every iteration, so let-bind once to produce less
        ;; garbage.  Not sure how elisp works... but profiling shows a speedup
        HEADING-POS HERE FAR END OUTLINE-DATA OLP-WITH-SELF ID-HERE
        DRAWER-BEG DRAWER-END
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
              (push FILE result:missing-files)
              (throw 'file-done t))
            (push FILE result:found-files)
            (setq org-node-worker--curr-file FILE)
            (setq buffer-read-only nil)
            (erase-buffer)
            ;; NOTE: Here I used `insert-file-contents-literally' in the past,
            ;; converting each captured substring afterwards with
            ;; `decode-coding-string', but it still made us record wrong values
            ;; for HEADING-POS when there was any Unicode in the file.
            ;; Instead, overriding `coding-system-for-read' and
            ;; `file-name-handler-alist' recoups much of the performance.
            (insert-file-contents FILE)
            (setq buffer-read-only t)
            ;; Verify there is at least one ID-node
            (unless (re-search-forward "^[[:space:]]*:id: " nil t)
              (throw 'file-done t))
            (goto-char 1)
            (setq OUTLINE-DATA nil)

            ;; If the very first line of file is a heading (typical for people
            ;; who nix `org-node-prefer-file-level-nodes'), don't try to scan
            ;; any file-level data.  Anyway, our usage of
            ;; `org-node-worker--next-heading' cannot handle that edge-case so
            ;; we MUST check.
            (if (looking-at-p "\\*")
                (progn
                  (setq FILE-ID nil)
                  (setq FILE-TITLE nil)
                  (setq FILE-TITLE-OR-BASENAME (file-name-nondirectory FILE))
                  (setq TODO-RE $global-todo-re))
              ;; Narrow until first heading
              (when (org-node-worker--next-heading)
                (narrow-to-region 1 (point))
                (goto-char 1))
              ;; Rough equivalent of `org-end-of-meta-data' for the file
              ;; level, can jump somewhat too far but that's ok
              (setq FAR (if (re-search-forward "^ *?[^#:]" nil t)
                            ;; NOTE Observe that if first char in file is an Org
                            ;;      heading star, this sets FAR=1 exactly
                            (1- (point))
                          ;; File is pretty much just a property drawer
                          (point-max)))
              (goto-char 1)
              (setq PROPS
                    (if (re-search-forward "^[[:space:]]*:properties:" FAR t)
                        (progn
                          (forward-line 1)
                          (org-node-worker--collect-properties
                           (point)
                           (if (re-search-forward "^[[:space:]]*:end:" nil t)
                               (pos-bol)
                             (error "Couldn't find :END: of drawer"))))
                      nil))
              (setq DRAWER-END (point))
              (goto-char 1)
              (setq FILE-TAGS
                    (if (re-search-forward "^#\\+filetags: " FAR t)
                        (split-string
                         (buffer-substring (point) (pos-eol))
                         ":" t)
                      nil))
              (goto-char 1)
              (setq TODO-RE
                    (if (re-search-forward $file-todo-option-re FAR t)
                        (progn
                          (setq FILE-TODO-SETTINGS nil)
                          ;; Because you can have multiple #+todo: lines...
                          (while (progn
                                   (push (buffer-substring (point) (pos-eol))
                                         FILE-TODO-SETTINGS)
                                   (re-search-forward
                                    $file-todo-option-re FAR t)))
                          (org-node-worker--make-todo-regexp
                           (string-join FILE-TODO-SETTINGS " ")))
                      $global-todo-re))
              (goto-char 1)
              (setq FILE-TITLE (when (re-search-forward "^#\\+title: " FAR t)
                                 (org-node-worker--org-link-display-format
                                  (buffer-substring (point) (pos-eol)))))
              (setq FILE-TITLE-OR-BASENAME
                    (or FILE-TITLE (file-name-nondirectory FILE)))
              (setq FILE-ID (cdr (assoc "ID" PROPS)))
              (when FILE-ID
                (goto-char DRAWER-END)
                (setq HERE (point))

                ;; Don't count org-super-links backlinks as forward links
                (if (re-search-forward $backlink-drawer-re nil t)
                    (progn
                      (setq END (point))
                      (unless (search-forward ":end:" nil t)
                        (error "Couldn't find matching :END: drawer"))
                      (org-node-worker--collect-links-until
                       nil FILE-ID nil $plain-re $merged-re))
                  (setq END (point-max)))
                (goto-char HERE)
                (org-node-worker--collect-links-until
                 END FILE-ID nil $plain-re $merged-re)
                ;; NOTE: A plist would be more readable than a record, but I
                ;; profiled it using:
                ;; (benchmark-run-compiled 10 (setq org-node--done-ctr 6) (org-node--handle-finished-job 7 #'org-node--finalize-full))
                ;; Result passing plists to `org-node--make-obj':
                ;; (8.152532984 15 4.110698459000105)
                ;; Result passing records as-is:
                ;; (5.928453786 10 2.7291036080000595)
                (push
                 (record 'org-node
                         (split-string-and-unquote
                          (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                         nil
                         FILE
                         FILE-TITLE
                         FILE-TITLE-OR-BASENAME
                         FILE-ID
                         nil
                         0
                         nil
                         1
                         nil
                         PROPS
                         (org-node-worker--split-refs-field
                          (cdr (assoc "ROAM_REFS" PROPS)))
                         nil
                         FILE-TAGS
                         FILE-TITLE-OR-BASENAME ;; Title mandatory
                         nil)
                 result:found-nodes))
              (goto-char (point-max))
              ;; We should now be at the first heading
              (widen))

            ;; Loop over the file's headings
            (while (not (eobp))
              (catch 'entry-done
                ;; Narrow til next heading
                (narrow-to-region (point)
                                  (save-excursion
                                    (or (org-node-worker--next-heading)
                                        (point-max))))
                (setq HEADING-POS (point))
                (setq LEVEL (skip-chars-forward "*"))
                (skip-chars-forward " ")
                (let ((case-fold-search nil))
                  (setq TODO-STATE
                        (if (looking-at TODO-RE)
                            (prog1 (buffer-substring (point) (match-end 0))
                              (goto-char (match-end 0))
                              (skip-chars-forward " "))
                          nil))
                  (setq PRIORITY
                        (if (looking-at "\\[#[A-Z0-9]+\\]")
                            (prog1 (match-string 0)
                              (goto-char (match-end 0))
                              (skip-chars-forward " "))
                          nil)))
                ;; Skip statistics-cookie such as "[2/10]"
                (when (looking-at "\\[[0-9]*/[0-9]*\\]")
                  (goto-char (match-end 0))
                  (skip-chars-forward " "))
                (setq HERE (point))
                ;; Tags in heading?
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
                (when (or SCHED
                          DEADLINE
                          (re-search-forward "[[:space:]]*CLOSED: +" FAR t))
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
                             (if (re-search-forward "^[[:space:]]*:end:" nil t)
                                 (prog1 (pos-bol)
                                   ;; In case seeking :END: landed us way down
                                   ;; the file.  Some error will likely be
                                   ;; printed about this subtree, but we can
                                   ;; keep going.
                                   ;; REVIEW What?
                                   (goto-char FAR))
                               (error "Couldn't find matching :END: drawer"))))
                        nil))
                (setq ID (cdr (assoc "ID" PROPS)))
                ;; NOTE: nil ID is allowed
                (push (list HEADING-POS TITLE LEVEL ID) OUTLINE-DATA)
                (when ID
                  (setq OLP (org-node-worker--pos->olp OUTLINE-DATA HEADING-POS))
                  (push
                   ;; NOTE Must be in same order as the defstruct
                   (record 'org-node
                           (split-string-and-unquote
                            (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                           DEADLINE
                           FILE
                           FILE-TITLE
                           FILE-TITLE-OR-BASENAME
                           ID
                           t
                           LEVEL
                           OLP
                           HEADING-POS
                           PRIORITY
                           PROPS
                           (org-node-worker--split-refs-field
                            (cdr (assoc "ROAM_REFS" PROPS)))
                           SCHED
                           TAGS
                           TITLE
                           TODO-STATE)
                   result:found-nodes))
                ;; Now collect links while we're here!
                (setq ID-HERE (or ID
                                  (org-node-worker--pos->parent-id
                                   OUTLINE-DATA HEADING-POS FILE-ID)
                                  (throw 'entry-done t)))
                (setq OLP-WITH-SELF (append OLP (list TITLE)))
                (setq HERE (point))
                ;; Don't count org-super-links backlinks
                ;; TODO: Generalize this mechanic to skip src blocks too
                (if (setq DRAWER-BEG
                          (re-search-forward $backlink-drawer-re nil t))
                    (unless (setq DRAWER-END (search-forward ":end:" nil t))
                      (push (list FILE (point)
                                  "Couldn't find matching :END: drawer")
                            org-node-worker--result:problems)
                      (throw 'entry-done t))
                  ;; Danger, Robinson
                  (setq DRAWER-END nil))
                ;; Gotcha... collect links inside the heading
                (goto-char HEADING-POS)
                (org-node-worker--collect-links-until
                 (pos-eol) ID-HERE OLP-WITH-SELF $plain-re $merged-re)
                ;; Collect links between property drawer and backlinks drawer
                (goto-char HERE)
                (when DRAWER-BEG
                  (org-node-worker--collect-links-until
                   DRAWER-BEG ID-HERE OLP-WITH-SELF $plain-re $merged-re))
                ;; Collect links until next heading
                (goto-char (or DRAWER-END HERE))
                (org-node-worker--collect-links-until
                 (point-max) ID-HERE OLP-WITH-SELF $plain-re $merged-re))
              (goto-char (point-max))
              (widen)))

        ;; Don't crash the process when there is an error signal,
        ;; report it and continue to the next file
        (( t error )
         (push (list FILE (point) err) org-node-worker--result:problems)
         (setq buffer-read-only nil))))

    (setq buffer-read-only nil)
    (let ((print-length nil)
          (print-level nil))
      (write-region
       (prin1-to-string (list result:missing-files
                              result:found-files
                              result:found-nodes
                              org-node-worker--result:paths-types
                              org-node-worker--result:found-links
                              org-node-worker--result:problems
                              (current-time)))
       nil
       (org-node-worker--tmpfile "results-%d.eld" i)))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
