;;; org-node-worker.el --- Gotta go fast -*- lexical-binding: t; -*-

;; TODO try not writing 6 file lists, just one and access correct list with
;;      cdr assq i ...

(eval-when-compile
  (require 'cl-macs)
  (require 'subr-x))

(cl-defstruct (org-node-data (:constructor org-node-data--create)
                             (:copier nil))
  "An org-node data object holds information about an Org ID
node.  By the term \"Org ID node\", we mean either a subtree with
an ID property, or a file with a file-level ID property.  The
information is stored in slots listed below.

For each slot, there exists an accessor function
\"org-node-get-FIELD\".

For example, the field \"deadline\" has an accessor
`org-node-get-deadline'.  So you would type
\"(org-node-get-deadline NODE)\", where NODE is one of the
elements of the `hash-table-values' of `org-nodes'.

(Technically, there also exists a function alias to
`org-node-get-deadline', called `org-node-data-deadline', but its
use is discouraged.)

For real-world usage of these accessors, see examples in the
documentation of `org-node-filter-fn', the documentation of
`org-node-format-candidate-fn', or the package README.

You may be able to find the README by typing:

    M-x find-library RET org-node RET

or you can visit the homepage:

    https://github.com/meedstrom/org-node"
  (aliases    nil :read-only t :type List_of_strings :documentation
              "List of ROAM_ALIASES.")
  (deadline   nil :read-only t :type String :documentation
              "The DEADLINE state.")
  (file-path  nil :read-only t :type String :documentation
              "Full file path.")
  (file-title nil :read-only t :type String :documentation
              "The #+title of the file where this node is. May be nil.")
  (file-title-or-basename nil :read-only t :type String  :documentation
                          "The title of the file where this node is, or its filename if untitled.")
  (id         nil :read-only t :type String :documentation
              "The ID property.")
  (is-subtree nil :read-only t :type Boolean :documentation
              "Valued t if it is a subtree, nil if it is a file-level node.")
  (level      nil :read-only t :type Integer :documentation
              "Number of stars in the heading. File-level node always 0.")
  (olp        nil :read-only t :type List_of_strings :documentation
              "List of ancestor headings to this node.")
  (pos        nil :read-only t :type Integer :documentation
              "Char position of the node. File-level node always 1.")
  (properties nil :read-only t :type Alist :documentation
              "Alist of properties from the :PROPERTIES: drawer.")
  (priority nil :read-only t :type String :documentation
            "Priority such as [#A], as a string.")
  (refs       nil :read-only t :type List_of_strings :documentation
              "List of ROAM_REFS.")
  (scheduled  nil :read-only t :type String :documentation
              "The SCHEDULED state.")
  (tags       nil :read-only t :type List_of_strings :documentation
              "List of tags.")
  (title      nil :read-only t :type String :documentation
              "The node's heading, or #+title if it is not a subtree.")
  (todo       nil :read-only t :type String :documentation
              "The TODO state."))

;; Note to self: file-name-concat is probably faster than expand-file-name when
;; we do not need to convert the filename to absolute anyway.  Basically even
;; better than setting file-name-handler-alist to nil.
(defsubst org-node-worker--tmpfile (&optional basename &rest args)
  "Return a path that puts BASENAME in a temporary directory.

On most systems, the resulting string will be
/tmp/org-node/BASENAME, but it depends on the output of
`temporary-file-directory'.  Also as a nicety, pass BASENAME and
ARGS through `format' first."
  (file-name-concat (temporary-file-directory)
                    "org-node"
                    (if basename
                        (apply #'format basename args)
                      "")))

(defsubst org-node-worker--elem-index (elem list)
  "Like `-elem-index', return first index of ELEM in LIST."
  ;; (declare (pure t) (side-effect-free t))
  (when list
    (let ((list list)
          (i 0))
      (while (and list (not (equal elem (car-safe list))))
        (setq i (1+ i)
              list (cdr list)))
      i)))

(defsubst org-node-worker--pos->parent-id (oldata pos file-id)
  "Return ID of the closest ancestor heading that has an ID.
See `org-node-worker--pos->olp' for explanation of OLDATA and POS.

Extra argument FILE-ID is the file-level id, used as a fallback
if no ancestor heading has an ID.  It can be nil."
  ;; (declare (pure t) (side-effect-free t))
  (let (;; Drop all the data about positions below POS
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

(defsubst org-node-worker--pos->olp (oldata pos)
  "Given buffer position POS, return the Org outline path.
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
the first element.  An exact match for POS must also be included
in one of the elements."
  ;; (declare (pure t) (side-effect-free t))
  (let* (olp
         (pos-data (or (assoc pos oldata)
                       (error "Broken algo: POS %s not found in OLDATA %s"
                              pos oldata)))
         ;; Drop all the data about positions below POS (using `nthcdr' because
         ;; oldata is in reverse order)
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

(defsubst org-node-worker--make-todo-regexp (keywords-string)
  "Make a regexp based on KEYWORDS-STRING,
that will match any of the TODO keywords within."
  (thread-last keywords-string
               (replace-regexp-in-string "(.*?)" "")
               (string-replace "|" "")
               (string-trim)
               (split-string)
               (regexp-opt)))

(defsubst org-node-worker--org-link-display-format (s)
  "Copy-pasted from `org-link-display-format'."
  (save-match-data
    (replace-regexp-in-string
     ;; Pasted from `org-link-bracket-re'
     "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
     (lambda (m) (or (match-string 2 m) (match-string 1 m)))
     s nil t)))

(defmacro org-node-worker--while-progn (&rest body)
  "Lets you indent less than a (while (progn)) form."
  `(while (progn ,@body)))

(defsubst org-node-worker--next-heading ()
  "Similar to `outline-next-heading'.
In the special case where point is at the beginning of the buffer
and there is a heading there, just return t without moving point."
  ;; if (and (bobp) (looking-at-p "^\\*+ "))
  ;; t
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward "^\\*+ " nil 'move)
      (goto-char (pos-bol))))

(defvar org-node-worker--results nil
  "List of results to pass back to the main Emacs.")

(defvar org-node-worker--result-missing-files nil)
(defvar org-node-worker--result-found-nodes nil)
(defvar org-node-worker--result-mtimes nil)
;; (defvar org-node-worker--result-found-ids nil)
(defvar org-node-worker--result-found-id-links nil)
(defvar org-node-worker--result-found-reflinks nil)

(defsubst org-node-worker--collect-links-until (end id-here olp-with-self link-re)
  "From here to buffer position END, look for forward-links.
Ensure these links will be used to populate tables
`org-node--links-table' and `org-node--reflinks-table' in the
main Emacs process.

Argument ID-HERE is the ID of the subtree where this function is
being executed (or that of an ancestor subtree, if the current
subtree has none), and will be put in each link's metadata.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own.

Argument OLP-WITH-SELF is the outline path to the current
subtree, with its own heading tacked onto the end.  This is data
that org-roam expects to have.

Argument LINK-RE is expected to be the value of
`org-link-plain-re', passed in this way only so that the child
process does not have to load org.el."
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
        (push (list :origin id-here
                    :pos (point)
                    :type link-type
                    :dest path
                    ;; Because org-roam asks for it
                    :properties (list :outline olp-with-self))
              (if (equal "id" link-type)
                  org-node-worker--result-found-id-links
                org-node-worker--result-found-reflinks))))))

(defsubst org-node-worker--collect-properties (beg end file)
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
  "Dangerous!  Assumes the current buffer is a temp buffer!

Using info in the temp files prepared by `org-node-cache--scan',
scan for ID-nodes and links in all Org files given in the file
list, and write results to another temp file."
  (insert-file-contents (org-node-worker--tmpfile "work-variables.eld"))
  (dolist (var (car (read-from-string (buffer-string))))
    (set (car var) (cdr var)))
  (erase-buffer)
  ;; The variable `i' was set via the command line that launched this process
  (insert-file-contents (org-node-worker--tmpfile "file-list-%d.eld" i))
  (setq $files (read (buffer-string)))
  (let ((case-fold-search t)
        ;; Perf
        ;; (gc-cons-threshold $gc-cons-threshold)
        (file-name-handler-alist $file-name-handler-alist)
        ;; REVIEW: reading source for `recover-file', it sounds like the
        ;; coding system for read can affect the system for write? If so, how
        ;; to pick a sane system for write?
        (coding-system-for-read $assume-coding-system)
        ;; Assigned on every iteration, so may as well let-bind once, hopefully
        ;; producing less garbage.  Not sure how elisp works... but profiling
        ;; shows a speedup.
        POS HERE FAR OUTLINE-DATA MTIME
        TITLE FILE-TITLE FILE-TITLE-OR-BASENAME
        TODO-STATE TODO-RE FILE-TODO-SETTINGS
        TAGS FILE-TAGS ID FILE-ID SCHED DEADLINE OLP PRIORITY LEVEL PROPS)
    (dolist (FILE $files)
      (condition-case err
          (if (not (setq MTIME (file-attribute-modification-time
                                (file-attributes FILE))))
              ;; We got here because user deleted a file in a way that we
              ;; didn't notice.  If it was actually a rename done outside
              ;; Emacs, the new name will get picked up on next reset.
              (push FILE org-node-worker--result-missing-files)
            (push (cons FILE MTIME) org-node-worker--result-mtimes)
            (erase-buffer)
            ;; NOTE: Here I used `insert-file-contents-literally' in the past,
            ;; converting each captured substring afterwards with
            ;; `decode-coding-string', but it still made us record wrong values
            ;; for POS when there was any Unicode in the file.  Instead,
            ;; setting `$assume-coding-system' and `$file-name-handler-alist'
            ;; regains much of the performance that it had.
            (insert-file-contents FILE)
            ;; Verify there is at least one ID-node, otherwise skip file
            (when (re-search-forward "^[[:space:]]*:id: " nil t)
              (goto-char 1)
              (setq OUTLINE-DATA nil)
              ;; Rough equivalent of `org-end-of-meta-data' for the file
              ;; level, can jump somewhat too far but that's ok
              (if (re-search-forward "^ *?[^#:]" nil t)
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
                    (if (re-search-forward $file-option-todo-re FAR t)
                        (progn
                          (setq FILE-TODO-SETTINGS nil)
                          ;; Because you can have multiple #+todo: lines...
                          (while (progn
                                   (push (buffer-substring (point) (pos-eol)) FILE-TODO-SETTINGS)
                                   (re-search-forward $file-option-todo-re FAR t)))
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
                ;; (push FILE-ID org-node-worker--result-found-ids)
                ;; Collect links
                (let ((END (save-excursion
                             (when (org-node-worker--next-heading)
                               (1- (point))))))
                  ;; Don't count org-super-links backlinks as forward links
                  (when (re-search-forward $backlink-drawer-re END t)
                    (or (search-forward ":end:" END t)
                        (error "Couldn't find matching :END: drawer in file %s" FILE)))
                  (org-node-worker--collect-links-until
                   END FILE-ID nil $link-re))
                (push (org-node-data--create
                       :title FILE-TITLE-OR-BASENAME ;; Uhm
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
                        (or (cdr (assoc "ROAM_REFS" PROPS)) "")))
                      org-node-worker--result-found-nodes))

              ;; This initial condition supports the special case where
              ;; the very first line of a file is a heading
              (when (or (looking-at-p "\\*")
                        (org-node-worker--next-heading))
                ;; Loop over the file's subtrees
                (while
                    (progn
                      (setq POS (point))
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
                      ;; Now we must be careful.  Imagine this subtree is just a
                      ;; heading, empty of content, and the very next line is another
                      ;; heading.  Gotta go forward 1 line, see if it is a
                      ;; planning-line, and if it is, then go forward 1 more line, and
                      ;; if that is a :PROPERTIES: line, then we know it belongs to the
                      ;; current subtree.  If we had just allowed the search for
                      ;; :PROPERTIES: to cross 2 lines, we could have matched a
                      ;; property drawer for the wrong heading.  Of course
                      ;; `narrow-to-region' could guard us against this kind of thing,
                      ;; but with this algorithm as solid as it is now, that'd be a
                      ;; superfluous instruction that just increases the amount of
                      ;; large point motions.
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
                        ;; (push ID org-node-worker--result-found-ids)
                        (setq OLP (org-node-worker--pos->olp OUTLINE-DATA POS))
                        (push (org-node-data--create
                               :title TITLE
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
                               :priority PRIORITY
                               :aliases
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                               :refs
                               (split-string-and-unquote
                                (or (cdr (assoc "ROAM_REFS" PROPS)) "")))
                              org-node-worker--result-found-nodes))
                      ;; Now collect links while we're here!
                      ;; REVIEW: Oddly, the number of ID-links drops somewhat
                      ;; when I do a save-restriction and narrow to a subtree
                      ;; at a time. Why might that be?
                      (let ((ID-HERE (or ID (org-node-worker--pos->parent-id
                                             OUTLINE-DATA POS FILE-ID))))
                        (when ID-HERE
                          (let ((END (save-excursion
                                       (when (org-node-worker--next-heading)
                                         (1- (point)))))
                                (OLP-WITH-SELF (append OLP (list TITLE))))
                            ;; Don't count org-super-links backlinks
                            (when (re-search-forward $backlink-drawer-re END t)
                              (or (search-forward ":end:" END t)
                                  (error "Couldn't find matching :END: drawer in file %s at position %d"
                                         FILE (point))))
                            (org-node-worker--collect-links-until
                             END ID-HERE OLP-WITH-SELF $link-re)
                            ;; Gotcha... also collect links inside the heading, not
                            ;; just the body text
                            (goto-char POS)
                            (org-node-worker--collect-links-until
                             (pos-eol) ID-HERE OLP-WITH-SELF $link-re))))
                      (org-node-worker--next-heading))))))

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
       (prin1-to-string (list org-node-worker--result-missing-files
                              org-node-worker--result-mtimes
                              org-node-worker--result-found-nodes
                              ;; org-node-worker--result-found-ids
                              org-node-worker--result-found-id-links
                              org-node-worker--result-found-reflinks))
       nil
       (org-node-worker--tmpfile "demands-%d.eld" i)))))

(provide 'org-node-worker)

;;; org-node-worker.el ends here
