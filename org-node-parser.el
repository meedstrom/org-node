;;; org-node-parser.el --- Gotta go fast -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file is worker code meant for child processes.  It should be designed
;; to compile quickly, and the compiled artifact should load no libraries at
;; runtime.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x))

;; Tell compiler these aren't free variables
(defvar $plain-re)
(defvar $merged-re)
(defvar $assume-coding-system)
(defvar $file-name-handler-alist)
(defvar $file-todo-option-re)
(defvar $global-todo-re)
(defvar $backlink-drawer-re)
(defvar $i)
(defvar $files)

(defvar org-node-parser--result-paths-types nil)
(defvar org-node-parser--result-found-links nil)

(defun org-node-parser--tmpfile (&optional basename &rest args)
  "Return a path that puts BASENAME in a temporary directory.
As a nicety, `format' BASENAME with ARGS too.

On most systems, the resulting string will be
/tmp/org-node/BASENAME, but it depends on
OS and variable `temporary-file-directory'."
  (file-name-concat temporary-file-directory
                    "org-node"
                    (when basename (apply #'format basename args))))

(defun org-node-parser--make-todo-regexp (keywords-string)
  "Build a regexp from KEYWORDS-STRING.
KEYWORDS-STRING is expected to be the sort of thing you see after
a #+todo: or #+seq_todo: or #+typ_todo: keyword in an Org file.

The resulting regexp should be able to match any of the TODO
keywords within."
  (thread-last keywords-string
               (replace-regexp-in-string "(.*?)" "")
               (string-replace "|" "")
               (string-trim)
               (split-string)
               (regexp-opt)))

;; ;; Ok, it seems plenty fast enough
;; (benchmark-run 1000 (org-node-parser--org-link-display-format " OUTCOME Made enough progress on [[id:f42d4ea8-ce6a-4ecb-bbcd-22dcb4c18671][Math blah]] to blah"))
(defun org-node-parser--org-link-display-format (s)
  "Copy of `org-link-display-format'.
Format string S for display - this means replace every link in S
with only their description if they have one, and in any case
strip the brackets."
  (replace-regexp-in-string
   ;; The regexp is `org-link-bracket-re'
   "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
   (lambda (m) (or (match-string 2 m) (match-string 1 m)))
   s nil t))

(defun org-node-parser--next-heading ()
  "Similar to `outline-next-heading'."
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward "^\\*+ " nil 'move)
      (goto-char (pos-bol))))

(defun org-node-parser--split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly.
What this means?  See test/org-node-test.el."
  (when roam-refs
    (with-temp-buffer
      (insert roam-refs)
      (goto-char 1)
      (let (links beg end)
        ;; Extract all [[bracketed links]]
        (while (search-forward "[[" nil t)
          (setq beg (match-beginning 0))
          (if (setq end (search-forward "]]" nil t))
              (progn
                (goto-char beg)
                (push (buffer-substring (+ 2 beg) (1- (search-forward "]")))
                      links)
                (delete-region beg end))
            (error "Missing close-bracket in ROAM_REFS property")))
        ;; Return merged list
        (cl-loop
         for link? in (append links (split-string-and-unquote (buffer-string)))
         ;; @citekey or &citekey
         if (string-match (rx (or bol (any ";:"))
                              (group (any "@&") (+ (not (any " ;]")))))
                          link?)
         collect (substring (match-string 1 link?) 1)
         ;; Some sort of uri://path
         else when (string-match "^\\(.*?\\):" link?)
         collect (let ((path (string-replace
                              "%20" " "
                              (substring link? (match-end 0)))))
                   ;; Remember the uri: prefix for pretty completions
                   (push (cons path (match-string 1 link?))
                         org-node-parser--result-paths-types)
                   ;; .. but the actual ref is just the //path
                   path))))))

;; Default keys of `org-ref-cite-types' 2024-07-25
(defconst org-node-parser--org-ref-types
  '("cite" "nocite" "citet" "citet*" "citep" "citep*" "citealt" "citealt*" "citealp" "citealp*" "citenum" "citetext" "citeauthor" "citeauthor*" "citeyear" "citeyearpar" "Citet" "Citep" "Citealt" "Citealp" "Citeauthor" "Citet*" "Citep*" "Citealt*" "Citealp*" "Citeauthor*" "Cite" "parencite" "Parencite" "footcite" "footcitetext" "textcite" "Textcite" "smartcite" "Smartcite" "cite*" "parencite*" "supercite" "autocite" "Autocite" "autocite*" "Autocite*" "citetitle" "citetitle*" "citeyear" "citeyear*" "citedate" "citedate*" "citeurl" "fullcite" "footfullcite" "notecite" "Notecite" "pnotecite" "Pnotecite" "fnotecite" "cites" "Cites" "parencites" "Parencites" "footcites" "footcitetexts" "smartcites" "Smartcites" "textcites" "Textcites" "supercites" "autocites" "Autocites" "bibentry"))
(defconst org-node-parser--org-ref-type-re
  (regexp-opt org-node-parser--org-ref-types))

(defun org-node-parser--collect-links-until (end id-here)
  "From here to buffer position END, look for forward-links.
vArgument ID-HERE is the ID of the subtree where this function is
being executed (or that of an ancestor heading, if the current
subtree has none), to be included in each link's metadata.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own.

Argument PLAIN-RE is expected to be the value of
`org-link-plain-re', and MERGED-RE a regexp that merges that with
`org-link-bracket-re'."
  (let ((beg (point))
        link-type path)
    (while (re-search-forward $merged-re end t)
      (if (setq path (match-string 1))
          ;; Link is the [[bracketed]] kind.  Is there an URI: style link
          ;; inside?  Here is the magic that allows links to have spaces, it is
          ;; not possible with $plain-re alone.
          (if (string-match $plain-re path)
              (setq link-type (match-string 1 path)
                    path (string-trim-left path ".*?:"))
            ;; Nothing of interest between the brackets
            nil)
        ;; Link is the unbracketed kind
        (setq link-type (match-string 3)
              path (match-string 4)))
      (when link-type
        (unless (save-excursion
                  ;; If point is on a # comment line, skip line
                  (goto-char (pos-bol))
                  (looking-at-p "[[:space:]]*# "))
          (if (and (string-search "&" path)
                   (string-match-p org-node-parser--org-ref-type-re link-type))
              ;; A citep:, citealt: or some such.  Specifically org-ref v3
              ;; because PATH contains at least one ampersand.
              (while (string-match "&.+?\\b" path)
                (let ((citekey (match-string 0 path)))
                  (setq path (substring path (match-end 0)))
                  (push (record 'org-node-link
                                id-here
                                (point)
                                link-type
                                (substring citekey 1)) ;; Drop &
                        org-node-parser--result-found-links)))
            (push (record 'org-node-link
                          id-here
                          (point)
                          link-type
                          (string-replace "%20" " " path))
                  org-node-parser--result-found-links)))))

    ;; Start over and look for @citekeys
    (goto-char beg)
    (while (search-forward "[cite" end t)
      (let ((closing-bracket (save-excursion (search-forward "]" end t))))
        (if closing-bracket
            ;; The regexp is a modified `org-element-citation-key-re'
            (while (re-search-forward "@[!#-+./:<>-@^-`{-~[:word:]-]+"
                                      closing-bracket t)
              (if (save-excursion
                    (goto-char (pos-bol))
                    (looking-at-p "[[:space:]]*# "))
                  ;; On a # comment, skip citation
                  ;; NOTE: Don't just skip whole line due to the search
                  ;;       boundary on this while-loop
                  (goto-char closing-bracket)
                (push (record 'org-node-link
                              id-here
                              (point)
                              nil
                              (substring (match-string 0) 1)) ;; drop @
                      org-node-parser--result-found-links)))
          (error "No closing bracket to [cite:")))))
  (goto-char (or end (point-max))))

(defun org-node-parser--collect-properties (beg end)
  "Collect Org properties between BEG and END into an alist.
Assumes BEG and END delimit the region in between
a :PROPERTIES: and :END: string."
  (let (result)
    (goto-char beg)
    (while (< (point) end)
      (skip-chars-forward "[:space:]")
      (unless (looking-at-p ":")
        (error "Possibly malformed property drawer"))
      (forward-char)
      (push (cons (upcase
                   (buffer-substring
                    (point)
                    (1- (or (search-forward ":" (pos-eol) t)
                            (error "Possibly malformed property drawer")))))
                  (string-trim
                   (buffer-substring
                    (point) (pos-eol))))
            result)
      (forward-line 1))
    result))


;;; Main

(defun org-node-parser--collect-dangerously ()
  "Dangerous!
Overwrites the current buffer!

Taking info from the temp files prepared by `org-node--scan',
which includes info such as a list of Org files, visit all those
files to look for ID-nodes and links, then finish by writing the
findings to another temp file."
  (let ((file-name-handler-alist nil))
    (insert-file-contents (org-node-parser--tmpfile "work-variables.eld"))
    (dolist (var (read (buffer-string)))
      (set (car var) (cdr var)))
    (erase-buffer)
    ;; The variable `$i' was set by the command line that launched this process
    (insert-file-contents (org-node-parser--tmpfile "file-list-%d.eld" $i)))
  (setq $files (read (buffer-string)))
  (setq buffer-read-only t)
  (let ((case-fold-search t)
        result/missing-files
        result/found-nodes
        result/mtimes
        result/problems
        ;; Perf
        (file-name-handler-alist $file-name-handler-alist)
        (coding-system-for-read $assume-coding-system)
        (coding-system-for-write $assume-coding-system)
        ;; Let-bind outside rather than inside the loop, even though they are
        ;; only used inside the loop, to produce less garbage.  Not sure how
        ;; Elisp works, but profiling shows a speedup... or it did once upon a
        ;; time.  Suspect it makes no diff now that we nix GC, but I'm not
        ;; gonna convert back to local `let' forms as it still matters if you
        ;; wanna run this code synchronously.
        HEADING-POS HERE FAR END ID-HERE OLPATH
        DRAWER-BEG DRAWER-END
        TITLE FILE-TITLE FILE-TITLE-OR-BASENAME
        TODO-STATE TODO-RE FILE-TODO-SETTINGS
        TAGS FILE-TAGS ID FILE-ID SCHED DEADLINE PRIORITY LEVEL PROPS)

    (dolist (FILE $files)
      (condition-case err
          (catch 'file-done
            (when (or (not (file-readable-p FILE))
                      (file-symlink-p FILE))
              ;; If FILE does not exist (not readable), user probably deleted
              ;; or renamed a file.  If it was a rename, hopefully the new name
              ;; is also in file list.  Else, like if it was done outside Emacs
              ;; by typing `mv' on the command line, it gets picked up on next
              ;; scan.
              ;; And symlinks... (Who symlinks a note?)  Skip for two reasons:
              ;; - Causes duplicates if the true file is also in the file list.
              ;; - For performance, the codebase rarely uses `file-truename'.
              (push FILE result/missing-files)
              (throw 'file-done t))
            (push (cons FILE (file-attribute-modification-time
                              (file-attributes FILE)))
                  result/mtimes)
            ;; NOTE: Don't use `insert-file-contents-literally'!  It causes
            ;;       wrong values for HEADING-POS when there is any Unicode in
            ;;       the file.  Just overriding `coding-system-for-read' and
            ;;       `file-name-handler-alist' grants similar performance.
            (let ((inhibit-read-only t))
              (erase-buffer)
              (insert-file-contents FILE))
            ;; Verify there is at least one ID-node
            (unless (re-search-forward "^[[:space:]]*:id: " nil t)
              (throw 'file-done t))
            (goto-char 1)
            (setq OLPATH nil)

            ;; If the very first line of file is a heading (typical for people
            ;; who set `org-node-prefer-with-heading'), don't try to scan any
            ;; file-level front matter.  Anyway, our usage of
            ;; `org-node-parser--next-heading' cannot handle that edge-case, so
            ;; we must check.
            (if (looking-at-p "\\*")
                (progn
                  (setq FILE-ID nil)
                  (setq FILE-TITLE nil)
                  (setq FILE-TITLE-OR-BASENAME (file-name-nondirectory FILE))
                  (setq TODO-RE $global-todo-re))
              ;; Narrow until first heading
              (when (org-node-parser--next-heading)
                (narrow-to-region 1 (point))
                (goto-char 1))
              ;; Rough equivalent of `org-end-of-meta-data' for the file
              ;; level front matter, can jump somewhat too far but that's ok
              (setq FAR (if (re-search-forward "^ *?[^#:]" nil t)
                            (1- (point))
                          ;; No content other than front matter
                          (point-max)))
              (goto-char 1)
              (setq PROPS
                    (if (re-search-forward "^[[:space:]]*:properties:" FAR t)
                        (progn
                          (forward-line 1)
                          (org-node-parser--collect-properties
                           (point)
                           (if (re-search-forward "^[[:space:]]*:end:" FAR t)
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
                          (org-node-parser--make-todo-regexp
                           (string-join FILE-TODO-SETTINGS " ")))
                      $global-todo-re))
              (goto-char 1)
              (setq FILE-TITLE (when (re-search-forward "^#\\+title: " FAR t)
                                 (org-node-parser--org-link-display-format
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
                        (error "Couldn't find :END: of drawer"))
                      (org-node-parser--collect-links-until nil FILE-ID))
                  (setq END (point-max)))
                (goto-char HERE)
                (org-node-parser--collect-links-until END FILE-ID)

                ;; NOTE: A plist would be more readable than a record, but then
                ;; main Emacs has more work to do.  Profiled using:
                ;; (benchmark-run 10 (setq org-node--done-ctr 6) (org-node--handle-finished-job 7 #'org-node--finalize-full))
                ;; Result when finalizer passes plists to `org-node--make-obj':
                ;; (8.152532984 15 4.110698459000105)
                ;; Result when finalizer accepts these premade records:
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
                         (org-node-parser--split-refs-field
                          (cdr (assoc "ROAM_REFS" PROPS)))
                         nil
                         FILE-TAGS
                         FILE-TITLE-OR-BASENAME ;; Title mandatory
                         nil)
                 result/found-nodes))
              (goto-char (point-max))
              ;; We should now be at the first heading
              (widen))

            ;; ;; For `org-node-fakeroam-db-feed-mode'
            ;; ;; Adds 30% scan time, so not doing this
            ;; (push (vector FILE FILE-TITLE
            ;;               (secure-hash 'sha1 (current-buffer))
            ;;               (file-attribute-access-time FILE-ATTRS)
            ;;               (file-attribute-modification-time FILE-ATTRS))
            ;;       result/roam-db-file-data-vectors)

            ;; Loop over the file's headings
            (while (not (eobp))
              (catch 'entry-done
                ;; Narrow til next heading
                (narrow-to-region (point)
                                  (save-excursion
                                    (or (org-node-parser--next-heading)
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
                  ;; [#A] [#B] [#C]
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
                ;; Any tags in heading?
                (if (re-search-forward " +:.+: *$" (pos-eol) t)
                    (progn
                      (goto-char (match-beginning 0))
                      (setq TAGS (split-string (match-string 0) ":" t " *"))
                      (setq TITLE (org-node-parser--org-link-display-format
                                   (buffer-substring HERE (point)))))
                  (setq TAGS nil)
                  (setq TITLE (org-node-parser--org-link-display-format
                               (buffer-substring HERE (pos-eol)))))
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
                            (org-node-parser--collect-properties
                             (point)
                             (if (re-search-forward "^[[:space:]]*:end:" nil t)
                                 (prog1 (pos-bol)
                                   ;; In case seeking :END: landed us way down
                                   ;; the file.  Some error will likely be
                                   ;; printed about this subtree, but we can
                                   ;; keep going.
                                   ;; REVIEW What?
                                   (goto-char FAR))
                               (error "Couldn't find :END: of drawer"))))
                        nil))
                (setq ID (cdr (assoc "ID" PROPS)))
                (cl-loop until (> LEVEL (or (caar OLPATH) 0))
                         do (pop OLPATH)
                         finally do (push (list LEVEL TITLE ID) OLPATH))
                (when ID
                  (push
                   ;; NOTE: See the defstruct for the ordering of fields
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
                           (nreverse (mapcar #'cadr (cdr OLPATH)))
                           HEADING-POS
                           PRIORITY
                           PROPS
                           (org-node-parser--split-refs-field
                            (cdr (assoc "ROAM_REFS" PROPS)))
                           SCHED
                           TAGS
                           TITLE
                           TODO-STATE)
                   result/found-nodes))

                ;; Heading recorded, now collect links in the entry body!
                (setq ID-HERE
                      (or ID
                          (cl-loop for crumb in OLPATH thereis (caddr crumb))
                          FILE-ID
                          (throw 'entry-done t)))
                (setq HERE (point))
                ;; Don't count org-super-links backlinks
                ;; TODO: Generalize this mechanism to skip src blocks too
                (if (setq DRAWER-BEG
                          (re-search-forward $backlink-drawer-re nil t))
                    (unless (setq DRAWER-END (search-forward ":end:" nil t))
                      (push (list FILE (point)
                                  "Couldn't find matching :END: drawer")
                            result/problems)
                      (throw 'entry-done t))
                  ;; Danger, Robinson
                  (setq DRAWER-END nil))
                ;; Gotcha... collect links inside the heading too
                (goto-char HEADING-POS)
                (org-node-parser--collect-links-until (pos-eol) ID-HERE)
                ;; Collect links between property drawer and backlinks drawer
                (goto-char HERE)
                (when DRAWER-BEG
                  (org-node-parser--collect-links-until DRAWER-BEG ID-HERE))
                ;; Collect links until next heading
                (goto-char (or DRAWER-END HERE))
                (org-node-parser--collect-links-until (point-max) ID-HERE))
              (goto-char (point-max))
              (widen)))

        ;; Don't crash the process when there is an error signal,
        ;; report it and continue to the next file
        (( t error )
         (push (list FILE (point) err) result/problems))))

    ;; All done
    (with-temp-file (org-node-parser--tmpfile "results-%d.eld" $i)
      (let ((write-region-inhibit-fsync nil) ;; Default t in batch mode
            (print-length nil)
            (print-level nil))
        (insert
         (prin1-to-string (list result/missing-files
                                result/mtimes
                                result/found-nodes
                                ;; result/roam-db-file-data-vectors
                                org-node-parser--result-paths-types
                                org-node-parser--result-found-links
                                result/problems
                                (current-time)))))))
  ;; TODO: Does emacs in batch mode garbage-collect at the end? I guess not but
  ;;       if it does then maybe exec a kill -9 on itself here to skip it.
  ;;       Profile this.
  ;; (call-process "kill" nil nil nil "-9" (number-to-string (emacs-pid)))
  )

(provide 'org-node-parser)

;;; org-node-parser.el ends here
