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

;; This file is worker code meant for child processes.  It should load no
;; libraries at runtime.

;; The child processes are expected to load this file, execute
;; `org-node-parser--collect-dangerously', then die.

;; NOTE: While developing, do not eval only specific defuns;
;; `el-job--find-lib' needs you to save the file and do `eval-buffer'.

;;; Code:

;; TODO: Drop the @ from @citations (needs change in several places)

;; REVIEW: Should all regexp char classes including \n (newline) also include
;; \r (carriage return)?  Looking at quite a bit of Org code, they don't seem
;; to bother.  The regexp engine translates anyway in DOS-coded buffers?

(eval-when-compile
  (require 'cl-lib)
  (require 'subr-x)
  (require 'compat))

;; Tell compiler these aren't free variables
(defvar $plain-re)
(defvar $merged-re)
(defvar $assume-coding-system)
(defvar $file-name-handler-alist)
(defvar $file-todo-option-re)
(defvar $global-todo-re)
(defvar $backlink-drawer-re)
(defvar $inlinetask-min-level)
(defvar $files)

(defvar org-node-parser--paths-types nil)
(defvar org-node-parser--found-links nil)

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
;; (benchmark-run 1000 (org-node-parser--org-link-display-format "DONE Made enough progress on [[id:f42d4ea8-ce6a-4ecb-bbcd-22dcb4c18671][Math blah]] to blah"))
(defun org-node-parser--org-link-display-format (s)
  "Copy of `org-link-display-format'.
Format string S for display - this means replace every link inside S
with only their description if they have one, and in any case strip the
brackets."
  (replace-regexp-in-string
   ;; The regexp is `org-link-bracket-re'
   "\\[\\[\\(\\(?:[^][\\]\\|\\\\\\(?:\\\\\\\\\\)*[][]\\|\\\\+[^][]\\)+\\)]\\(?:\\[\\([^z-a]+?\\)]\\)?]"
   (lambda (m) (or (match-string 2 m) (match-string 1 m)))
   s nil t))

(defvar org-node-parser--heading-re (rx bol (repeat 1 14 "*") " "))
(defun org-node-parser--next-heading ()
  "Similar to `outline-next-heading'."
  (if (and (bolp) (not (eobp)))
      ;; Prevent matching the same line forever
      (forward-char))
  (if (re-search-forward org-node-parser--heading-re nil 'move)
      (goto-char (pos-bol))))

(defun org-node-parser--split-refs-field (roam-refs)
  "Split a ROAM-REFS field correctly.
What this means?  See test/org-node-test.el."
  (when roam-refs
    ;; TODO: Use `with-work-buffer' in Emacs 31
    (with-temp-buffer
      (insert roam-refs)
      (goto-char 1)
      (let (links beg end colon-pos)
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
                              (group (any "@&")
                                     (+ (not (any " ;]")))))
                          link?)
         ;; Replace & with @
         collect (let ((path (substring (match-string 1 link?) 1)))
                   (push (cons path (substring link? 0 colon-pos))
                         org-node-parser--paths-types)
                   (concat "@" path))
         ;; Some sort of uri://path
         else when (setq colon-pos (string-search ":" link?))
         collect (let ((path (string-replace
                              "%20" " "
                              (substring link? (1+ colon-pos)))))
                   ;; Remember the uri: prefix for pretty completions
                   (push (cons path (substring link? 0 colon-pos))
                         org-node-parser--paths-types)
                   ;; .. but the actual ref is just the //path
                   path))))))

(defun org-node-parser--collect-links-until (end id-here)
  "From here to buffer position END, look for forward-links.
Argument ID-HERE is the ID of the subtree where this function is
being executed (or that of an ancestor heading, if the current
subtree has none), to be included in each link's metadata.

It is important that END does not extend past any sub-heading, as
the subheading potentially has an ID of its own."
  (let ((beg (point))
        link-type path link-pos)
    ;; Here it may help to know that:
    ;; - `$plain-re' will be set to basically `org-link-plain-re'
    ;; - `$merged-re' to a combination of that and `org-link-bracket-re'
    (while (re-search-forward $merged-re end t)
      (setq link-pos (- (match-end 0) 1)) ;; Agree with `org-roam-db-map-links'
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
                  ;; If point is on a # comment line, skip
                  (goto-char (pos-bol))
                  (looking-at-p "[\t\s]*# "))
          (push (record 'org-node-link
                        id-here
                        link-pos
                        link-type
                        (string-replace "%20" " " path))
                org-node-parser--found-links))))

    ;; Start over and look for @citekeys
    (goto-char beg)
    (while (search-forward "[cite" end t)
      (let ((closing-bracket (save-excursion (search-forward "]" end t))))
        (if closing-bracket
            ;; The regexp is a modified `org-element-citation-key-re'
            (while (re-search-forward "[&@][!#-+./:<>-@^-`{-~[:word:]-]+"
                                      closing-bracket t)
              ;; Agree with `org-roam-db-map-citations'
              (setq link-pos (1+ (match-beginning 0)))
              (if (save-excursion
                    (goto-char (pos-bol))
                    (looking-at-p "[\t\s]*# "))
                  ;; On a # comment, skip citation
                  (goto-char closing-bracket)
                (push (record 'org-node-link
                              id-here
                              link-pos
                              nil
                              ;; Replace & with @
                              (concat "@" (substring (match-string 0) 1)))
                      org-node-parser--found-links)))
          (error "No closing bracket to [cite:")))))
  (goto-char (or end (point-max))))

(defun org-node-parser--collect-properties (beg end)
  "Collect Org properties between BEG and END into an alist.
Assumes BEG and END delimit the region in between
a :PROPERTIES: and :END: string."
  (let (result pos-start pos-eol)
    (goto-char beg)
    (while (< (point) end)
      (skip-chars-forward "\t\s")
      (unless (looking-at-p ":")
        (error "Possibly malformed property drawer"))
      (forward-char)
      (setq pos-start (point))
      (setq pos-eol (pos-eol))
      (or (search-forward ":" pos-eol t)
          (error "Possibly malformed property drawer"))
      (unless (= 0 (- pos-eol (point)))
        (push (cons (upcase (buffer-substring pos-start (1- (point))))
                    (string-trim (buffer-substring (1+ (point)) pos-eol)))
              result))
      (forward-line 1))
    result))


;;; Main

(defvar org-node-parser--buf nil
  "Throwaway buffer.
May be useful to look inside when debugging in main process rather than
running el-job processes.")

(defun org-node-parser--init ()
  "Setup a throwaway buffer in which to work.
Also set some variables."
  (switch-to-buffer (setq org-node-parser--buf
                          (get-buffer-create " *org-node-parser*" t)))
  (setq buffer-read-only t)
  (setq case-fold-search t)
  (setq file-name-handler-alist $file-name-handler-alist)
  (setq coding-system-for-read $assume-coding-system)
  (when $inlinetask-min-level
    (setq org-node-parser--heading-re
          (rx-to-string
           `(seq bol (repeat 1 ,(1- $inlinetask-min-level) "*") " ")))))

(defun org-node-parser--collect-dangerously (FILE)
  "Dangerous - overwrites the current buffer!

Read FILE contents into current buffer, analyze it for ID-nodes, links
and other data, then return the data."
  (setq org-node-parser--paths-types nil)
  (setq org-node-parser--found-links nil)
  (let (missing-file
        found-nodes
        file-mtime
        problem
        HEADING-POS HERE FAR END ID-HERE OLPATH
        DRAWER-BEG DRAWER-END
        TITLE FILE-TITLE
        TODO-STATE TODO-RE FILE-TODO-SETTINGS
        TAGS FILE-TAGS ID FILE-ID SCHED DEADLINE PRIORITY LEVEL PROPS)
    (condition-case err
        (catch 'file-done
          (when (not (file-readable-p FILE))
            ;; If FILE does not exist (not readable), user probably deleted
            ;; or renamed a file.  If it was a rename, hopefully the new name
            ;; is also in the file list.  Else, like if it was done outside
            ;; Emacs by typing `mv' on the command line, it gets picked up on
            ;; next scan.
            (setq missing-file FILE)
            (throw 'file-done t))
          ;; Skip symlinks for two reasons:
          ;; - Causes duplicates if the true file is also in the file list.
          ;; - For performance, the codebase rarely uses `file-truename' or
          ;;   `file-equal-p'.
          ;; Note that symlinks should not count as missing files, since they
          ;; get re-picked up every time by `org-node-list-files', leading to
          ;; pointlessly repeating `org-node--forget-id-locations'.
          (when (file-symlink-p FILE)
            (throw 'file-done t))
          ;; Transitional cleanup due to bug fixed in commit f900975
          (unless (string-suffix-p ".org" FILE)
            (setq missing-file FILE)
            (throw 'file-done t))
          ;; NOTE: Don't use `insert-file-contents-literally'!  It sets
          ;;       `coding-system-for-read' to `no-conversion', which results in
          ;;       wrong values for HEADING-POS when the file contains Unicode.
          ;;       We get close to similar performance just overriding
          ;;       `coding-system-for-read' to some fixed value, and
          ;;       especially minimizing `file-name-handler-alist'.
          (let ((inhibit-read-only t))
            (erase-buffer)
            (insert-file-contents FILE))
          ;; Verify there is at least one ID-node
          (unless (re-search-forward "^[\t\s]*:id: " nil t)
            (setq missing-file FILE) ;; Transitional cleanup
            (throw 'file-done t))
          (goto-char 1)

          ;; If the very first line of file is a heading (typical for people
          ;; who set `org-node-prefer-with-heading'), don't try to scan any
          ;; file-level front matter.  Our usage of
          ;; `org-node-parser--next-heading' cannot handle that edge-case.
          (if (looking-at-p "\\*")
              (progn
                (setq FILE-ID nil)
                (setq FILE-TITLE nil)
                (setq TODO-RE $global-todo-re))
            ;; Narrow until first heading
            (when (org-node-parser--next-heading)
              (narrow-to-region 1 (point))
              (goto-char 1))
            ;; Rough equivalent of `org-end-of-meta-data' for the file
            ;; level front matter, can jump somewhat too far but that's ok
            (setq FAR (if (re-search-forward "^ *?[^#:\n]" nil t)
                          (1- (point))
                        ;; There's no content other than front matter
                        (point-max)))
            (goto-char 1)
            (setq PROPS
                  (if (re-search-forward "^[\t\s]*:properties:" FAR t)
                      (progn
                        (forward-line 1)
                        (org-node-parser--collect-properties
                         (point)
                         (if (re-search-forward "^[\t\s]*:end:" FAR t)
                             (pos-bol)
                           (error "Couldn't find :END: of drawer"))))
                    nil))
            (setq HERE (point))
            (setq FILE-TAGS
                  (if (re-search-forward "^#\\+filetags: " FAR t)
                      (split-string
                       (buffer-substring (point) (pos-eol))
                       ":" t)
                    nil))
            (goto-char HERE)
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
            (goto-char HERE)
            (setq FILE-TITLE (when (re-search-forward "^#\\+title: " FAR t)
                               (org-node-parser--org-link-display-format
                                (buffer-substring (point) (pos-eol)))))
            (setq FILE-ID (cdr (assoc "ID" PROPS)))
            (when FILE-ID
              (goto-char HERE)
              ;; Don't count org-super-links backlinks as forward links
              ;; TODO: Rewrite more readably
              (if (re-search-forward $backlink-drawer-re nil t)
                  (progn
                    (setq END (point))
                    (unless (search-forward ":end:" nil t)
                      (error "Couldn't find :END: of drawer"))
                    ;; Collect from end of backlinks drawer to first heading
                    (org-node-parser--collect-links-until nil FILE-ID))
                (setq END (point-max)))
              (goto-char HERE)
              (org-node-parser--collect-links-until END FILE-ID)

              ;; NOTE: A plist would be more readable than a record, but then
              ;;       the mother Emacs has more work to do.
              ;;       Profiled using this snippet (won't work now):
              ;; (benchmark-run 10 (setq org-node--done-ctr 6) (org-node--handle-finished-job 7 #'org-node--finalize-full))
              ;;
              ;;       Result when finalizer accepts plists and passes them
              ;;       to `org-node--make-obj':
              ;; (8.152532984 15 4.110698459000105)
              ;;       Result when finalizer accepts these premade records:
              ;; (5.928453786 10 2.7291036080000595)
              (push (record 'org-node
                            (split-string-and-unquote
                             (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                            nil
                            FILE
                            FILE-TITLE
                            FILE-ID
                            0
                            nil
                            1
                            nil
                            PROPS
                            (org-node-parser--split-refs-field
                             (cdr (assoc "ROAM_REFS" PROPS)))
                            nil
                            FILE-TAGS
                            FILE-TAGS
                            ;; Title mandatory
                            (or FILE-TITLE (file-name-nondirectory FILE))
                            nil)
                    found-nodes))
            (goto-char (point-max))
            ;; We should now be at the first heading
            (widen))

          ;; Loop over the file's headings
          (setq OLPATH nil)
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
              ;; :PROPERTIES: line, then we're safe to collect properties
              (forward-line 1)
              (setq HERE (point))
              (setq FAR (pos-eol))
              (setq SCHED
                    (if (re-search-forward "[\t\s]*SCHEDULED: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (setq DEADLINE
                    (if (re-search-forward "[\t\s]*DEADLINE: +" FAR t)
                        (prog1 (buffer-substring
                                (point)
                                (+ 1 (point) (skip-chars-forward "^]>\n")))
                          (goto-char HERE))
                      nil))
              (when (or SCHED
                        DEADLINE
                        (re-search-forward "[\t\s]*CLOSED: +" FAR t))
                ;; Alright, so there was a planning-line, meaning any
                ;; :PROPERTIES: are not on this line but the next.
                (forward-line 1)
                (setq FAR (pos-eol)))
              (skip-chars-forward "\t\s")
              (setq PROPS
                    (if (looking-at-p ":properties:")
                        (progn
                          (forward-line 1)
                          (org-node-parser--collect-properties
                           (point)
                           (if (re-search-forward "^[\t\s]*:end:" nil t)
                               (pos-bol)
                             (error "Couldn't find :END: of drawer"))))
                      nil))
              (setq ID (cdr (assoc "ID" PROPS)))
              (cl-loop until (> LEVEL (or (caar OLPATH) 0))
                       do (pop OLPATH)
                       finally do (push (list LEVEL TITLE ID TAGS)
                                        OLPATH))
              (when ID
                (push (record 'org-node
                              (split-string-and-unquote
                               (or (cdr (assoc "ROAM_ALIASES" PROPS)) ""))
                              DEADLINE
                              FILE
                              FILE-TITLE
                              ID
                              LEVEL
                              (nreverse (mapcar #'cadr (cdr OLPATH)))
                              HEADING-POS
                              PRIORITY
                              PROPS
                              (org-node-parser--split-refs-field
                               (cdr (assoc "ROAM_REFS" PROPS)))
                              SCHED
                              TAGS
                              (delete-dups
                               (apply #'append FILE-TAGS
                                      (mapcar #'cadddr OLPATH)))
                              TITLE
                              TODO-STATE)
                      found-nodes))

              ;; Heading analyzed, now collect links in entry body!

              (setq ID-HERE
                    (or ID
                        (cl-loop for crumb in OLPATH thereis (caddr crumb))
                        FILE-ID
                        (throw 'entry-done t)))
              (setq HERE (point))
              ;; Don't count org-super-links backlinks
              ;; TODO: Generalize this mechanism to skip src blocks too
              (setq DRAWER-BEG (re-search-forward $backlink-drawer-re nil t))
              (setq DRAWER-END
                    (and DRAWER-BEG
                         (or (search-forward ":end:" nil t)
                             (error "Couldn't find :END: of drawer"))))

              ;; Collect links inside the heading
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
            (widen))

          (setq file-mtime
                (cons FILE
                      ;; Convert mtime into integer for `eq' operations
                      (truncate (float-time
                                 (file-attribute-modification-time
                                  (file-attributes FILE)))))))

      ;; Don't crash when there is an error signal, just report it.
      ;; Could allow for plural problems here, but one per file is plenty
      (( t error )
       (setq problem (list FILE (point) err))))

    (list (if missing-file (list missing-file))
          (if file-mtime (list file-mtime))
          found-nodes
          org-node-parser--paths-types
          org-node-parser--found-links
          (if problem (list problem)))))

(provide 'org-node-parser)

;;; org-node-parser.el ends here
