;;; org-id-node-butler.el -*- lexical-binding: t; -*-

(require 'org-id-node-common)

;;;###autoload
(define-minor-mode org-id-node-butler-mode
  "Keep Org buffers in good state to be grepped.
This involves:

- sorting the property drawers
- sorting the file-level keywords (front-matter like #+title)
- updating the :CACHED_BACKLINKS: properties"
  :global t
  :group 'org-id-node
  (if org-id-node-butler-mode
      (progn
        ;; hate the UX of before-save-hook, should probably hard depend on
        ;; apheleia or such system
        (add-hook 'before-save-hook #'org-id-node-butler-fix-buffer)
        (add-hook 'org-roam-post-node-insert-hook #'org-id-node-butler--add-backlink-to-here-in-target-a -99)
        (add-hook 'org-id-node-insert-link-hook #'org-id-node-butler--add-backlink-to-here-in-target-a -99)
        (advice-add 'org-insert-link :after #'org-id-node-butler--add-backlink-to-here-in-target-a))
    (remove-hook 'before-save-hook #'org-id-node-butler-fix-buffer)
    (remove-hook 'org-roam-post-node-insert-hook #'org-id-node-butler--add-backlink-to-here-in-target-a)
    (remove-hook 'org-id-node-insert-link-hook #'org-id-node-butler--add-backlink-to-here-in-target-a)
    (advice-remove 'org-insert-link #'org-id-node-butler--add-backlink-to-here-in-target-a)))

(defcustom org-id-node-butler-upcase-properties t
  "Whether to upcase or downcase all :PROPERTIES:."
  :group 'org-id-node
  :type 'boolean)

(defcustom org-id-node-butler-upcase-keywords nil
  "Whether to upcase or downcase file keywords such as #+title."
  :group 'org-id-node
  :type 'boolean)

;; Anything on `before-save-hook' MUST fail gracefully...
(defun org-id-node-butler-fix-buffer ()
  "Designed for `before-save-hook'."
  (condition-case err
      (progn
        (org-id-node-butler--fix-findable-backlinks)
        (org-id-node-butler--sort-properties)
        (org-id-node-butler--sort-keywords))
    ((t error debug)
     (message "org-id-node-butler failed with message %s %s" (car err) (cdr err)))))

(defvar org-id-node-butler--progress-total 0)
(defvar org-id-node-butler--progress-total-backlinks 0)
(defvar org-id-node-butler--progress-total-stale 0)
(defvar org-id-node-butler--progress-file nil)
(defvar org-id-node-butler--progress-total-worked 0)
(defvar org-id-node-butler--progress-total-files 0)

(defun org-id-node-butler--progress-print-message ()
  (message
   "Checking... (file %d/%d) link %d, added %d backlinks, removed %d stale, current file %s"
   org-id-node-butler--progress-total-worked
   org-id-node-butler--progress-total-files
   org-id-node-butler--progress-total
   org-id-node-butler--progress-total-backlinks
   org-id-node-butler--progress-total-stale
   org-id-node-butler--progress-file)
  (redisplay))

;; Separate function so the debugger don't have to step thru 2000 items
(defun org-id-node-butler--pre-open (files total)
  (let ((ctr-open 0)
        bufs)
    (dolist (file files)
      (message "Pre-opening buffers... (%d/%d)" (cl-incf ctr-open) total)
      (push (delay-mode-hooks
              (find-file-noselect file))
            bufs))
    bufs))

(defun org-id-node--consent-to-problematic-modes-for-mass-op ()
  (--all-p (if (auto-minor-mode-enabled-p it)
               (y-or-n-p
                (format "%S is active - proceed anyway?" it))
             t)
           '(auto-save-mode
             auto-save-visited-mode
             git-auto-commit-mode)))

(defvar org-id-node-butler--fixed (list))

(defun org-id-node-butler-fix-all ()
  "Thoroughly check every file in `org-id-locations'.

This ensures that all have up-to-date backlinks, and that
org-id-node can collect other metadata.

This command is not expected to ever be necessary if you always
have `org-id-node-butler-mode' active.  Think of it as a spring
cleaning."
  (interactive)
  (when current-prefix-arg
    (setq org-id-node-butler--fixed nil))
  (org-id-node-butler-mode 0)
  (org-id-node--init-org-id-locations-or-die)
  (let* ((files (-uniq (hash-table-values org-id-locations)))
         (total (length files)))
    (when (and (yes-or-no-p
                (format "Edit the %d files found in `org-id-locations'?" total))
               (org-id-node--consent-to-problematic-modes-for-mass-op))
      (let (bufs
            (ctr-worked 0)
            (find-file-hook nil)
            (after-save-hook nil)
            (before-save-hook nil)
            (org-agenda-files nil)
            (org-inhibit-startup t)
            (gc-cons-threshold (max gc-cons-threshold 2000000000)))
        (setq org-id-node-butler--progress-total-worked 0
              org-id-node-butler--progress-total-files total
              org-id-node-butler--progress-total 0
              org-id-node-butler--progress-total-backlinks 0
              org-id-node-butler--progress-total-stale 0)
        ;; Pre-open all files, not just for performance, also correctness.
        (setq bufs (org-id-node-butler--pre-open files total))
        (condition-case err
            (progn
              (dolist (buf bufs)
                (cl-incf org-id-node-butler--progress-total-worked)
                (unless (member buf org-id-node-butler--fixed)
                  (with-current-buffer buf
                    (setq org-id-node-butler--progress-file (buffer-file-name))
                    (widen)
                    (org-id-node-butler--fix-findable-backlinks t)
                    (push buf org-id-node-butler--fixed))))
              ;; Sort all files' properties only after all files have fixed each
              ;; other's backlinks, ensuring no wasted work
              (dolist (buf bufs)
                (with-current-buffer buf
                  (org-id-node-butler--sort-properties)
                  (org-id-node-butler--sort-keywords))))
          ((t error debug)
           (when (> org-id-node-butler--progress-total-worked 10)
             (message "Many buffers left unsaved"))
           (signal (car err) (cdr err)))
          (:success
           (setq org-id-node-butler--fixed nil)
           (when (yes-or-no-p "Fixed all buffers!  Save them?")
             (dolist (buf bufs)
               (save-buffer)))
           (org-id-node-butler-mode)
           (when org-id-node-butler--fails
             (delete-dups org-id-node-butler--fails)
             (org-id-node-die "All done, but couldn't find these IDs: \n%s%s"
                              (string-join org-id-node-butler--fails "\n")
                              ;; In case it's a stupid-long warning
                              (if (> (length org-id-node-butler--fails) 15)
                                  "--- End warning ---"
                                "")))
           (setq org-id-node-butler--fails nil)
           (when (yes-or-no-p "Fixed all files!  Kill their buffers?")
             (dolist (buf bufs)
               (kill-buffer buf)))))))))

;; TODO: Much faster: just collect grep buffer of all the org-id-locations and
;;       suggest the user do wgrep + search and replace.
(defun org-id-node-butler-regret (dir)
  "Remove :CACHED_BACKLINKS: from all files under DIR."
  (interactive "DWipe :CACHED_BACKLINKS: from Org files under directory: ")
  (let ((ctr 0))
    (dolist (file (directory-files-recursively dir "\\.org$"))
      (when (--none-p (string-search it file) '("/logseq/bak/"
                                                "/logseq/version-files/"
                                                ".sync-conflict-"))
        (org-with-file-buffer file
          (without-restriction
            (goto-char (point-min))
            (while (progn
                     (org-entry-delete nil "CACHED_BACKLINKS")
                     (outline-next-heading))))
          (and org-file-buffer-created
               (buffer-modified-p)
               (save-buffer))))
      (message "Removing CACHED_BACKLINKS... (%d files)" (cl-incf ctr)))))


;;; Plumbing

(defun org-id-node-butler--sort-keywords ()
  "Rearrange file-level keywords like #+title alphabetically.

This allows simple regular expressions to capture any set of
keywords, meaning you can use grep instead of depending on the
org element parser.

Meant as an autoformatter, perhaps on `before-save-hook'."
  (save-excursion
    (and
     (derived-mode-p 'org-mode)
     (goto-char (point-min))
     (search-forward "\n#+" nil t)
     ;; Rough check that we are in the front matter and not e.g. #+begin_src in
     ;; a file without front matter.  There may be other things in the front
     ;; matter but if there is no #+title, then it's not a proper node and there
     ;; is no need to sort.
     (org-get-title)
     (let* ((beg (line-beginning-position))
            (end (save-excursion
                   (when-let ((far (re-search-forward "^$\\|^[^#]" nil t)))
                     (goto-char beg)
                     (while (search-forward "\n#+" far t))
                     (line-end-position))))
            (prop-lines nil))
       (when (not end)
         (error "Something odd about keywords in %s" (current-buffer)))
       (with-restriction beg end
         (goto-char (point-min))
         (while (not (eobp))
           (if org-id-node-butler-upcase-keywords
               (upcase-region (point) (search-forward ":"))
             (downcase-region (point) (search-forward ":")))
           (push (string-trim-right (buffer-substring-no-properties
                                     (line-beginning-position)
                                     (line-end-position)))
                 prop-lines)
           (forward-line 1))
         (setq prop-lines (nreverse prop-lines))
         (let ((sorted-lines (-sort #'string-lessp prop-lines)))
           ;; If wasn't in alphabetic order, replace the region
           (unless (equal sorted-lines prop-lines)
             (atomic-change-group
               (delete-region (point-min) (point-max))
               (insert (string-join sorted-lines "\n"))))))))))

;; TODO: Also ensure:
;; - No initial whitespace before file-level properties
;; - That single-word members of ROAM_ALIASES are still wrapped in quotes
(defun org-id-node-butler--sort-properties ()
  "Rearrange all Org property drawers alphabetically.

This allows simple regular expressions to capture any set of
properties, meaning you can use grep instead of depending on the
org element parser.

Meant as an autoformatter, perhaps on `before-save-hook'."
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (let ((case-fold-search t))
        (while (re-search-forward "^[ \t]*:properties:" nil t)
          (unless (looking-at-p " *$")
            (user-error "Text after :PROPERTIES: at %s" (point)))
          (delete-horizontal-space)
          (if org-id-node-butler-upcase-properties
              (upcase-region (line-beginning-position) (line-end-position))
            (downcase-region (line-beginning-position) (line-end-position)))
          (let ((beg (1+ (point)))
                (end (progn
                       (re-search-forward "^[ \t]*:end:")
                       (unless (looking-at-p " *$")
                         (user-error "Text after :END: at %s" (point)))
                       (delete-horizontal-space)
                       (1- (line-beginning-position))))
                (prop-lines nil))
            (if org-id-node-butler-upcase-properties
                (upcase-region (line-beginning-position) (line-end-position))
              (downcase-region (line-beginning-position) (line-end-position)))
            (with-restriction beg end
              (goto-char (point-min))
              (when (search-forward "\f" nil t)
                ;; We reserve these chars to split matches from grep
                (user-error
                 "Form-feed characters in property drawer near %s" (point)))
              (while (not (eobp))
                (if org-id-node-butler-upcase-properties
                    (upcase-region (search-forward ":") (search-forward ":"))
                  (downcase-region (search-forward ":") (search-forward ":")))
                (push (string-trim-right (buffer-substring-no-properties
                                          (line-beginning-position)
                                          (line-end-position)))
                      prop-lines)
                (forward-line 1))
              (setq prop-lines (nreverse prop-lines))
              (dolist (line prop-lines)
                (unless (string-match-p "^[ \t]*:[[:alpha:]]" line)
                  (org-id-node-die "Drawer looks strange at %d in %s%s"
                                   (point) (buffer-file-name)
                                   ", can be a missing :END: or some such")))
              (let* ((sorted-lines (-sort #'string-lessp prop-lines)))
                ;; If wasn't in alphabetic order, replace the region
                (unless (equal sorted-lines prop-lines)
                  (atomic-change-group
                    (delete-region (point-min) (point-max))
                    (insert (string-join sorted-lines "\n"))))))
            ;; https://github.com/meedstrom/quickroam/issues/1
            (search-forward ":end:")
            (forward-line 1)
            (when (looking-at-p "\\*")
              (open-line 1))))))))

(defvar org-id-node-butler--fails nil
  "List of IDs that could not be resolved.")

(defun org-id-node-butler--target-has-link-to-here-p ()
  "Visit link at point and check if it has a backlink to here."
  ;; NB: Do not use `org-element-property' because it ignores links on property
  ;; lines.  That's also why org-roam won't double-count our backlinks, because
  ;; they're on a property line and it does consult `org-element-property'!
  (save-match-data
    (let ((src-id (org-id-get nil nil nil t))
          (bounds (org-in-regexp org-link-any-re))
          (case-fold-search t))
      (save-excursion
        (goto-char (car bounds))
        (when (looking-at-p "\\[\\[id:")
          ;; NOTE: If a bug has created malformed ids [[id:][abcdef]], this fails
          (re-search-forward "id:\\([^\]]+\\)" (cdr bounds))
          (let* ((target-id (match-string-no-properties 1))
                 (target-file (org-id-find-id-file target-id)))
            (when (and src-id target-file)
              (org-with-file-buffer target-file
                (save-excursion
                  (without-restriction
                    (goto-char (point-min))
                    (re-search-forward (concat "^[ \t]*:id: +" target-id))
                    ;; TODO Bound the-file level node
                    (unless (org-before-first-heading-p)
                      (org-narrow-to-subtree))
                    (cl-loop while (search-forward src-id nil t)
                             when (and (not (org-at-property-p))
                                       (not (org-at-comment-p)))
                             return t)))))))))))

(defun org-id-node-butler--add-backlink-to-here-in-target-a (&rest _)
  "Meant as advice after commands that insert a link.
See `org-id-node-butler--add-backlink-to-here-in-target', this is
merely a wrapper that drops the input."
  (org-id-node-butler--add-backlink-to-here-in-target))

(defun org-id-node-butler--add-backlink-to-here-in-target (&optional part-of-mass-op)
  "Visit the link under point and leave a backlink.

Then call `org-id-node-butler--sort-properties' (in the target file) to ensure the
backlink is in the correct place.  That's an expensive operation
if repeated many times, so with optional argument
PART-OF-MASS-OP, do not do that and assume the caller will do it
later."
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (when-let* ((case-fold-search t)
              (elm (org-element-context))
              (target-id (and (equal "id" (org-element-property :type elm))
                              (org-element-property :path elm)))
              (src-id (org-id-get nil nil nil t))
              (src-title (save-excursion
                           (re-search-backward (concat "^[ \t]*:id: +" src-id))
                           (or (org-get-heading t t t t)
                               (org-get-title))))
              (src-link (concat "[[id:" src-id "][" src-title "]]"))
              (target-file (or (org-id-find-id-file target-id)
                               (progn
                                 (push target-id org-id-node-butler--fails)
                                 (user-error "ID not found \"%s\"%s"
                                             target-id
                                             org-id-node--standard-tip)))))
    (if (not src-id)
        (message "Unable to find ID in file, so it won't get backlinks %s"
                 (buffer-file-name))
      (org-with-file-buffer target-file
        (org-with-wide-buffer
         (let ((otm (bound-and-true-p org-transclusion-mode)))
           (when otm (org-transclusion-mode 0))
           (goto-char (point-min))
           (if (not (re-search-forward
                     (concat "^[ \t]*:id: +" (regexp-quote target-id))
                     nil t))
               (push target-id org-id-node-butler--fails)
             (let ((backlinks-string (org-entry-get nil "CACHED_BACKLINKS"))
                   new-value)
               (if backlinks-string
                   ;; Build a temp list to check we don't add the same link
                   ;; twice. To use the builtin
                   ;; `org-entry-add-to-multivalued-property', the link
                   ;; descriptions would have to be free of spaces.
                   (let ((ls (delete-dups
                              (string-split (replace-regexp-in-string
                                             "]][[:space:]]+\\[\\["
                                             "]]\f[["
                                             (string-trim backlinks-string))
                                            "\f" t))))
                     ;; TODO: Ensure this works
                     (dolist (id-dup (--filter (string-search src-id it) ls))
                       (setq ls (delete id-dup ls)))
                     (push src-link ls)
                     ;; Two spaces between links help them look distinct
                     (setq new-value (string-join ls "  ")))
                 (setq new-value src-link))
               (unless (equal backlinks-string new-value)
                 (org-entry-put nil "CACHED_BACKLINKS" new-value)
                 (cl-incf org-id-node-butler--progress-total-backlinks)
                 (unless part-of-mass-op
                   (org-id-node-butler--sort-properties)
                   (and org-file-buffer-created
                        (buffer-modified-p)
                        (save-buffer))))
               (when otm (org-transclusion-mode))))))))))

(defvar org-id-node-butler--last-warnings nil)

(defun org-id-node-butler--fix-findable-backlinks (&optional part-of-mass-op)
  "Visit all [[id:... links and give the targets a backlink.
Also clean current file's own backlinks, by visiting them and
checking whether they still have a reference to the current file.

Note what that means.  This never generates new backlinks in the
current buffer, as that would require searching the world!  That
is why `org-id-node-butler-mode' advises link-insertion commands,
taking care of the other side of the equation.

To do a mass fix, try \\[org-id-node-butler-fix-all].

Optional argument PART-OF-MASS-OP means skip some cleanup."
  ;; INFO For all the link regexps see `org-link-make-regexps' in org-el
  (when (derived-mode-p 'org-mode)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-bracket-re nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (cl-incf org-id-node-butler--progress-total)
          (cond
           ;; On a # comment or #+keyword, ignore
           ((save-excursion
              (forward-line 0)
              (looking-at-p "#"))
            (forward-line 1))
           ;; On a CACHED_BACKLINKS property or in :BACKLINKS:...:END: drawer,
           ;; delete link if it's stale
           ((or (save-excursion
                  (save-match-data
                    (forward-line 0)
                    (and (looking-at org-property-re)
                         (string-equal-ignore-case (match-string 2)
                                                   "CACHED_BACKLINKS"))))
                (org-id-node-butler--in-backlinks-drawer-p))
            ;; REVIEW: ensure it works
            (unless (org-id-node-butler--target-has-link-to-here-p)
              (cl-incf org-id-node-butler--progress-total-stale)
              (kill-region beg end)
              ;; Delete when there's nothing left
              (let ((prop (org-entry-get nil "CACHED_BACKLINKS")))
                (when (and prop (string-blank-p prop))
                  (org-entry-delete nil "CACHED_BACKLINKS")))))
           ;; Not a backlink, so it's a regular forward-link - add a backlink
           (t
            (org-id-node-butler--add-backlink-to-here-in-target part-of-mass-op))))
        (when part-of-mass-op
          (org-id-node-butler--progress-print-message))))
    (and (not part-of-mass-op)
         org-id-node-butler--fails
         (not (equal org-id-node-butler--fails org-id-node-butler--last-warnings))
         (message "Sought backlinks but couldn't find these IDs: %s"
                  (string-join org-id-node-butler--fails "\n"))
         (setq org-id-node-butler--last-warnings org-id-node-butler--fails)
         (setq org-id-node-butler--fails nil))))

;; Can't find an Org builtin to check if we are inside a drawer
(defun org-id-node-butler--in-backlinks-drawer-p ()
  (save-match-data
    (let ((case-fold-search t))
      (let ((drawer-beg-above
             (save-excursion (search-backward "\n:backlinks:\n" nil t)))
            (drawer-end-above
             (save-excursion (search-backward "\n:end:\n" nil t)))
            (drawer-beg-below
             (save-excursion (search-forward "\n:backlinks:\n" nil t)))
            (drawer-end-below
             (save-excursion (search-forward "\n:end:\n" nil t))))
        (and (and drawer-beg-above drawer-end-below)
             (if drawer-end-above (> drawer-end-above drawer-beg-above) t)
             (if drawer-beg-below (> drawer-end-below drawer-beg-below) t))))))

(provide 'org-id-node-butler)

;;; org-id-node-butler.el ends here
