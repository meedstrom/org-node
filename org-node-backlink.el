;;; org-node-backlink.el -*- lexical-binding: t; -*-

(require 'org-node-common)

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :CACHED_BACKLINKS: properties updated."
  :global t
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        ;; hate the UX of before-save-hook, should probably hard depend on
        ;; apheleia or such system
        (add-hook 'before-save-hook #'org-node-backlink-check-buffer)
        (add-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-to-here-in-target -99)
        (add-hook 'org-node-insert-link-hook #'org-node-backlink--add-to-here-in-target -99)
        (advice-add 'org-insert-link :after #'org-node-backlink--add-to-here-in-target))
    (remove-hook 'before-save-hook #'org-node-backlink-check-buffer)
    (remove-hook 'org-roam-post-node-insert-hook #'org-node-backlink--add-to-here-in-target)
    (remove-hook 'org-node-insert-link-hook #'org-node-backlink--add-to-here-in-target)
    (advice-remove 'org-insert-link #'org-node-backlink--add-to-here-in-target)))

;; TODO Large files would still be slow to save (now we disable it if there are
;;      too many links), so instead of checking the entire file on
;;      before-save-hook, try to just check the most local subtree on every
;;      auto-save, and maybe on some other infrequent hooks.  Also, leave the
;;      target file buffers open (but buried).
(defun org-node-backlink-check-buffer ()
  "Designed for `before-save-hook'."
  (let ((n-links
         (save-mark-and-excursion
           (cl-loop while (re-search-forward org-link-bracket-re nil t)
                    count t))))
    ;; If there's a ton of links in this file, don't do anything
    (unless (> n-links 100)
      ;; Catch and drop any error, bc `before-save-hook' should never error
      (condition-case err
          (org-node-backlink--fix-findable-backlinks)
        ((t error debug)
         (message "org-node-backlink failed with message %s %s"
                  (car err) (cdr err)))))))

(defvar org-node-backlink--progress-total 0)
(defvar org-node-backlink--progress-total-backlinks 0)
(defvar org-node-backlink--progress-total-stale 0)
(defvar org-node-backlink--progress-file nil)
(defvar org-node-backlink--progress-total-worked 0)
(defvar org-node-backlink--progress-total-files 0)

(defun org-node-backlink--progress-print-message ()
  (message
   "Checking... (file %d/%d) link %d, added %d backlinks, removed %d stale, current file %s"
   org-node-backlink--progress-total-worked
   org-node-backlink--progress-total-files
   org-node-backlink--progress-total
   org-node-backlink--progress-total-backlinks
   org-node-backlink--progress-total-stale
   org-node-backlink--progress-file)
  (redisplay))

;; Separate function so the debugger don't have to step thru 2000 items
(defun org-node-backlink--pre-open (files total)
  (let ((ctr-open 0)
        bufs)
    (dolist (file files)
      (message
       "Pre-opening buffers... (%d/%d) (you may cancel and resume anytime)"
       (cl-incf ctr-open)
       total)
      (push (delay-mode-hooks
              (find-file-noselect file))
            bufs))
    bufs))

(defun org-node--consent-to-problematic-modes-for-mass-op ()
  (--all-p (if (and (boundp it) (symbol-value it))
               (y-or-n-p
                (format "%S is active - proceed anyway?" it))
             t)
           '(auto-save-mode
             auto-save-visited-mode
             git-auto-commit-mode)))

(defvar org-node-backlink--fixed (list))
(defvar org-node-backlink--had-work (list))

(defun org-node-backlink-fix-all ()
  "Thoroughly check every file in `org-id-locations'.

This ensures that all have up-to-date backlinks, and that
org-node can collect other metadata.

This command is not expected to ever be necessary if you always
have `org-node-backlink-mode' active.  Think of it as a spring
cleaning."
  (interactive)
  (when current-prefix-arg
    (setq org-node-backlink--fixed nil))
  (org-node-backlink-mode 0)
  (org-node--init-org-id-locations-or-die)
  (let* ((files (-uniq (hash-table-values org-id-locations)))
         (total (length files)))
    (when (and (yes-or-no-p
                (format "Edit the %d files found in `org-id-locations'?" total))
               (org-node--consent-to-problematic-modes-for-mass-op))
      (let (bufs
            (ctr-worked 0)
            (find-file-hook nil)
            (after-save-hook nil)
            (before-save-hook nil)
            (org-agenda-files nil)
            (auto-save-default nil)
            (org-inhibit-startup t)
            (gc-cons-threshold (max gc-cons-threshold 2000000000)))
        (setq org-node-backlink--progress-total-worked 0
              org-node-backlink--progress-total-files total
              org-node-backlink--progress-total 0
              org-node-backlink--progress-total-backlinks 0
              org-node-backlink--progress-total-stale 0)
        ;; Pre-open all files, not just for performance, also correctness.
        (setq bufs (org-node-backlink--pre-open files total))
        (condition-case err
            (progn
              (dolist (buf bufs)
                (cl-incf org-node-backlink--progress-total-worked)
                (unless (member buf org-node-backlink--fixed)
                  (with-current-buffer buf
                    (setq org-node-backlink--progress-file (buffer-file-name))
                    (and (not (member buf org-node-backlink--had-work))
                         (not (eq t buffer-undo-list))
                         (not (null buffer-undo-list))
                         (push buf org-node-backlink--had-work))
                    (widen)
                    (org-node-backlink--fix-findable-backlinks t)
                    (push buf org-node-backlink--fixed)))))
          ((t error debug)
           (when (> org-node-backlink--progress-total-worked 10)
             (message "Many buffers left unsaved"))
           (signal (car err) (cdr err)))
          (:success
           (setq org-node-backlink--fixed nil)
           (when (yes-or-no-p "Fixed all buffers!  Save them?")
             (dolist (buf bufs)
               (with-current-buffer buf
                 (save-buffer buf)
                 (unless (member buf org-node-backlink--had-work)
                   (kill-buffer buf)))))
           (org-node-backlink-mode)
           (when org-node-backlink--fails
             (delete-dups org-node-backlink--fails)
             (org-node-die "All done, but couldn't find these IDs: \n%s%s"
                           (string-join org-node-backlink--fails "\n")
                           ;; In case it's a stupid-long warning
                           (if (> (length org-node-backlink--fails) 15)
                               "\n--- End warning ---"
                             "")))
           (setq org-node-backlink--fails nil)
           (when (yes-or-no-p "Fixed all files!  Kill their buffers?")
             (dolist (buf bufs)
               (kill-buffer buf)))))))))

;; TODO: Much faster: just collect grep buffer of all the org-id-locations and
;;       suggest the user do wgrep + search and replace.
(defun org-node-backlink-regret (dir)
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

(defvar org-node-backlink--fails nil
  "List of IDs that could not be resolved.")

(defun org-node-backlink--target-has-link-to-here-p ()
  "Visit link at point and check if it has a backlink to here."
  ;; NB: Do not use `org-element-property' because it ignores links on property
  ;; lines.  That's also why org-roam won't double-count our backlinks, because
  ;; they're on a property line and org-roam does use `org-element-property'!
  (save-match-data
    (let ((src-id (org-id-get nil nil nil t))
          (bounds (org-in-regexp org-link-any-re))
          (case-fold-search t))
      (save-excursion
        (goto-char (car bounds))
        (let (target-id target-file)
          (if (looking-at-p "\\[\\[id:")
              ;; Backlink
              (progn
                (re-search-forward "id:\\([^\]]+\\)" (cdr bounds))
                (setq target-id (match-string-no-properties 1)))
            ;; Reflink
            (re-search-forward (rx (+ (not (any "][")))) (cdr bounds))
            (setq target-id (gethash (match-string-no-properties 0)
                                     org-node--refs-table)))
          ;; Gotcha: (org-id-find-id-file nil) actually returns something!
          ;; So many ways org-id.el unsafe.  Should write an org-id2.el.
          (setq target-file (gethash target-id org-id-locations))
          (when (and src-id target-file)
            ;; Time to visit the target!
            ;; Here I would use `org-with-file-buffer' and such niceties if
            ;; org-mode was fast.  Instead, let's write hairy code.
            (with-temp-buffer
              (insert-file-contents target-file)
              (setq-local outline-regexp org-outline-regexp)
              (when-let ((after-id (re-search-forward
                                    (concat "^[[:space:]]*:id: +"
                                            (regexp-quote target-id)))))
                ;; OK, we found the ID-node.  Now we gotta determine boundaries
                ;; of this tree in order to search for links here only.
                (outline-previous-heading)
                (let ((beg (point))
                      (end (org-node-backlink--end-of-node)))
                  (goto-char after-id)
                  (narrow-to-region beg end)
                  (catch 'link-found
                    (while (progn
                             (let ((boundary (save-excursion
                                               (outline-next-heading))))
                               ;; Do not descend into subtrees that have IDs
                               (if (re-search-forward
                                    "^[[:space:]]*:id: " boundary t)
                                   (goto-char (org-node-backlink--end-of-node))
                                 (cl-loop
                                  while (search-forward src-id boundary t)
                                  when (save-excursion
                                         (goto-char (line-beginning-position))
                                         ;; Not a comment or property line
                                         (and (not (looking-at "# " t))
                                              (not (looking-at "#\\+" t))
                                              (not (looking-at ":[^ ]" t))))
                                  do (throw 'link-found t))))
                             (outline-next-heading)))))))))))))

(defun org-node-backlink--end-of-node ()
  "Similar idea as `org-forward-heading-same-level'.
Works in `fundamental-mode'.  Don't move point, just return a
buffer position.  Return `point-max' if before first heading."
  (save-excursion
    (if (or (looking-at-p org-outline-regexp-bol)
            (re-search-backward org-outline-regexp-bol nil t))
        ;; Go to next heading of same or higher level
        (let ((stars (skip-chars-forward "*")))
          (if (re-search-forward
               (rx-to-string `(and bol (** 1 ,stars "*")))
               nil t)
              (1- (line-beginning-position))
            ;; No next heading -- EOB it is
            (point-max)))
      ;; Wasn't in a subtree but file-level node
      (point-max))))

(defun org-node-backlink--add-to-here-in-target (&rest _)
  "Meant as advice after any command that inserts a link.
See `org-node-backlink--add-to-here-in-target-1' - this is
merely a wrapper that drops the input."
  (org-node--init-org-id-locations-or-die)
  (org-node-backlink--add-to-here-in-target-1))

(defun org-node-backlink--add-to-here-in-target-1 (&optional part-of-mass-op)
  "For known link at point, leave a backlink in the ref node."
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (let ((elm (org-element-context)))
    (let ((path (org-element-property :path elm))
          (type (org-element-property :type elm))
          id file)
      (when (and path type)
        (if (equal "id" type)
            ;; Classic backlink
            (progn
              (setq id path)
              (setq file (org-id-find-id-file id))
              (unless file
                (push id org-node-backlink--fails)
                (user-error "ID not found \"%s\"%s"
                            id
                            org-node--standard-tip)))
          ;; "Reflink"
          (setq id (gethash (concat type ":" path) org-node--refs-table))
          (setq file (plist-get (gethash id org-nodes) :file-path)))
        (when (and id file)
          (org-node-backlink--add-to-here-in-target-2 file id part-of-mass-op))))))

(defun org-node-backlink--add-to-here-in-target-2 (target-file target-id &optional part-of-mass-op)
  (let ((case-fold-search t)
        (src-id (org-id-get nil nil nil t)))
    (if (not src-id)
        (message "Unable to find ID in file, so it won't get backlinks: %s"
                 (buffer-file-name))
      (let* ((src-title (save-excursion
                          (re-search-backward (concat "^[ \t]*:id: +" src-id))
                          (or (org-get-heading t t t t)
                              (org-get-title))))
             (src-link (concat "[[id:" src-id "][" src-title "]]")))
        (org-with-file-buffer target-file
          (org-with-wide-buffer
           (let ((otm (bound-and-true-p org-transclusion-mode)))
             (when otm (org-transclusion-mode 0))
             (goto-char (point-min))
             (if (not (re-search-forward
                       (concat "^[ \t]*:id: +" (regexp-quote target-id))
                       nil t))
                 (push target-id org-node-backlink--fails)
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
                       (dolist (id-dup (--filter (string-search src-id it) ls))
                         (setq ls (delete id-dup ls)))
                       (push src-link ls)
                       ;; Prevent unnecessary work from putting the most recent
                       ;; link in front even if it was already in the list
                       (sort ls #'string-lessp)
                       ;; Two spaces between links help them look distinct
                       (setq new-value (string-join ls "  ")))
                   (setq new-value src-link))
                 (unless (equal backlinks-string new-value)
                   (org-entry-put nil "CACHED_BACKLINKS" new-value)
                   (cl-incf org-node-backlink--progress-total-backlinks)
                   (unless part-of-mass-op
                     (and org-file-buffer-created
                          (buffer-modified-p)
                          (save-buffer))))
                 (when otm (org-transclusion-mode)))))))))))

(defvar org-node-backlink--last-warnings nil)

;; TODO Rewrite to avoid org-mode in the link targets
(defun org-node-backlink--fix-findable-backlinks (&optional part-of-mass-op)
  "Visit all [[id:... links and give the targets a backlink.
Also clean current file's own backlinks, by visiting them and
checking whether they still have a reference to the current file.

Note what that means.  This never generates new backlinks in the
current buffer, as that would require searching the world!  That
is why `org-node-backlink-mode' advises link-insertion commands,
taking care of the other side of the equation.

To do a mass fix, try \\[org-node-backlink-fix-all].

Optional argument PART-OF-MASS-OP means skip some cleanup."
  ;; INFO For all the link regexps see `org-link-make-regexps' in org-el
  (when (derived-mode-p 'org-mode)
    (org-node--init-org-id-locations-or-die)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward org-link-plain-re nil t)
        (let ((beg (match-beginning 0))
              (end (match-end 0)))
          (cl-incf org-node-backlink--progress-total)
          (cond
           ;; On a # comment or #+keyword, ignore
           ((save-excursion
              (forward-line 0)
              (looking-at-p "#"))
            (forward-line 1))
           ;; On a CACHED_BACKLINKS property or in :BACKLINKS:...:END: drawer,
           ;; delete link if it's stale
           ((or (save-excursion
                  (forward-line 0)
                  (and (looking-at org-property-re)
                       (string-equal-ignore-case (match-string 2)
                                                 "CACHED_BACKLINKS")))
                (org-node-backlink--in-backlinks-drawer-p))
            ;; REVIEW: ensure it works
            (unless (org-node-backlink--target-has-link-to-here-p)
              (cl-incf org-node-backlink--progress-total-stale)
              (goto-char beg)
              (if (looking-back "\\[\\[")
                  (kill-region (- (point) 2) (search-forward "]]"))
                (kill-region (point) end))
              ;; Delete when there's nothing left
              (let ((prop (org-entry-get nil "CACHED_BACKLINKS")))
                (when (and prop (string-blank-p prop))
                  (org-entry-delete nil "CACHED_BACKLINKS")))))
           ;; Not a backlink, so it's a regular forward-link - add a backlink
           (t
            (org-node-backlink--add-to-here-in-target-1 part-of-mass-op))))
        (when part-of-mass-op
          (org-node-backlink--progress-print-message))))
    (and (not part-of-mass-op)
         org-node-backlink--fails
         (not (equal org-node-backlink--fails org-node-backlink--last-warnings))
         (message "Sought backlinks but couldn't find these IDs: %s"
                  (string-join org-node-backlink--fails "\n"))
         (setq org-node-backlink--last-warnings org-node-backlink--fails)
         (setq org-node-backlink--fails nil))))

;; Can't find an Org builtin to check if we are inside a drawer
(defun org-node-backlink--in-backlinks-drawer-p ()
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

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
