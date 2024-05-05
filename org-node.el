;;; org-node.el --- Use org-id locations as a pile of notes -*- lexical-binding: t; -*-

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

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Version:          0.1pre
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "24.4") (compat "29.1.4.5") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-node

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; TODO Merge -async.el into -cache.el
;; TODO Simplify backlink-mode
;; TODO What happens when we move a subtree to a differetn file but save the destination before saving  the origin file?
;; TODO Like we feed the org-roam-db, maybe feed org-id-locations too?  We can probably do it faster.  Then we can also provide saner user config.
;; TODO Better initial setup for people with incomplete org-id
;; TODO Annotations for completion
;; TODO Completion category https://github.com/alphapapa/org-ql/issues/299
;; TODO Command to grep across all files
;; TODO Command to explore feedback arc sets
;; TODO Bit of a test suite
;; TODO Test a custom id format involving emoji to see if that breaks regexps

(require 'org-node-common)
(require 'org-node-cache)
(require 'org-node-backlink)
(require 'org-node-roam)
(require 'org-node-async)
(require 'org-node-worker)
(require 'org-faces)
(require 'org-macs) ;; Test a fix for #4

;; Will deprecate soon
;;;###autoload
(defun org-node-enable ()
  "Deprecated.
Please add onto org-mode-hook:
- `org-node-cache-mode'
- `org-node-backlink-mode' (optional)"
  (remove-hook 'org-mode-hook #'org-node-enable)
  (add-hook 'org-mode-hook #'org-node-backlink-mode)
  (org-node-backlink-mode)
  (org-node-cache-mode)
  ;; 2024-04-30
  (message "Org-node has new recommendations for init, see README"))


;;; API not used inside this package

(defun org-node-at-point ()
  (gethash (org-id-get nil nil nil t) org-nodes))

(defun org-node-read ()
  (gethash (completing-read "Node: " org-node-collection
                            () () () 'org-node-hist)
           org-node-collection))


;;; Plumbing

(defvar org-node-hist nil
  "Minibuffer history.")

(defun org-node-convert-link-to-super (&rest _)
  "Wrapper for `org-super-links-convert-link-to-super'."
  (require 'org-super-links)
  (org-super-links-convert-link-to-super nil))

(defun org-node--visit-get-true-heading (node)
  "Visit subtree NODE and get the heading, in a way that's aware of
buffer-local #+todo settings so the todo state is not taken as part
of the heading."
  (if (org-node-get-is-subtree node)
      (delay-mode-hooks
        (org-with-file-buffer (org-node-get-file-path node)
          (save-excursion
            (without-restriction
              (goto-char (org-node-get-pos node))
              (nth 4 (org-heading-components))))))
    (org-node-get-title node)))

(defun org-node-slugify-like-roam (title)
  "From TITLE, make a filename in the default org-roam style."
  (unless (fboundp #'org-roam-node-slug)
    (user-error
     "Didn't create node! Install org-roam or configure `org-node-slug-fn'"))
  (require 'org-roam-node)
  (concat (format-time-string "%Y%m%d%H%M%S-")
          (org-roam-node-slug (org-roam-node-create :title title))
          ".org"))

(defun org-node-slugify-as-url (title)
  "From TITLE, make a filename suitable as URL component.

A title like \"Löb's Theorem\" becomes \"lobs-theorem.org\".
Note that it retains Unicode symbols classified as alphabetic
or numeric, so for example kanji and Greek letters remain.

As a surprise, it does NOT preface the name with a timestamp like
many zettelkasten packages do.  If you want that, you can use
a small wrapper such as:

(setq org-node-slug-fn (lambda (title)
                        (concat (format-time-string \"%Y%m%d%H%M%S-\")
                                (org-node-slugify-as-url title))))"
  (concat
   (thread-last title
                (string-glyph-decompose)
                (string-to-list)
                (--reject (< 767 it 818)) ;; remove diacritics
                (concat)
                (string-glyph-compose)
                (downcase)
                (string-trim)
                (replace-regexp-in-string "[[:space:]]+" "-")
                (replace-regexp-in-string "[^[:alnum:]\\/-]" "")
                (replace-regexp-in-string "\\/" "-")
                (replace-regexp-in-string "--*" "-")
                (replace-regexp-in-string "^-" "")
                (replace-regexp-in-string "-$" ""))
   ".org"))

;; Some useful test cases if you want to hack on the above function!

;; (org-node-slugify-as-url "A/B testing")
;; (org-node-slugify-as-url "\"But there's still a chance, right?\"")
;; (org-node-slugify-as-url "Löb's Theorem")
;; (org-node-slugify-as-url "How to convince me that 2 + 2 = 3")
;; (org-node-slugify-as-url "C. S. Peirce")
;; (org-node-slugify-as-url "Amnesic recentf, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (org-node-slugify-as-url "Slimline/\"pizza box\" computer chassis")
;; (org-node-slugify-as-url "#emacs")

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.")

(defun org-node--create (title id)
  "Call `org-node-creation-fn' and set necessary variables."
  (setq org-node-proposed-title title)
  (setq org-node-proposed-id id)
  (condition-case err
      ;; (let ((result (funcall org-node-creation-fn)))
      ;;   (when (bufferp result)
      ;;     (switch-to-buffer result)))
      (funcall org-node-creation-fn)
    ((t debug error)
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil)
     (signal (car err) (cdr err)))
    (:success
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil))))

(defun org-node-capture-target ()
  "Can be used as TARGET in a capture template.
See `org-capture-templates' for what TARGET means.

In simple terms, let's say you have a template targeting
`(function org-node-capture-target)'.  Here's a possible workflow:

1. Run M-x org-capture
2. Select your template
3. Type name of known or unknown node
4a. If it was known, it will capture into that node.
4b. If it was unknown, it will create a file-level node and then capture
    into there.

Additionally, if you've set (setq org-node-creation-fn #'org-capture),
commands like `org-node-find' will also outsource to capture when you
type the name of a node that does not exist:

1. Run M-x org-node-find
2. Type name of an unknown node
3. Select your template
4. Same as 4b earlier."
  (org-node-cache-ensure-fresh)
  (let (title node id)
    (if org-node-proposed-title
        ;; Was called from `org-node--create', so the user already typed the
        ;; title and no such node exists
        (progn
          (setq title org-node-proposed-title)
          (setq id org-node-proposed-id))
      ;; Was called from `org-capture', which means the user has not yet typed
      ;; the title

      (let ((input (completing-read "Node: " org-node-collection
                                    () () () 'org-node-hist)))
        (setq node (gethash input org-node-collection))
        (if node
            (progn
              (setq title (org-node-get-title node))
              (setq id (org-node-get-id node)))
          (setq title input)
          (setq id (org-id-new)))))
    (if node
        ;; Node exists; capture into it
        (progn
          (find-file (org-node-get-file-path node))
          (widen)
          (goto-char (org-node-get-pos node))
          (org-reveal)
          ;; TODO: Figure out how to play well with :prepend vs not :prepend.
          ;; Now it's just like it always prepends, I think?
          (unless (and (= 1 (point)) (org-at-heading-p))
            ;; Go to just before next heading, or end of buffer if there are no
            ;; more headings.  This allows the template to insert subtrees
            ;; without swallowing content that was already there.
            (when (outline-next-heading)
              (backward-char 1))))
      ;; Node does not exist; capture into new file-level node
      (let* ((dir (if org-node-ask-directory
                      (read-directory-name "New file in which directory? ")
                    (car (org-node--root-dirs
                          (hash-table-values org-id-locations)))))
             (path-to-write (file-name-concat
                             dir (funcall org-node-slug-fn title))))
        (if (or (file-exists-p path-to-write)
                (find-buffer-visiting path-to-write))
            (error "File or buffer already exists: %s" path-to-write)
          (find-file path-to-write)
          (insert ":PROPERTIES:"
                  "\n:ID:       " id
                  "\n:END:"
                  "\n#+title: " title
                  "\n")
          (run-hooks 'org-node-creation-hook))))))

(defun org-node-new-by-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called as `org-node-creation-fn', during which it
gets some necessary variables."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "org-node-new-by-roam-capture is meant to be called indirectly")
    (unless (fboundp #'org-roam-capture-)
      (org-node-die "Didn't create node! Either install org-roam or %s"
                    "configure `org-node-creation-fn'"))
    (require 'org-roam)
    (org-roam-capture- :node (org-roam-node-create :title org-node-proposed-title
                                                   :id    org-node-proposed-id))))

(defun org-node-new-file ()
  "Create a file-level node.
Meant to be called as `org-node-creation-fn', during which it
gets some necessary variables."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "org-node-new-file is meant to be called indirectly")
    (let* ((dir (if org-node-ask-directory
                    (read-directory-name "New file in which directory? ")
                  (car (org-node--root-dirs
                        (hash-table-values org-id-locations)))))
           (path-to-write (file-name-concat dir (funcall org-node-slug-fn
                                                         org-node-proposed-title))))
      (if (or (file-exists-p path-to-write)
              (find-buffer-visiting path-to-write))
          (message "A file or buffer already exists for path %s"
                   (file-name-nondirectory path-to-write))
        (find-file path-to-write)
        (insert ":PROPERTIES:"
                "\n:ID:       " org-node-proposed-id
                "\n:END:"
                "\n#+title: " org-node-proposed-title
                "\n")
        (unwind-protect
            (run-hooks 'org-node-creation-hook)
          (save-buffer))))))

(defun org-node--goto (node)
  "Visit NODE."
  (find-file (org-node-get-file-path node))
  (widen)
  (goto-char (org-node-get-pos node))
  (org-reveal)
  (recenter 5))


;;; Commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-node-creation-fn' to `org-node-new-by-roam-capture'."
  (interactive)
  (org-node-cache-ensure-fresh)
  (let* ((input (completing-read "Node: " org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node-collection)))
    (if node
        (org-node--goto node)
      (org-node--create input (org-id-new)))))

;;;###autoload
(defun org-node-insert-link ()
  "Insert a link to one of your ID nodes.

To behave like `org-roam-node-insert' when creating new nodes,
set `org-node-creation-fn' to
`org-node-new-by-roam-capture'.

If you find the behavior different, perhaps you have something in
`org-roam-post-node-insert-hook'.  Then perhaps copy it to
`org-node-insert-link-hook'."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure-fresh)
  (let* ((beg nil)
         (end nil)
         (region-text (when (region-active-p)
                        (setq end (region-end))
                        (goto-char (region-beginning))
                        (skip-chars-forward "\n[:space:]")
                        (setq beg (point))
                        (goto-char end)
                        (skip-chars-backward "\n[:space:]")
                        (setq end (point))
                        (org-link-display-format
                         (buffer-substring-no-properties beg end))))
         (input (completing-read "Node: " org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node-collection))
         (id (or (org-node-get-id node) (org-id-new)))
         (link-desc (or region-text
                        (if-let ((aliases (org-node-get-aliases node)))
                            (--find (string-match it input) aliases))
                        (org-node-get-title node)
                        input)))
    (atomic-change-group
      (if region-text
          (delete-region beg end))
      (insert (org-link-make-string (concat "id:" id) link-desc))
      (run-hook-with-args 'org-node-insert-link-hook id link-desc))
    ;; TODO: Delete the link if a node was not created
    (unless node
      (org-node--create input id))))

;;;###autoload
(defun org-node-random ()
  (interactive)
  (org-node-cache-ensure-fresh)
  (org-node--goto (nth (random (hash-table-count org-nodes))
                       (hash-table-values org-nodes))))

;;;###autoload
(defun org-node-insert-transclusion-as-subtree ()
  "Insert a link and a transclusion.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level 3

but adapt to the surrounding outline level.  I recommend
adding keywords to the things to exclude:

(setq org-transclusion-exclude-elements
       '(property-drawer comment keyword))
"
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure-fresh)
  (let ((node (gethash (completing-read "Node: " org-node-collection
                                        () () () 'org-node-hist)
                       org-node-collection)))
    (let ((id (org-node-get-id node))
          (title (org-node-get-title node))
          (level (or (org-current-level) 0))
          (m1 (make-marker)))
      (insert (org-link-make-string (concat "id:" id) title))
      (set-marker m1 (1- (point)))
      (duplicate-line)
      (goto-char (eol))
      (insert (make-string (+ 1 level) ?\*) " ")
      (forward-line 1)
      (insert "#+transclude: ")
      (goto-char (bol))
      (insert " :level " (number-to-string (+ 2 level)))
      ;; If the target is a subtree rather than file-level node, I'd like to
      ;; cut out the initial heading because we already made a heading.  (And
      ;; we made the heading so that this transclusion will count as a
      ;; backlink, plus it makes sense on export to HTML).
      ;;
      ;; Unfortunately the :lines trick would prevent
      ;; `org-transclusion-exclude-elements' from having an effect, and the
      ;; subtree's property drawer shows up!
      ;; TODO: Patch `org-transclusion-content-range-of-lines' to respect
      ;; `org-transclusion-exclude-elements', or make a different argument like
      ;; ":no-initial-heading"
      ;;
      ;; For now, just let it nest an extra heading. Looks odd but doesn't
      ;; break things.

      (goto-char (marker-position m1))
      (set-marker m1 nil)
      (run-hook-with-args 'org-node-insert-link-hook id title))))

;;;###autoload
(defun org-node-insert-transclusion ()
  "Insert a #+transclude: referring to a node."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure-fresh)
  (let ((node (gethash (completing-read "Node: " org-node-collection
                                        () () () 'org-node-hist)
                       org-node-collection)))
    (let ((id (org-node-get-id node))
          (title (org-node-get-title node))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id) title))
      (goto-char (line-beginning-position))
      (insert "#+transclude: ")
      (goto-char (line-end-position))
      (insert " :level " (number-to-string (+ 1 level)))
      (forward-char -10)
      (run-hook-with-args 'org-node-insert-link-hook id title))))

;;;###autoload
(defun org-node-rename-file-by-title (&optional path)
  "Rename the current file according to `org-node-slug-fn'.
Can also operate on a file at given PATH."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (unless path
    (setq path (buffer-file-name)))
  (unless (equal "org" (file-name-extension path))
    (user-error "File doesn't end in .org: %s" path))
  (let* ((title (org-get-title))
         (name (file-name-nondirectory path))
         (new-path (concat (file-name-directory path)
                           (funcall org-node-slug-fn title)
                           ".org"))
         (visiting (find-buffer-visiting path))
         (visiting-on-window (and visiting (get-buffer-window visiting))))
    (if (equal path new-path)
        (message "Filename already correct: %s" path)
      (if (and visiting (buffer-modified-p visiting))
          (message "Unsaved file, letting it be: %s" path)
        (if (get-file-buffer new-path)
            (message "A buffer already visiting the would-be new filename")
          (unless (file-writable-p path)
            (user-error "No permissions to rename file: %s" path))
          (unless (file-writable-p new-path)
            (user-error "No permissions to write a new file at: %s" new-path))
          ;; Kill buffer before renaming, because it will not update itself
          (when visiting
            (kill-buffer visiting))
          (rename-file path new-path)
          (prog1 (message "File %s renamed to %s"
                          (file-name-nondirectory path)
                          (file-name-nondirectory new-path))
            ;; Visit the file again if you had it open
            (when visiting
              (let ((buf (find-file-noselect new-path)))
                (when visiting-on-window
                  (set-window-buffer visiting-on-window buf))))))))))

(defface org-node-rewrite-links-face
  '((t :inherit 'org-link))
  "Face for use in `org-node-rewrite-links-ask'.")

;;;###autoload
(defun org-node-rewrite-links-ask (&optional file)
  "Look for links to update to match the current title.
Prompt the user for each one."
  (interactive)
  (require 'org-macs) ;; Test a fix for #4
  (require 'ol)
  (org-node-cache-ensure-fresh)
  (set-face-inverse-video 'org-node-rewrite-links-face
                          (not (face-inverse-video-p 'org-link)))
  (when (org-node--consent-to-problematic-modes-for-mass-op)
    (dolist (file (if file (list file) (hash-table-values org-id-locations)))
      (find-file-noselect file)
      (org-with-file-buffer file
        (goto-char (point-min))
        (while-let ((end (re-search-forward org-link-bracket-re nil t)))
          (let* ((beg (match-beginning 0))
                 (link (match-string 0))
                 (parts (split-string link "]\\["))
                 (target (substring (car parts) 2))
                 (desc (when (cadr parts)
                         (substring (cadr parts) 0 -2)))
                 (id (when (string-prefix-p "id:" target)
                       (substring target 3)))
                 (node (gethash id org-nodes))
                 (true-title (when node
                               (org-node-get-title node)))
                 (answered-yes nil))
            (when (and id node desc
                       (not (string-equal-ignore-case desc true-title))
                       (not (member-ignore-case desc
                                                (org-node-get-aliases node))))
              (switch-to-buffer (current-buffer))
              (org-reveal)
              (recenter)
              (highlight-regexp (rx (literal link)) 'org-node-rewrite-links-face)
              ;; (highlight-regexp (rx (literal link)))
              (unwind-protect
                  (setq answered-yes (y-or-n-p
                                      (format "Rewrite link? Will become: \"%s\""
                                              true-title)))
                (unhighlight-regexp (rx (literal link))))
              (when answered-yes
                (goto-char beg)
                (atomic-change-group
                  (delete-region beg end)
                  (insert (org-link-make-string target true-title)))
                ;; Give user 110+ ms to glimpse the result before moving on
                (redisplay)
                (sleep-for .11))
              (goto-char end)))))))
  (when (yes-or-no-p "Save the edited buffers?")
    (save-some-buffers)))

;;;###autoload
(defun org-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created buffer.

You may find it a common situation that the subtree had not yet
been assigned an ID or any other property that you normally
assign.  Thus, this creates an ID for you, copies over
any inherited tags, and runs `org-node-creation-hook'.

Adding to that, here is an example advice to copy any inherited
\"CREATED\" property, if an ancestor has such a property:

(advice-add 'org-node-extract-subtree :around
            (defun my-inherit-creation-date (fn &rest args)
              (let ((inherited-creation-date
                     (save-excursion
                       (while (not (or (org-entry-get nil \"CREATED\")
                                       (bobp)))
                         (org-up-heading-or-point-min))
                       (org-entry-get nil \"CREATED\"))))
                (apply fn args)
                (org-entry-put nil \"CREATED\"
                               (or inherited-creation-date
                                   (format-time-string \"[%F]\")))))))
"
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure-fresh)
  (let ((dir (if org-node-ask-directory
                 (read-directory-name "Extract to new file in directory: ")
               (car (org-node--root-dirs
                     (hash-table-values org-id-locations))))))
    (save-excursion
      (org-back-to-heading t)
      (save-buffer)
      (let* ((tags (org-get-tags))
             (title (org-get-heading t t t t))
             (id (org-id-get-create))
             (boundary (save-excursion
                         (org-end-of-meta-data t)
                         (point)))
             ;; Why is category autocreated by `org-entry-properties'...
             (category (save-excursion
                         (when (search-forward ":category:" boundary t)
                           (org-entry-get nil "CATEGORY"))))
             (properties (--filter (not (equal "CATEGORY" (car it)))
                                   (org-entry-properties nil 'standard)))
             (path-to-write (file-name-concat
                             dir (funcall org-node-slug-fn title))))
        (if (file-exists-p path-to-write)
            (message "A file already exists named %s" path-to-write)
          (org-cut-subtree)
          ;; Leave a link where the subtree was
          (open-line 1)
          (insert (format-time-string "[%F] Created ")
                  (org-link-make-string (concat "id:" id) title))
          (save-buffer)
          (find-file path-to-write)
          (org-paste-subtree)
          (save-buffer)
          (goto-char (point-min))
          (org-end-of-meta-data t)
          (kill-region (point-min) (point))
          (org-map-region #'org-promote (point-min) (point-max))
          (insert
           ":PROPERTIES:\n"
           (string-join (--map (concat ":" (car it) ": " (cdr it)) properties)
                        "\n")
           "\n:END:"
           (if category
               (concat "\n#+category: " category)
             "")
           (if tags
               (concat "\n#+filetags: :" (string-join tags ":") ":")
             "")
           "\n#+title: " title
           "\n")
          (run-hooks 'org-node-creation-hook)
          (save-buffer))))))

;;;###autoload
(defun org-node-rename-asset-and-rewrite-links ()
  "Prompt for an asset such as an image file to be renamed, then
search recursively for Org files with a link to that asset, give
the user a wgrep buffer of the search hits, and start an
interactive search-replace that updates the links.  After the
user completes the replacements, finally rename the file itself."
  (interactive)
  (require 'wgrep)
  (let ((root (car (org-node--root-dirs (hash-table-values org-id-locations))))
        (default-directory default-directory))
    (or (equal default-directory root)
        (if (y-or-n-p (format "Go to this folder? %s" root))
            (setq default-directory root)
          (setq default-directory
                (read-directory-name
                 "Directory with Org notes to operate on: "))))
    (when-let ((bufs (--filter (string-search "*grep*" (buffer-name it))
                               (buffer-list))))
      (when (yes-or-no-p "Kill other *grep* buffers to be sure this works?")
        (mapc #'kill-buffer bufs)))
    (let* ((filename (file-relative-name (read-file-name "File to rename: ")))
           (new (read-string "New name: " filename)))
      (mkdir (file-name-directory new) t)
      (unless (file-writable-p new)
        (error "New path wouldn't be writable"))
      (rgrep (regexp-quote filename) "*.org")
      (run-with-timer
       1 nil
       (lambda ()
         (pop-to-buffer (--find (string-search "*grep*" (buffer-name it))
                                (buffer-list)))
         (wgrep-change-to-wgrep-mode)
         (goto-char (point-min))
         (query-replace filename new)
         ;; NOTE: If the user quits the replaces with C-g, this code won't run,
         ;;       which is good.
         (when (buffer-modified-p)
           (wgrep-finish-edit)
           (rename-file filename new)
           (message "File renamed from %s to %s" filename new))))
      (message "Waiting for rgrep to populate buffer..."))))

;;;###autoload
(defun org-node-insert-heading ()
  "Insert a heading with ID and properties."
  (interactive nil org-mode)
  (org-insert-heading)
  (org-node-nodeify-entry))

;;;###autoload
(defun org-node-nodeify-entry ()
  "Add an ID to entry at point and run `org-node-creation-hook'."
  (interactive nil org-mode)
  (org-node-cache-ensure-fresh)
  (org-id-get-create)
  (run-hooks 'org-node-creation-hook))

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive nil org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED" (format-time-string "[%F]"))))

;;;###autoload
(defun org-node-reset ()
  (interactive)
  (let ((then (current-time)))
    (org-node-cache-reset)
    ;; Multicore is async, so it can't report any numbers right now
    (unless org-node-perf-multicore
      (let ((n-subtrees (cl-loop for node being the hash-values of org-nodes
                                 count (org-node-get-is-subtree node))))
        (message "org-node: found %d files, %d subtrees and %d links in %.2fs"
                 (- (hash-table-count org-nodes) n-subtrees)
                 n-subtrees
                 (+ (length (apply #'append
                                   (hash-table-values org-node--links-table)))
                    (length (apply #'append
                                   (hash-table-values org-node--reflinks-table))))
                 (float-time (time-since then)))))))

(provide 'org-node)

;;; org-node.el ends here
