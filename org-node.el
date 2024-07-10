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
;; Version:          0.2
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.5") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-node

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; TODO What happens when user manually moves a subtree to a different file
;;      then saves the destination before saving the origin file?
;;      - The nodes table would be correct, but maybe not completion collection
;; TODO Annotations for completion? Completion categories? https://github.com/alphapapa/org-ql/issues/299

(require 'org-node-common)
(require 'org-node-cache)
(require 'org-node-backlink)
(require 'org-node-roam)


;;; API not used inside this package

(defun org-node-at-point ()
  "Return the ID-node at point.
This may refer to the current heading, else an ancestor heading,
else the file-level node, whichever has an ID."
  (gethash (org-entry-get nil "ID" t) org-nodes))

(defun org-node-read ()
  "Prompt for a known node."
  (gethash (completing-read "Node: " org-node-collection
                            () () () 'org-node-hist)
           org-node-collection))

(defun org-node-get-reflinks (node)
  (cl-loop for ref in (org-node-get-refs node)
           append (gethash ref org-node--latent-reflinks)))

(defun org-node-get-backlinks (node)
  (gethash (org-node-get-id node) org-node--links))


;;; Plumbing

(defvar org-node-hist nil
  "Minibuffer history.")

(defun org-node-guess-or-ask-dir (prompt)
  "Maybe prompt for a directory, and if so, show string PROMPT.
Behavior depends on the user option `org-node-ask-directory'."
  (if (eq t org-node-ask-directory)
      (read-directory-name prompt)
    (if (stringp org-node-ask-directory)
        org-node-ask-directory
      (car (org-node--root-dirs (org-node-files t))))))

(defun org-node-convert-link-to-super (&rest _)
  "Drop input and call `org-super-links-convert-link-to-super'."
  (require 'org-super-links)
  (org-super-links-convert-link-to-super nil))

(defun org-node-slugify-like-roam (title)
  "From TITLE, make a filename in the default org-roam style."
  (unless (fboundp #'org-roam-node-slug)
    (user-error
     "Didn't create node! Install org-roam or configure `org-node-filename-fn'"))
  (require 'org-roam-node)
  (concat (format-time-string "%Y%m%d%H%M%S-")
          (org-roam-node-slug (org-roam-node-create :title title))
          ".org"))

(defun org-node-slugify-as-url (title)
  "From TITLE, make a filename that looks nice as URL component.

A title like \"Löb's Theorem\" becomes \"lobs-theorem.org\".
Note that while diacritical marks are stripped, it retains
Unicode symbols classified as alphabetic or numeric, so for
example kanji and Greek letters remain.

As a surprise, it does NOT preface the name with a timestamp like
many zettelkasten packages do.  If you want that, you can use
a small wrapper such as:

(setq org-node-filename-fn
      (lambda (title)
       (concat (format-time-string \"%Y%m%d%H%M%S-\")
               (org-node-slugify-as-url title))))

Applying the above to \"Löb's Theorem\" results in something like
\"20240604223645-lobs-theorem.org\"."
  (concat
   (thread-last title
                (string-glyph-decompose)
                (string-to-list)
                (--reject (< 767 it 818)) ;; Remove diacritics
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
;; (org-node-slugify-as-url "E. T. Jaynes")
;; (org-node-slugify-as-url "Amnesic recentf, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (org-node-slugify-as-url "Slimline/\"pizza box\" computer chassis")
;; (org-node-slugify-as-url "#emacs")
;; (org-node-slugify-as-url "칹え🐛")

(defvar org-node-proposed-title nil
  "For use by `org-node-creation-fn'.")

(defvar org-node-proposed-id nil
  "For use by `org-node-creation-fn'.")

(defun org-node--create (title id)
  "Call `org-node-creation-fn' with necessary variables set.

When writing custom code, you should not assume anything about
which buffer will be current afterwards, since it depends on
`org-node-creation-fn' and whether TITLE or ID already existed.

To visit a node after creation, write:

    (org-node--create TITLE ID)
    (org-node-cache-ensure t)
    (let ((node (gethash ID org-nodes)))
      (if node (org-node--goto node)))"
  (setq org-node-proposed-title title)
  (setq org-node-proposed-id id)
  (condition-case err
      (funcall org-node-creation-fn)
    (( t debug error )
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil)
     (signal (car err) (cdr err)))
    (:success
     (setq org-node-proposed-title nil)
     (setq org-node-proposed-id nil))))

(defun org-node--goto (node)
  "Visit NODE."
  (if node
      (let ((file (org-node-get-file-path node)))
        (if (file-exists-p file)
            (progn
              (find-file file)
              (widen)
              (goto-char (org-node-get-pos node))
              (when (org-node-get-is-subtree node)
                (org-fold-show-context)))
          (message "This node's file is missing, re-scanning all dirs...")
          (org-node-cache--scan-all)))
    (error "Tried to visit node not known")))

(defun org-node-capture-target ()
  "Can be used as target in a capture template.
See `org-capture-templates' for more info about targets.

In simple terms, let's say you have configured
`org-capture-templates' so it has a template that
targets `(function org-node-capture-target)'.  Now here's a
possible workflow:

1. Run M-x org-capture
2. Select your template
3. Type name of known or unknown node
4a. If it was known, it will capture into that node.
4b. If it was unknown, it will create a file-level node and then capture
    into there.

Additionally, if you've set (setq org-node-creation-fn #'org-capture),
commands like `org-node-find' will outsource to org-capture when you
type the name of a node that does not exist.  That enables this
\"inverted\" workflow:

1. Run M-x org-node-find
2. Type name of an unknown node
3. Select your template
4. Same as 4b earlier."
  (org-node-cache-ensure)
  (let (title node id)
    (if org-node-proposed-title
        ;; Was called from `org-node--create', so the user already typed the
        ;; title and no such node exists yet
        (progn
          (setq title org-node-proposed-title)
          (setq id org-node-proposed-id))
      ;; Was called from `org-capture', which means the user has not yet typed
      ;; the title; let them type it now
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
      ;; Node does not exist; capture into new file
      (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
             (path-to-write (file-name-concat
                             dir (funcall org-node-filename-fn title))))
        (if (or (file-exists-p path-to-write)
                (find-buffer-visiting path-to-write))
            (error "File or buffer already exists: %s" path-to-write)
          (mkdir (file-name-directory path-to-write) t)
          (find-file path-to-write)
          (if org-node-make-file-level-nodes
              (insert ":PROPERTIES:"
                      "\n:ID:       " id
                      "\n:END:"
                      "\n#+title: " title
                      "\n")
            (insert "* " title
                    "\n:PROPERTIES:"
                    "\n:ID:       " id
                    "\n:END:"
                    "\n"))
          (unwind-protect
              (run-hooks 'org-node-creation-hook)
            (save-buffer)
            (org-node-cache--scan-targeted (list path-to-write))))))))

(defun org-node-new-by-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments.
Meant to be called indirectly as `org-node-creation-fn', at which
time some necessary variables are set."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "`org-node-new-by-roam-capture' is meant to be called indirectly via `org-node--create'")
    (unless (fboundp #'org-roam-capture-)
      (org-node--die "Didn't create node! Either install org-roam or %s"
                     "configure `org-node-creation-fn'"))
    (require 'org-roam)
    (org-roam-capture- :node (org-roam-node-create
                              :title org-node-proposed-title
                              :id    org-node-proposed-id))
    ;; REVIEW: redundant given save-hooks?
    (org-node-cache--scan-new-or-modified)))

(defun org-node-new-file ()
  "Create a file-level node.
Meant to be called indirectly as `org-node-creation-fn', during
which it gets some necessary variables."
  (if (or (null org-node-proposed-title)
          (null org-node-proposed-id))
      (message "org-node-new-file is meant to be called indirectly")
    (let* ((dir (org-node-guess-or-ask-dir "New file in which directory? "))
           (path-to-write (file-name-concat dir
                                            (funcall org-node-filename-fn
                                                     org-node-proposed-title))))
      (if (or (file-exists-p path-to-write)
              (find-buffer-visiting path-to-write))
          (message "A file or buffer already exists for path %s"
                   (file-name-nondirectory path-to-write))
        (mkdir (file-name-directory path-to-write) t)
        (find-file path-to-write)
        (if org-node-make-file-level-nodes
            (insert ":PROPERTIES:"
                    "\n:ID:       " org-node-proposed-id
                    "\n:END:"
                    "\n#+title: " org-node-proposed-title
                    "\n")
          (insert "* " org-node-proposed-title
                  "\n:PROPERTIES:"
                  "\n:ID:       " org-node-proposed-id
                  "\n:END:"
                  "\n"))
        (goto-char (point-max))
        (unwind-protect
            (run-hooks 'org-node-creation-hook)
          (save-buffer)
          (org-id-add-location org-node-proposed-id path-to-write) ;; Redundant
          (org-node-cache--scan-targeted (list path-to-write)))))))


;;; Commands

;;;###autoload
(defun org-node-find ()
  "Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-node-creation-fn' to `org-node-new-by-roam-capture'."
  (interactive)
  (org-node-cache-ensure)
  (let* ((input (completing-read "Node: " org-node-collection
                                 () () () 'org-node-hist))
         (node (gethash input org-node-collection)))
    (if node
        (org-node--goto node)
      (org-node--create input (org-id-new)))))

;;;###autoload
(defun org-node-insert-link (&optional region-as-initial-input)
  "Insert a link to one of your ID nodes.

To behave more exactly like org-roam's `org-roam-node-insert',
see `org-node-insert-link*' and its docstring.

Optional argument REGION-AS-INITIAL-INPUT t means behave like
`org-node-insert-link*'."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
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
         (input (completing-read "Node: "
                                 org-node-collection
                                 nil
                                 nil
                                 (if region-as-initial-input region-text nil)
                                 'org-node-hist))
         (node (gethash input org-node-collection))
         (id (if node (org-node-get-id node) (org-id-new)))
         (link-desc (or region-text
                        (and node
                             (let ((aliases (org-node-get-aliases node)))
                               (--first (string-search it input) aliases)))
                        (and node
                             (org-node-get-title node))
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
(defun org-node-insert-link* ()
  "Insert a link to one of your ID nodes.

Unlike `org-node-insert-link', emulate org-roam and paste any
selected region text into the minibuffer first.

To behave like org-roam when creating new nodes, set
`org-node-creation-fn' to `org-node-new-by-roam-capture'.

If you still find the behavior different, perhaps you had
something in `org-roam-post-node-insert-hook'.  Configure
`org-node-insert-link-hook' the same way."
  (interactive nil org-mode)
  (org-node-insert-link t))

;;;###autoload
(defun org-node-visit-random ()
  "Visit a random node."
  (interactive)
  (org-node-cache-ensure)
  (org-node--goto (nth (random (hash-table-count org-node-collection))
                       (hash-table-values org-node-collection))))

(define-obsolete-function-alias
  #'org-node-random #'org-node-visit-random "2024-07-07")

;;;###autoload
(defun org-node-insert-transclusion-as-subtree ()
  "Insert a link and a transclusion.

Result will basically look like:

** [[Note]]
#+transclude: [[Note]] :level 3

but adapt to the surrounding outline level.  I recommend
adding keywords to the things to exclude:

(setq org-transclusion-exclude-elements
      '(property-drawer comment keyword))"
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (error "Only works in org-mode buffers"))
  (org-node-cache-ensure)
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
      (goto-char (pos-bol))
      (insert (make-string (+ 1 level) ?\*) " ")
      (forward-line 1)
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level " (number-to-string (+ 2 level)))
      ;; If the target is a subtree rather than file-level node, I'd like to
      ;; cut out the initial heading because we already made a heading.  (And
      ;; we made the heading so that this transclusion will count as a
      ;; backlink, plus it makes more sense to me on export to HTML).
      ;;
      ;; Unfortunately the :lines trick would prevent
      ;; `org-transclusion-exclude-elements' from having an effect, and the
      ;; subtree's property drawer shows up!
      ;; TODO: Patch `org-transclusion-content-range-of-lines' to respect
      ;; `org-transclusion-exclude-elements', or make a different argument like
      ;; ":no-initial-heading"
      ;;
      ;; For now, just let it nest an extra heading. Looks odd, but doesn't
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
  (org-node-cache-ensure)
  (let ((node (gethash (completing-read "Node: " org-node-collection
                                        () () () 'org-node-hist)
                       org-node-collection)))
    (let ((id (org-node-get-id node))
          (title (org-node-get-title node))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id) title))
      (goto-char (pos-bol))
      (insert "#+transclude: ")
      (goto-char (pos-eol))
      (insert " :level " (number-to-string (+ 1 level))))))

;;;###autoload
(defun org-node-rename-file-by-title (&optional path)
  "Rename the current file according to `org-node-filename-fn'.

When called from Lisp, can take argument PATH to operate on the
file located there."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (unless path
    (setq path (buffer-file-name)))
  (unless (equal "org" (file-name-extension path))
    (user-error "File doesn't end in .org: %s" path))
  (let* ((title (or (cadar (org-collect-keywords '("TITLE")))
                    (save-excursion
                      (without-restriction
                        (goto-char 1)
                        (or (org-at-heading-p)
                            (outline-next-heading))
                        (org-get-heading t t t t)))))
         (name (file-name-nondirectory path))
         (new-path (file-name-concat (file-name-directory path)
                                     (funcall org-node-filename-fn title)))
         (visiting (find-buffer-visiting path))
         (visiting-on-window (and visiting (get-buffer-window visiting))))
    (if (equal path new-path)
        (message "Filename already correct: %s" path)
      (if (and visiting (buffer-modified-p visiting))
          (message "Unsaved file, letting it be: %s" path)
        (if (get-file-buffer new-path)
            (message "A buffer is already visiting the would-be new filename")
          (unless (file-writable-p path)
            (user-error "No permissions to rename file: %s" path))
          (unless (file-writable-p new-path)
            (user-error "No permissions to write a new file at: %s" new-path))
          ;; Unnecessary b/c `rename-file' will already warn, but hey
          (when (file-exists-p new-path)
            (user-error "Canceled because a file exists at: %s" new-path))
          ;; Kill buffer before renaming, because it will not follow the rename
          (when visiting
            (kill-buffer visiting))
          (rename-file path new-path)
          ;; Visit the file again if you had it open
          (when visiting
            (let ((buf (find-file-noselect new-path)))
              (when visiting-on-window
                (set-window-buffer visiting-on-window buf))))
          (message "File %s renamed to %s"
                   (file-name-nondirectory path)
                   (file-name-nondirectory new-path)))))))

;;;###autoload
(defun org-node-rewrite-links-ask (&optional files)
  "Search all files for ID-links where the link description has
gotten out of sync from the destination's current title.

At each link, prompt for user consent, then auto-update the link
so it matches the destination's current title."
  (interactive)
  (require 'ol)
  (require 'org-faces)
  (defface rewrite-face
    '((t :inverse-video (not (face-inverse-video-p 'org-link))
       :inherit 'org-link))
    "Face for use in `org-node-rewrite-links-ask'.")
  (org-node-cache-ensure)
  (when (org-node--consent-to-problematic-modes-for-mass-edit)
    (dolist (file (or files (org-node-files)))
      (with-current-buffer (find-file-noselect file)
        (save-excursion
          (without-restriction
            (goto-char (point-min))
            (while-let ((end (re-search-forward org-link-bracket-re nil t)))
              (let* ((beg (match-beginning 0))
                     (link (substring-no-properties (match-string 0)))
                     (exact-link (rx (literal link)))
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
                           (not (member-ignore-case
                                 desc (org-node-get-aliases node))))
                  (switch-to-buffer (current-buffer))
                  (org-reveal)
                  (recenter)
                  (highlight-regexp exact-link 'rewrite-face)
                  (unwind-protect
                      (setq answered-yes
                            (y-or-n-p
                             (format "Rewrite link? Will become: \"%s\""
                                     true-title)))
                    (unhighlight-regexp exact-link))
                  (when answered-yes
                    (goto-char beg)
                    (atomic-change-group
                      (delete-region beg end)
                      (insert (org-link-make-string target true-title)))
                    ;; Give user 110+ ms to glimpse the result before moving on
                    ;; so they have a chance to see if something went wildly
                    ;; wrong
                    (redisplay)
                    (sleep-for .11))
                  (goto-char end))))))))))

;; TODO: check what happens if Org invisible regions interfere
;;;###autoload
(defun org-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created file.

You may find it a common situation that the subtree had not yet
been assigned an ID nor any other property that you normally
assign to a proper node.  Thus, this creates an ID for you if
there was no ID, copies over any inherited tags, and runs
`org-node-creation-hook'.

Adding to that, see below for an example advice that copies any
inherited \"CREATED\" property, if an ancestor has such a
property.  It is subjective whether you'd want this behavior, but
it can be desirable if you know the subtree had been part of the
source file for ages so that you see the ancestor's creation-date
as more \"truthful\" than today's date.

(advice-add \\='org-node-extract-subtree :around
            (defun my-inherit-creation-date (orig-fn &rest args)
              (let ((inherited-creation-date
                     (save-excursion
                       (while (not (or (org-entry-get nil \"CREATED\")
                                       (bobp)))
                         (org-up-heading-or-point-min))
                       (org-entry-get nil \"CREATED\"))))
                (apply orig-fn args)
                ;; Now in the new buffer
                (org-entry-put nil \"CREATED\"
                               (or inherited-creation-date
                                   (format-time-string \"[%F %a]\")))))))"
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "This command expects an org-mode buffer"))
  (org-node-cache-ensure)
  (let ((dir (org-node-guess-or-ask-dir "Extract to new file in directory: ")))
    (save-excursion
      (when (org-invisible-p)
        (user-error "Better not run this command in an invisible region"))
      (org-back-to-heading t)
      (save-buffer)
      (when (org-invisible-p)
        (user-error "Better not run this command in an invisible region"))
      (let* ((tags (org-get-tags))
             (title (org-get-heading t t t t))
             (id (org-id-get-create))
             (boundary (save-excursion
                         (org-end-of-meta-data t)
                         (point)))
             ;; Why is category autocreated by `org-entry-properties'...  It's
             ;; an invisible property that's always present and usually not
             ;; interesting, unless user has entered some explicit value
             (explicit-category (save-excursion
                                  (when (search-forward ":category:" boundary t)
                                    (org-entry-get nil "CATEGORY"))))
             (properties (--filter (not (equal "CATEGORY" (car it)))
                                   (org-entry-properties nil 'standard)))
             (path-to-write (file-name-concat
                             dir (funcall org-node-filename-fn title)))
             (source-path buffer-file-name))
        (if (file-exists-p path-to-write)
            (message "A file already exists named %s" path-to-write)
          (org-cut-subtree)
          ;; Leave a link under the parent heading pointing to the subheading
          ;; that was extracted.
          (save-excursion
            (org-up-heading-safe)
            (when (org-at-heading-p)
              (org-end-of-meta-data t))
            (open-line 1)
            (insert (format-time-string
                     (format "[%s] Created " (car org-timestamp-formats)))
                    (org-link-make-string (concat "id:" id) title)
                    "\n"))
          (save-buffer)
          (find-file path-to-write)
          (org-paste-subtree)
          (save-buffer)
          (when org-node-make-file-level-nodes
            ;; Replace the root heading and its properties with file-level
            ;; keywords &c.
            (goto-char (point-min))
            (org-end-of-meta-data t)
            (kill-region (point-min) (point))
            (org-map-region #'org-promote (point-min) (point-max))
            (insert
             ":PROPERTIES:\n"
             (string-join (--map (concat ":" (car it) ": " (cdr it)) properties)
                          "\n")
             "\n:END:"
             (if explicit-category
                 (concat "\n#+category: " explicit-category)
               "")
             (if tags
                 (concat "\n#+filetags: :" (string-join tags ":") ":")
               "")
             "\n#+title: " title
             "\n"))
          (run-hooks 'org-node-creation-hook)
          (save-buffer)
          (org-node-cache--scan-targeted (list path-to-write source-path)))))))

;; Rough code, hope it works for everyone
;;;###autoload
(defun org-node-rename-asset-and-rewrite-links ()
  "Prompt for an asset such as an image file to be renamed, then
search recursively for Org files containing a link to that asset,
open a wgrep buffer of the search hits, and start an interactive
search-replace that updates the links.  After the user consents
to replacing all the links, finally rename the asset file itself."
  (interactive)
  (unless (fboundp 'wgrep-change-to-wgrep-mode)
    (user-error "This command requires the wgrep package"))
  (require 'wgrep)
  (let ((root (car (org-node--root-dirs (org-node-files))))
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
         ;; Interactive replaces
         (query-replace filename new)
         ;; NOTE: If the user quits the replaces with C-g, the following code
         ;;       never runs, which is good.
         (when (buffer-modified-p)
           (wgrep-finish-edit)
           (rename-file filename new)
           (message "File renamed from %s to %s" filename new))))
      (message "Waiting for rgrep to populate buffer..."))))

;;;###autoload
(defun org-node-insert-heading ()
  "Insert a heading with ID and run `org-node-creation-hook'."
  (interactive nil org-mode)
  (org-insert-heading)
  (org-node-nodeify-entry))

;;;###autoload
(defun org-nodeify-entry ()
  "Add an ID to entry at point and run `org-node-creation-hook'."
  (interactive nil org-mode)
  (org-node-cache-ensure)
  (org-id-get-create)
  (run-hooks 'org-node-creation-hook))

(defalias 'org-node-nodeify-entry 'org-nodeify-entry)

;;;###autoload
(defun org-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive nil org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED"
                   (concat "["
                           (format-time-string (car org-timestamp-formats))
                           "]"))))

;;;###autoload
(defun org-node-reset ()
  "Wipe and rebuild the cache."
  (interactive)
  (org-node-cache-ensure nil t))

;;;###autoload
(defun org-node-forget-dir (dir)
  "Remove references in `org-id-locations' to files in DIR.

Note that if DIR can be found under `org-node-extra-id-dirs',
this action may make no practical impact unless you also add DIR
to `org-node-extra-id-dirs-exclude'."
  (interactive "DForget all IDs in directory: ")
  (org-node-cache-ensure t)
  (let ((files (-intersection (directory-files-recursively dir "\\.org$")
                              (hash-table-values org-id-locations))))
    (when files
      (message "Forgetting all IDs in directory... (%s)" dir)
      (org-node--forget-id-locations files)
      (org-id-locations-save)
      (org-node-reset))))

;;;###autoload
(defun org-node-grep ()
  (interactive)
  (unless (fboundp #'consult--grep)
    (user-error "This command requires the consult package"))
  (require 'consult)
  (org-node-cache-ensure)
  (consult--grep "Grep across all files known to org-node"
                 #'consult--grep-make-builder
                 (org-node-files)
                 nil))

(define-derived-mode org-node-lint-results-mode tabulated-list-mode
  "Org-Lint Results"
  nil
  (setq tabulated-list-format
        [("File" 35 t)
         ("Line" 5 t)
         ("Trust" 5 t)
         ("Explanation" 100 t)])
  (tabulated-list-init-header))

(defvar org-node--linted nil
  "List of files linted so far.")

;;;###autoload
(defun org-node-lint-all-files ()
  "Run `org-lint' on all known Org files, and report results."
  (interactive)
  (org-node-cache-ensure t)
  (let* ((warnings nil)
         (report-buffer (get-buffer-create "*org-node lint report*"))
         ;; (files (-uniq (hash-table-values org-id-locations)))
         (files (-difference (org-node-files) org-node--linted))
         (ctr (length org-node--linted))
         (ctrmax (+ (length files) (length org-node--linted)))
         (entries nil))
    (with-current-buffer report-buffer
      (when (null files)
        ;; Reset
        (when (y-or-n-p "Wipe the previous lint results? ")
          (setq files (org-node-files))
          (setq org-node--linted nil)
          (setq tabulated-list-entries nil)
          (let ((inhibit-read-only t))
            (erase-buffer))))
      (org-node-lint-results-mode)
      (unwind-protect
          (dolist (file files)
            (message "Linting file... (you may quit and resume anytime) (%d/%d) %s"
                     (cl-incf ctr) ctrmax file)
            (org-node--with-quick-file-buffer file
              (setq warnings (org-lint)))
            (dolist (warning warnings)
              (let ((array (cadr warning)))
                (push (list warning
                            (vector
                             (list (file-name-nondirectory file)
                                   'face 'link
                                   'action `(lambda (_button)
                                              (find-file ,file)
                                              (goto-line
                                               ,(string-to-number (elt array 0))))
                                   'follow-link t)
                             (elt array 0)
                             (elt array 1)
                             (elt array 2)))
                      tabulated-list-entries)))
            (push file org-node--linted))
        (display-buffer report-buffer)
        (tabulated-list-print t t)
        (tabulated-list-sort 2))
      (when (null tabulated-list-entries)
        (message "All good, no lint warnings!")))))

(define-derived-mode org-node-list-feedback-arcs-mode tabulated-list-mode
  "Feedback Arcs List"
  nil
  (setq tabulated-list-format
        [("Node containing link" 39 t)
         ("Target of link" 78 t)])
  (add-hook 'tabulated-list-revert-hook #'org-node-list-feedback-arcs nil t)
  (tabulated-list-init-header))

(defun org-node-list-feedback-arcs ()
  "Show a feedback-arc-set of forward id-links.

Requires GNU R installed, with R packages tidyverse and igraph.

A feedback arc set is a set of links such that if they are all
cut (though sometimes it suffices to reverse the direction rather
than cut them), the remaining links in the network will
constitute a DAG (directed acyclic graph).

You may consider this as merely one of many ways to view your
network to quality-control it.  Rationale:

    https://edstrom.dev/zvjjm/slipbox-workflow#ttqyc"
  (interactive)
  (unless (executable-find "Rscript")
    (user-error
     "This command requires GNU R, with R packages tidyverse and igraph"))
  (let ((r-script (org-node-worker--tmpfile "analyze_feedback_arcs.R"))
        (digraph-tsv (org-node-worker--tmpfile "id_node_digraph.tsv")))
    (write-region
     "library(stringr)
library(readr)
library(igraph)

tsv <- commandArgs(TRUE)[1]

g <- graph_from_data_frame(
  read_tsv(tsv),
  directed = TRUE
)

fas1 <- feedback_arc_set(g, algo = \"approx_eades\")
fas1

lisp_data <- str_c(\"(\\\"\", as_ids(fas1), \"\\\")\") |>
  str_replace(\"\\\\|\", \"\\\" . \\\"\") |>
  str_flatten(\"\n \") |>
  (function(x) {
    str_c(\"(\", x, \")\")
  })()

write_file(lisp_data, file.path(dirname(tsv), \"feedback-arcs.eld\"))
" nil r-script)
    (write-region (org-node--make-digraph-tsv-string)
                  nil
                  digraph-tsv)
    (with-current-buffer (get-buffer-create "*feedback arcs*")
      (fundamental-mode)
      (setq-local buffer-read-only nil)
      (erase-buffer)
      (unless (= 0 (call-process "Rscript" nil t nil r-script digraph-tsv))
        (error "%s" (buffer-string)))
      (erase-buffer)
      (insert-file-contents (org-node-worker--tmpfile "feedback-arcs.eld"))
      (setq feedbacks (read (buffer-string)))
      (when (listp feedbacks)
        (erase-buffer)
        (org-node-list-feedback-arcs-mode)
        (setq tabulated-list-entries
              (cl-loop
               for (origin . dest) in feedbacks
               collect
               (list
                (cons origin dest)
                (vector
                 (list (org-node-get-title (gethash origin org-nodes))
                       'face 'link
                       'action `(lambda (_button)
                                  (org-node--goto ,(gethash origin org-nodes)))
                       'follow-link t)
                 (list (org-node-get-title (gethash dest org-nodes))
                       'face 'link
                       'action `(lambda (_button)
                                  (org-node--goto ,(gethash dest org-nodes)))
                       'follow-link t)))))
        (tabulated-list-print))
      (display-buffer (current-buffer)))))

(defun org-node--make-digraph-tsv-string ()
  "From `org-node--links', generate a list of
destination-origin pairings, expressed as tab-separated values."
  (concat
   "src\tdest\n"
   (string-join
    (-uniq (cl-loop
            for dest being the hash-keys of org-node--links
            using (hash-values links)
            append (cl-loop
                    for link in links
                    collect (concat dest "\t" (plist-get link :origin)))))
    "\n")))

;; hmmmm uhhhh a variable for this seems maybe unnecessary
(defcustom org-node-completion-everywhere nil
  ""
  :group 'org-node
  :type 'string)

(defun org-node-complete-at-point ()
  (when (and ;; org-node-completion-everywhere
         (derived-mode-p 'org-mode)
         (thing-at-point 'word)
         (not (org-in-src-block-p))
         (not (save-match-data (org-in-regexp org-link-any-re))))
    (let ((bounds (bounds-of-thing-at-point 'word)))
      (list (car bounds)
            (cdr bounds)
            (hash-table-keys org-node-collection-actually-links)
            :exit-function
            (lambda (text _)
              (message "BOCA BOCA BOCA %s" text)
              (let ((node (gethash text org-node-collection-actually-links)))
                (when node
                  (atomic-change-group
                    (delete-char (- (length text)))
                    (insert (org-link-make-string
                             (concat "id:" (org-node-get-id node))
                             text))))))
            :exclusive 'no
            ))))

;;(add-hook 'completion-at-point-functions #'org-node-complete-at-point)

(provide 'org-node)

;;; org-node.el ends here
