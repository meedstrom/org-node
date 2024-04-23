;;; org-id-node.el --- Use org-id locations as a big pile of notes -*- lexical-binding: t; -*-

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
;; Package-Requires: ((emacs "29.1") (pcre2el "1.12") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-id-node

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Do you have a love-hate relationship with org-roam v2?  This package can
;; work as an alternative way to access and work with your notes, or it can
;; replace org-roam if you did not need all its features.
;;
;; Requires ripgrep installed.
;;
;; The main commands can use your org-roam-capture-templates if configured:
;;
;; - `org-id-node-find'
;; - `org-id-node-insert-link'
;;
;; To make these commands as fast as can be, enable `org-id-node-cache-mode'.
;;
;; Later on, AFTER you read and understood the readme, there is also
;; `org-id-node-butler-mode'.  Read the readme to know what it does.

;;; Code:

;; Good to do soon
;;
;; TODO Ensure that ROAM_ALIASES are always wrapped in quotes
;; TODO Test each user commmand again
;; TODO Annotations for completion

;; Other todos
;;
;; TODO Test with plain org-capture
;; TODO Write example capture template
;; TODO Maybe move user commands into a -commands.el and minimize the core
;; TODO Command to grep across all files (then deprecate the regret command, and teach the user wgrep)
;; TODO Command to visit a random node
;; TODO Command to insert link into :BACKLINKS:
;; TODO Avoid double-counting links inside :BACKLINKS:
;; TODO React to any org-element-cache error (they're common) and disable the cache during fix-all

;; Stretch todos
;;
;; TODO Command to explore feedback arc sets
;; TODO Bit of a test suite

(require 'org-id-node-common)
(require 'org-id-node-cache)

;;;###autoload
(defun org-id-node-enable ()
  "Designed for `org-mode-hook' and will remove itself."
  (require 'org-id-node-butler)
  (remove-hook 'org-mode-hook #'org-id-node-enable)
  (org-id-node-butler-mode)
  (org-id-node-cache-mode))

(defcustom org-id-node-slug-fn #'org-id-node-slugify-as-url
  "Function to transform title into a filename.

Built-in choices:
- `org-id-node-slugify-as-url'
- `org-id-node-slugify-like-roam'
"
  :group 'org-id-node
  :type 'function)

(defcustom org-node-creation-fn #'org-node-creator-backend-basic
  "Function called by `org-id-node-find', and `org-id-node-insert-link' to
create a node.  During execution, two variables are set:
`org-id-node-proposed-title' and `org-id-node-proposed-id'.

Some options
- `org-node-creator-backend-basic'
- `org-node-creator-backend-by-roam-capture'
- `org-capture'
"
  :group 'org-id-node
  :type 'function)

(defcustom org-id-node-insert-link-hook ()
  "Hook run after inserting a link to an Org-ID node.

Two arguments provided: the ID and the link description.

Functions here should not leave point outside the link."
  :group 'org-id-node
  :type 'hook)

(defcustom org-id-node-creation-hook '(org-id-node-put-created)
  "Hook run with point in the newly created buffer or entry.

Applied only by `org-id-node-creator-backend-basic',
`org-id-node-create-subtree' and `org-id-node-extract-subtree'.

For `org-id-node-creator-backend-by-roam-capture', you want the
hook `org-roam-capture-new-node-hook' instead.

A good member for this hook is `org-id-node-put-created', especially
since the default `org-id-node-slug-fn' does not put a timestamp in
the filename.
"
  :type 'hook
  :group 'org-id-node)


;;; Plumbing

(defvar org-id-node-hist nil
  "Minibuffer history.")

(defun org-id-node--visit-get-true-heading (node)
  "Visit subtree NODE and get the heading, in a way that's aware of
buffer-local #+todo settings so the todo state is not taken as part
of the heading."
  (unless (plist-get node :is-subtree)
    (error "Not a subtree node %s" node))
  (org-with-file-buffer (plist-get node :file-path)
    (save-excursion
      (without-restriction
        (goto-char 1)
        (forward-line (plist-get node :line-number))
        (nth 4 (org-heading-components))))))

(defun org-id-node-slugify-like-roam (title)
  "From TITLE, make a filename in the default org-roam style."
  (unless (fboundp #'org-roam-node-slug)
    (user-error
     "Didn't create node! Install org-roam or configure `org-id-node-slug-fn'"))
  (require 'org-roam-node)
  (concat (format-time-string "%Y%m%d%H%M%S-")
          (org-roam-node-slug (org-roam-node-create :title title))
          ".org"))

(defun org-id-node-slugify-as-url (title)
  "From TITLE, make a filename suitable as URL component.

A title like \"Löb's Theorem\" becomes \"lobs-theorem.org\".
Note that it retains Unicode symbols classified as alphabetic
or numeric, so for example kanji and Greek letters remain.

As a surprise, it does NOT preface the name with a timestamp like
many zettelkasten packages do.  If you want that, you can use
a small wrapper such as:

(setq org-id-node-slug-fn (lambda (title)
                        (concat (format-time-string \"%Y%m%d%H%M%S-\")
                                (org-id-node-slugify-as-url title))))"
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

;; (org-id-node-slugify-as-url "A/B testing")
;; (org-id-node-slugify-as-url "\"But there's still a chance, right?\"")
;; (org-id-node-slugify-as-url "Löb's Theorem")
;; (org-id-node-slugify-as-url "How to convince me that 2 + 2 = 3")
;; (org-id-node-slugify-as-url "C. S. Peirce")
;; (org-id-node-slugify-as-url "Amnesic recentf, org-id-locations? Solution: Run kill-emacs-hook periodically.")
;; (org-id-node-slugify-as-url "Slimline/\"pizza box\" computer chassis")
;; (org-id-node-slugify-as-url "#emacs")


;;; Shim for the org-roam buffer

;; To make `org-roam-buffer-toggle' work, eval this:
;; (advice-add 'org-roam-backlinks-get :override #'org-id-node--fabricate-roam-backlinks)

(defun org-id-node--fabricate-roam-object (node)
  "Construct an org-roam-node object from NODE.
This just uses information in org-id-node's own `org-id-nodes'.
Some of the fields are blank."
  (require 'org-roam)
  (org-roam-node-create
   :file (plist-get node :file-path)
   :id (plist-get node :id)
   :level (plist-get node :level)
   :title (plist-get node :title)
   :tags (plist-get node :tags)
   :aliases (plist-get node :aliases)
   :point (org-id-node--visit-get-pos node)))

(defun org-id-node--fabricate-roam-backlinks (roam-object &rest _)
  "Return org-roam-backlink objects targeting ROAM-OBJECT.

A drop-in replacement for `org-roam-backlinks-get', but ignores
its UNIQUE argument because org-id-node does not track multiple
backlinks from the same node anyway: literally all the
information comes from the CACHED_BACKLINKS property, which is
deduplicated, as if :unique t."
  (require 'org-roam)
  (let ((node (gethash (org-roam-node-id roam-object) org-id-nodes)))
    (when node
      (cl-loop
       for backlink-id in (plist-get node :backlink-ids)
       as node-that-contains-link = (gethash backlink-id org-id-nodes)
       when node-that-contains-link
       collect (let ((roam-object-that-contains-link
                      (org-id-node--fabricate-roam-object node-that-contains-link)))
                 (org-roam-backlink-create
                  :target-node roam-object
                  :source-node roam-object-that-contains-link
                  ;; Differs from upstream.  Supposed to be position of the
                  ;; link, but we just give it the position of the subtree!
                  ;; In other words, it's as if a link sits inside the heading.
                  :point (org-roam-node-point roam-object-that-contains-link)
                  :properties nil))))))


;;; API extras
;; Conveniences for users. Avoided inside this package.

(defun org-id-node-at-point (&optional _point)
  (gethash (org-id-get nil nil nil t) org-id-nodes))

(defun org-id-node-read ()
  (gethash (completing-read "Node: " org-id-node-collection
                            () () () 'org-id-node-hist)
           org-id-node-collection))


;;; Commands

;;;###autoload
(defun org-id-node-find ()
  "Select and visit one of your ID nodes.

To behave like `org-roam-node-find' when creating new nodes, set
`org-id-node-creation-fn' to
`org-node-creator-backend-by-roam-capture'."
  (interactive)
  (org-id-node-cache-ensure-fresh)
  (let* ((input (completing-read "Node: " org-id-node-collection
                                 () () () 'org-id-node-hist))
         (node (gethash input org-id-node-collection)))
    (if node
        (progn
          (find-file (plist-get node :file-path))
          (widen)
          (goto-char 1)
          (forward-line (1- (plist-get node :line-number)))
          (recenter 5))
      (setq org-id-node-proposed-title input)
      (setq org-id-node-proposed-id (org-id-new))
      (let ((result (funcall org-id-node-creation-fn)))
        (when (bufferp result)
          (switch-to-buffer result)))
      (setq org-id-node-proposed-title nil)
      (setq org-id-node-proposed-id nil))))

(defvar org-id-node-proposed-title nil)
(defvar org-id-node-proposed-id nil)

;; Just an example...
(defun org-id-node-capture-target ()
  "Can be used as TARGET in `org-capture-templates'."
  (let* ((dir (read-directory-name
               "Where to create the node? "
               (car (org-id-node--root-dirs (hash-table-values org-id-locations)))))
         (path-to-write (file-name-concat dir (funcall org-id-node-slug-fn
                                                       org-id-node-proposed-title))))
    (find-file path-to-write)
    (goto-char (point-max))
    (if (org-entry-get nil "ID")
        (user-error "Already an ID in this location")
      (org-entry-put nil "ID" org-id-node-proposed-id))
    (run-hooks 'org-id-node-creation-hook)))

(defun org-id-node-creator-backend-by-roam-capture ()
  "Call `org-roam-capture-' with predetermined arguments."
  (unless (fboundp #'org-roam-capture-)
    (org-id-node-die "Didn't create node! Either install org-roam or %s"
                     "configure `org-id-node-creation-fn'"))
  (require 'org-roam)
  (org-roam-capture- :node (org-roam-node-create :title org-id-node-proposed-title
                                                 :id    org-id-node-proposed-id)))

(defun org-id-node-creator-backend-basic ()
  "Create a file-level node and ask where to save it."
  (let* ((dir (read-directory-name
               "Where to create the node? "
               (car (org-id-node--root-dirs (hash-table-values org-id-locations)))))
         (path-to-write (file-name-concat dir (funcall org-id-node-slug-fn
                                                       org-id-node-proposed-title))))
    (if (or (file-exists-p path-to-write)
            (find-buffer-visiting path-to-write))
        (message "A file or buffer already exists for path %s"
                 (file-name-nondirectory path-to-write))
      (let ((buf (find-file-noselect path-to-write)))
        (with-current-buffer buf
          (insert ":PROPERTIES:"
                  "\n:ID:       " org-id-node-proposed-id
                  "\n:END:"
                  "\n#+title: " org-id-node-proposed-title
                  "\n")
          (run-hooks 'org-id-node-creation-hook)
          (save-buffer))
        buf))))

;;;###autoload
(defun org-id-node-insert-link ()
  "Insert a link to one of your ID nodes.

To behave like `org-roam-node-insert' when creating new nodes,
set `org-id-node-creation-fn' to
`org-id-node-create-by-roam-capture'.

If you find the behavior different, perhaps you have something in
`org-roam-post-node-insert-hook'.  Then perhaps copy it to
`org-id-node-insert-link-hook'."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-id-node-cache-ensure-fresh)
  (let* ((beg nil)
         (end nil)
         (region-text (when (region-active-p)
                        (setq end (region-end))
                        (goto-char (region-beginning))
                        (skip-chars-forward "[:space:]")
                        (setq beg (point))
                        (goto-char end)
                        (skip-chars-backward "[:space:]")
                        (setq end (point))
                        (org-link-display-format
                         (buffer-substring-no-properties beg end))))
         (input (completing-read "Node: " org-id-node-collection
                                 () () () 'org-id-node-hist))
         (node (gethash input org-id-node-collection))
         (id (or (plist-get node :id) (org-id-new)))
         (link-desc (or region-text (plist-get node :title) input)))
    (atomic-change-group
      (if region-text
          (delete-region beg end)
        ;; Try to strip the todo keyword, looking up what counts as todo syntax
        ;; in the target file.  Fail silently because it matters little.
        (ignore-errors
          (setq link-desc (org-id-node--visit-get-true-heading node))))
      (insert (org-link-make-string (concat "id:" id) link-desc))
      (run-hook-with-args 'org-id-node-insert-link-hook id link-desc))
    ;; Node doesn't exist yet, create it

    ;; TODO: Maybe let the creater change the proposed-id, and insert the link
    ;; after
    (unless node
      ;; In the event someone's creation-fn moves point
      (save-excursion
        (setq org-id-node-proposed-title input)
        (setq org-id-node-proposed-id id)
        (condition-case err
            (funcall org-id-node-creation-fn)
          ((t debug error)
           (setq org-id-node-proposed-title nil)
           (setq org-id-node-proposed-id nil)
           (signal (car err) (cdr err)))
          (:success
           (setq org-id-node-proposed-title nil)
           (setq org-id-node-proposed-id nil)))))))

;;;###autoload
(defun org-id-node-insert-transclusion-as-subtree ()
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
  (org-id-node-cache-ensure-fresh)
  (let ((node (gethash (completing-read "Node: " org-id-node-collection
                                        () () () 'org-id-node-hist)
                       org-id-node-collection)))
    (let ((id (plist-get node :id))
          (title (plist-get node :title))
          (level (or (org-current-level) 0))
          (m1 (make-marker)))
      (insert (org-link-make-string (concat "id:" id title)))
      (set-marker m1 (1- (point)))
      (duplicate-line)
      (goto-char (line-beginning-position))
      (insert (make-string (+ 1 level) ?\*) " ")
      (forward-line 1)
      (insert "#+transclude: ")
      (goto-char (line-end-position))
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
      (run-hook-with-args 'org-id-node-insert-link-hook id title))))

;;;###autoload
(defun org-id-node-insert-include ()
  "Insert an #+include: referring to a node."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-id-node-cache-ensure-fresh)
  (let ((node (gethash (completing-read "Node: " org-id-node-collection
                                        () () () 'org-id-node-hist)
                       org-id-node-collection)))
    (let ((id (plist-get node :id))
          (title (plist-get node :title))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id title)))
      (goto-char (line-beginning-position))
      (insert "#+include: ")
      (forward-char 1)
      (run-hook-with-args 'org-id-node-insert-link-hook id title))))

;;;###autoload
(defun org-id-node-insert-transclusion ()
  "Insert a #+transclude: referring to a node."
  (interactive nil org-mode)
  (unless (derived-mode-p 'org-mode)
    (user-error "Only works in org-mode buffers"))
  (org-id-node-cache-ensure-fresh)
  (let ((node (gethash (completing-read "Node: " org-id-node-collection
                                        () () () 'org-id-node-hist)
                       org-id-node-collection)))
    (let ((id (plist-get node :id))
          (title (plist-get node :title))
          (level (or (org-current-level) 0)))
      (insert (org-link-make-string (concat "id:" id title)))
      (goto-char (line-beginning-position))
      (insert "#+transclude: ")
      (goto-char (line-end-position))
      (insert " :level " (number-to-string (+ 1 level)))
      (forward-char -10)
      (run-hook-with-args 'org-id-node-insert-link-hook id title))))

;;;###autoload
(defun org-id-node-rename-file-by-title (&optional path)
  "Rename the current file according to `org-id-node-slug-fn'.
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
                           (funcall org-id-node-slug-fn title)
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

(defface org-id-node-rewrite-links-face
  '((t :inherit 'org-link))
  "Face for use in `org-id-node-rewrite-links-ask'.")

;;;###autoload
(defun org-id-node-rewrite-links-ask ()
  "Look for links to update to match the current title.
Prompt the user for each one."
  (interactive)
  (require 'ol)
  (org-id-node-cache-ensure-fresh)
  ;; (set-face-attribute 'org-id-node-rewrite-links-face nil
  ;;                     :inverse-video (face-inverse-video-p 'org-link)
  ;;                     :foreground (face-background 'default)
  ;;                     :background (face-foreground 'org-link))
  (set-face-inverse-video 'org-id-node-rewrite-links-face
                          (not (face-inverse-video-p 'org-link)))
  (when auto-save-visited-mode
    (when (yes-or-no-p "Disable auto-save-visited-mode? (recommended)")
      (auto-save-visited-mode 0)))
  (dolist (file (hash-table-values org-id-locations))
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
               (node (when id
                       (gethash id org-id-nodes)))
               (true-title (when node
                             (plist-get node :title)))
               (answered-yes nil))
          (when (and node
                     (not (string-equal-ignore-case desc true-title))
                     (not (member-ignore-case desc
                                              (plist-get node :aliases))))
            (switch-to-buffer (current-buffer))
            (org-reveal)
            (recenter)
            (highlight-regexp (rx (literal link)) 'org-id-node-rewrite-links-face)
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
              (sleep-for .11)))))))
  (when (yes-or-no-p "Save the edited buffers?")
    (save-some-buffers)))

;;;###autoload
(defun org-id-node-extract-subtree ()
  "Extract subtree at point into a file of its own.
Leave a link in the source file, and show the newly created buffer.

You may find it a common situation that the subtree had not yet
been assigned an ID or any other property that you normally
assign.  Thus, this creates an ID for you, copies over
any inherited tags, and runs `org-id-node-creation-hook'.

Adding to that, here is an example advice to copy any inherited
\"CREATED\" property, if an ancestor has such a property:

(advice-add 'org-id-node-extract-subtree :around
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
  (org-id-node--init-org-id-locations-or-die)
  (let ((dir (read-directory-name
              "Where to create the node? "
              (car (org-id-node--root-dirs (hash-table-values org-id-locations))))))
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
                             dir (funcall org-id-node-slug-fn title))))
        (if (file-exists-p path-to-write)
            (message "A file already exists named %s"
                     (file-name-nondirectory path-to-write))
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
          (run-hooks 'org-id-node-creation-hook)
          (save-buffer))))))

;; FIXME WIP
;;;###autoload
(defun org-id-node-rename-asset-and-rewrite-links ()
  (interactive)
  (require 'dash)
  (require 'wgrep)
  (when (or (equal default-directory org-roam-directory)
            (when (yes-or-no-p "Not in org-roam-directory, go there?")
              (find-file org-roam-directory)
              t))
    (when-let ((bufs (--filter (string-search "*grep*" (buffer-name it))
                               (buffer-list))))
      (when (yes-or-no-p "Some *grep* buffers, kill to be sure this works?")
        (mapc #'kill-buffer bufs)))
    (let* ((filename (file-relative-name
                      (read-file-name "File: ") org-roam-directory))
           (new (read-string "New name: " filename)))
      (mkdir (file-name-directory new) t)
      (unless (file-writable-p new)
        (error "New path wouldn't be writable"))
      (rgrep (regexp-quote filename) "*.org")
      (run-with-timer
       1 nil
       (lambda ()
         (save-window-excursion
           (delete-other-windows)
           (switch-to-buffer (--find (string-search "*grep*" (buffer-name it))
                                     (buffer-list)))
           (wgrep-change-to-wgrep-mode)
           (goto-char (point-min))
           (query-replace filename new)
           (wgrep-finish-edit)
           (when (y-or-n-p "Finished editing links, rename file?")
             (rename-file filename new)
             (message "File renamed from %s to %s" filename new)))))
      (message "Waiting for rgrep to populate buffer..."))))

;;;###autoload
(defun org-id-node-create-subtree ()
  (interactive nil org-mode)
  (org-insert-heading)
  (org-id-node-nodeify-entry))

;;;###autoload
(defun org-id-node-nodeify-entry ()
  "Add an ID to entry at point and run `org-id-node-creation-hook'."
  (interactive nil org-mode)
  (org-id-node--init-org-id-locations-or-die)
  (org-id-get-create)
  (run-hooks 'org-id-node-creation-hook))

;;;###autoload
(defun org-id-node-put-created ()
  "Add a CREATED property to entry at point, if none already."
  (interactive nil org-mode)
  (unless (org-entry-get nil "CREATED")
    (org-entry-put nil "CREATED" (format-time-string "[%F]"))))

(provide 'org-id-node)

;;; org-id-node.el ends here
