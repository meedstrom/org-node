;;; org-node-fakeroam.el --- Stand-ins for org-roam-autosync-mode -*-  no-byte-compile: t;  no-native-compile: t; lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "28.1") (compat "30") (org-node "0.6.0.50-git") (org-roam "2.2.2") (emacsql "4.0.0"))
;; URL:              https://github.com/meedstrom/org-node

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode'.

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'org-node)
(require 'org-node-changes)
(if (require 'org-roam nil t)
    (require 'emacsql)
  (message "Install org-roam to use org-node-fakeroam library"))

;; TODO: Remove after most users have switched to the Melpa recipes
(defun org-node-fakeroam--check-compile ()
  "Compile all fakeroam functions if not compiled.

This is transitional.  Org-node used to be a single package which
did not depend explicitly on org-roam, which meant it had to
defer compiling org-node-fakeroam.el until runtime after ensuring
that org-roam is available."
  (if (not (require 'org-roam nil t))
      (user-error "Install org-roam to use org-node-fakeroam library")
    ;; NOTE: Do not native-compile, it would be a slow init every time
    (mapc #'byte-compile
          '(org-node-fakeroam--run-without-fontifying
            org-node-fakeroam--accelerate-get-contents
            org-node-fakeroam--mk-node
            org-node-fakeroam--mk-backlinks
            org-node-fakeroam--mk-reflinks
            org-node-fakeroam--db-update-files
            org-node-fakeroam--db-add-file-level-data
            org-node-fakeroam--db-add-node
            org-node-fakeroam-db-rebuild
            org-node-fakeroam-list-files
            org-node-fakeroam-list-dailies
            org-node-fakeroam-daily-note-p
            org-node-fakeroam-daily-create))))

;;;###autoload
(define-minor-mode org-node-fakeroam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off.

See also `org-node-fakeroam-fast-render-mode'.

-----"
  :global t
  :group 'org-node
  (when (require 'org-roam nil t)
    (if org-node-fakeroam-redisplay-mode
        (progn
          (org-node-fakeroam--check-compile)
          (unless org-node-cache-mode
            (message "`org-node-fakeroam-redisplay-mode' may show stale previews without `org-node-cache-mode' enabled"))
          (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
          (dolist (buf (org-buffer-list 'files t))
            (with-current-buffer buf
              (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))))
      (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
      (unless org-roam-db-autosync-mode
        (dolist (buf (org-buffer-list 'files t))
          (with-current-buffer buf
            (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t)))))))

;;;###autoload
(define-minor-mode org-node-fakeroam-fast-render-mode
  "Advise the Roam buffer to be faster.

1. Make the buffer build faster by nullifying certain Org options
   inside the context previews.

2. Cache the previews, so that there is less or no lag the next
   time the same nodes are visited.

3. Persist this cache on disk.

-----"
  :global t
  :group 'org-node
  (when (require 'org-roam nil t)
    (if org-node-fakeroam-fast-render-mode
        (progn
          (org-node-fakeroam--check-compile)
          ;; Undo a thing done by old versions of this package
          (when (boundp 'savehist-additional-variables)
            (delete 'org-node--file<>previews savehist-additional-variables)
            (delete 'org-node--file<>mtime savehist-additional-variables))
          ;; Relying only on `kill-emacs-hook' is always a mistake
          (run-with-idle-timer 60 t (lambda ()
                                      (persist-save 'org-node--file<>previews)
                                      (persist-save 'org-node--file<>mtime)))
          (advice-add #'org-roam-preview-get-contents :around
                      #'org-node-fakeroam--accelerate-get-contents)
          (advice-add #'org-roam-node-insert-section :around
                      #'org-node-fakeroam--run-without-fontifying))
      (advice-remove #'org-roam-preview-get-contents
                     #'org-node-fakeroam--accelerate-get-contents)
      (advice-remove #'org-roam-node-insert-section
                     #'org-node-fakeroam--run-without-fontifying))))

(defun org-node-fakeroam--accelerate-get-contents (orig-fn file pt)
  "Designed as around-advice for `org-roam-preview-get-contents'.

Normally the first time you open an org-roam buffer, Emacs hangs
for as long as a minute on a slow machine when huge files are
involved.  This may eliminate most of that.

Aside from huge files, it is also slow when there are backlinks
coming from from extremely many files.  To deal with that, this
caches all results so that it should only be slow the first time.

Argument ORIG-FN is presumably `org-roam-preview-get-contents',
which see for FILE and PT."
  (if-let ((cached (alist-get pt (gethash file org-node--file<>previews))))
      cached
    (setf (alist-get pt (gethash file org-node--file<>previews))
          (let ((org-inhibit-startup t))
            (delay-mode-hooks
              ;; NOTE: We cannot use `org-roam-fontify-like-in-org-mode' since
              ;;       it is temporarily overridden by
              ;;       `org-node-fakeroam--run-without-fontifying' at the time
              ;;       this runs.  But that's OK; it looks outdated.
              (org-fontify-like-in-org-mode (funcall orig-fn file pt)))))))

(defun org-node-fakeroam--run-without-fontifying (orig-fn &rest args)
  "Intended as around-advice for `org-roam-node-insert-section'.
Run ORIG-FN with ARGS, while overriding
`org-roam-fontify-like-in-org-mode' so it does nothing."
  (when (require 'org-roam nil t)
    (cl-letf (((symbol-function 'org-roam-fontify-like-in-org-mode) #'identity))
      (apply orig-fn args))))

;; Just an useful command
(defun org-node-fakeroam-show-roam-buffer ()
  "Display an org-roam buffer or refresh an already visible one.
To reiterate: if it was not visible, only bring it up for
display, do NOT also refresh it.  Leave that for the second time
the user invokes the command."
  (interactive nil org-mode org-roam-mode)
  (when (require 'org-roam nil t)
    (if (derived-mode-p 'org-roam-mode)
        (org-roam-buffer-refresh)
      (pcase (org-roam-buffer--visibility)
        ('visible (and (derived-mode-p 'org-mode)
                       (org-roam-buffer-persistent-redisplay)))
        ((or 'exists 'none)
         (display-buffer (get-buffer-create org-roam-buffer)))))))


;;;; JIT method
;; Fabricate knockoff Roam backlinks in real time, so that a DB is not needed
;; to display the Roam buffer

(org-node-changes--def-whiny-alias org-node-fakeroam-nosql-mode
                                   org-node-fakeroam-jit-backlinks-mode
                                   nil "2024-08-18" "2024-09-30")

;;;###autoload
(define-minor-mode org-node-fakeroam-jit-backlinks-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed, and you can delete org-roam.db if you do
not need it for other things.

-----"
  :global t
  :group 'org-node
  (when (require 'org-roam nil t)
    (if org-node-fakeroam-jit-backlinks-mode
        (progn
          (org-node-fakeroam--check-compile)
          (unless org-node-cache-mode
            (message "`org-node-fakeroam-jit-backlinks-mode' will do poorly without `org-node-cache-mode'"))
          (advice-add 'org-roam-backlinks-get :override
                      #'org-node-fakeroam--mk-backlinks)
          (advice-add 'org-roam-reflinks-get :override
                      #'org-node-fakeroam--mk-reflinks))
      (advice-remove 'org-roam-backlinks-get #'org-node-fakeroam--mk-backlinks)
      (advice-remove 'org-roam-reflinks-get #'org-node-fakeroam--mk-reflinks))))

(defun org-node-fakeroam--mk-node (node)
  "Make an org-roam-node object from org-node object NODE."
  (when (require 'org-roam nil t)
    (org-roam-node-create
     :file (org-node-get-file-path node)
     :id (org-node-get-id node)
     :olp (org-node-get-olp node)
     :scheduled (when-let ((scheduled (org-node-get-scheduled node)))
                  (format-time-string
                   "%FT%T%z"
                   (encode-time (org-parse-time-string scheduled))))
     :deadline (when-let ((deadline (org-node-get-deadline node)))
                 (format-time-string
                  "%FT%T%z"
                  (encode-time (org-parse-time-string deadline))))
     :level (org-node-get-level node)
     :title (org-node-get-title node)
     :file-title (org-node-get-file-title node)
     :tags (org-node-get-tags node)
     :aliases (org-node-get-aliases node)
     :todo (org-node-get-todo node)
     :refs (org-node-get-refs node)
     :point (org-node-get-pos node)
     :priority (org-node-get-priority node)
     :properties (org-node-get-properties node))))

(defun org-node-fakeroam--mk-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-backlinks-get'."
  (when (require 'org-roam nil t)
    (let* ((target-id (org-roam-node-id target-roam-node))
           (links (gethash target-id org-node--dest<>links)))
      (cl-loop
       for link in links
       as src-id = (org-node-link-origin link)
       as src-node = (gethash src-id org-node--id<>node)
       when src-node
       collect (org-roam-backlink-create
                :target-node target-roam-node
                :source-node (org-node-fakeroam--mk-node src-node)
                :point (org-node-link-pos link)
                :properties (org-node-link-properties link))))))

(defun org-node-fakeroam--mk-reflinks (target-roam-node &rest _)
  "Make org-roam-reflink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-reflinks-get'."
  (when (require 'org-roam nil t)
    (let* ((target-id (org-roam-node-id target-roam-node))
           (node (gethash target-id org-node--id<>node)))
      (when node
        (cl-loop
         for ref in (org-node-get-refs node)
         append (cl-loop
                 for link in (gethash ref org-node--dest<>links)
                 as src-id = (org-node-link-origin link)
                 as src-node = (gethash src-id org-node--id<>node)
                 when src-node
                 collect (org-roam-reflink-create
                          :ref (org-node-link-dest link)
                          :source-node (org-node-fakeroam--mk-node src-node)
                          :point (org-node-link-pos link)
                          :properties (org-node-link-properties link))))))))


;;;; Feed method: supply data to Roam's DB

;;;###autoload
(define-minor-mode org-node-fakeroam-db-feed-mode
  "Supply data to the org-roam SQLite database on save.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-db-feed-mode
      (progn
        (org-node-fakeroam--check-compile)
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-db-feed-mode' will do nothing without `org-node-cache-mode'"))
        (add-hook 'org-node-rescan-functions #'org-node-fakeroam--db-update-files)
        (add-hook 'kill-emacs-hook #'org-roam-db--close-all))

    (remove-hook 'org-node-rescan-functions #'org-node-fakeroam--db-update-files)
    (unless org-roam-db-autosync-mode
      (remove-hook 'kill-emacs-hook #'org-roam-db--close-all))))

;; Purpose-focused alternative to `org-node-fakeroam-db-rebuild'
;; because that is not instant.
;; FIXME: Still too damn slow on a file with 400 nodes.  Profiler says most of
;;        it is in EmacSQL, maybe some SQL PRAGMA settings would fix?
;;        Or collect all data for one mega `emacsql' call?
(defun org-node-fakeroam--db-update-files (files)
  "Update the Roam DB about nodes and links involving FILES."
  (when (require 'org-roam nil t)
    (emacsql-with-transaction (org-roam-db)
      (dolist (file files)
        (org-roam-db-query [:delete :from files :where (= file $s1)]
                           file))
      (let (already)
        (cl-loop
         for node being the hash-values of org-nodes
         as file = (org-node-get-file-path node)
         when (member file files)
         do
         (unless (member file already)
           (push file already)
           (org-node-fakeroam--db-add-file-level-data node))
         ;; Clear backlinks to prevent duplicates
         (dolist (dest (cons (org-node-get-id node)
                             (org-node-get-refs node)))
           (org-roam-db-query [:delete :from links :where (= dest $s1)]
                              dest))
         (org-node-fakeroam--db-add-node node))))))

;; TODO: Was hoping to just run this on every save.  Is SQLite really so slow
;;       to accept 0-2 MB of data?  Must be some way to make it instant.
;; (benchmark-run (org-node-fakeroam-db-rebuild))
;; => (13.048531745 8 2.3832248650000025)
;; (benchmark-run (org-roam-db-sync 'force))
;; => (179.921311207 147 37.955398732)
(defun org-node-fakeroam-db-rebuild ()
  "Wipe the Roam DB and rebuild."
  (when (require 'org-roam nil t)
    (interactive)
    (org-node-cache-ensure)
    (org-roam-db--close)
    (delete-file org-roam-db-location)
    (emacsql-with-transaction (org-roam-db)
      (let ((ctr 0)
            (max (hash-table-count org-nodes))
            (already (make-hash-table :test #'equal)))
        (cl-loop for node being the hash-values of org-nodes
                 as file = (org-node-get-file-path node)
                 do (if (= 0 (% (cl-incf ctr)
                                (cond ((> ctr 200) 100)
                                      ((> ctr 20) 10)
                                      (t 1))))
                        (message "Inserting into %s... %d/%d"
                                 org-roam-db-location ctr max))
                 (unless (gethash file already)
                   (puthash file t already)
                   (org-node-fakeroam--db-add-file-level-data node))
                 (org-node-fakeroam--db-add-node node))))))

;; REVIEW: I don't like that this accesses actual files on disk when nothing
;;         else does.  Maybe the async parser can collect the whole vector that
;;         this needs, ahead of time.
(defun org-node-fakeroam--db-add-file-level-data (node)
  "Send to the database the metadata for the file where NODE is."
  (when (require 'org-roam nil t)
    (let* ((file (org-node-get-file-path node))
           (attr (ignore-errors (file-attributes file))))
      ;; See `org-roam-db-insert-file'
      (org-roam-db-query [:insert :into files :values $v1]
                         (vector file
                                 (org-node-get-file-title node)
                                 ;; HACK: Costs a lot of time, pass a nil hash
                                 ;; (ignore-errors (org-roam-db--file-hash file))
                                 nil
                                 (file-attribute-access-time attr)
                                 (file-attribute-modification-time attr))))))

(defun org-node-fakeroam--db-add-node (node)
  "Send to the SQLite database all we know about NODE.
This includes all links and citations that touch NODE."
  (when (require 'org-roam nil t)
    ;; PERF: Produce less garbage compared to `let'.  ~20% faster!
    (cl-symbol-macrolet
        ((id         (org-node-get-id node))
         (file-path  (org-node-get-file-path node))
         (tags       (org-node-get-tags node))  ;; NOTE: no inherits!
         (aliases    (org-node-get-aliases node))
         (roam-refs  (org-node-get-refs node))
         (title      (org-node-get-title node))
         (properties (org-node-get-properties node)) ;; NOTE: no inherits!
         (level      (org-node-get-level node))
         (todo       (org-node-get-todo node))
         (scheduled  (org-node-get-scheduled node))
         (deadline   (org-node-get-deadline node))
         (olp        (org-node-get-olp node))
         (priority   (org-node-get-priority node))
         (pos        (org-node-get-pos node)))
      ;; See `org-roam-db-insert-aliases'
      (when aliases
        (org-roam-db-query [:insert :into aliases :values $v1]
                           (cl-loop for alias in aliases
                                    collect (vector id alias))))
      ;; See `org-roam-db-insert-tags'
      (when tags
        (org-roam-db-query [:insert :into tags :values $v1]
                           (cl-loop for tag in tags
                                    collect (vector id tag))))
      ;; See `org-roam-db-insert-file-node' and `org-roam-db-insert-node-data'
      (org-roam-db-query
       [:insert :into nodes :values $v1]
       (vector id
               file-path
               level
               pos
               todo
               priority
               (when scheduled (format-time-string
                                "%FT%T%z"
                                (encode-time (org-parse-time-string scheduled))))
               (when deadline (format-time-string
                               "%FT%T%z"
                               (encode-time (org-parse-time-string deadline))))
               title
               properties
               olp))
      ;; See `org-roam-db-insert-refs'
      (dolist (ref roam-refs)
        (let ((type (gethash ref org-node--uri-path<>uri-type)))
          (org-roam-db-query [:insert :into refs :values $v1]
                             (if type
                                 ;; Ref is //www.gnu.org or some such
                                 (vector id ref type)
                               ;; Ref is a @citekey
                               (vector id ref "cite")))))
      (dolist (link (nconc (org-node-get-id-links node)
                           (org-node-get-reflinks node)))
        ;; See `org-roam-db-insert-link'
        ;; Don't add citations (type=nil), they go in a separate table
        (if (org-node-link-type link)
            (org-roam-db-query [:insert :into links :values $v1]
                               (vector (org-node-link-pos link)
                                       (org-node-link-origin link)
                                       id
                                       (org-node-link-type link)
                                       (org-node-link-properties link)))
          ;; See `org-roam-db-insert-citation'
          (org-roam-db-query [:insert :into citations :values $v1]
                             (vector (org-node-link-origin link)
                                     (org-node-link-dest link)
                                     (org-node-link-pos link)
                                     (org-node-link-properties link))))))))


;;;; Bonus advices

;; (benchmark-call (byte-compile #'org-roam-list-files))
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-files))
(defun org-node-fakeroam-list-files ()
  "Faster than `org-roam-list-files'."
  (cl-loop for file in (org-node-list-files t)
           when (string-prefix-p org-node-fakeroam-dir file)
           collect file))

;; (benchmark-call (byte-compile #'org-roam-dailies--list-files) 10)
;; (benchmark-call (byte-compile #'org-node-fakeroam-list-dailies) 10)
(defun org-node-fakeroam-list-dailies (&rest extra-files)
  "Faster than `org-roam-dailies--list-files' on a slow fs.
Makes little difference if your filesystem is not the bottleneck.
For argument EXTRA-FILES, see that function."
  (require 'org-roam-dailies)
  (append extra-files
          (cl-loop
           for file in (org-node-list-files t)
           when (string-prefix-p org-node-fakeroam-daily-dir file)
           collect file)))

;; (benchmark-call (byte-compile #'org-roam-dailies--daily-note-p) 100)
;; (benchmark-call (byte-compile #'org-node-fakeroam-daily-note-p) 100)
(defun org-node-fakeroam-daily-note-p (&optional file)
  "Faster than `org-roam-dailies--daily-note-p' on a slow fs.
Makes little difference if your filesystem is not the bottleneck.
For argument FILE, see that function."
  (setq file (org-node-abbrev-file-names
              (file-truename (or file
                                 (buffer-file-name (buffer-base-buffer))))))
  (and (string-suffix-p ".org" file)
       (string-prefix-p (downcase org-node-fakeroam-daily-dir)
                        (downcase file))
       (not (cl-loop for exclude in org-node-extra-id-dirs-exclude
                     when (string-search exclude file) return t))))


;;;; Series-related

;; TODO: Somehow make `org-node-new-via-roam-capture' able to do this?
;;;###autoload
(defun org-node-fakeroam-daily-create (ymd series-key &optional goto keys)
  "Create a daily-note, for a day implied by YMD.
YMD must be a time string in YYYY-MM-DD form.

SERIES-KEY is the key that corresponds to the member of
`org-node-series-defs' that should grow after the capture is
done.

GOTO and KEYS are like in `org-roam-dailies--capture'."
  (require 'org-roam-dailies)
  (add-hook 'org-roam-capture-new-node-hook #'org-node--add-series-item)
  (setq org-node-proposed-series-key series-key)
  (unwind-protect
      (org-roam-dailies--capture
       (encode-time
        (parse-time-string (concat ymd (format-time-string " %T %z"))))
       goto keys)
    (remove-hook 'org-roam-capture-new-node-hook #'org-node--add-series-item)
    (setq org-node-proposed-series-key nil)))

;; DEPRECATED
;;;###autoload
(defun org-node-fakeroam-daily-creator (sortstr)
  "Create a daily-note, for a day implied by SORTSTR."
  (declare (obsolete nil "2024-08-21"))
  (org-node-fakeroam-daily-create sortstr "d" t))

(defvar org-node-fakeroam-dir nil
  "Cached value of `org-roam-directory' transformed for org-node.
This path should be directly comparable to the paths saved in
org-node objects, which lets you skip using `file-truename' to
compare paths.

See also `org-node-fakeroam-daily-dir'.")

(defvar org-node-fakeroam-daily-dir nil
  "Cached value for Roam's dailies dir transformed for org-node.
This path should be directly comparable to the paths saved in
org-node objects, which lets you skip using `file-truename' to
compare paths.

Rationale: The original `org-roam-dailies-directory' was a
relative path, which incurred verbosity penalties in all code
that used it (plus practically a major performance penalty since
`expand-file-name' was often used instead of `file-name-concat').

Even more verbosity is added on top for org-node, which needs to
process the path through `org-node-abbrev-file-names'.  Thus
this variable provides an easy shorthand.")

(defun org-node-fakeroam--cache-roam-dirs ()
  "Cache some variables.
See docstring of `org-node-fakeroam-daily-dir'."
  (when (require 'org-roam nil t)
    (setq org-node-fakeroam-dir
          (org-node-abbrev-file-names
           (file-truename org-roam-directory)))
    (when (boundp 'org-roam-dailies-directory)
      (setq org-node-fakeroam-daily-dir
            (org-node-abbrev-file-names
             (file-truename
              (file-name-concat org-roam-directory
                                org-roam-dailies-directory)))))))

(org-node-fakeroam--cache-roam-dirs)
(add-hook 'org-node-before-update-tables-hook
          #'org-node-fakeroam--cache-roam-dirs)

;; Seems like unhygienic packaging?
;; (eval-after-load 'org-roam-dailies #'org-node-fakeroam--cache-roam-dirs)

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
