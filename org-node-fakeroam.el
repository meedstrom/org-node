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
;; Version:          0.5
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "28.1") (org-node "0.5") (org-roam "2.2.2") (emacsql "4.0.0") (compat "29.1.4.5"))
;; URL:              https://github.com/meedstrom/org-node

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode'.

;;; Code:

(require 'cl-lib)
(require 'org-node)
(require 'ol)
(if (require 'org-roam nil t)
    (require 'emacsql)
  (user-error "Install org-roam to use org-node-fakeroam library"))

(defun org-node-fakeroam--precompile ()
  "Compile all fakeroam functions.

This is transitional.  Org-node used to be a single package which
did not depend explicitly on org-roam, which meant it had to
defer compiling org-node-fakeroam.el until runtime after ensuring
that org-roam is available."
  (if (not (require 'org-roam nil t))
      (user-error "Install org-roam to use org-node-fakeroam library")
    (require 'emacsql)
    (dolist (fn '(org-node-fakeroam--run-without-fontifying
                  org-node-fakeroam--accelerate-get-contents
                  org-node-fakeroam--mk-node
                  org-node-fakeroam--mk-backlinks
                  org-node-fakeroam--mk-reflinks
                  org-node-fakeroam--db-update-files
                  org-node-fakeroam--db-add-node))
      (unless (compiled-function-p (symbol-function fn))
        (byte-compile fn)))))

;;;###autoload
(define-minor-mode org-node-fakeroam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off.

As a bonus, advise the Roam buffer to open faster, by nullifying
certain Org options inside the context previews, and caching the
previews.  This is done thru
`org-node-fakeroam--accelerate-get-contents', which see.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-redisplay-mode
      (progn
        (org-node-fakeroam--precompile)
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-redisplay-mode' may show stale previews without `org-node-cache-mode' enabled"))
        (when (boundp 'savehist-additional-variables)
          (add-to-list 'savehist-additional-variables 'org-node--file<>previews)
          (add-to-list 'savehist-additional-variables 'org-node--file<>mtime))
        (advice-add #'org-roam-preview-get-contents :around
                    #'org-node-fakeroam--accelerate-get-contents)
        (advice-add #'org-roam-node-insert-section :around
                    #'org-node-fakeroam--run-without-fontifying)
        (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
        (dolist (buf (org-buffer-list))
          (with-current-buffer buf
            (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))))

    (advice-remove #'org-roam-preview-get-contents
                   #'org-node-fakeroam--accelerate-get-contents)
    (advice-remove #'org-roam-node-insert-section
                   #'org-node-fakeroam--run-without-fontifying)
    (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
    (unless org-roam-db-autosync-mode
      (dolist (buf (org-buffer-list))
        (with-current-buffer buf
          (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t))))))

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
    ;; NOTE: We cannot use `org-roam-fontify-like-in-org-mode'
    ;;       since it is temporarily overridden by
    ;;       `org-node-fakeroam--run-without-fontifying'
    ;;       at the time this runs.  But that's OK; it looks outdated.
    (setf (alist-get pt (gethash file org-node--file<>previews))
          (let ((org-inhibit-startup t))
            (delay-mode-hooks
              (org-fontify-like-in-org-mode (funcall orig-fn file pt)))))))

(defun org-node-fakeroam--run-without-fontifying (orig-fn &rest args)
  "Intended as around-advice for `org-roam-node-insert-section'.
Run ORIG-FN with ARGS, while overriding
`org-roam-fontify-like-in-org-mode' so it does nothing."
  (cl-letf (((symbol-function 'org-roam-fontify-like-in-org-mode) #'identity))
    (apply orig-fn args)))


;;;; NoSQL method: fabricate knockoff roam backlinks

;;;###autoload
(define-minor-mode org-node-fakeroam-nosql-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed, and you can delete the org-roam.db.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-nosql-mode
      (progn
        (org-node-fakeroam--precompile)
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-nosql-mode' will do nothing without `org-node-cache-mode'"))
        (advice-add 'org-roam-backlinks-get :override
                    #'org-node-fakeroam--mk-backlinks)
        (advice-add 'org-roam-reflinks-get :override
                    #'org-node-fakeroam--mk-reflinks))
    (advice-remove 'org-roam-backlinks-get #'org-node-fakeroam--mk-backlinks)
    (advice-remove 'org-roam-reflinks-get #'org-node-fakeroam--mk-reflinks)))

(defun org-node-fakeroam--mk-node (node)
  "Make an org-roam-node object from org-node NODE."
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
   :properties (org-node-get-properties node)))

(defun org-node-fakeroam--mk-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-backlinks-get'."
  (let ((target-id (org-roam-node-id target-roam-node)))
    (if (not target-id)
        (error "org-node-fakeroam: Going to get backlinks, but given nil id")
      (let ((links (gethash target-id org-node--dest<>links)))
        (cl-loop
         for link in links
         as src-id = (org-node-link-origin link)
         as src-node = (gethash src-id org-node--id<>node)
         when src-node
         collect (org-roam-backlink-create
                  :target-node target-roam-node
                  :source-node (org-node-fakeroam--mk-node src-node)
                  :point (org-node-link-pos link)
                  :properties (org-node-link-properties link)))))))

(defun org-node-fakeroam--mk-reflinks (target-roam-node &rest _)
  "Make org-roam-reflink objects pointing to TARGET-ROAM-NODE.
Designed to override `org-roam-reflinks-get'."
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
                        :properties (org-node-link-properties link)))))))


;;;; Feed method: supply data to Roam's DB

;;;###autoload
(define-minor-mode org-node-fakeroam-db-feed-mode
  "Supply data to the org-roam SQLite database on save.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-db-feed-mode
      (progn
        (org-node-fakeroam--precompile)
        (unless org-node-cache-mode
          (message "`org-node-fakeroam-db-feed-mode' will do nothing without `org-node-cache-mode'"))
        (add-hook 'org-node-rescan-hook #'org-node-fakeroam--db-update-files))
    (remove-hook 'org-node-rescan-hook #'org-node-fakeroam--db-update-files)))

;; TODO: Was hoping to just run this on every save.  Is SQLite really so slow
;;       to accept 0-2 MB of data?  Must be some way to make it instant, else
;;       how do people work with petabytes?
(defun org-node-fakeroam-db-rebuild ()
  "Wipe the Roam DB and rebuild."
  (interactive)
  (org-node-cache-ensure)
  (org-roam-db)
  (org-roam-db-clear-all)
  (org-roam-db--close)
  (delete-file org-roam-db-location)
  (emacsql-with-transaction (org-roam-db)
    (let ((ctr 0)
          (max (hash-table-count org-nodes))
          (file-level-data-already-added nil))
      (cl-loop for node being the hash-values of org-nodes
               as file = (org-node-get-file-path node)
               do (when (= 0 (% (cl-incf ctr)
                                (cond ((> ctr 200) 100)
                                      ((> ctr 20) 10)
                                      (t 1))))
                    (message "Inserting into %s... %d/%d"
                             org-roam-db-location ctr max))
               (unless (member file file-level-data-already-added)
                 (push file file-level-data-already-added)
                 (org-node-fakeroam--db-add-file-level-data node))
               (org-node-fakeroam--db-add-node node)))))

;; REVIEW: I don't like that this accesses actual files on disk
;;         when nothing else does
(defun org-node-fakeroam--db-add-file-level-data (node)
  "Send to the database the metadata for the file where NODE is."
  (let* ((file (org-node-get-file-path node))
         (attr (ignore-errors (file-attributes file))))
    ;; See `org-roam-db-insert-file'
    (org-roam-db-query [:insert :into files :values $v1]
                       (vector file
                               (org-node-get-file-title node)
                               (ignore-errors (org-roam-db--file-hash file))
                               (file-attribute-access-time attr)
                               (file-attribute-modification-time attr)))))

(defun org-node-fakeroam--db-add-node (node)
  "Send to the SQLite database all we know about NODE.
This includes all links and citations that touch NODE."
  (let ((id         (org-node-get-id node))
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
    (when scheduled (setq scheduled
                          (format-time-string
                           "%FT%T%z"
                           (encode-time (org-parse-time-string scheduled)))))
    (when deadline (setq deadline
                         (format-time-string
                          "%FT%T%z"
                          (encode-time (org-parse-time-string deadline)))))
    (org-roam-db-query [:insert :into nodes :values $v1]
                       (vector id file-path level pos todo priority
                               scheduled deadline title properties olp))
    ;; See `org-roam-db-insert-refs'
    (dolist (ref roam-refs)
      (let ((type (gethash ref org-node--uri-path<>uri-type)))
        (org-roam-db-query [:insert :into refs :values $v1]
                           (if type
                               ;; Ref is //www.gnu.org or some such
                               (vector id ref type)
                             ;; Ref is a @citekey
                             (vector id ref "cite")))))
    ;; See `org-roam-db-insert-citation'
    (dolist (cite (cl-loop for link in (org-node-get-reflinks node)
                           when (null (org-node-link-type link))
                           collect link))
      (org-roam-db-query [:insert :into citations :values $v1]
                         (vector (org-node-link-origin cite)
                                 (org-node-link-dest cite)
                                 (org-node-link-pos cite)
                                 (org-node-link-properties cite))))
    ;; See `org-roam-db-insert-link'
    (dolist (link (append (org-node-get-id-links node)
                          (org-node-get-reflinks node)))
      ;; Don't add citations (type=nil), they go in a separate table
      (when (org-node-link-type link)
        (org-roam-db-query [:insert :into links :values $v1]
                           (vector (org-node-link-pos link)
                                   (org-node-link-origin link)
                                   id
                                   (org-node-link-type link)
                                   (org-node-link-properties link)))))))

;; This function is a restricted alternative to `org-node-fakeroam-db-rebuild'
;; because that is not instant.
;; FIXME: Still too damn slow on a file with 400 nodes.  Profiler says most of
;;        it is in EmacSQL, maybe some SQL PRAGMA settings would fix?
(defun org-node-fakeroam--db-update-files (files)
  "Update the Roam DB about nodes and links involving FILES."
  (emacsql-with-transaction (org-roam-db)
    (dolist (file files)
      (org-roam-db-query [:delete :from files :where (= file $s1)]
                         file))
    (let (file-level-data-already-added)
      (cl-loop
       for node being the hash-values of org-nodes
       as file = (org-node-get-file-path node)
       when (member file files)
       do
       (unless (member file file-level-data-already-added)
         (push file file-level-data-already-added)
         (org-node-fakeroam--db-add-file-level-data node))
       ;; Clear backlinks to prevent duplicates
       (dolist (dest (cons (org-node-get-id node)
                           (org-node-get-refs node)))
         (org-roam-db-query [:delete :from links :where (= dest $s1)]
                            dest))
       (org-node-fakeroam--db-add-node node)))))

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
