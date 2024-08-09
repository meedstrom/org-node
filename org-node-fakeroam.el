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
;; Package-Requires: ((emacs "28.1") (compat "29.1.4.5") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-node

;;; Commentary:

;; Provide stand-ins for `org-roam-autosync-mode'.

;;; Code:

(require 'org-node)
(require 'ol)
(if (not (fboundp 'org-roam-list-files))
    (user-error "Install org-roam to use org-node-fakeroam stuff")
  (require 'org-roam)
  (require 'emacsql))

(defun org-node-fakeroam--precompile ()
  "Compile all fakeroam functions.
Because org-node-fakeroam.el depends on org-roam, which is not an
explicit dependency of org-node, it is uncompilable at package
installation and must be compiled at runtime after ensuring that
the user has installed org-roam themselves."
  (if (not (fboundp 'org-roam-list-files))
      (user-error "Install org-roam to use org-node-fakeroam stuff")
    (require 'org-roam)
    (require 'emacsql)
    (dolist (fn '(org-node-fakeroam--mk-node
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
certain Org options inside the context previews.  This is done
thru `org-node-fakeroam--accelerate-get-contents', which see.

-----"
  :global t
  :group 'org-node
  (if org-node-fakeroam-redisplay-mode
      (progn
        (advice-add #'org-roam-preview-get-contents :around
                    #'org-node-fakeroam--accelerate-get-contents)
        (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
        (dolist (buf (org-buffer-list))
          (with-current-buffer buf
            (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))))
    (advice-remove #'org-roam-preview-get-contents
                   #'org-node-fakeroam--accelerate-get-contents)
    (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
    (unless org-roam-db-autosync-mode
      (dolist (buf (org-buffer-list))
        (with-current-buffer buf
          (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t))))))

(defun org-node-fakeroam--accelerate-get-contents (orig-fn file pt)
  "Designed as around-advice for `org-roam-preview-get-contents'.

Normally the first time you open an org-roam-buffer, Emacs hangs
for as long as a minute on a slow machine when huge files are
involved.  This may eliminate most of that.

Also caches the results, so when there are backlinks from
extremely many files, it should only be slow the first time the
the org-roam-buffer is built."
  (let* ((cached (alist-get pt (gethash file org-node--file<>previews)))
         (result (or cached
                     (let ((org-inhibit-startup t))
                       (funcall orig-fn file pt)))))
    (unless cached
      (push (cons pt result)
            (gethash file org-node--file<>previews)))
    result))


;;;; NoSQL method: fabricate knockoff roam backlinks

;;;###autoload
(define-minor-mode org-node-fakeroam-nosql-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed.

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
  "Make org-roam-backlink objects targeting TARGET-ROAM-NODE.
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
  "Make org-roam-reflink objects targeting TARGET-ROAM-NODE.
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
  "Supply data to the org-roam database on save.

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

;; Was hoping to just run this on every save, is SQLite really so slow to
;; accept 0-2 MB of data?  Must be some way to make it instant, else how do
;; people work with petabytes?
(defun org-node-fakeroam-db-rebuild ()
  "Wipe the Roam DB and rebuild."
  (interactive)
  (org-node-cache-ensure)
  (org-roam-db)
  (org-roam-db-clear-all)
  (org-roam-db--close)
  (delete-file org-roam-db-location)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop with ctr = 0
             with max = (hash-table-count org-nodes)
             for node being the hash-values of org-nodes
             do (when (= 0 (% (cl-incf ctr) 10))
                  (message "Inserting into %s... %d/%d"
                           org-roam-db-location ctr max))
             (org-node-fakeroam--db-add-node node))))

;; This thing just exists because the above is not instant.
;; But even this gets a bit slow on the deletion queries...
(defun org-node-fakeroam--db-update-files (files)
  "Update the Roam DB about nodes and links involving FILES."
  (emacsql-with-transaction (org-roam-db)
    (dolist (file files)
      (org-roam-db-query [:delete :from files :where (= file $s1)]
                         file))
    (cl-loop
     for node being the hash-values of org-node--id<>node
     when (member (org-node-get-file-path node) files)
     do (progn
          (dolist (dest (cons (org-node-get-id node)
                              (org-node-get-refs node)))
            ;; Clear backlinks to prevent getting duplicates in next step
            (org-roam-db-query [:delete :from links :where (= dest $s1)]
                               dest))
          (org-node-fakeroam--db-add-node node)))))

(defun org-node-fakeroam--db-add-node (node)
  "Send to the SQLite database all we know about NODE.
This includes all links and citations that touch NODE."
  (let ((id         (org-node-get-id node))
        (file-path  (org-node-get-file-path node))
        (file-title (org-node-get-file-title node))
        (tags       (org-node-get-tags node))  ;; NOTE: no inherits!
        (aliases    (org-node-get-aliases node))
        (roam-refs  (org-node-get-refs node))
        (title      (org-node-get-title node))
        (properties (org-node-get-properties node)) ;; NOTE: no inherits!
        (level      (org-node-get-level node))
        (todo       (org-node-get-todo node))
        (is-subtree (org-node-get-is-subtree node))
        (olp        (org-node-get-olp node))
        (priority   (org-node-get-priority node))
        (pos        (org-node-get-pos node)))
    ;; See `org-roam-db-insert-file'
    (let ((attr (file-attributes file-path)))
      (org-roam-db-query [:insert :into files :values $v1]
                         (vector file-path
                                 file-title
                                 (org-roam-db--file-hash file-path)
                                 (file-attribute-access-time attr)
                                 (file-attribute-modification-time attr))))
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
    ;; See `org-roam-db-insert-node-data'
    (when is-subtree
      (let ((scheduled (when-let ((scheduled (org-node-get-scheduled node)))
                         (format-time-string
                          "%FT%T%z"
                          (encode-time (org-parse-time-string scheduled)))))
            (deadline (when-let ((deadline (org-node-get-deadline node)))
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string deadline))))))
        (org-roam-db-query [:insert :into nodes :values $v1]
                           (vector id file-path level pos todo priority
                                   scheduled deadline title properties olp))))
    ;; See `org-roam-db-insert-file-node'
    (when (not is-subtree)
      (let ((scheduled nil)
            (deadline nil))
        (org-roam-db-query [:insert :into nodes :values $v1]
                           (vector id file-path level pos todo priority
                                   scheduled deadline title properties olp))))
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

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
