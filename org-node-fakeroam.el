;;; org-node-fakeroam.el --- Rudimentary Org-roam replica -*- lexical-binding: t; -*-

(require 'org-node)
(require 'ol)
(require 'org-roam)
(require 'emacsql)

;;;; Deprecations

;;;###autoload
(defun org-node-feed-file-to-roam-db (&optional files)
  (declare (obsolete 'org-node-fakeroam--db-update-files "2024-07-11"))
  (display-warning 'org-node "Your config uses deprecated `org-node-feed-file-to-roam-db', use `org-node-fakeroam-db-feed-mode' instead")
  (org-node-fakeroam--db-update-files files))

;; Used to be autoloaded
(org-node--defobsolete
 org-node-roam-db-shim-mode org-node-fakeroam-db-feed-mode t)
(org-node--defobsolete
 org-node-roam-db-reset org-node-fakeroam-db-rebuild t)
(org-node--defobsolete
 org-node-roam-redisplay-mode org-node-fakeroam-redisplay-mode t)
(org-node--defobsolete
 org-node-roam-no-sql-mode org-node-fakeroam-nosql-mode t)

(org-node--defobsolete
 org-node--fabricate-roam-backlinks org-node-fakeroam--mk-backlinks)
(org-node--defobsolete
 org-node--fabricate-roam-reflinks org-node-fakeroam--mk-reflinks)
(org-node--defobsolete
 org-node-roam-db-feed org-node-fakeroam--db-update-files)


;;;; Bonus tools

;;;###autoload
(define-minor-mode org-node-fakeroam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off.

As a bonus, advise the Roam buffer to open faster, by nullifying
startup options such as `org-startup-indented' inside the context
previews.  See `org-node-fakeroam--accelerate-get-contents'."
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

(defun org-node-fakeroam--accelerate-get-contents (fn &rest args)
  "Designed as around-advice for `org-roam-preview-get-contents'.

Normally in huge files, the first time you open an
org-roam-buffer, Emacs hangs for as long as a minute on a slow
machine.  This may eliminate most of that."
  (let ((org-inhibit-startup t))
    (apply fn args)))


;;;; NoSQL method: fabricate knockoff roam backlinks

;;;###autoload
(define-minor-mode org-node-fakeroam-nosql-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed."
  :global t
  :group 'org-node
  (if org-node-fakeroam-nosql-mode
      (progn
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

;; Eval to see examples of what it has to work with!
;; (seq-random-elt (hash-table-keys org-node--dest<>links))
;; (seq-random-elt (hash-table-values org-node--dest<>links))

(defun org-node-fakeroam--mk-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-backlinks-get'."
  (let ((target-id (org-roam-node-id target-roam-node)))
    (when target-id
      (let ((links (gethash target-id org-node--dest<>links)))
        (cl-loop
         for link-data in links
         as src-id = (plist-get link-data :origin)
         as src-node = (gethash src-id org-node--id<>node)
         when src-node
         collect (org-roam-backlink-create
                  :target-node target-roam-node
                  :source-node (org-node-fakeroam--mk-node src-node)
                  :point (plist-get link-data :pos)
                  :properties (plist-get link-data :properties)))))))

(defun org-node-fakeroam--mk-reflinks (target-roam-node &rest _)
  "Make org-roam-reflink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-reflinks-get'."
  (let* ((target-id (org-roam-node-id target-roam-node))
         (node (gethash target-id org-node--id<>node)))
    (when node
      (cl-loop
       for ref in (org-node-get-refs node)
       append (cl-loop
               for link in (gethash ref org-node--dest<>links)
               as src-id = (plist-get link :origin)
               as src-node = (gethash src-id org-node--id<>node)
               when src-node
               collect (org-roam-reflink-create
                        :ref (plist-get link :dest)
                        :source-node (org-node-fakeroam--mk-node src-node)
                        :point (plist-get link :pos)
                        :properties (plist-get link :properties)))))))


;;;; Feed method: supply data to Roam's DB

;;;###autoload
(define-minor-mode org-node-fakeroam-db-feed-mode
  "Supply data to the org-roam database on save."
  :global t
  :group 'org-node
  (if org-node-fakeroam-db-feed-mode
      (progn
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
  "Tell the Roam DB about all nodes and links involving FILES."
  (emacsql-with-transaction (org-roam-db)
    (dolist (file files)
      ;; (org-roam-db-query [:delete :from files :where (= file $s1)] file))
      ;; Same as
      (org-roam-db-clear-file file))
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
                             (vector id (substring ref 1) "cite")))))
    ;; See `org-roam-db-insert-citation'
    ;; Confu
    (dolist (cite (cl-loop for link in (org-node-get-reflinks node)
                           when (null (plist-get link :type))
                           collect link))
      (org-roam-db-query [:insert :into citations :values $v1]
                         (vector (plist-get cite :origin)
                                 (plist-get cite :dest)
                                 (plist-get cite :pos)
                                 (plist-get cite :properties))))
    ;; See `org-roam-db-insert-link'
    (dolist (link (append (org-node-get-id-links node)
                          (org-node-get-reflinks node)))
      ;; Don't add citations (type=nil), they go in a separate table
      (when (plist-get link :type)
        (org-roam-db-query [:insert :into links :values $v1]
                           (vector (plist-get link :pos)
                                   (plist-get link :origin)
                                   id
                                   (plist-get link :type)
                                   (plist-get link :properties)))))))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
