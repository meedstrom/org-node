;;; org-node-roam.el --- Rudimentary Org-roam replica -*- lexical-binding: t; -*-

(require 'org-node)
(require 'ol)

;;;###autoload
(defun org-node-feed-file-to-roam-db (&optional files)
  (declare (obsolete 'org-node-roam-db-feed "2024-07-11"))
  (unless warned-once
    (setq warned-once t)
    (display-warning 'org-node "Your config uses deprecated `org-node-feed-file-to-roam-db', use `org-node-roam-db-shim-mode' instead"))
  (org-node-roam-db-feed files))

(org-node--defobsolete
 org-node--fabricate-roam-backlinks org-node-roam--mk-fake-backlinks)
(org-node--defobsolete
 org-node--fabricate-roam-reflinks org-node-roam--mk-fake-reflinks)

;;;###autoload
(define-minor-mode org-node-roam-redisplay-mode
  "Make the Roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
mode exists for people who prefer to turn that off."
  :global t
  (if org-node-roam-redisplay-mode
      (progn
        (require 'org-roam)
        (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
        (dolist (buf (org-buffer-list))
          (with-current-buffer buf
            (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t))))
    (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h)
    (unless (bound-and-true-p org-roam-db-autosync-mode)
      (dolist (buf (org-buffer-list))
        (with-current-buffer buf
          (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t))))))


;;;; NoSQL method: fabricate knockoff roam backlinks

;;;###autoload
(define-minor-mode org-node-roam-no-sql-mode
  "Override org-roam backlink-getters to look up org-node tables.

As a result, \\[org-roam-buffer-toggle] will function without
having SQLite installed."
  :global t
  (require 'org-roam)
  (if org-node-roam-no-sql-mode
      (progn
        (unless org-node-cache-mode
          (message "`org-node-roam-no-sql-mode' will do nothing without `org-node-cache-mode'"))
        (advice-add 'org-roam-backlinks-get :override
                    #'org-node-roam--mk-fake-backlinks)
        (advice-add 'org-roam-reflinks-get :override
                    #'org-node-roam--mk-fake-reflinks))
    (advice-remove 'org-roam-backlinks-get #'org-node-roam--mk-fake-backlinks)
    (advice-remove 'org-roam-reflinks-get #'org-node-roam--mk-fake-reflinks)))

(defun org-node-roam--mk-fake-obj (node)
  "Make an org-roam-node object from org-node NODE."
  (require 'org-roam)
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
;; (seq-random-elt (hash-table-keys org-node--id<>id-links))
;; (seq-random-elt (hash-table-values org-node--id<>id-links))

(defun org-node-roam--mk-fake-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-backlinks-get'."
  (require 'org-roam)
  (let ((target-id (org-roam-node-id target-roam-node)))
    (when target-id
      (let ((links (gethash target-id org-node--id<>id-links)))
        (cl-loop
         for link-data in links
         as src-id = (plist-get link-data :origin)
         as src-node = (gethash src-id org-node--id<>node)
         when src-node
         collect (org-roam-backlink-create
                  :target-node target-roam-node
                  :source-node (org-node-roam--mk-fake-obj src-node)
                  :point (plist-get link-data :pos)
                  :properties (plist-get link-data :properties)))))))

;; Eval to see examples of what it has to work with!
;; (seq-random-elt (hash-table-keys org-node--ref<>reflinks))
;; (seq-random-elt (hash-table-values org-node--ref<>reflinks))

;; TODO Add the citations too
(defun org-node-roam--mk-fake-reflinks (target-roam-node &rest _)
  "Make org-roam-reflink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-reflinks-get'."
  (require 'org-roam)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (node (gethash target-id org-node--id<>node)))
    (when node
      (cl-loop
       for ref in (org-node-get-refs node)
       as reflinks = (cl-loop
                      for link in (gethash ref org-node--ref<>reflinks)
                      as src-id = (plist-get link :origin)
                      as src-node = (gethash src-id org-node--id<>node)
                      when src-node
                      collect (org-roam-reflink-create
                               :ref (plist-get link :dest)
                               :source-node (org-node-roam--mk-fake-obj src-node)
                               :point (plist-get link :pos)
                               :properties (plist-get link :properties)))
       when reflinks append reflinks))))


;;;; Shim method: feed Roam's DB

;;;###autoload
(define-minor-mode org-node-roam-db-shim-mode
  "Send "
  :global t
  (require 'org-roam)
  (if org-node-roam-db-shim-mode
      (progn
        (unless org-node-cache-mode
          (message "`org-node-roam-db-shim-mode' will do nothing without `org-node-cache-mode'"))
        (add-hook 'org-node-rescan-hook #'org-node-roam-db-feed))
    (remove-hook 'org-node-rescan-hook #'org-node-roam-db-feed)))

(defun org-node-roam-db-feed (files)
  "Tell the Roam DB about all nodes and links involving FILES."
  (require 'org-roam)
  (require 'emacsql)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop for node being the hash-values of org-nodes
             when (member (org-node-get-file-path node) (ensure-list files))
             do (org-node-roam--db-add-node node))))

;; Was hoping to just run this on every save, is SQLite really so slow
;; accepting 0-2 MB of data?  Must be some way to make it instant, else how do
;; people work with petabytes?
(defun org-node-roam-db-reset ()
  "Wipe the Roam DB and rebuild."
  (interactive)
  (require 'org-roam)
  (require 'emacsql)
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
             (org-node-roam--db-add-node node))))

;; REVIEW Maybe erase before add?  Can we get duplicate links when
;;        constantly re-adding the same links?
(defun org-node-roam--db-add-node (node)
  "Send to the SQLite database all we know about NODE.
This includes all links and citations that touch NODE."
  (require 'url-parse)
  (let ((id         (org-node-get-id node))
        (file-path  (org-node-get-file-path node))
        (file-title (org-node-get-file-title node))
        (tags       (org-node-get-tags node))  ;; NOTE: no inherit!
        (aliases    (org-node-get-aliases node))
        (roam-refs  (org-node-get-refs node))
        (title      (org-node-get-title node))
        (properties (org-node-get-properties node)) ;; NOTE: no inherit!
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
                                 (file-attribute-access-time attr)
                                 (file-attribute-modification-time attr)
                                 (org-roam-db--file-hash file-path))))
    ;; See `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query [:insert :into aliases :values $v1]
                         (mapcar (lambda (alias)
                                   (vector id alias))
                                 aliases)))
    ;; See `org-roam-db-insert-tags'
    (when tags
      (org-roam-db-query [:insert :into tags :values $v1]
                         (mapcar (lambda (tag)
                                   (vector id tag))
                                 tags)))
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
      (let ((pos 1)
            (todo nil)
            (priority nil)
            (scheduled nil)
            (deadline nil)
            (level 0))
        (org-roam-db-query [:insert :into nodes :values $v1]
                           (vector id file-path level pos todo priority
                                   scheduled deadline title properties olp))))
    ;; See `org-roam-db-insert-refs'
    (dolist (ref roam-refs)
      (let ((row (cond
                  ;; Ref is a @citekey
                  ((string-prefix-p "@" ref)
                   (vector id (substring ref 1) "cite"))
                  ;; Ref is //gnu.org or some such path out of an
                  ;; URI:PATH.  Actually, it can also be plain
                  ;; [[URI-less link]], but don't send that to org-roam.
                  ((let ((type (gethash ref org-node--uri-path<>uri-type)))
                     (when type
                       ;; Not sure it's necessary, but Roam does this
                       (setq ref (thread-last
                                   (org-link-encode ref '(#x20))
                                   (url-generic-parse-url)
                                   (url-recreate-url)
                                   (org-link-decode)))
                       (vector id ref type)))))))
        (when row
          (org-roam-db-query [:insert :into refs :values $v1]
                             row))))
    ;; See `org-roam-db-insert-citation'
    (dolist (cite (org-node-get-citations node))
      (org-roam-db-query [:insert :into citations :values $v1]
                         (vector (plist-get link :origin)
                                 (plist-get link :key)
                                 (plist-get link :pos)
                                 (plist-get link :properties))))
    ;; See `org-roam-db-insert-link'
    (dolist (link (append (gethash id org-node--id<>id-links)
                          (org-node-get-reflinks node)))
      (org-roam-db-query [:insert :into links :values $v1]
                         (vector (plist-get link :pos)
                                 (plist-get link :origin)
                                 id
                                 (plist-get link :type)
                                 (plist-get link :properties))))))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
