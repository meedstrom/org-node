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
 org-node--fabricate-roam-backlinks org-node-roam--make-backlinks)
(org-node--defobsolete
 org-node--fabricate-roam-reflinks org-node-roam--make-reflinks)

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
  "Instruct org-roam to use fake `org-roam-backlink-p' objects etc
from org-node so that you can use \\[org-roam-buffer-toggle]
without having SQLite installed."
  :global t
  (require 'org-roam)
  (if org-node-roam-no-sql-mode
      (progn
        (unless org-node-cache-mode
          (message "`org-node-roam-no-sql-mode' will do nothing without `org-node-cache-mode'"))
        (advice-add 'org-roam-backlinks-get :override
                    #'org-node-roam--make-backlinks)
        (advice-add 'org-roam-reflinks-get :override
                    #'org-node-roam--make-reflinks))
    (advice-remove 'org-roam-backlinks-get #'org-node-roam--make-backlinks)
    (advice-remove 'org-roam-reflinks-get #'org-node-roam--make-reflinks)))

(defun org-node-roam--make-obj (node)
  "Make a knockoff org-roam-node object from org-node node NODE."
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
   :properties (org-node-get-properties node)))


;; Eval to see examples of what it has to work with...
;; (seq-random-elt (hash-table-keys org-node--id<>backlinks))
;; (seq-random-elt (hash-table-values org-node--id<>backlinks))

(defun org-node-roam--make-backlinks (target-roam-node &rest _)
  "Make org-roam-backlink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-backlinks-get'."
  (require 'org-roam)
  (let ((target-id (org-roam-node-id target-roam-node)))
    (when target-id
      (let ((links (gethash target-id org-node--id<>backlinks)))
        (cl-loop
         for link-data in links
         as src-id = (plist-get link-data :origin)
         as src-node = (gethash src-id org-node--id<>node)
         when src-node
         collect (org-roam-backlink-create
                  :target-node target-roam-node
                  :source-node (org-node-roam--make-obj src-node)
                  :point (plist-get link-data :pos)
                  :properties (plist-get link-data :properties)))))))

;; Eval to see examples of what it has to work with...
;; (seq-random-elt (hash-table-keys org-node--ref<>reflinks))
;; (seq-random-elt (hash-table-values org-node--ref<>reflinks))

(defun org-node-roam--make-reflinks (target-roam-node &rest _)
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
                               :source-node (org-node-roam--make-obj src-node)
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
  (require 'org-roam)
  (require 'emacsql)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop for node being the hash-values of org-nodes
             when (member (org-node-get-file-path node) files)
             do (org-node-roam--db-add-node node))))

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

;; (let ((s "... \"https://gnu.org/A Link With Spaces/index.htm\" ..."))
;;   (string-match org-link-any-re s)
;;   (match-string 0 s))

;; (let ((s "...[[https://gnu.org/A Link With Spaces/index.htm][foo]] ..."))
;;   (string-match org-link-any-re s)
;;   (match-string 0 s))

(defun org-node-roam--convert-ref (ref node-id)
  "Convert ref as org-roam expects it.
Adapted from `org-roam-db-insert-refs'."
  (let (rows)
    (cond ((string-prefix-p "@" ref) ;; Ref is a @citekey
           (push (vector node-id (substring ref 1) "cite") rows))

          ((string-match org-link-any-re ref)
           ;; Ref is a link, such as https://gnu.org or file:/home/me

           ;; Org-roam actually scans for links matching `org-link-any-re',
           ;; which combines bracket-re, angle-re and plain-re (in that order);
           ;; but org-node-worker.el only scans links matching plain-re atm.
           ;;
           ;; It looks like it's done to be able to have links with spaces.  So
           ;; I will probs migrate org-node use that regexp too.
           ;;
           ;; But until I do it, this org-link-encode stuff won't do anything.
           (setq ref (org-link-encode ref '(#x20)))
           (let* ((ref-url (url-generic-parse-url (match-string 2 ref)))
                  (type (url-type ref-url))
                  path)
             ;; ??? https://github.com/org-roam/org-roam/commit/4e6f9346903f4083684e099d25e534d6daca1f7c
             (setf (url-type ref-url) nil)
             (setq path (org-link-decode (url-recreate-url ref-url)))
             ;; org-roam supports org-ref
             (if (and (boundp 'org-ref-cite-types)
                      (or (assoc type org-ref-cite-types)
                          (member type org-ref-cite-types)))
                 (dolist (key (org-roam-org-ref-path-to-keys path))
                   (push (vector node-id key type) rows))
               (push (vector node-id path type) rows))))
          (t
           (lwarn '(org-node) :warning "Invalid ref %s in %s"
                  ref (buffer-file-name))))
    rows))

(defun org-node-roam--db-add-node (node)
  (let ((id         (org-node-get-id node))
        (file-path  (org-node-get-file-path node))
        (file-title (org-node-get-file-title node))
        (tags       (org-node-get-tags node))
        (aliases    (org-node-get-aliases node))
        (roam-refs  (org-node-get-refs node))
        (title      (org-node-get-title node))
        (properties (org-node-get-properties node))
        (level      (org-node-get-level node))
        (todo       (org-node-get-todo node))
        (is-subtree (org-node-get-is-subtree node))
        (olp        (org-node-get-olp node))
        (pos        (org-node-get-pos node)))
    ;; See `org-roam-db-insert-file'
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file-path file-title "" "" "")))
    ;; See `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query
       [:insert :into aliases
        :values $v1]
       (mapcar (lambda (alias)
                 (vector id alias))
               aliases)))
    ;; See `org-roam-db-insert-tags'
    ;; FIXME there's no inheritance
    (when tags
      (org-roam-db-query
       [:insert :into tags
        :values $v1]
       (mapcar (lambda (tag)
                 (vector id tag))
               tags)))
    ;; See `org-roam-db-insert-node-data'
    (when is-subtree
      (let ((priority nil) ;; TODO
            (scheduled (when-let ((scheduled (org-node-get-scheduled node)))
                         (format-time-string
                          "%FT%T%z"
                          (encode-time (org-parse-time-string scheduled)))))
            (deadline (when-let ((deadline (org-node-get-deadline node)))
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string deadline)))))
            (title (org-link-display-format title)))
        (org-roam-db-query
         [:insert :into nodes
          :values $v1]
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
        (org-roam-db-query
         [:insert :into nodes
          :values $v1]
         (vector id file-path level pos todo priority
                 scheduled deadline title properties olp))))
    ;; See `org-roam-db-insert-refs'
    (dolist (ref roam-refs)
      (org-roam-db-query [:insert :into refs
                          :values $v1]
                         (org-node-roam--convert-ref ref id)))
    ;; See `org-roam-db-insert-citation'
    (dolist (cite (org-node-get-citations node))
      (org-roam-db-query [:insert :into citations
                          :values $v1]
                         (vector (plist-get link :origin)
                                 (plist-get link :key)
                                 (plist-get link :pos)
                                 (plist-get link :properties))))
    ;; See `org-roam-db-insert-link'
    (dolist (link (append (gethash id org-node--id<>backlinks)
                          (org-node-get-reflinks node)))
      (org-roam-db-query [:insert :into links
                          :values $v1]
                         (vector (plist-get link :pos)
                                 (plist-get link :origin)
                                 id
                                 (plist-get link :type)
                                 (plist-get link :properties))))))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
