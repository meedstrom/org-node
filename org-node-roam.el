;;; org-node-roam.el -*- lexical-binding: t; -*-

;; To make `org-roam-buffer-toggle' work without any DB, eval this:
;; (advice-add 'org-roam-backlinks-get :override #'org-node--fabricate-roam-backlinks)

(defun org-node--fabricate-roam-object (node)
  "Construct an org-roam-node object from NODE.
This just uses information in org-node's own `org-nodes'.
Some of the fields are blank."
  (require 'org-roam)
  (org-roam-node-create
   :file (plist-get node :file-path)
   :id (plist-get node :id)
   :level (plist-get node :level)
   :title (plist-get node :title)
   :tags (plist-get node :tags)
   :aliases (plist-get node :aliases)
   :point (org-node--visit-get-pos node)))

(defun org-node--fabricate-roam-backlinks (roam-object &rest _)
  "Return org-roam-backlink objects targeting ROAM-OBJECT.

A drop-in replacement for `org-roam-backlinks-get', but ignores
its UNIQUE argument because org-node does not track multiple
backlinks from the same node anyway: literally all the
information comes from the CACHED_BACKLINKS property, which is
deduplicated, as if :unique t."
  (require 'org-roam)
  (let ((node (gethash (org-roam-node-id roam-object) org-nodes)))
    (when node
      (cl-loop
       for backlink-id in (plist-get node :backlink-origins)
       as node-that-contains-link = (gethash backlink-id org-nodes)
       when node-that-contains-link
       collect (let ((roam-object-that-contains-link
                      (org-node--fabricate-roam-object node-that-contains-link)))
                 (org-roam-backlink-create
                  :target-node roam-object
                  :source-node roam-object-that-contains-link
                  ;; Differs from upstream.  Supposed to be position of the
                  ;; link, but we just give it the position of the subtree!
                  ;; In other words, it's as if a link sits inside the heading.
                  :point (org-roam-node-point roam-object-that-contains-link)
                  :properties nil))))))


;;; Feed the DB

(defun org-node-feed-file-to-roam-db ()
  (emacsql-with-transaction (org-roam-db)
    (cl-loop with file = (buffer-file-name)
             for node being the hash-values of org-nodes
             when (equal file (plist-get node :file-path))
             do (org-node--feed-node-to-roam-db node))))

(defun org-node--feed-node-to-roam-db (node)
  (let ((id (plist-get node :id))
        (file-path (plist-get node :file-path))
        (tags (plist-get node :tags))
        (aliases (plist-get node :aliases))
        (roam-refs (plist-get node :roam-refs))
        (title (plist-get node :title))
        (properties (plist-get node :properties))
        (backlink-origins (plist-get node :backlink-origins))
        (is-subtree (plist-get node :is-subtree))
        (pos (org-node--visit-get-pos node)))
    ;; `org-roam-db-insert-file'
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file-path nil "" "" "")))
    ;; `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query
       [:insert :into aliases
        :values $v1]
       (mapcar (lambda (alias)
                 (vector id alias))
               aliases)))
    ;; `org-roam-db-insert-tags'
    (when tags
      (org-roam-db-query
       [:insert :into tags
        :values $v1]
       (mapcar (lambda (tag)
                 (vector id tag))
               tags)))
    ;; `org-roam-db-insert-node-data'
    ;; TODO actually sposed to insert formatted title, see src
    (when is-subtree
      (let ((file file-path)
            (todo (plist-get node :todo))
            (priority nil)
            (level (plist-get node :level))
            (scheduled (when-let ((scheduled (plist-get node :scheduled)))
                         (format-time-string
                          "%FT%T%z"
                          (encode-time (org-parse-time-string scheduled)))))
            (deadline (when-let ((deadline (plist-get node :deadline)))
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string deadline)))))
            (olp nil))
        (org-roam-db-query
         [:insert :into nodes
          :values $v1]
         (vector id file level pos todo priority
                 scheduled deadline title properties olp))))
    ;; `org-roam-db-insert-file-node'
    (when (not is-subtree)
      (let ((file file-path)
            (pos 1)
            (todo nil)
            (priority nil)
            (scheduled nil)
            (deadline nil)
            (level 0)
            (tags tags) ;; meaning is a bit off
            (olp nil))
        (org-roam-db-query
         [:insert :into nodes
          :values $v1]
         (vector id file level pos todo priority
                 scheduled deadline title properties olp))))
    ;; `org-roam-db-insert-refs'
    (dolist (ref-link roam-refs)
      (dolist (individual-ref (org-node--ref-link->list ref-link id))
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           individual-ref)))
    ;; `org-roam-db-insert-link'
    ;; TODO real pos and outline, once we have that info
    (dolist (backlink backlink-origins)
      (org-roam-db-query
       [:insert :into links
        :values $v1]
       (vector pos backlink id "id" (list :outline nil))))
    ;; `org-roam-db-insert-citation'
    ;; TODO once we have that info
    ))

(defun org-node-feed-roam-db ()
  (require 'org-roam)
  (org-node-cache-ensure-fresh)
  (org-roam-db--close)
  (org-roam-db-clear-all)
  (emacsql-with-transaction (org-roam-db)
    ;; (emacsql-with-connection (db (org-roam-db))
    ;; Attempt to make it faster... doesn't help
    ;; (emacsql-send-message db "pragma journal_mode = WAL")
    ;; (emacsql-send-message db "pragma synchronous = normal")
    ;; (emacsql-send-message db "pragma temp_store = memory")
    ;; (emacsql-send-message db "pragma mmap_size = 30000000000")
    (emacsql (org-roam-db) [:begin :transaction])
    (cl-loop
     with ctr = 0
     with max = (hash-table-count org-nodes)
     for node being the hash-values of org-nodes
     do
     (when (= 0 (% (cl-incf ctr) 10))
       (message "Inserting into SQL DB... %d/%d" ctr max))
     (org-node--feed-node-to-roam-db node))
    (emacsql (org-roam-db) [:end :transaction])))

(defun org-node--ref-link->list (ref node-id)
  "Worker code adapted from `org-roam-db-insert-ref'"
  (let (rows)
    (cond (;; @citeKey
           (string-prefix-p "@" ref)
           (push (vector node-id (substring ref 1) "cite") rows))
          (;; [cite:@citeKey]
           ;; Basically, a given citation can contain multiple keys
           (string-prefix-p "[cite:" ref)
           (condition-case nil
               (let ((cite-obj (org-cite-parse-objects ref)))
                 (org-element-map cite-obj 'citation-reference
                   (lambda (cite)
                     (let ((key (org-element-property :key cite)))
                       (push (vector node-id key "cite") rows)))))
             (error
              (lwarn '(org-roam) :warning
                     "%s:%s\tInvalid cite %s, skipping..." (buffer-file-name) (point) ref))))
          (;; https://google.com, cite:citeKey
           ;; Note: we use string-match here because it matches any link:
           ;; e.g. [[cite:abc][abc]] But this form of matching is loose, and can
           ;; accept invalid links e.g. [[cite:abc]
           (string-match org-link-any-re (org-link-encode ref '(#x20)))
           (setq ref (org-link-encode ref '(#x20)))
           (let ((ref-url (url-generic-parse-url (or (match-string 2 ref) (match-string 0 ref))))
                 (link-type ()) ;; clear url-type for backward compatible.
                 (path ()))
             (url-type (url-generic-parse-url "https://google.com"))
             (setq link-type (url-type ref-url))
             (setf (url-type ref-url) nil)
             (setq path (org-link-decode (url-recreate-url ref-url)))
             (if (and (boundp 'org-ref-cite-types)
                      (or (assoc link-type org-ref-cite-types)
                          (member link-type org-ref-cite-types)))
                 (dolist (key (org-roam-org-ref-path-to-keys path))
                   (push (vector node-id key link-type) rows))
               (push (vector node-id path link-type) rows))))
          (t
           (lwarn '(org-roam) :warning
                  "%s:%s\tInvalid ref %s, skipping..." (buffer-file-name) (point) ref)))
    rows))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
