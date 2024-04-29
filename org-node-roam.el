;;; org-node-roam.el -*- lexical-binding: t; -*-

;; To make `org-roam-buffer-toggle' work without any DB, eval this:
;; (advice-add 'org-roam-backlinks-get :override #'org-node--fabricate-roam-backlinks)

(defun org-node--fabricate-roam-object (node)
  "Construct an org-roam-node object from NODE.
This just uses information in org-node's own `org-nodes'.
Some of the fields are blank."
  (require 'org-roam)
  (org-roam-node-create
   :file (org-node-file-path node)
   :id (org-node-id node)
   :level (org-node-level node)
   :title (org-node-title node)
   :tags (org-node-tags node)
   :aliases (org-node-aliases node)
   :point (org-node-pos node)))

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
       for backlink-id in (org-node-backlink-origins node)
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
             when (equal file (org-node-file-path node))
             do (org-node--feed-node-to-roam-db node))))

(defun org-node--feed-node-to-roam-db (node)
  (let ((id (org-node-id node))
        (file-path (org-node-file-path node))
        (tags (org-node-tags node))
        (aliases (org-node-aliases node))
        (roam-refs (org-node-refs node))
        (title (org-node-title node))
        (properties (org-node-properties node))
        (backlink-origins (org-node-backlink-origins node))
        (is-subtree (org-node-is-subtree node))
        (pos (org-node-pos node)))
    ;; Replace `org-roam-db-insert-file'
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file-path nil "" "" "")))
    ;; Replace `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query
       [:insert :into aliases
        :values $v1]
       (mapcar (lambda (alias)
                 (vector id alias))
               aliases)))
    ;; Replace `org-roam-db-insert-tags'
    (when tags
      (org-roam-db-query
       [:insert :into tags
        :values $v1]
       (mapcar (lambda (tag)
                 (vector id tag))
               tags)))
    ;; Replace `org-roam-db-insert-node-data'
    ;; TODO actually sposed to insert formatted title, see src
    (when is-subtree
      (let ((file file-path)
            (todo (org-node-todo node))
            (priority nil)
            (level (org-node-level node))
            (scheduled (when-let ((scheduled (org-node-scheduled node)))
                         (format-time-string
                          "%FT%T%z"
                          (encode-time (org-parse-time-string scheduled)))))
            (deadline (when-let ((deadline (org-node-deadline node)))
                        (format-time-string
                         "%FT%T%z"
                         (encode-time (org-parse-time-string deadline)))))
            (olp nil))
        (org-roam-db-query
         [:insert :into nodes
          :values $v1]
         (vector id file level pos todo priority
                 scheduled deadline title properties olp))))
    ;; Replace `org-roam-db-insert-file-node'
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
    ;; Replace `org-roam-db-insert-refs'
    (dolist (ref-link roam-refs)
      (dolist (individual-ref (org-node--ref-link->list ref-link id))
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           individual-ref)))
    ;; Replace `org-roam-db-insert-link'
    ;; TODO real pos and outline, once we have that info
    (dolist (backlink backlink-origins)
      (org-roam-db-query
       [:insert :into links
        :values $v1]
       (vector pos backlink id "id" (list :outline nil))))
    ;; Replace `org-roam-db-insert-citation'
    ;; TODO once we have that info
    ))

(defun org-node-feed-roam-db ()
  (require 'org-roam)
  (org-roam-db-clear-all)
  (org-roam-db--close)
  (delete-file org-roam-db-location)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop
     with nodes = (--filter (plist-get it :id) (hash-table-values org-nodes))
     with ctr = 0
     with max = (length nodes)
     for node in nodes
     do (when (= 0 (% (cl-incf ctr) 10))
          (message "Inserting into %s... %d/%d"
                   org-roam-db-location ctr max))
     (org-node--feed-node-to-roam-db node))))

;; Revamp backlink-check-buffer
;;  one thing aside from the cacher will possibly be aware of refs:
;; - check-buffer
;;
;; Make a decision about how to treat links that look like refs.  Both the
;; cacher and the check-buffer (if we even keep it) will do the same thing.
;;
;; From the POV of the cacher, already know what to do. Will just add non-id
;; links to some refs-table.
;;
;; The question that remains: how to use that refs-table?  Well, fix-all (and
;; whatever small hooks/advices we add to mirror its effect) is going to use the
;; refs-table to...  insert them as backlinks.

;; ok look ,two sides.  One side COLLECTS links into tables, other side USES
;; the;; tables.  and check-buffer style functions would actually do some
;; collection, and additionally consume the data it just collected, but the
;; intent is to reflect what a fix-all would have done.
;;

(defun org-node--ref-link->list (ref node-id)
  "Worker code adapted from `org-roam-db-insert-refs'"
  (let (rows)
    (cond (;; @citeKey
           (string-prefix-p "@" ref)
           (push (vector node-id (substring ref 1) "cite") rows))
          (;; [cite:@citeKey]
           ;; Turns out a given citation can contain multiple keys
           (string-prefix-p "[cite:" ref)
           (condition-case nil
               (let ((cite-obj (org-cite-parse-objects ref)))
                 (org-element-map cite-obj 'citation-reference
                   (lambda (cite)
                     (let ((key (org-element-property :key cite)))
                       (push (vector node-id key "cite") rows)))))
             (error
              (lwarn '(org-node) :warning
                     "%s:%s\tInvalid cite %s, skipping..."
                     (buffer-file-name) (point) ref))))
          (;; https://google.com, cite:citeKey
           (string-match org-link-any-re (org-link-encode ref '(#x20)))
           (setq ref (org-link-encode ref '(#x20)))
           (let ((ref-url (url-generic-parse-url
                           (or (match-string 2 ref) (match-string 0 ref))))
                 (link-type ()) ;; clear url-type for backward compatible.
                 (path ()))
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
           (lwarn '(org-node) :warning
                  "%s:%s\tInvalid ref %s, skipping..."
                  (buffer-file-name) (point) ref)))
    rows))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
