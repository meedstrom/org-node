;;; org-node-roam.el -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'url-parse)
(require 'ol)

;;; Method 1: no DB at all

;; To make `org-roam-buffer-toggle' work without any DB, eval this:
;; (advice-add 'org-roam-backlinks-get :override #'org-node--fabricate-roam-backlinks)

(defun org-node--convert-to-roam (node)
  "Construct an org-roam-node object from NODE."
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

;; Eval to see the form
;; (seq-random-elt (hash-table-keys org-node--links-table))
;; (seq-random-elt (hash-table-values org-node--links-table))

(defun org-node--fabricate-roam-backlinks (roam-target-node &rest _)
  "Return org-roam-backlink objects targeting ROAM-OBJECT.
Designed as override advice for `org-roam-backlinks-get'."
  (require 'org-roam)
  (let ((target-id (org-roam-node-id roam-target-node)))
    (when target-id
      (cl-loop
       for link-data in (gethash target-id org-node--links-table)
       as src-id = (plist-get link-data :src)
       as src-node = (gethash src-id org-nodes)
       when src-node
       collect (org-roam-backlink-create
                :target-node roam-target-node
                :source-node (org-node--convert-to-roam src-node)
                :point (plist-get link-data :pos)
                :properties (plist-get link-data :properties))))))

;; Eval to see the form
;; (seq-random-elt (hash-table-keys org-node--reflinks-table))
;; (seq-random-elt (hash-table-values org-node--reflinks-table))

(defun org-node--fabricate-roam-reflinks (roam-target-node &rest _)
  "Return org-roam-backlink objects targeting ROAM-OBJECT.
Designed as override advice for `org-roam-backlinks-get'."
  (require 'org-roam)
  (let* ((target-id (org-roam-node-id roam-target-node))
         (node (gethash target-id org-nodes)))
    (when node
      (cl-loop
       for ref in (--map (replace-regexp-in-string "https:" "" it)
                         (org-node-get-refs node))
       as reflinks = (cl-loop
                      for link-data in (gethash ref org-node--reflinks-table)
                      as src-id = (plist-get link-data :src)
                      as src-node = (gethash src-id org-nodes)
                      when src-node
                      collect (org-roam-reflink-create
                               :ref ref
                               :source-node (org-node--convert-to-roam src-node)
                               :point (plist-get link-data :pos)
                               :properties (plist-get link-data :properties)))
       when reflinks append reflinks))))


;;; Method 2: feed the DB

(defun org-node--ref->list (ref node-id)
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

(defun org-node--feed-node-to-roam-db (node)
  (let ((id (org-node-get-id node))
        (file-path (org-node-get-file-path node))
        (file-title (org-node-get-file-title node))
        (tags (org-node-get-tags node))
        (aliases (org-node-get-aliases node))
        (roam-refs (org-node-get-refs node))
        (title (org-node-get-title node))
        (properties (org-node-get-properties node))
        (level (org-node-get-level node))
        (todo (org-node-get-todo node))
        (is-subtree (org-node-get-is-subtree node))
        (olp (org-node-get-olp node))
        (pos (org-node-get-pos node)))
    ;; `org-roam-db-insert-file'
    (org-roam-db-query
     [:insert :into files
      :values $v1]
     (list (vector file-path file-title "" "" "")))
    ;; `org-roam-db-insert-aliases'
    (when aliases
      (org-roam-db-query
       [:insert :into aliases
        :values $v1]
       (mapcar (lambda (alias)
                 (vector id alias))
               aliases)))
    ;; `org-roam-db-insert-tags'
    ;; FIXME there's no inheritance
    (when tags
      (org-roam-db-query
       [:insert :into tags
        :values $v1]
       (mapcar (lambda (tag)
                 (vector id tag))
               tags)))
    ;; `org-roam-db-insert-node-data'
    (when is-subtree
      (let ((priority nil)
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
    ;; `org-roam-db-insert-file-node'
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
    ;; `org-roam-db-insert-refs'
    (dolist (ref-link roam-refs)
      (dolist (individual-ref (org-node--ref->list ref-link id))
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           individual-ref)))
    ;; `org-roam-db-insert-citation'
    ;; TODO once we have that info
    ))

(defun org-node--feed-links-to-roam-db (target-id)
  (let ((backlinks (gethash target-id org-node--links-table)))
    (dolist (backlink backlinks)
      ;; See `org-roam-db-insert-link'
      (org-roam-db-query [:insert :into links
                          :values $v1]
                         (vector (plist-get backlink :pos)
                                 (plist-get backlink :src)
                                 target-id
                                 (plist-get backlink :type)
                                 (plist-get backlink :properties))))))

(defun org-node-feed-roam-db ()
  (interactive)
  (require 'org-roam)
  (require 'emacsql)
  (org-roam-db-clear-all)
  (org-roam-db--close)
  (delete-file org-roam-db-location)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop
     with nodes = (-filter #'org-node-id (hash-table-values org-nodes))
     with ctr = 0
     with max = (length nodes)
     for node in nodes
     do (when (= 0 (% (cl-incf ctr) 10))
          (message "Inserting into %s... %d/%d"
                   org-roam-db-location ctr max))
     (org-node--feed-node-to-roam-db node)
     (org-node--feed-links-to-roam-db (org-node-get-id node)))))

(defun org-node-feed-file-to-roam-db ()
  (require 'emacsql)
  (emacsql-with-transaction (org-roam-db)
    (cl-loop with file = (buffer-file-name)
             for node being the hash-values of org-nodes
             when (equal file (org-node-get-file-path node))
             do
             (let ((id (org-node-get-id node)))
               (org-node--feed-node-to-roam-db node)
               (org-node--feed-links-to-roam-db id)
               ))))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
