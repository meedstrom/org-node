;;; org-node-roam.el --- Rudimentary Org-roam replica -*- lexical-binding: t; -*-

(require 'org-node-common)
(require 'url-parse)
(require 'ol)

(define-obsolete-function-alias
  'org-node--convert-to-roam 'org-node-roam--make-fake "2024-07-11")

(define-obsolete-function-alias
  'org-node--fabricate-roam-backlinks 'org-node-roam--make-backlinks "2024-07-11")

(define-obsolete-function-alias
  'org-node--fabricate-roam-reflinks 'org-node-roam--make-reflinks "2024-07-11")

(define-obsolete-function-alias
  'org-node-feed-roam-db 'org-node-roam-db-reset "2024-07-11")

(let (warned-once)
  (defun org-node-feed-file-to-roam-db (&optional files)
    (unless warned-once
      (setq warned-once t)
      (display-warning 'org-node "Deprecated function `org-node-feed-file-to-roam-db', use `org-node-roam-db-shim-mode' instead"))
    (org-node-roam-db-feed files)))

(defun org-node-roam-enable-redisplay ()
  "Make the roam buffer react when point moves in any Org buffer.

Normally, `org-roam-db-autosync-mode' sets this up for you - this
exists for people who turn that off.

Revert with `org-node-roam-disable-redisplay'."
  (interactive)
  (require 'org-roam)
  (dolist (buf (org-buffer-list))
    (with-current-buffer buf
      (add-hook 'post-command-hook #'org-roam-buffer--redisplay-h nil t)))
  (add-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h))

(defun org-node-roam-disable-redisplay ()
  "Stop the roam buffer's responsiveness."
  (interactive)
  (dolist (buf (org-buffer-list))
    (with-current-buffer buf
      (remove-hook 'post-command-hook #'org-roam-buffer--redisplay-h t)))
  (remove-hook 'org-mode-hook #'org-roam-buffer--setup-redisplay-h))

;;; Method 1: Fake roam backlinks, no SQLite

(define-minor-mode org-node-roam-no-sql-mode
  "Instruct org-roam to consult `org-roam-backlink-p' objects faked
by org-node so that you can use \\[org-roam-buffer-toggle]
without syncing org-roam's SQL database nor having SQLite
installed."
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

(defun org-node-roam--make-fake (node)
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
;; (seq-random-elt (hash-table-keys org-node--backlinks-by-id))
;; (seq-random-elt (hash-table-values org-node--backlinks-by-id))

(defun org-node-roam--make-backlinks (target-roam-node &rest _)
  "Return org-roam-backlink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-backlinks-get'."
  (require 'org-roam)
  (let ((target-id (org-roam-node-id target-roam-node)))
    (when target-id
      (let ((links (gethash target-id org-node--backlinks-by-id))
            ;; TODO: this probs necessary, but verify in org-roam-buffer becsue
            ;; i expect we see duplicates, and if not, why not?
            ;; (links (delete-dups (gethash target-id org-node--backlinks-by-id)))
            ))
      (cl-loop
       for link-data in links
       as src-id = (plist-get link-data :origin)
       as src-node = (gethash src-id org-node--node-by-id)
       when src-node
       collect (org-roam-backlink-create
                :target-node target-roam-node
                :source-node (org-node-roam--make-fake src-node)
                :point (plist-get link-data :pos)
                :properties (plist-get link-data :properties))))))

;; Eval to see examples of what it has to work with...
;; (seq-random-elt (hash-table-keys org-node--reflinks-by-ref))
;; (seq-random-elt (hash-table-values org-node--reflinks-by-ref))

(defun org-node-roam--make-reflinks (target-roam-node &rest _)
  "Return org-roam-reflink objects targeting TARGET-ROAM-NODE.
Designed as override advice for `org-roam-reflinks-get'."
  (require 'org-roam)
  (let* ((target-id (org-roam-node-id target-roam-node))
         (node (gethash target-id org-node--node-by-id)))
    (when node
      (cl-loop
       for ref in (org-node-get-refs node)
       as reflinks = (cl-loop
                      for link in (gethash ref org-node--reflinks-by-ref)
                      as src-id = (plist-get link :origin)
                      as src-node = (gethash src-id org-node--node-by-id)
                      when src-node
                      collect (org-roam-reflink-create
                               :ref (plist-get link :dest)
                               :source-node (org-node-roam--make-fake src-node)
                               :point (plist-get link :pos)
                               :properties (plist-get link :properties)))
       when reflinks append reflinks))))


;;; Method 2: feed the DB

(define-minor-mode org-node-roam-db-shim-mode
  ""
  :global t
  (require 'org-roam)
  (remove-hook 'org-node-rescan-hook 'org-node-feed-file-to-roam-db) ;; deprec
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
             do
             (org-node-roam--db-add-node node)
             (org-node-roam--db-add-links (org-node-get-id node)))))

(defun org-node-roam-db-reset ()
  (interactive)
  (require 'org-roam)
  (require 'emacsql)
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
             (org-node-roam--db-add-node node)
             (org-node-roam--db-add-links (org-node-get-id node)))))

(defun org-node-roam--ref->list (ref node-id)
  "Code adapted from `org-roam-db-insert-refs'"
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
    (dolist (ref-chunk roam-refs)
      (dolist (individual-ref (org-node-roam--ref->list ref-chunk id))
        (org-roam-db-query [:insert :into refs
                            :values $v1]
                           individual-ref)))
    ;; See `org-roam-db-insert-citation'
    ;; TODO once we have that info
    ))

(defun org-node-roam--db-add-links (target-id)
  "Add ID-links and reflinks pointing to TARGET-ID."
  ;; See `org-roam-db-insert-link'
  (dolist (link (append (gethash target-id org-node--backlinks-by-id)
                        (org-node-get-reflinks
                         (gethash target-id org-node--node-by-id))))
    (org-roam-db-query [:insert :into links
                        :values $v1]
                       (vector (plist-get link :pos)
                               (plist-get link :origin)
                               target-id
                               (plist-get link :type)
                               (plist-get link :properties)))))

(provide 'org-node-roam)

;;; org-node-roam.el ends here
