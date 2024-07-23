;;; org-node-obsolete.el --- Outta sight so I'm not tempted to clean them up -*- lexical-binding: t; -*-

;; One-shot variable warnings
(let ((aliases '(org-node-cache-rescan-file-hook org-node-rescan-hook
                 org-node-cache-reset-hook nil
                 org-node-format-candidate-fn nil
                 org-node-collection nil)))
  (defun org-node--warn-obsolete-variables ()
    "To be called when turning some mode on."
    (while aliases
      (let ((old (pop aliases))
            (new (pop aliases)))
        (when (and (boundp old) (symbol-value old))
          (unless new
            (lwarn 'org-node :warning "Your config uses removed variable: %S" old))
          (when new
            (lwarn 'org-node :warning "Your config uses old variable: %S,  new name: %S"
                   old new)
            (set new (symbol-value old))))))))

(defmacro org-node--defobsolete (old new &optional interactive)
  "Define OLD as a function that runs NEW.
Also, running OLD will emit a deprecation warning the first time.
If INTERACTIVE, define it as an interactive function.  But
remember, it is not autoloaded."
  `(let (warned-once)
     (defun ,old (&rest args)
       (declare (obsolete ',new "July 2024"))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your config uses old function name: %S,  new name: %S"
                ',old ',new))
       (apply ',new args))))

(org-node--defobsolete org-nodeify-entry
                       org-node-nodeify-entry t)

;; Existed for just an hour or so
(org-node--defobsolete org-node-rename-file-by-title-if-roam
                       org-node-rename-file-by-title-maybe)

(org-node--defobsolete org-node-slugify-like-roam-defaults
                       org-node-slugify-like-roam-default)

(org-node--defobsolete org-node-random
                       org-node-visit-random t)

(org-node--defobsolete org-node-slugify-as-url
                       org-node-slugify-for-web)

(org-node--defobsolete org-node-new-by-roam-capture
                       org-node-new-via-roam-capture)

(org-node--defobsolete org-node-get-backlinks
                       org-node-get-id-links)


;;; Function names from fakeroam

(org-node--defobsolete org-node-roam-db-shim-mode
                       org-node-fakeroam-db-feed-mode t)

(org-node--defobsolete org-node-roam-db-reset
                       org-node-fakeroam-db-rebuild t)

(org-node--defobsolete org-node-roam-redisplay-mode
                       org-node-fakeroam-redisplay-mode t)

(org-node--defobsolete org-node-roam-no-sql-mode
                       org-node-fakeroam-nosql-mode t)

;; New autoloads because fakeroam used to be loaded together with core (and
;; didn't need autoloads)

;;;###autoload
(defun org-node-feed-file-to-roam-db (&optional _)
  (declare (obsolete 'org-node-fakeroam--db-update-files "2024-07-11"))
  (display-warning 'org-node "Your config uses deprecated `org-node-feed-file-to-roam-db', use `org-node-fakeroam-db-feed-mode' instead"))

;;;###autoload
(defun org-node--fabricate-roam-backlinks (&optional _)
  (declare (obsolete 'org-node-fakeroam--mk-backlinks "2024-07-11"))
  (display-warning 'org-node "Your config uses deprecated `org-node--fabricate-roam-backlinks', use `org-node-fakeroam-nosql-mode' instead"))

;;;###autoload
(defun org-node--fabricate-roam-reflinks (&optional _)
  (declare (obsolete 'org-node-fakeroam--mk-reflinks "2024-07-11"))
  (display-warning 'org-node "Your config uses deprecated `org-node--fabricate-roam-reflinks', use `org-node-fakeroam-nosql-mode' instead"))

(provide 'org-node-obsolete)

;;; org-node-obsolete.el ends here
