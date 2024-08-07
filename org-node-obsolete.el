;;; org-node-obsolete.el --- Outta sight so I'm not tempted to clean them up -*- lexical-binding: t; -*-

;; One-shot variable warnings
(let ((aliases '((org-node-eagerly-update-link-tables org-node-perf-eagerly-update-link-tables "8 August 2024")
                 (org-node-cache-rescan-file-hook org-node-rescan-hook)
                 (org-node-cache-reset-hook nil)
                 (org-node-format-candidate-fn nil)
                 (org-node-filename-fn nil)
                 (org-node-collection nil))))
  (defun org-node--warn-obsolete-variables ()
    "To be called when turning some mode on."
    (while-let ((row (pop aliases)))
      (seq-let (old new removed-by) row
        (when (and (boundp old) (symbol-value old))
          (unless new
            (lwarn 'org-node :warning "Your config uses removed variable: %S" old))
          (when new
            (lwarn 'org-node :warning "Your config uses old variable: %S, will be removed by %s.  New name: %S"
                   old (or removed-by "30 July 2024") new)
            (set new (symbol-value old))))))))

;; All these hacks...  because I like renaming things
(advice-add 'org-node--warn-obsolete-variables :after
            (let (warned-once)
              (lambda ()
                (unless warned-once
                  (setq warned-once t)
                  (when (boundp 'org-node-prefer-file-level-nodes)
                    (display-warning
                     'org-node "Your config uses old variable: `org-node-prefer-file-level-nodes', will be removed by 8 August 2024.  New option is `org-node-prefer-with-heading', with opposite meaning!")
                    (setq org-node-prefer-with-heading
                          (not org-node-prefer-file-level-nodes)))))))

(defmacro org-node--defobsolete (old new &optional interactive when removed-by)
  "Define OLD as a function that runs NEW.
Also, running OLD will emit a deprecation warning the first time.
If INTERACTIVE, define it as an interactive function.  But
remember, it is still not autoloaded.  Optional string WHEN says
when it was deprecated and REMOVED-BY when it may be removed."
  `(let (warned-once)
     (defun ,old (&rest args)
       (declare (obsolete ',new ,(or when "July 2024")))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your config uses old function name: %S, which will be removed by %s.  New name: %S"
                ',old ,(or removed-by "8 August 2024") ',new))
       (apply ',new args))))

(org-node--defobsolete org-nodeify-entry
                       org-node-nodeify-entry t)

(org-node--defobsolete org-node-random
                       org-node-visit-random t)

(org-node--defobsolete org-node-slugify-as-url
                       org-node-slugify-for-web)

(org-node--defobsolete org-node-new-by-roam-capture
                       org-node-new-via-roam-capture)


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
