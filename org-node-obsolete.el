;;; org-node-obsolete.el --- Outta sight so I'm not tempted to clean them up -*- lexical-binding: t; -*-

(defvar org-node-obsolete-names
  '()
  "Alist of deprecated symbol names and their new names.")

(defun org-node-obsolete-warn ()
  "Maybe print one-shot warnings, then become a no-op."
  (while-let ((row (pop org-node-obsolete-names)))
    (seq-let (old new removed-by) row
      (unless removed-by
        (setq removed-by "30 August 2024"))
      (when (boundp old)
        (if new
            (progn
              (lwarn 'org-node :warning "Your config sets old variable: %S, will be REMOVED by %s.  Please use new name: %S"
                     old removed-by new)
              (set new (symbol-value old)))
          (lwarn 'org-node :warning "Your config sets removed variable: %S" old)))
      (when (where-is-internal old)
        (if new
            (lwarn 'org-node :warning "Your config key-binds an old command name: %S.  Please use new name: %S"
                   old new)
          (lwarn 'org-node :warning "Your config key-binds a removed command: %S"
                 old))))))

(defmacro org-node-obsolete-defun (old new &optional interactive when removed-by)
  "Define OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function (but not
autoloaded).  Optional string WHEN says when it was deprecated
and REMOVED-BY when it may be removed."
  `(let (warned-once)
     (add-to-list 'org-node-obsolete-names '(,old ,new ,removed-by))
     (defun ,old (&rest args)
       (declare (obsolete ',new ,(or when "2024")))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your config uses old function name: %S, which will be REMOVED by %s.  Please use new name: %S"
                ',old ,(or removed-by "30 August 2024") ',new))
       (apply ',new args))))

(org-node-obsolete-defun org-node-files
                         org-node-list-files)

(org-node-obsolete-defun org-node-rename-file-by-title-maybe
                         org-node-rename-file-by-title)

(org-node-obsolete-defun org-node--series-standard-goto
                         org-node--series-standard-try-goto)

(org-node-obsolete-defun org-node--default-daily-goto
                         org-node--default-daily-try-goto)

;;; Stuff expunged to fakeroam.el

(defun org-node-faster-roam-list-files ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-list-files' to `org-node-fakeroam-list-files'")
  (org-node-fakeroam-list-files))

(defun org-node-faster-roam-list-dailies ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-list-dailies' to `org-node-fakeroam-list-dailies'")
  (org-node-fakeroam-list-dailies))

(defun org-node-faster-roam-daily-note-p ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-daily-note-p' to `org-node-fakeroam-daily-note-p'")
  (org-node-fakeroam-daily-note-p))

(provide 'org-node-obsolete)

;;; org-node-obsolete.el ends here
