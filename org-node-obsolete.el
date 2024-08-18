;;; org-node-obsolete.el --- Outta sight I'm not tempted to clean them up -*- lexical-binding: t; -*-

(require 'seq)
(require 'cl-lib)

(defvar org-node-obsolete-names
  '(org-node-rescan-hook org-node-rescan-functions "30 September 2024")
  "Alist of deprecated symbol names and their new names.")

(defun org-node-obsolete-warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

Warn if any old name in `org-node-obsolete-names' is bound.  Then
copy the value in the old name so that the new name gets the same
value."
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

(add-hook 'org-node-insert-link-hook #'org-node--deprec-insert-link-hook -99)
(defun org-node--deprec-insert-link-hook (&rest args)
  (when args
    (display-warning 'org-node "Hook `org-node-insert-link-hook' has changed, now passes no arguments")))

;;; Stuff expunged to fakeroam.el

(defun org-node-faster-roam-list-files ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-list-files' to `org-node-fakeroam-list-files'")
  (when (fboundp 'org-node-fakeroam-list-files)
    (org-node-fakeroam-list-files)))

(defun org-node-faster-roam-list-dailies ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-list-dailies' to `org-node-fakeroam-list-dailies'")
  (when (fboundp 'org-node-fakeroam-list-dailies)
    (org-node-fakeroam-list-dailies)))

(defun org-node-faster-roam-daily-note-p ()
  (require 'org-node-fakeroam)
  (message "Renames for the rename god.  Renamed `org-node-faster-roam-daily-note-p' to `org-node-fakeroam-daily-note-p'")
  (when (fboundp 'org-node-fakeroam-daily-note-p)
    (org-node-fakeroam-daily-note-p)))

(provide 'org-node-obsolete)

;;; org-node-obsolete.el ends here
