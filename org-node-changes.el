;;; org-node-changes.el --- Help user transit renamed user options -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström
;;
;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; For a full copy of the GNU General Public License
;; see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I rename things a lot.  That would break things for users unless I make
;; aliases.  But `define-obsolete-variable-alias' does not warn users about
;; user options, so they can blissfully keep referring to a thrice-deprecated
;; variable name for years and not even know.

;; Thus this file.  Actually tell the user, once, for each old symbol that
;; they set or call.

;;; Code:

(require 'seq)
(require 'cl-lib)

(defvar org-node-changes--new-names
  '((org-node-rescan-hook org-node-rescan-functions "30 September 2024")
    (org-node-series org-node-series-defs "15 September 2024"))
  "Alist of deprecated symbol names and their new names.")

(defun org-node-changes--warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

Warn if any old name in `org-node-changes--new-names' is bound.  Then
copy the value in the old name so that the new name gets the same
value."
  (while-let ((row (pop org-node-changes--new-names)))
    (seq-let (old new removed-by) row
      (unless removed-by
        (setq removed-by "30 August 2024"))
      (when (boundp old)
        (if new
            (progn
              (lwarn 'org-node :warning "Your initfiles set old variable: %S, will be REMOVED by %s.  Please use new name: %S"
                     old removed-by new)
              (set new (symbol-value old)))
          (lwarn 'org-node :warning "Your initfiles set removed variable: %S" old)))
      (when (and old (where-is-internal old))
        (if new
            (lwarn 'org-node :warning "Your initfiles key-bind an old command name: %S.  Please use new name: %S"
                   old new)
          (lwarn 'org-node :warning "Your initfiles key-bind a removed command: %S"
                 old)))))
  ;; Bonus: a hook changed, warn if used in old way
  ;; FIXME: Leads to (wrong-type-argument listp t).
  ;; (cl-loop
  ;;  for fn in org-node-insert-link-hook
  ;;  when (and (help-function-arglist fn)
  ;;            (not (member (car (help-function-arglist fn))
  ;;                         '(&optional &rest &body))))
  ;;  return (display-warning
  ;;          'org-node "Hook `org-node-insert-link-hook' has changed, now passes no arguments"))
  )

(defmacro org-node-changes--def-whiny-alias (old new &optional interactive when removed-by)
  "Define OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function.  Optional
string WHEN says when it was deprecated and REMOVED-BY when it
may be removed.  When these strings are omitted, fall back on
hardcoded strings."
  `(let (warned-once)
     (add-to-list 'org-node-changes--new-names '(,old ,new ,removed-by))
     (defun ,old (&rest args)
       (declare (obsolete ',new ,(or when "2024")))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your initfiles use old function name: %S, which will be REMOVED by %s.  Please use new name: %S"
                ',old ,(or removed-by "30 August 2024") ',new))
       (apply ',new args))))

(org-node-changes--def-whiny-alias org-node-files
                                   org-node-list-files)

(org-node-changes--def-whiny-alias org-node-rename-file-by-title-maybe
                                   org-node-rename-file-by-title)

(org-node-changes--def-whiny-alias org-node-faster-roam-list-files
                                   org-node-fakeroam-list-files)

(org-node-changes--def-whiny-alias org-node-faster-roam-list-dailies
                                   org-node-fakeroam-list-dailies)

(org-node-changes--def-whiny-alias org-node-faster-roam-daily-note-p
                                   org-node-fakeroam-daily-note-p)

;; Many of these existed only briefly, 0-3 days 2024-08-19

(org-node-changes--def-whiny-alias org-node--series-standard-goto
                                   org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias org-node--series-standard-try-goto
                                   org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias org-node--standard-series-try-goto-id
                                   org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias org-node--series-standard-prompter
                                   org-node--example-prompter)

(org-node-changes--def-whiny-alias org-node--standard-series-prompter
                                   org-node--example-prompter)

(org-node-changes--def-whiny-alias org-node--default-daily-goto
                                   org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias org-node--standard-series-try-goto-file
                                   org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias org-node--default-daily-try-goto
                                   org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias org-node--default-daily-classifier
                                   org-node--example-daily-classifier)

(org-node-changes--def-whiny-alias org-node--default-daily-whereami
                                   org-node--example-daily-whereami)

(org-node-changes--def-whiny-alias org-node--default-daily-creator
                                   org-node--example-daily-creator)

;; Polite aliases for now, upgrade to whiny later

(define-obsolete-function-alias
  'org-node--extract-ymd #'org-node-extract-ymd  "2024-08-21")

(define-obsolete-function-alias
  'org-node--create #'org-node-create "2024-08-21")

;; (define-obsolete-variable-alias
;;   'org-node-creation-fn 'org-node-new-node-fn "2024-08-22")

;; (define-obsolete-variable-alias
;;   'org-node-creation-hook 'org-node-new-node-hook "2024-08-22")

;; (define-obsolete-function-alias
;;   'org-node-create #'org-node-new-node "2024-08-22")

;;;###autoload
(define-obsolete-function-alias
  'org-node-series-menu #'org-node-series-dispatch "2024-08-21")

;; Variables

(define-obsolete-variable-alias
  'org-node--series-info 'org-node--series "2024-08-21")

(define-obsolete-variable-alias
  'org-node-mark-days 'org-node--mark-days "2024-08-21")

(define-obsolete-function-alias
  'org-node-helper/try-goto-id #''org-node-helper-try-goto-id "2024-08-24")

(define-obsolete-function-alias
  'org-node-helper/try-visit-file #'org-node-helper-try-visit-file "2024-08-24")

(define-obsolete-function-alias
  'org-node-helper/filename->ymd #'org-node-helper-filename->ymd   "2024-08-24")

(provide 'org-node-changes)

;;; org-node-changes.el ends here
