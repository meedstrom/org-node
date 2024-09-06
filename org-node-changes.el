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
;; user options, which means they can blissfully keep referring to a
;; thrice-deprecated variable name for years and not even know.

;; Thus this file.  Actually tell the user, once, for each old symbol that
;; they set or call.

;;; Code:

(require 'seq)
(require 'cl-lib)

(defvar org-node-changes--new-names
  '((org-node-rescan-hook org-node-rescan-functions)
    (org-node-series org-node-series-defs "15 September 2024")
    (org-node--series-info org-node--series)
    (org-node-mark-days org-node--mark-days))
  "Alist of deprecated symbol names and their new names.")

(defun org-node-changes--warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

Warn if any old name in `org-node-changes--new-names' is bound.  Then
copy the value in the old name so that the new name gets the same
value."
  (while-let ((row (pop org-node-changes--new-names)))
    (seq-let (old new removed-by) row
      (unless removed-by
        (setq removed-by "30 September 2024"))
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
  (cl-loop
   for fn in (bound-and-true-p org-node-insert-link-hook)
   when (and (not (eq t fn))
             (help-function-arglist fn)
             (not (member (car-safe (help-function-arglist fn))
                          '(&optional &rest &body))))
   return (lwarn 'org-node :warning
                 "Hook `org-node-insert-link-hook' has changed, now passes no arguments, but function expects them: %s"
                 fn))
  (when (and (featurep 'org-roam)
             (equal (file-name-directory (locate-library "org-node-fakeroam"))
                    (file-name-directory (locate-library "org-node"))))
    (message "org-node-fakeroam now has its own repo. If you need it, use the new MELPA recipe, or change your recipe to point to https://github.com/meedstrom/org-node-fakeroam")))

(defmacro org-node-changes--def-whiny-alias (old new &optional when interactive removed-by)
  "Define OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function.  Optional
string WHEN says when it was deprecated and REMOVED-BY when it
may be removed.  When these strings are omitted, fall back on
hardcoded strings."
  `(let (warned-once)
     (add-to-list 'org-node-changes--new-names '(,(cadr old) ,(cadr new) ,removed-by))
     (defun ,(cadr old) (&rest args)
       (declare (obsolete ,new ,(or when "2024")))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your initfiles use old function name: %S, which will be REMOVED by %s.  Please use new name: %S"
                ,old ,(or removed-by "30 August 2024") ,new))
       (apply ,new args))))


;; Old, remove soon

(org-node-changes--def-whiny-alias 'org-node-files
                                   'org-node-list-files)

(org-node-changes--def-whiny-alias 'org-node-rename-file-by-title-maybe
                                   'org-node-rename-file-by-title)

(org-node-changes--def-whiny-alias 'org-node-faster-roam-list-files
                                   'org-node-fakeroam-list-files)

(org-node-changes--def-whiny-alias 'org-node-faster-roam-list-dailies
                                   'org-node-fakeroam-list-dailies)

(org-node-changes--def-whiny-alias 'org-node-faster-roam-daily-note-p
                                   'org-node-fakeroam-daily-note-p)

(org-node-changes--def-whiny-alias 'org-node--series-standard-goto
                                   'org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias 'org-node--series-standard-try-goto
                                   'org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias 'org-node--standard-series-try-goto-id
                                   'org-node--example-try-goto-id)

(org-node-changes--def-whiny-alias 'org-node--series-standard-prompter
                                   'org-node--example-prompter)

(org-node-changes--def-whiny-alias 'org-node--standard-series-prompter
                                   'org-node--example-prompter)

(org-node-changes--def-whiny-alias 'org-node--default-daily-goto
                                   'org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias 'org-node--standard-series-try-goto-file
                                   'org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias 'org-node--default-daily-try-goto
                                   'org-node--example-try-goto-file)

(org-node-changes--def-whiny-alias 'org-node--default-daily-classifier
                                   'org-node--example-daily-classifier)

(org-node-changes--def-whiny-alias 'org-node--default-daily-whereami
                                   'org-node--example-daily-whereami)

(org-node-changes--def-whiny-alias 'org-node--default-daily-creator
                                   'org-node--example-daily-creator)


;; Polite aliases for now, upgrade to whiny later

(org-node-changes--def-whiny-alias
 'org-node--extract-ymd 'org-node-extract-ymd)

(org-node-changes--def-whiny-alias
 'org-node--create 'org-node-create)

(org-node-changes--def-whiny-alias
 'org-node-series-menu 'org-node-series-dispatch)

(org-node-changes--def-whiny-alias
 'org-node-helper/try-goto-id 'org-node-helper-try-goto-id)

(org-node-changes--def-whiny-alias
 'org-node-helper/try-visit-file 'org-node-helper-try-visit-file)

(org-node-changes--def-whiny-alias
 'org-node-helper/filename->ymd 'org-node-helper-filename->ymd)

(org-node-changes--def-whiny-alias
 'org-node-helper/mk-series-with-tag-sorted-by-property
 'org-node-mk-series-on-tag-sorted-by-property)

(org-node-changes--def-whiny-alias
 'org-node-mk-series-on-tag-by-property
 'org-node-mk-series-on-tag-sorted-by-property)

;; (define-obsolete-variable-alias
;;   'org-node-creation-fn 'org-node-new-node-fn "2024-08-22")

;; (define-obsolete-variable-alias
;;   'org-node-creation-hook 'org-node-new-node-hook "2024-08-22")

;; (org-node-changes--def-whiny-alias
;;   'org-node-create #'org-node-new-node "2024-08-22")

;; Variables

(provide 'org-node-changes)

;;; org-node-changes.el ends here
