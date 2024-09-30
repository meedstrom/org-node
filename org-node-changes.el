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
;; thrice-deprecated variable name for years and not know.

;; Thus this file.  Actually tell the user, once, for each old symbol that
;; they set or call.

;;; Code:

(require 'seq)
(require 'cl-lib)

(defvar org-node-changes--new-names
  '((org-node--series org-node-built-series))
  "Alist of deprecated symbol names and their new names.")

(defvar org-node-changes--warned-once nil)
(defun org-node-changes--warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

Warn if any old name in `org-node-changes--new-names' is bound.  Then
copy the value in the old name so that the new name gets the same
value."
  (while-let ((row (pop org-node-changes--new-names)))
    (seq-let (old new removed-by) row
      (unless removed-by
        (setq removed-by "30 October 2024"))
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
  (unless org-node-changes--warned-once
    (setq org-node-changes--warned-once t)
    (display-warning
     'org-node "This package is no longer updated on the \"melpa\" branch, but \"main\"; change your package recipe")
    ;; 2024-09-19 Clean up deprecated persist-defvars
    (unless (memq system-type '(windows-nt ms-dos))
      (cl-loop
       for sym in '(org-node--file<>mtime
                    org-node--file<>previews)
       as dir = (or (get sym 'persist-location)
                    (bound-and-true-p persist--directory-location))
       when dir do
       (let ((file (expand-file-name (symbol-name sym) dir)))
         (and (file-exists-p file)
              (file-writable-p file)
              (delete-file file)))))))

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
                ,old ,(or removed-by "30 October 2024") ,new))
       (apply ,new args))))

;; (define-obsolete-variable-alias
;;   'org-node-creation-fn 'org-node-new-node-fn "2024-08-22")

;; (define-obsolete-variable-alias
;;   'org-node-creation-hook 'org-node-new-node-hook "2024-08-22")

;; (org-node-changes--def-whiny-alias
;;   'org-node-create #'org-node-new-node "2024-08-22")

;; 2024-09-17
;; NOTE: Marking them as obsolete or whiny has to be done inside that library
(declare-function org-node-fakeroam-new-via-roam-capture "org-node-fakeroam")
(declare-function org-node-fakeroam-slugify-via-roam "org-node-fakeroam")
(defalias 'org-node-new-via-roam-capture #'org-node-fakeroam-new-via-roam-capture)
(defalias 'org-node-slugify-like-roam-actual #'org-node-fakeroam-slugify-via-roam)

(org-node-changes--def-whiny-alias
 'org-node-affix-with-olp 'org-node-prefix-with-olp "2024-09-19")

(org-node-changes--def-whiny-alias
 'org-node-complete-at-point-global-mode
 'org-node-complete-at-point-mode "2024-09-29")

(provide 'org-node-changes)

;;; org-node-changes.el ends here
