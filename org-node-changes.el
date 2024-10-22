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
(require 'ol)

(defvar org-node-changes--new-names
  '()
  "Alist of deprecated symbol names and their new names.")

(defvar org-node-changes--warned-roam-id nil)
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
    ;; 2024-09-19 Clean up deprecated persist-defvars
    (unless (memq system-type '(windows-nt ms-dos))
      (cl-loop for sym in '(org-node--file<>mtime
                            org-node--file<>previews)
               do (let ((file (org-node-changes--guess-persist-filename sym)))
                    (when (file-exists-p file)
                      (delete-file file))))))
  ;; 2024-10-18
  (unless org-node-changes--warned-roam-id
    (when (eq (org-link-get-parameter "id" :follow) 'org-roam-id-open)
      (setq org-node-changes--warned-roam-id t)
      (message
       "%s" "Note: org-roam overrides ID-link behavior, you may want to
      revert to vanilla by evalling:
      (org-link-set-parameters
       \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)"))))

;; Remove in Dec or so
(defun org-node-changes--guess-persist-filename (sym)
  (let ((dir (or (get sym 'persist-location)
                 (bound-and-true-p persist--directory-location)
                 (locate-user-emacs-file "persist"))))
    (expand-file-name (symbol-name sym) dir)))

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

(org-node-changes--def-whiny-alias 'org-node-affix-with-olp
                                   'org-node-prefix-with-olp)

(org-node-changes--def-whiny-alias 'org-node-complete-at-point-global-mode
                                   'org-node-complete-at-point-mode)

(define-obsolete-function-alias
  'org-node-get-id-links 'org-node-get-id-links-to "2024-10-04")

(define-obsolete-function-alias
  'org-node-get-reflinks 'org-node-get-reflinks-to "2024-10-04")

(define-obsolete-variable-alias
  'org-node--series 'org-node-built-series "2024-10-07")

(provide 'org-node-changes)

;;; org-node-changes.el ends here
