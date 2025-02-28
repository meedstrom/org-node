;;; org-node-changes.el --- Help user transit renamed user options -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström
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
;; aliases.

;; But `define-obsolete-variable-alias' does not warn users about
;; user options, which means they can blissfully keep referring to a
;; thrice-deprecated variable name for years and not know.

;; Thus this file.  Actually tell the user, once, for each old symbol that
;; they set or call.

;;; Code:

(require 'seq)
(require 'subr-x)
(require 'cl-lib)
(require 'ol)
(require 'el-job)

(unless (and (boundp 'el-job-major-version)
             (>= el-job-major-version 1))
  (display-warning
   'org-node "Update el-job to use this version of org-node"))

(defvar org-node-changes--new-names
  '()
  "Alist of deprecated symbol names and their new names.
Names here will cause complaints if bound.")

(defvar org-node-changes--warned-about-roam-id nil
  "Non-nil if did warn about org-roam overriding a link parameter.")

(defun org-node-changes--warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

First, warn if any old name in `org-node-changes--new-names' is bound.
Then copy the value in the old name so that the new name gets the same
value.

Then do other one-shot warnings while we\\='re at it."
  (let ((names org-node-changes--new-names))
    (while-let ((row (pop names)))
      (seq-let (old new removed-by) row
        (unless removed-by
          (setq removed-by "30 January 2025"))
        (when (boundp old)
          (if new
              (progn
                (lwarn 'org-node :warning "Your initfiles set old variable: %S, will be REMOVED by %s.  Please use new name: %S"
                       old removed-by new)
                (set new (symbol-value old)))
            (lwarn 'org-node :warning "Your initfiles set removed variable: %S" old))
          (set old nil)
          (makunbound old))
        (when (and old (where-is-internal old))
          (if new
              (lwarn 'org-node :warning "Your initfiles key-bind an old command name: %S.  Please use new name: %S"
                     old new)
            (lwarn 'org-node :warning "Your initfiles key-bind a removed command: %S"
                   old))))))
  ;; 2024-10-18
  (unless org-node-changes--warned-about-roam-id
    (when (and (not (and (bound-and-true-p org-roam-autosync-mode)
                         (bound-and-true-p org-roam-db-update-on-save)))
               (eq (org-link-get-parameter "id" :follow) 'org-roam-id-open))
      (setq org-node-changes--warned-about-roam-id t)
      (message
       "%s" "Note: org-roam overrides ID-link behavior, you may want to
      revert to vanilla by evalling:
      (org-link-set-parameters
       \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)"))))

(defmacro org-node-changes--def-whiny-alias (old new &optional when removed-by interactive)
  "Define function OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function.  Optional
string WHEN says when it was deprecated and REMOVED-BY when it
may be removed.  When these strings are omitted, fall back on
hardcoded strings."
  (when (or (eq t removed-by) (stringp interactive))
    (warn "org-node-changes--def-whiny-alias: Argument order has changed")
    (let ((date interactive))
      (when (eq t removed-by)
        (setq interactive t))
      (if (stringp date)
          (setq removed-by date)
        (setq removed-by nil))))
  `(let (warned-once)
     (add-to-list 'org-node-changes--new-names '(,(cadr old) ,(cadr new) ,removed-by))
     (defun ,(cadr old) (&rest args)
       (declare (obsolete ,new ,(or when "2025")))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn 'org-node :warning "Your initfiles use old function name: %S, which will be REMOVED by %s.  Please use new name: %S"
                ,old ,(or removed-by "30 February 2025") ,new))
       (apply ,new args))))

;;; v1.9

(define-obsolete-function-alias 'org-node-get-file-path 'org-node-get-file "2025-01-31")
(define-obsolete-function-alias 'org-node-ref-add 'org-node-add-refs "2025-02-21")
(define-obsolete-function-alias 'org-node-tag-add 'org-node-add-tags "2025-02-21")
(define-obsolete-function-alias 'org-node-tag-add-here 'org-node-add-tags-here "2025-02-21")
(define-obsolete-function-alias 'org-node-alias-add 'org-node-add-alias "2025-02-21")

;;; v2.0

;; Removed the org-node-link struct.  Less cognitive work to read code that has
;; the familiar (plist-get LINK :origin) instead of (org-node-link-origin LINK)
;; all over the place, and you can use `map-let' syntactic sugar.
(defun org-node-link-origin (link)
  (declare (obsolete "use (plist-get LINK :origin) instead" "2025-02-26"))
  (plist-get link :origin))
(defun org-node-link-pos (link)
  (declare (obsolete "use (plist-get LINK :pos) instead" "2025-02-26"))
  (plist-get link :pos))
(defun org-node-link-type (link)
  (declare (obsolete "use (plist-get LINK :type) instead" "2025-02-26"))
  (plist-get link :type))
(defun org-node-link-dest (link)
  (declare (obsolete "use (plist-get LINK :dest) instead" "2025-02-26"))
  (plist-get link :dest))

(org-node-changes--def-whiny-alias 'org-node-insert-link*-immediate
                                   'org-node-insert-link-novisit*
                                   "2025-02-26" "2025-03-30" t)

(provide 'org-node-changes)

;;; org-node-changes.el ends here
