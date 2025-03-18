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
(require 'indexed)
(require 'indexed-x)
(require 'indexed-roam)
(require 'indexed-list)

(defvar org-node-major-version 3
  "Number incremented for breaking changes.")

(defvar org-node-changes--new-names
  '()
  "Alist of deprecated symbol names and their new names.
Names here will cause complaints if bound.")

(defvar org-node-changes--warned-about-roam-id nil)
(defvar org-node-changes--warned-once nil)
(defvar org-node-changes--warned-about-lazy nil
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
          (setq removed-by "v2.0"))
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
       "%s" "Note: org-roam overrides ID-link behavior to prefer its own DB!
This becomes a problem if the DB goes outdated: clicking a link can
send you to an empty file due to an uncaught rename, for example.
Org-node always keeps `org-id-locations' up to date for you.
So you can revert ID-link behavior to its default,
by adding to initfiles AFTER org-roam loads:
(org-link-set-parameters
 \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)"))))

(defmacro org-node-changes--def-whiny-alias
    (old new when removed-by &optional interactive)
  "Define function OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function.  Optional
string WHEN says when it was deprecated and REMOVED-BY when it
may be removed.  When these strings are omitted, fall back on
hardcoded strings."
  (setq removed-by (or removed-by "April"))
  `(let (warned-once)
     (add-to-list 'org-node-changes--new-names '(,(cadr old) ,(cadr new) ,removed-by))
     (defun ,(cadr old) (&rest args)
       (declare (obsolete ,(cadr new) ,when))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn ,old :warning "Obsolete as of %s, will be removed by %s; use `%s' instead. (Check your initfiles)"
                ,when ,removed-by ,new))
       (apply ,new args))))

(defmacro org-node-changes--def-whiny-fn
    (name arglist when removed-by newname-or-expl &rest body)
  "Define a function that triggers `display-warning' on first use.
Variable `lexical-binding' must be t in files that contain this macro.

NAME, ARGLIST and BODY as in `defun'."
  (declare (indent defun))
  `(let (warned-once)
     (defun ,name ,arglist
       ,@(when (stringp (car body))
           (list (pop body)))
       ,@(if (eq (caar body) 'declare)
             (list (pop body))
           `((declare (obsolete ,newname-or-expl ,when))))
       ,@(when (eq (caar body) 'interactive)
           (list (pop body)))
       (unless warned-once
         (setq warned-once t)
         ,(if (symbolp newname-or-expl)
              (if (null newname-or-expl)
                  `(lwarn ',name :warning
                          "Obsolete as of %s, will be removed by %s. (Check your initfiles)"
                          ,when ,removed-by)
                `(lwarn ',name :warning
                        "Obsolete as of %s, will be removed by %s; use `%s' instead. (Check your initfiles)"
                        ,when ,removed-by ,newname-or-expl))
            `(lwarn ',name :warning
                    "Obsolete as of %s, will be removed by %s (check your initfiles); %s"
                    ,when ,removed-by ,newname-or-expl)))
       ,@body)))


;;; v1.9

(org-node-changes--def-whiny-alias 'org-node-get-file-path
                                   'indexed-file-name
                                   "February 2025" "May")
(org-node-changes--def-whiny-alias 'org-node-ref-add
                                   'org-node-add-refs
                                   "February 2025" "April")
(org-node-changes--def-whiny-alias 'org-node-tag-add
                                   'org-node-add-tags
                                   "February 2025" "April")
(org-node-changes--def-whiny-alias 'org-node-tag-add-here
                                   'org-node-add-tags-here
                                   "February 2025" "April")
(org-node-changes--def-whiny-alias 'org-node-alias-add
                                   'org-node-add-alias
                                   "February 2025" "April")

(define-obsolete-variable-alias
  'org-node-series-that-marks-calendar
  'org-node-seq-that-marks-calendar "1.9.0")


;;; v2.0

(org-node-changes--def-whiny-alias 'org-node-insert-link*-immediate
                                   'org-node-insert-link-novisit*
                                   "2.0.0 (March 2025)" "April" t)


;;; v3.0

(org-node-changes--def-whiny-alias 'org-node-get-tags-with-inheritance
                                   'indexed-tags
                                   "3.0.0 (March 2025)" "May")
(org-node-changes--def-whiny-alias 'org-node-proposed-sequence
                                   'org-node-proposed-seq
                                   "3.0.0 (March 2025)" "May")
(org-node-changes--def-whiny-alias 'org-nodes-in-file
                                   'indexed-entries-in
                                   "3.0.0 (March 2025)" "May")

(org-node-changes--def-whiny-fn org-node-get-is-subtree (node)
  "3.0.0 (March 2025)" "May" "use (/= 0 (indexed-heading-lvl ENTRY)) instead"
  (/= 0 (indexed-heading-lvl node)))

(org-node-changes--def-whiny-fn org-node-is-subtree (node)
  "3.0.0 (March 2025)" "May" "use (/= 0 (indexed-heading-lvl ENTRY)) instead"
  (/= 0 (indexed-heading-lvl node)))

(org-node-changes--def-whiny-alias 'org-node-link-dest
                                   'indexed-dest
                                   "3.0.0 (March 2025)" "May" t)

(org-node-changes--def-whiny-alias 'org-node-link-origin
                                   'indexed-nearby-id
                                   "3.0.0 (March 2025)" "May" t)

(org-node-changes--def-whiny-alias 'org-node-link-pos
                                   'indexed-pos
                                   "3.0.0 (March 2025)" "May" t)

(org-node-changes--def-whiny-alias 'org-node-link-type
                                   'indexed-type
                                   "3.0.0 (March 2025)" "May" t)

(define-obsolete-variable-alias 'org-node--dest<>links              'indexed--dest<>links "2025-03-19")
(define-obsolete-variable-alias 'org-node--id<>node                 'indexed--id<>entry "2025-03-19")
(define-obsolete-variable-alias 'org-node--idle-timer               'indexed--timer "2025-03-19")
(define-obsolete-variable-alias 'org-node--time-elapsed             'indexed--time-elapsed "2025-03-19")
(define-obsolete-variable-alias 'org-node--title<>id                'indexed--title<>id "2025-03-19")
(define-obsolete-variable-alias 'org-node-before-update-tables-hook 'indexed-pre-full-reset-functions "2025-03-19")
(define-obsolete-variable-alias 'org-node-extra-id-dirs             'indexed-org-dirs "2025-03-19")
(define-obsolete-variable-alias 'org-node-extra-id-dirs-exclude     'indexed-org-dirs-exclude "2025-03-19")
(define-obsolete-variable-alias 'org-node-link-types                'indexed-seek-link-types "2025-03-19")
(define-obsolete-variable-alias 'org-node-warn-title-collisions     'indexed-warn-title-collisions "2025-03-19")
(define-obsolete-variable-alias 'org-node--ref-path<>ref-type       'indexed-roam--ref<>type "2025-03-19")

(define-obsolete-function-alias 'org-node--dir-files-recursively     #'indexed--dir-files-recursive "2025-03-19")
(define-obsolete-function-alias 'org-node--maybe-adjust-idle-timer   #'indexed--activate-timer "2025-03-19")
(define-obsolete-function-alias 'org-node-abbrev-file-names          #'indexed--abbrev-file-names "2025-03-19")
(define-obsolete-function-alias 'org-node-by-id                      #'indexed-entry-by-id "2025-03-19")
(define-obsolete-function-alias 'org-node-get-deadline               #'indexed-deadline "2025-03-19")
(define-obsolete-function-alias 'org-node-get-file                   #'indexed-file-name "2025-03-19")
(define-obsolete-function-alias 'org-node-get-file-title             #'indexed-file-title "2025-03-19")
(define-obsolete-function-alias 'org-node-get-file-title-or-basename #'indexed-file-title-or-basename "2025-03-19")
(define-obsolete-function-alias 'org-node-get-id                     #'indexed-id "2025-03-19")
(define-obsolete-function-alias 'org-node-get-id-links-to            #'indexed-id-links-to "2025-03-19")
(define-obsolete-function-alias 'org-node-get-level                  #'indexed-heading-lvl "2025-03-19")
(define-obsolete-function-alias 'org-node-get-links-from             #'indexed-links-from "2025-03-19")
(define-obsolete-function-alias 'org-node-get-lnum                   #'indexed-lnum "2025-03-19")
(define-obsolete-function-alias 'org-node-get-nodes-in-files         #'indexed-entries-in "2025-03-19")
(define-obsolete-function-alias 'org-node-get-olp                    #'indexed-olpath "2025-03-19")
(define-obsolete-function-alias 'org-node-get-olp-full               #'indexed-olpath-with-title "2025-03-19")
(define-obsolete-function-alias 'org-node-get-olp-with-self          #'indexed-olpath-with-self "2025-03-19")
(define-obsolete-function-alias 'org-node-get-olp-with-self-full     #'indexed-olpath-with-self-with-title "2025-03-19")
(define-obsolete-function-alias 'org-node-get-pos                    #'indexed-pos "2025-03-19")
(define-obsolete-function-alias 'org-node-get-priority               #'indexed-priority "2025-03-19")
(define-obsolete-function-alias 'org-node-get-aliases                #'indexed-roam-aliases "2025-03-19")
(define-obsolete-function-alias 'org-node-get-refs                   #'indexed-roam-refs "2025-03-19")
(define-obsolete-function-alias 'org-node-get-reflinks-to            #'indexed-roam-reflinks-to "2025-03-19")
(define-obsolete-function-alias 'org-node-get-scheduled              #'indexed-scheduled "2025-03-19")
(define-obsolete-function-alias 'org-node-get-tags                   #'indexed-tags "2025-03-19")
(define-obsolete-function-alias 'org-node-get-properties             #'indexed-properties "2025-03-19")
(define-obsolete-function-alias 'org-node-get-props                  #'indexed-properties "2025-03-19")
(define-obsolete-function-alias 'org-node-get-tags-inherited         #'indexed-tags-inherited "2025-03-19")
(define-obsolete-function-alias 'org-node-get-tags-local             #'indexed-tags-local "2025-03-19")
(define-obsolete-function-alias 'org-node-get-title                  #'indexed-title "2025-03-19")
(define-obsolete-function-alias 'org-node-get-todo                   #'indexed-todo-state "2025-03-19")
(define-obsolete-function-alias 'org-node-list-collisions            #'indexed-list-title-collisions "2025-03-19")
(define-obsolete-function-alias 'org-node-list-dead-links            #'indexed-list-dead-id-links "2025-03-19")
(define-obsolete-function-alias 'org-node-list-scan-problems         #'indexed-list-problems "2025-03-19")


;;; Change in dependencies

(unless (and (boundp 'el-job-major-version)
             (>= el-job-major-version 1))
  (display-warning
   'org-node "Update el-job to use this version of org-node"))

;; https://github.com/toshism/org-super-links/pull/104
(org-node-changes--def-whiny-fn org-node-convert-link-to-super (&rest _)
  "March 2025" "May"
  "upgrade org-super-links and use their `org-super-links-convert-link-to-super' instead."
  (require 'org-super-links)
  (when (fboundp 'org-super-links-convert-link-to-super)
    (org-super-links-convert-link-to-super nil)))

(provide 'org-node-changes)

;;; org-node-changes.el ends here
