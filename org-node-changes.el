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
(require 'el-job)
(require 'org-mem)
(require 'org-mem-x)
(require 'org-mem-list)
(eval-when-compile
  (require 'ol))

(defvar org-node-major-version 3
  "Number incremented for breaking changes.")

(defvar org-node-changes--new-names
  '()
  "Alist of deprecated symbol names and their new names.
Names here will cause complaints if bound.")

(defvar org-node-changes--warned-about-roam-id nil
  "Non-nil if did warn about org-roam overriding a link parameter.")

(defun org-node-changes--onetime-warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

First, warn if any old name in `org-node-changes--new-names' is bound.
Then copy the value in the old name so that the new name gets the same
value.

Then do other one-shot warnings while we\\='re at it."
  (let ((names org-node-changes--new-names))
    (while-let ((row (pop names)))
      (seq-let (old new removed-by) row
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
    (when (and (fboundp 'org-link-get-parameter)
               (not (and (bound-and-true-p org-roam-autosync-mode)
                         (bound-and-true-p org-roam-db-update-on-save)))
               (eq (org-link-get-parameter "id" :follow) 'org-roam-id-open))
      (setq org-node-changes--warned-about-roam-id t)
      (message
       "%s" "Note: org-roam overrides ID-link behavior to prefer its own DB!
This becomes a problem if the DB goes outdated: clicking a link can
send you to an empty file due to an uncaught rename, for example.
Org-node keeps `org-id-locations' up to date for you,
so you can revert ID-link behavior to its default,
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
                                   'org-mem-entry-file
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
;; (There was more here, but some of same things got renamed again)

(org-node-changes--def-whiny-alias 'org-node-insert-link*-immediate
                                   'org-node-insert-link-novisit*
                                   "2.0.0 (March 2025)" "April" t)


;;; v3.0

(org-node-changes--def-whiny-alias 'org-node-proposed-sequence 'org-node-proposed-seq "3.0.0 (May 2025)" "May")


(define-obsolete-variable-alias 'org-node--dest<>links              'org-mem--dest<>links                        "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--id<>node                 'org-mem--id<>entry                          "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--title<>id                'org-mem--title<>id                          "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--time-elapsed             'org-mem--time-elapsed                       "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--idle-timer               'org-mem-x--timer                            "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-before-update-tables-hook 'org-mem-pre-full-scan-functions             "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-extra-id-dirs             'org-mem-watch-dirs                          "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-extra-id-dirs-exclude     'org-mem-watch-dirs-exclude                  "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-link-types                'org-mem-seek-link-types                     "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-warn-title-collisions     'org-mem-do-warn-title-collisions            "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--ref-path<>ref-type       'org-mem--roam-ref<>type                     "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--ref<>id       'org-mem--roam-ref<>id                     "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--id<>refs       'org-mem--id<>roam-refs                     "3.0.0 (May 2025)")

(define-obsolete-function-alias 'org-node--dir-files-recursively     #'org-mem--dir-files-recursive              "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node--maybe-adjust-idle-timer   #'org-mem-x--activate-timer                 "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-abbrev-file-names          #'org-mem--fast-abbrev                      "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-by-id                      #'org-mem-entry-by-id                       "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-is-subtree             #'org-mem-entry-subtree-p                   "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-is-subtree                 #'org-mem-entry-subtree-p                   "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-subtree-p                  #'org-mem-entry-subtree-p                   "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-deadline               #'org-mem-entry-deadline                    "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-file                   #'org-mem-entry-file                        "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-file-title             #'org-mem-file-title-strict                 "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-file-title-or-basename #'org-mem-file-title-or-basename            "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-id                     #'org-mem-entry-id                          "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-id-links-to            #'org-mem-id-links-to-entry                 "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-level                  #'org-mem-entry-level                       "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-links-from             #'org-mem-links-from-id                     "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-lnum                   #'org-mem-entry-lnum                        "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-nodes-in-files         #'org-mem-entries-in                        "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-olp                    #'org-mem-entry-olpath                      "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-olp-full               #'org-mem-entry-olpath-with-title           "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-olp-with-self          #'org-mem-entry-olpath-with-self            "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-olp-with-self-full     #'org-mem-entry-olpath-with-self-with-title "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-pos                    #'org-mem-entry-pos                         "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-priority               #'org-mem-entry-priority                    "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-aliases                #'org-mem-entry-roam-aliases                "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-refs                   #'org-mem-entry-roam-refs                   "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-reflinks-to            #'org-mem-roam-reflinks-to-entry            "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-scheduled              #'org-mem-entry-scheduled                   "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-tags                   #'org-mem-entry-tags                        "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-properties             #'org-mem-entry-properties                  "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-props                  #'org-mem-entry-properties                  "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-tags-inherited         #'org-mem-entry-tags-inherited              "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-tags-local             #'org-mem-entry-tags-local                  "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-tags                   #'org-mem-entry-tags                  "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-title                  #'org-mem-entry-title                       "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-todo                   #'org-mem-entry-todo-state                  "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-list-collisions            #'org-mem-list-title-collisions             "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-list-dead-links            #'org-mem-list-dead-id-links                "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-list-scan-problems         #'org-mem-list-problems                     "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-get-tags-with-inheritance #'org-mem-entry-tags "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-nodes-in-file                  #'org-mem-id-nodes-in-files "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-link-dest                 #'org-mem-link-dest "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-link-origin               #'org-mem-link-nearby-id "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-link-pos                  #'org-mem-link-pos "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-link-type                 #'org-mem-link-type "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-link-p                 #'org-mem-link-p "3.0.0 (May 2025)")
(define-obsolete-function-alias 'org-node-p                 #'org-mem-entry-p "3.0.0 (May 2025)")


(define-obsolete-function-alias 'org-node-seq-try-visit-file 'org-node-seq-try-goto-file "2025-04-26")

(provide 'org-node-changes)

;;; org-node-changes.el ends here
