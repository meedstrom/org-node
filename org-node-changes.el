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
  ;; 2024-09-19 Clean up deprecated persist-defvars
  (unless org-node-changes--warned-once
    (setq org-node-changes--warned-once t)
    (unless (memq system-type '(windows-nt ms-dos))
      (cl-loop for sym in '(org-node--file<>mtime
                            org-node--file<>previews)
               do (let ((file (org-node-changes--guess-persist-filename sym)))
                    (when (file-exists-p file)
                      (delete-file file))))))
  ;; 2024-10-18
  (unless org-node-changes--warned-about-roam-id
    (when (and (not (and (bound-and-true-p org-roam-autosync-mode)
                         (bound-and-true-p org-roam-db-update-on-save)))
               (eq (org-link-get-parameter "id" :follow) 'org-roam-id-open))
      (setq org-node-changes--warned-about-roam-id t)
      (message
       <<<<<<< HEAD
       "%s" "Note: org-roam overrides ID-link behavior, you may want to
      revert to vanilla by evalling:
      (org-link-set-parameters
       \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)")))

  )
=======
"%s" "Note: org-roam overrides ID-link behavior to prefer its own DB!
This becomes a problem if the DB goes outdated: clicking a link can
send you to an empty file due to an uncaught rename, for example.
Org-node always keeps `org-id-locations' up to date for you.
So you can revert ID-link behavior to its default,
by adding to initfiles AFTER org-roam loads:
(org-link-set-parameters
 \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)"))))
>>>>>>> volatile

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
         <<<<<<< HEAD
         (lwarn 'org-node :warning "Your initfiles use old function name: %S, which will be REMOVED by %s.  Please use new name: %S"
                ,old ,(or removed-by "v2.0") ,new))
       (apply ,new args))))

;; Nov 2024 or older

(declare-function org-node-fakeroam-new-via-roam-capture "org-node-fakeroam")
(declare-function org-node-fakeroam-slugify-via-roam "org-node-fakeroam")
(defalias 'org-node-new-via-roam-capture #'org-node-fakeroam-new-via-roam-capture)
(defalias 'org-node-slugify-like-roam-actual #'org-node-fakeroam-slugify-via-roam)

(org-node-changes--def-whiny-alias 'org-node-affix-with-olp
                                   'org-node-prefix-with-olp "2024-09-19")

(org-node-changes--def-whiny-alias 'org-node-complete-at-point-global-mode
                                   'org-node-complete-at-point-mode "2024-09-29")

(org-node-changes--def-whiny-alias 'org-node-get-id-links
                                   'org-node-get-id-links-to "2024-10-04")

(org-node-changes--def-whiny-alias 'org-node-get-reflinks
                                   'org-node-get-reflinks-to "2024-10-04")

(org-node-changes--def-whiny-alias 'org-node-parser--tmpfile
                                   'org-node--tmpfile "2024-10-28")

;; 1.9.0 (2024-11-18) moved series-related code into its own file, whereupon
;; the namespace had to be made consistent.
(org-node-changes--def-whiny-alias 'org-node--series-jump
                                   'org-node-seq--jump
                                   "2024-11-18" "v1.9")
(org-node-changes--def-whiny-alias 'org-node--series-goto-next
                                   'org-node-seq--goto-next
                                   "2024-11-18" "v1.9.")
(org-node-changes--def-whiny-alias 'org-node--series-goto-previous
                                   'org-node-seq--goto-previous
                                   "2024-11-18" "v1.9")
(org-node-changes--def-whiny-alias 'org-node--add-series-item
                                   'org-node-seq--add-item
                                   "2024-11-18" "v1.9")
(org-node-changes--def-whiny-alias 'org-node-series-goto
                                   'org-node-seq-goto
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-series-dispatch
                                   'org-node-seq-dispatch
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-try-goto-id
                                   'org-node-seq-try-goto-id
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-try-visit-file
                                   'org-node-seq-try-visit-file
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-series-capture-target
                                   'org-node-seq-capture-target
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-filename->ymd
                                   'org-node-seq-filename->ymd
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-extract-ymd
                                   'org-node-seq-extract-ymd
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-sorted-by-property
                                   'org-node-seq-def-on-any-sort-by-property
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-on-tags-sorted-by-property
                                   'org-node-seq-def-on-tags-sort-by-property
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-on-filepath-sorted-by-basename
                                   'org-node-seq-def-on-filepath-sort-by-basename
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--build-series
                                   'org-node-seq--build-from-def
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--add-series-to-dispatch
                                   'org-node-seq--add-to-dispatch
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-previous*
                                   'org-node-seq--goto-previous*
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-next*
                                   'org-node-seq--goto-next*
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-jump*
                                   'org-node-seq--jump*
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-capture
                                   'org-node-seq--capture
                                   "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--mark-days
                                   'org-node-seq--mark-days
                                   "2024-11-18")
(define-obsolete-variable-alias
  'org-node-series-that-marks-calendar 'org-node-seq-that-marks-calendar "2024-12-13")
(define-obsolete-variable-alias
  'org-node-proposed-series-key 'org-node-proposed-sequence  "2024-11-18")

;; 2025

(defalias 'org-node-get-file-path 'org-node-get-file) ;; 2025-01-31
(org-node-changes--def-whiny-alias 'org-node-ref-add 'org-node-add-refs "2025-02-21")
(org-node-changes--def-whiny-alias 'org-node-tag-add 'org-node-add-tags "2025-02-21")
(org-node-changes--def-whiny-alias 'org-node-tag-add-here 'org-node-add-tags-here "2025-02-21")
(org-node-changes--def-whiny-alias 'org-node-tag-add* 'org-node-add-tags-here "2025-01-30" nil)
(org-node-changes--def-whiny-alias 'org-node-alias-add 'org-node-add-alias "2025-02-21")

;; Some names that may be missing

(defun org-node--write-eld (file object)
  (display-warning
   'org-node "Looks like you updated org-node, please also update org-node-fakeroam")
  (if (stringp object)
      (let ((obj file))
        (setq file object)
        (setq object obj)))
  (when-let ((buf (if (fboundp 'get-truename-buffer) ;; Emacs 30
                      (get-truename-buffer file)
                    (get-file-buffer file))))
    (kill-buffer buf))
  (write-region (prin1-to-string object nil '((length . nil) (level . nil)))
                nil file nil 'quiet))

(defun org-node-changes--guess-persist-filename (sym)
  (let ((dir (or (get sym 'persist-location)
                 (bound-and-true-p persist--directory-location)
                 (locate-user-emacs-file "persist"))))
    (expand-file-name (symbol-name sym) dir)))
=======
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
                                   'org-node-get-file
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

;;; v2.0

;; Removed the `org-node-link' struct.  Less cognitive work to read the
;; familiar (plist-get LINK :origin) instead of (org-node-link-origin LINK) all
;; over the place, and you can use `seq-let' syntactic sugar.
(org-node-changes--def-whiny-fn org-node-link-origin (link)
  "2.0.0 (March 2025)" "May" "use (plist-get LINK :origin) instead"
  (plist-get link :origin))

(org-node-changes--def-whiny-fn org-node-link-pos (link)
  "2.0.0 (March 2025)" "May" "use (plist-get LINK :pos) instead"
  (plist-get link :pos))

(org-node-changes--def-whiny-fn org-node-link-type (link)
  "2.0.0 (March 2025)" "May" "use (plist-get LINK :type) instead"
  (plist-get link :type))

(org-node-changes--def-whiny-fn org-node-link-dest (link)
  "2.0.0 (March 2025)" "May" "use (plist-get LINK :dest) instead"
  (plist-get link :dest))

(define-obsolete-function-alias
  'org-node-get-tags-with-inheritance 'org-node-get-tags "2025-03-03")

(define-obsolete-function-alias
  'org-node-proposed-sequence 'org-node-proposed-seq "2025-03-03")

(org-node-changes--def-whiny-alias 'org-node-insert-link*-immediate
                                   'org-node-insert-link-novisit*
                                   "2.0.0 (March 2025)" "April" t)
>>>>>>> volatile

(provide 'org-node-changes)

;;; org-node-changes.el ends here
