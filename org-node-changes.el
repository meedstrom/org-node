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

(unless (fboundp 'el-job-launch)
  (display-warning
   'org-node "Org-node has new dependency el-job, update your
    package menus (e.g. by M-x package-refresh-contents
    or M-x elpaca-update-menus)"))

(unless (fboundp 'get-truename-buffer)
  (display-warning
   'org-node "Update compat.el to use this version of org-node"))

(defvar org-node-changes--new-names
  '((org-node-built-series org-node-seqs)
    (org-node-series-defs org-node-seq-defs)
    (org-node-current-series-key org-node-seq--current-key))
  "Alist of deprecated symbol names and their new names.
Names here will cause complaints if bound.")

(defvar org-node-changes--warned-roam-id nil
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
  (unless org-node-changes--warned-roam-id
    (when (and (not (and (bound-and-true-p org-roam-autosync-mode)
                         (bound-and-true-p org-roam-db-update-on-save)))
               (eq (org-link-get-parameter "id" :follow) 'org-roam-id-open))
      (setq org-node-changes--warned-roam-id t)
      (message
       "%s" "Note: org-roam overrides ID-link behavior, you may want to
      revert to vanilla by evalling:
      (org-link-set-parameters
       \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)"))))

(defmacro org-node-changes--def-whiny-alias (old new &optional when interactive removed-by)
  "Define function OLD as effectively an alias for NEW.
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
                ,old ,(or removed-by "30 January 2025") ,new))
       (apply ,new args))))

;; 1.9.0 (2024-11-18) moved series-related code into its own file, whereupon
;; the namespace had to be made consistent.
(org-node-changes--def-whiny-alias 'org-node--guess-daily-dir      'org-node-seq--guess-daily-dir "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-jump          'org-node-seq--jump "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-next     'org-node-seq--goto-next "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-previous 'org-node-seq--goto-previous "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-series-goto           'org-node-seq-goto "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-series-dispatch       'org-node-seq-dispatch "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-try-goto-id    'org-node-seq-try-goto-id "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-try-visit-file 'org-node-seq-try-visit-file "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-series-capture-target 'org-node-seq-capture-target "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-helper-filename->ymd  'org-node-seq-filename->ymd "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-extract-ymd           'org-node-seq-extract-ymd "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-sorted-by-property             'org-node-seq-def-on-any-sort-by-property "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-on-tags-sorted-by-property     'org-node-seq-def-on-tags-sort-by-property "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node-mk-series-on-filepath-sorted-by-basename 'org-node-seq-def-on-filepath-sort-by-basename "2024-11-18")
(define-obsolete-variable-alias 'org-node-series-that-marks-calendar 'org-node-seq-that-marks-calendar "2024-12-13")

;; Used by org-node-fakeroam until 1.6.0 (co-released with org-node 1.9.0)
(define-obsolete-variable-alias 'org-node-proposed-series-key 'org-node-proposed-sequence  "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--add-series-item 'org-node-seq--add-item "2024-11-18")

(defun org-node--tmpfile (&optional basename &rest args)
  "Return a path that puts BASENAME in a temporary directory.
As a nicety, `format' BASENAME with ARGS too.

On most Unix systems, the resulting string will be
/tmp/org-node/BASENAME."
  (display-warning 'org-node
                   "Old function `org-node--tmpfile' will be removed on 10 January 2025; please update org-node-fakeroam and call `org-node-fakeroam--tmpfile'")
  (mkdir (file-name-concat temporary-file-directory "org-node")
         t)
  (file-name-concat temporary-file-directory
                    "org-node"
                    (when basename (apply #'format basename args))))

(provide 'org-node-changes)

;;; org-node-changes.el ends here
