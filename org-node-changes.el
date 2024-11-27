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
    package menus (e.g. by M-x package-refresh-contents)"))

(unless (fboundp 'get-truename-buffer)
  (display-warning
   'org-node "Update compat.el to use this version of org-node"))

(defvar org-node-changes--new-names
  '(;; Later (in Dec):
    ;; (org-node-built-series org-node-seqs)
    ;; (org-node-series-defs org-node-seq-defs)
    (org-node-current-series-key org-node-seq--current-key)
    (org-node--series org-node-seqs))
  "Alist of deprecated symbol names and their new names.
Names here will be loudly complained-about.")

(defvar org-node-changes--warned-roam-id nil
  "Non-nil if did warn about org-roam overriding a link parameter.")

(defvar org-node-changes--warned-once nil)
(defun org-node-changes--warn-and-copy ()
  "Maybe print one-shot warnings, then become a no-op.

First, warn if any old name in `org-node-changes--new-names' is bound.
Then copy the value in the old name so that the new name gets the same
value.

Then do other one-shot warnings while we\\='re at it."
  (while-let ((row (pop org-node-changes--new-names)))
    (seq-let (old new removed-by) row
      (unless removed-by
        (setq removed-by "30 November 2024"))
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
       \"id\" :follow #'org-id-open :store #'org-id-store-link-maybe)")))
  (unless org-node-changes--warned-once
    (setq org-node-changes--warned-once t)
    ;; 2024-11-27: In case children are running old bytecode of
    ;; org-node-parser, which expected this directory to exist.  The version
    ;; mismatch will still break things, but with different messages.
    (mkdir (file-name-concat temporary-file-directory "org-node") t)))

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
                ,old ,(or removed-by "30 November 2024") ,new))
       (apply ,new args))))

;; API transition underway: get-tags will include inherited tags in future
;; ... Let's say Jan/Feb.
(define-obsolete-function-alias 'org-node-get-tags #'org-node-get-tags-local
  "2024-10-22")

;; 2024-09-17
;; NOTE: Can't mark as obsolete here, it has be done inside that library
(declare-function org-node-fakeroam-new-via-roam-capture "org-node-fakeroam")
(declare-function org-node-fakeroam-slugify-via-roam "org-node-fakeroam")
(defalias 'org-node-new-via-roam-capture #'org-node-fakeroam-new-via-roam-capture)
(defalias 'org-node-slugify-like-roam-actual #'org-node-fakeroam-slugify-via-roam)

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

(org-node-changes--def-whiny-alias
 'org-node-get-id-links 'org-node-get-id-links-to "2024-10-04" nil "30 November")

(org-node-changes--def-whiny-alias
 'org-node-get-reflinks 'org-node-get-reflinks-to "2024-10-04" nil "30 November")

;; 2024-11-18 (v1.9) moved series-related code into own file, whereupon the
;; namespace had to be made consistent.  The following can be removed soon---
(org-node-changes--def-whiny-alias 'org-node--build-series           'org-node-seq--build-from-def "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--add-series-to-dispatch 'org-node-seq--add-to-dispatch "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-previous*  'org-node-seq--goto-previous* "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-goto-next*      'org-node-seq--goto-next* "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-jump*           'org-node-seq--jump* "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--series-capture         'org-node-seq--capture "2024-11-18")
(org-node-changes--def-whiny-alias 'org-node--mark-days              'org-node-seq--mark-days "2024-11-18")

;; ---but deprecate these more slowly
(define-obsolete-function-alias 'org-node--guess-daily-dir        'org-node-seq--guess-daily-dir "2024-11-18")
(define-obsolete-variable-alias 'org-node-built-series          'org-node-seqs "2024-11-18")
(define-obsolete-variable-alias 'org-node-series-defs           'org-node-seq-defs "2024-11-18")
(define-obsolete-function-alias 'org-node--series-jump          'org-node-seq--jump "2024-11-18")
(define-obsolete-function-alias 'org-node--series-goto-next     'org-node-seq--goto-next "2024-11-18")
(define-obsolete-function-alias 'org-node--series-goto-previous 'org-node-seq--goto-previous "2024-11-18")
(define-obsolete-function-alias 'org-node-series-goto           'org-node-seq-goto "2024-11-18")
(define-obsolete-function-alias 'org-node-series-dispatch       'org-node-seq-dispatch "2024-11-18")
(define-obsolete-function-alias 'org-node-helper-try-goto-id    'org-node-seq-try-goto-id "2024-11-18")
(define-obsolete-function-alias 'org-node-helper-try-visit-file 'org-node-seq-try-visit-file "2024-11-18")
(define-obsolete-function-alias 'org-node-series-capture-target 'org-node-seq-capture-target "2024-11-18")
(define-obsolete-function-alias 'org-node-helper-filename->ymd  'org-node-seq-filename->ymd "2024-11-18")
(define-obsolete-function-alias 'org-node-extract-ymd           'org-node-seq-extract-ymd "2024-11-18")
(define-obsolete-function-alias 'org-node-mk-series-sorted-by-property             'org-node-seq-def-on-any-sort-by-property "2024-11-18")
(define-obsolete-function-alias 'org-node-mk-series-on-tags-sorted-by-property     'org-node-seq-def-on-tags-sort-by-property "2024-11-18")
(define-obsolete-function-alias 'org-node-mk-series-on-filepath-sorted-by-basename 'org-node-seq-def-on-filepath-sort-by-basename "2024-11-18")

;; Used by org-node-fakeroam until 1.8, deprecate slowly as well.
(define-obsolete-variable-alias 'org-node-proposed-series-key 'org-node-proposed-sequence  "2024-11-18")
(define-obsolete-function-alias 'org-node--add-series-item    'org-node-seq--add-item "2024-11-18")

;; TODO
(defun org-node-changes--version ()
  "Guess which version of org-node is in `load-path'."
  (let ((path (locate-library "org-node")))
    (cond
     ;; ... elpaca
     ((and (bound-and-true-p elpaca-directory)
           (string-search elpaca-directory path)
           (fboundp 'elpaca<-repo-dir)
           (fboundp 'elpaca-get)
           (fboundp 'elpaca--declared-version)
           (fboundp 'elpaca-latest-tag)
           (fboundp 'elpaca-process-output))
      (when-let* ((e (elpaca-get 'org-node))
                  (default-directory (elpaca<-repo-dir e))
                  (version (ignore-errors (or (elpaca--declared-version e)
                                              (elpaca-latest-tag e)))))
        (concat (string-trim version) " "
                (ignore-errors
                  (string-trim (elpaca-process-output
                                "git" "rev-parse" "--short" "HEAD"))))))

     ;; ... package.el
     ((and (bound-and-true-p package-user-dir)
           (string-search package-user-dir path)
           (fboundp 'package-get-descriptor)
           (fboundp 'package-desc-version))
      (mapconcat #'number-to-string
                 (package-desc-version (package-get-descriptor 'org-node))
                 "."))

     ;; ... straight

     ;; ... quelpa

     ;; ... vc-use-package

     ;; el-get, borg etc?

     (t
      (message "Sorry, couldn't figure out version, try checking the Git tag")
      nil))))

(provide 'org-node-changes)

;;; org-node-changes.el ends here
