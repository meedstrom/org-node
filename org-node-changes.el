;;; org-node-changes.el --- Help user transit renamed user options -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2026 Martin Edström
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
;; thrice-deprecated variable name for years.

;; Thus this file.  Gradually increase the "whine level" for each name, over
;; the months.

;;; Code:

(require 'subr-x)
(require 'cl-lib)

(defvar org-node-major-version 3
  "Number incremented for breaking changes that require reading README.")

(defmacro org-node-changes--def-whiny-alias
    (old new when removed-by &optional interactive)
  "Define function OLD as effectively an alias for NEW.
Also, running OLD will emit a deprecation warning the first time.

If INTERACTIVE, define it as an interactive function.  Optional
string WHEN says when it was deprecated and REMOVED-BY when it
may be removed.  When these strings are omitted, fall back on
hardcoded strings."
  `(let (warned-once)
     (defun ,(cadr old) (&rest args)
       (declare (obsolete ,(cadr new) ,when))
       ,@(if interactive '((interactive)))
       (unless warned-once
         (setq warned-once t)
         (lwarn ,old :warning "Obsolete as of %s, will be removed by %s; use `%s' instead. (Check your initfiles)"
                ,when ,removed-by ,new))
       (apply ,new args))))

(defconst org-node-before-update-tables-hook :renamed-3.0.0) ; org-mem-pre-full-scan-functions
(defconst org-node-extra-id-dirs             :renamed-3.0.0) ; org-mem-watch-dirs
(defconst org-node-extra-id-dirs-exclude     :renamed-3.0.0) ; org-mem-exclude
(defconst org-node-warn-title-collisions     :renamed-3.0.0) ; org-mem-do-warn-title-collisions

(define-obsolete-variable-alias 'org-nodes                             'org-mem--id<>entry                 "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--dest<>links                 'org-mem--target<>links             "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--id<>node                    'org-mem--id<>entry                 "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--id<>refs                    'org-mem--id<>roam-refs             "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--ref-path<>ref-type          'org-mem--roam-ref<>type            "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--ref<>id                     'org-mem--roam-ref<>id              "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--title<>id                   'org-mem--title<>id                 "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node--candidate<>node             'org-node--candidate<>entry         "3.0.0 (May 2025)")
(define-obsolete-variable-alias 'org-node-ask-directory                'org-node-file-directory-ask        "3.3.15 (May 2025)")
(define-obsolete-variable-alias 'org-node-datestamp-format             'org-node-file-timestamp-format     "3.3.15 (May 2025)")
(define-obsolete-variable-alias 'org-node-slug-fn                      'org-node-file-slug-fn              "3.3.15 (May 2025)")

(require 'org-mem)
(require 'org-mem-updater)

(if (or (not (boundp 'org-mem-internal-version))
        (< org-mem-internal-version 27))
    (display-warning 'org-node "Update org-mem to use this version of org-node"))

(org-node-changes--def-whiny-alias 'org-node--goto                      'org-node-goto                         "3.8.0 (July 2025)" "May 2026")
(org-node-changes--def-whiny-alias 'org-node--goto-id                   'org-node-goto-id                      "3.8.0 (July 2025)" "May 2026")
(org-node-changes--def-whiny-alias 'org-node--infer-title-etc           'org-node-capture-infer-title-etc      "3.8.0 (July 2025)" "May 2026")
(org-node-changes--def-whiny-alias 'org-node--pop-to-fresh-file-buffer  'org-node-pop-to-fresh-file-buffer     "3.8.0 (July 2025)" "May 2026")
(org-node-changes--def-whiny-alias 'org-node-collection-main            'org-node-collection                   "3.9.0 (October 2025)" "June 2026")
(org-node-changes--def-whiny-alias 'org-node-collection-basic           'org-node-collection                   "3.9.0 (October 2025)" "June 2026")
(org-node-changes--def-whiny-alias 'org-node-update-mtime               'org-node-update-mtime-property        "3.11.0 (December 2025)" "July 2026")
(org-node-changes--def-whiny-alias 'org-node-sort-by-crtime             'org-node-sort-by-crtime-property      "3.11.1 (December 2025)" "July 2026")
(org-node-changes--def-whiny-alias 'org-node-sort-by-crtime-cheap       'org-node-sort-by-crtime-property      "3.11.1 (December 2025)" "July 2026")
(org-node-changes--def-whiny-alias 'org-node-sort-by-mtime-cheap        'org-node-sort-by-mtime-property       "3.11.1 (December 2025)" "July 2026")

(provide 'org-node-changes)

;;; org-node-changes.el ends here
