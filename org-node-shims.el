;;; org-node-shims.el --- Reimplement org-roam API -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Martin Edström

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'cl-lib)
(require 'ol)
(require 'eieio)
(require 'org)
(require 'org-macs)
(require 'org-node)


;;; Library
;; (see end of file for actual shims)

(defvar org-node-shims-dir nil
  "Cached value of `org-roam-directory' transformed for org-node.
This path should be directly comparable to the paths saved in
org-node objects, which lets you skip using `file-truename' and
`abbreviate-file-name' in many cases.

See `org-node-shims-daily-dir'.")

(defvar org-node-shims-daily-dir nil
  "Cached value for org-roam's dailies dir transformed for org-node.
This path should be directly comparable to the file paths saved in
org-node objects, which lets you skip using `file-truename' and
`abbreviate-file-name' in many cases.

Extra rationale: The original `org-roam-dailies-directory' can be
a relative path, which incurred verbosity penalties in all code
that used it \(plus a practical performance penalty since
`expand-file-name' was often used instead of `file-name-concat').

Even more verbosity is added on top for org-node, which compares file
paths as simple strings and needs to process the path with
`abbreviate-file-name'.  This variable provides an easy shorthand.

Will stay nil until sometime after org-roam-dailies is loaded.")

(defun org-node-shims--remember-roam-dirs ()
  "Cache some convenience variables.
See docstring of `org-node-shims-daily-dir'."
  (when (boundp 'org-roam-directory)
    (setq org-node-shims-dir
          (org-node-abbrev-file-names
           (file-truename org-roam-directory)))
    (when (boundp 'org-roam-dailies-directory)
      (setq org-node-shims-daily-dir
            (org-node-abbrev-file-names
             (file-truename
              (if (file-name-absolute-p org-roam-dailies-directory)
                  org-roam-dailies-directory
                (file-name-concat org-roam-directory
                                  org-roam-dailies-directory))))))))

(org-node-shims--remember-roam-dirs)
(add-hook 'org-node-before-update-tables-hook
          #'org-node-shims--remember-roam-dirs)

;; (benchmark-call #'org-node-shims-list-files) ; 0.001399 seconds
;; (benchmark-call #'org-roam-list-files)      ; 5.652997 seconds
(defun org-node-shims-list-files ()
  "Faster than `org-roam-list-files'."
  (cl-loop for file in (org-node-list-files t)
           when (string-prefix-p org-node-shims-dir file)
           collect file))

;; (benchmark-call #'org-node-shims-list-dailies 10)
;; (benchmark-call #'org-roam-dailies--list-files 10)
(defun org-node-shims-list-dailies (&rest extra-files)
  "May be faster than `org-roam-dailies--list-files'.
Makes little difference if your filesystem is not a bottleneck.

For argument EXTRA-FILES, see that function."
  (append extra-files
          (cl-loop
           for file in (org-node-list-files t)
           when (string-prefix-p org-node-shims-daily-dir file)
           collect file)))

;; (benchmark-call #'org-node-shims-daily-note-p 1000)
;; (benchmark-call #'org-roam-dailies--daily-note-p 1000)
(defun org-node-shims-daily-note-p (&optional file)
  "May be faster than `org-roam-dailies--daily-note-p'.
Makes little difference if your filesystem is not a bottleneck.

Meant to be called with FILE nil.  Behavior not well defined otherwise."
  (unless file
    ;; Fast way to get the abbreviated truename
    (setq file (buffer-local-value
                'buffer-file-truename (or (buffer-base-buffer)
                                          (current-buffer)))))
  (and (string-suffix-p ".org" file)
       (string-prefix-p (downcase org-node-shims-daily-dir)
                        (downcase file))
       (cl-loop for exclude in org-node-extra-id-dirs-exclude
                never (string-search exclude file))))


;;; Namespace takeover


;; If all goes well, these definitions get totally overridden
;; if and when the user actually loads org-roam.
;; Well, not the defvars, but they match upstream standard values.
(unless (featurep 'org-roam)

  ;;; Shimmed variables (default values up to us)

  (defvar org-roam-db-autosync-mode nil)
  (defvar org-roam-db-version 18)

  ;;; Copy-pasted customizables (sync default values with upstream)

  (defvar org-roam-directory (expand-file-name "~/org-roam/"))
  (defvar org-roam-db-location (locate-user-emacs-file "org-roam.db"))
  (defvar org-roam-dailies-directory "daily/")
  (defvar org-roam-dailies-capture-templates
    `(("d" "default" entry
     "* %?"
     :target (file+head "%<%Y-%m-%d>.org"
                        "#+title: %<%Y-%m-%d>\n"))))

  ;;; Shimmed functions

  (defun org-roam-db ()
    (org-node-db))
  (defun org-roam-db-autosync-mode (&optional _)
    (message "Nah"))
  (defun org-roam-list-files ()
    (org-node-shims-list-files))
  (defun org-roam-dailies--list-files (&rest extra-files)
    (apply #'org-node-shims-list-dailies extra-files))
  (defun org-roam-dailies--daily-note-p (&optional file)
    (org-node-shims-daily-note-p file))


  ;;; Literal copypasta

  (defun org-roam-db-query (sql &rest args)
    (apply #'emacsql (org-roam-db) sql args))
  (define-error 'emacsql-constraint "SQL constraint violation")
  (defun org-roam-db-query! (handler sql &rest args)
    (condition-case err
        (org-roam-db-query sql args)
      (emacsql-constraint
       (funcall handler err))))


  ;;; TODO: Shim

  (cl-defun org-roam-node-find
      ( &optional other-window initial-input filter-fn pred
        &key templates))
  (cl-defun org-roam-node-insert (&optional filter-fn &key templates info))
  (cl-defun org-roam-backlinks-get ())
  (cl-defun org-roam-reflinks-get ())
  (cl-defun org-roam-node-create ())
  (cl-defun org-roam-node-slug ())
  (cl-defun org-roam-capture- (&key goto keys node info props templates))
  (defun emacsql ())
  (defun org-roam-dailies--capture ())
  (defun org-roam-buffer--setup-redisplay-h ())
  (defun org-roam-buffer--redisplay-h ())
  (defun org-roam-reflink-create ())
  (defun org-roam-backlink-create ())
  (defun org-roam-db ())
  (defun org-roam-db-sync ())
  (defun org-roam-db--close ())
  (defun org-roam-db--close-all ())
  (defun org-roam-db--get-connection ())
  (defun org-roam-db--init ())
  (defun org-roam-node-create ())
  (defun org-roam-node-slug ())
  (defun org-roam-capture- ())

  )

(provide 'org-node-shims)

;;; org-node-shims.el ends here
