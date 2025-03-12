;;; org-node-shims-unhygienic.el --- ... -*- lexical-binding: t; -*-

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

;; Every `defmacro' and `provide' from org-roam.

;; Macros need to be defined before byte-compiling files that use those
;; macros, and in the same file that calls `provide', so the trick with putting
;;     (provide 'org-roam-utils)
;; into your initfiles would not work, if I understand correctly.

;; Up-to-date as of 2025-03-12 (feel free to bump this datestamp).

;;; Code:

(require 'org)

(defmacro org-roam-dolist-with-progress (spec msg &rest body)
  "Loop over a list and report progress in the echo area.
Like `dolist-with-progress-reporter', but falls back to `dolist'
if the function does not yet exist.

Evaluate BODY with VAR bound to each car from LIST, in turn.
Then evaluate RESULT to get return value, default nil.

MSG is a progress reporter object or a string.  In the latter
case, use this string to create a progress reporter.

SPEC is a list, as per `dolist'."
  (declare (indent 2))
  (if (fboundp 'dolist-with-progress-reporter)
      `(dolist-with-progress-reporter ,spec ,msg ,@body)
    `(dolist ,spec ,@body)))

(defmacro org-roam-with-file (file keep-buf-p &rest body)
  "Execute BODY within FILE.
If FILE is nil, execute BODY in the current buffer.
Kills the buffer if KEEP-BUF-P is nil, and FILE is not yet visited."
  (declare (indent 2) (debug t))
  `(let* (new-buf
          (auto-mode-alist nil)
          (find-file-hook nil)
          (buf (or (and (not ,file)
                        (current-buffer)) ;If FILE is nil, use current buffer
                   (find-buffer-visiting ,file) ; If FILE is already visited, find buffer
                   (progn
                     (setq new-buf t)
                     (find-file-noselect ,file)))) ; Else, visit FILE and return buffer
          res)
     (with-current-buffer buf
       (unless (derived-mode-p 'org-mode)
         (delay-mode-hooks
           (let ((org-inhibit-startup t)
                 (org-agenda-files nil))
             (org-mode)
             (hack-local-variables))))
       (setq res (progn ,@body))
       (unless (and new-buf (not ,keep-buf-p))
         (save-buffer)))
     (if (and new-buf (not ,keep-buf-p))
         (when (find-buffer-visiting ,file)
           (kill-buffer (find-buffer-visiting ,file))))
     res))

(defmacro org-roam-with-temp-buffer (file &rest body)
  "Execute BODY within a temp buffer.
Like `with-temp-buffer', but propagates `org-roam-directory'.
If FILE, set `default-directory' to FILE's directory and insert its contents."
  (declare (indent 1) (debug t))
  (let ((current-org-roam-directory (make-symbol "current-org-roam-directory")))
    `(let ((,current-org-roam-directory org-roam-directory))
       (with-temp-buffer
         (let ((org-roam-directory ,current-org-roam-directory))
           (delay-mode-hooks (org-mode))
           (when ,file
             (insert-file-contents ,file)
             (setq-local default-directory (file-name-directory ,file)))
           ,@body)))))

(require 'org-node-shims)

(provide 'org-roam)
(provide 'org-roam-compat)
(provide 'org-roam-utils)
(provide 'org-roam-db)
(provide 'org-roam-node)
(provide 'org-roam-id)
(provide 'org-roam-capture)
(provide 'org-roam-mode)
(provide 'org-roam-log)
(provide 'org-roam-migrate)

(provide 'org-node-shims-unhygienic)

;;; org-node-shims-unhygienic.el ends here
