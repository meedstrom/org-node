;;; org-node.el --- Use org-id locations as a pile of notes -*- lexical-binding: t; -*-

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

;; Author:           Martin Edström <meedstrom91@gmail.com>
;; Created:          2024-04-13
;; Keywords:         org, hypermedia
;; Package-Requires: ((emacs "29.1") (dash "2.19.1"))
;; URL:              https://github.com/meedstrom/org-node

;; This file is not part of GNU Emacs.

;;; Commentary:

;;; Code:

;; TODO Annotations for completion
;; TODO Completion category https://github.com/alphapapa/org-ql/issues/299
;; TODO Command to grep across all files (then deprecate the -regret command, and teach the user wgrep)
;; TODO Rename :CACHED_BACKLINKS: to :BACKLINKS: ?
;; TODO Option to insert backlink into org-super-links :BACKLINKS: drawer
;; TODO React to any org-element-cache error (they're common) and disable the cache during fix-all
;; TODO Use fundamental-mode in `org-node-backlink-fix-all'
;; TODO Command to explore feedback arc sets
;; TODO Bit of a test suite
;; TODO Test a custom id format involving emoji to see if that breaks regexps
;; TODO Do you get more performant searches by disabling case-fold-search?

(require 'org-node-lib)
(require 'org-node-cache)
(require 'org-node-commands)
(require 'org-node-backlink)
(require 'org-node-roam)

;;;###autoload
(defun org-node-enable ()
  "Designed for `org-mode-hook' and will remove itself."
  (remove-hook 'org-mode-hook #'org-node-enable)
  (org-node-backlink-mode)
  (org-node-cache-mode))

(provide 'org-node)

;;; org-node.el ends here
