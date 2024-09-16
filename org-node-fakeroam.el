;;; org-node-fakeroam.el --- DEPRECATED -*- lexical-binding: t; -*-

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

;; Deprecation stub (to be removed soon)

;;; Code:

(defun org-node-fakeroam-warn ()
  (display-warning
   'org-node (concat
              "Please check out the new MELPA packages to update org-node."
              "\n\tInstructions at: https://github.com/meedstrom/org-node")))

;;;###autoload
(defun org-node-fakeroam-redisplay-mode (&rest _)
  (org-node-fakeroam-warn))

;;;###autoload
(defun org-node-fakeroam-fast-render-mode (&rest _)
  (org-node-fakeroam-warn))

;;;###autoload
(defun org-node-fakeroam-daily-create (&rest _)
  (org-node-fakeroam-warn))

;;;###autoload
(defun org-node-fakeroam-db-feed-mode (&rest _)
  (org-node-fakeroam-warn))

;;;###autoload
(defun org-node-fakeroam-jit-backlinks-mode (&rest _)
  (org-node-fakeroam-warn))

;; NOTE: The MELPA recipe excludes this file.  So this warning is only
;; displayed to people who are still cloning the package using a manually
;; written recipe passed to Straight/Elpaca.
;;
;; (Even Straight/Elpaca reuse the MELPA recipe when the recipe is not
;; specified.)
(org-node-fakeroam-warn)

(provide 'org-node-fakeroam)

;;; org-node-fakeroam.el ends here
