;;; org-node-test.el --- Test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024-2025 Martin Edström

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

;;; Code:

(require 'ert)
(require 'seq)
(require 'find-func)
(require 'dash)
(require 'org-node)
(require 'indexed-org-parser)
(require 'org-node-backlink)
(require 'org-node-seq)

(ert-deftest org-node/test-various ()
  ;; (should (-all-p #'plistp
  ;;               (apply #'append (hash-table-values indexed--dest<>links))))
  (let ((org-node-ask-directory "/tmp/org-node/test/")
        ;; NOTE you should manually test the other creation-fns
        (org-node-creation-fn #'org-node-new-file))
    (delete-directory org-node-ask-directory t)
    ;; (should-error (org-node-create "New node" "not-an-uuid1234"))
    (mkdir org-node-ask-directory t)
    (org-node-create "New node" "not-an-uuid1234")
    (org-node-cache-ensure t)
    (let ((node (gethash "not-an-uuid1234" org-nodes)))
      (org-node--goto node)
      (should (file-equal-p default-directory org-node-ask-directory))
      (should (equal (indexed-id node) (org-entry-get nil "ID" t)))
      (should (equal (indexed-id node) "not-an-uuid1234"))
      (should (equal (indexed-title node) "New node"))
      (should (equal (indexed-file-title node) "New node"))
      (should (equal (indexed-file-title-or-basename node) "New node")))
    (let ((org-node-prefer-with-heading nil))
      (org-node-create "A top-level heading" "not-an-uuid5678")
      (org-node-cache-ensure t)
      (let ((node (gethash "not-an-uuid5678" org-nodes))
            (expected-filename
             (concat (format-time-string org-node-datestamp-format)
                     (funcall org-node-slug-fn "A top-level heading")
                     ".org")))
        (should (equal (indexed-title node) "A top-level heading"))
        (should (equal (indexed-file-title node) nil))
        (should (equal (indexed-file-title-or-basename node)
                       expected-filename))))))

(ert-deftest org-node/test-time-format-hacks ()
  (let ((fmt "Wild%Y--%m%dexample%H%M%S-"))
    (should (equal (org-node-seq-extract-ymd
                    (format-time-string fmt '(26300 36406 109008 201000))
                    fmt)
                   "2024-08-14"))))

(ert-deftest org-node/test-parsing-testfile2.org ()
  (let ((file (file-name-concat
               (file-name-directory (locate-library "org-node"))
               "test"
               "testfile2.org")))
    (should (file-exists-p file))
    (org-node--scan-targeted (list file)))
  (org-node-cache-ensure t)
  (let ((node (gethash "bb02315f-f329-4566-805e-1bf17e6d892d" org-nodes)))
    (should (equal (indexed-olp node) nil))
    (should (equal (indexed-file-title node) "Title"))
    (should (equal (indexed-todo-state node) "CUSTOMDONE"))
    (should (equal (indexed-scheduled node) "<2024-06-17 Mon>")))
  (let ((node (gethash "d28cf9b9-d546-46b0-8615-9880a4d2463d" org-nodes)))
    (should (equal (indexed-olp node) '("1st-level" "TODO 2nd-level, invalid todo state")))
    (should (equal (indexed-title node) "3rd-level, has ID"))
    (should (equal (indexed-todo-state node) nil))))

(ert-deftest org-node/test-having-multiple-id-dirs ()
  (mkdir "/tmp/org-node/test1" t)
  (mkdir "/tmp/org-node/test2" t)
  (write-region "" nil "/tmp/org-node/test2/emptyfile.org")
  (write-region "" nil "/tmp/org-node/test2/emptyfile2.org")
  (write-region "" nil "/tmp/org-node/test1/emptyfile3.org")
  (let ((org-id-locations (make-hash-table :test 'equal))
        (indexed--file<>data (make-hash-table :test 'equal))
        (org-node-extra-id-dirs '("/tmp/org-node/test1/"
                                  "/tmp/org-node/test2/"))
        (org-node-ask-directory nil))
    (should (equal (car (org-node--root-dirs (indexed--relist-org-files)))
                   "/tmp/org-node/test2/"))))

(defun org-node-test--file (file)
  "Look for FILE in test/ directory or in default directory."
  (let ((loaded-org-node (find-library-name "org-node")))
    (or (and loaded-org-node
             (let ((file (file-name-concat
                          (file-name-directory (file-truename loaded-org-node))
                          "test" file)))
               (if (file-exists-p file) file)))
        file)))

(ert-deftest org-node/test-goto-random ()
  (org-node--scan-targeted (list (org-node-test--file "testfile2.org")))
  (org-node-cache-ensure t)
  (let ((node (seq-random-elt (hash-table-values org-nodes))))
    (org-node--goto node)
    (should (equal (point) (indexed-pos node)))
    (should (equal (abbreviate-file-name (buffer-file-name))
                   (indexed-file-name node)))))

(ert-deftest org-node/test-file-naming ()
  (let ((org-node-datestamp-format "")
        (org-node-slug-fn #'org-node-slugify-for-web))
    (should (equal (funcall org-node-slug-fn "19 Foo bär Baz 588")
                   "19-foo-bar-baz-588")))
  (should (equal (org-node--make-regexp-for-time-format "%Y%M%d")
                 "^[[:digit:]]+$"))
  (should (equal (org-node--make-regexp-for-time-format "%A%Y%M%d-")
                 "^[[:alpha:]]+[[:digit:]]+-")))

;;; org-node-test.el ends here
