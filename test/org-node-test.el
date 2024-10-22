;;; org-node-test.el --- Test suite -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Martin Edström

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
(require 'dash)
(require 'find-func)
(require 'org-node)
(require 'org-node-parser)
(require 'org-node-backlink)
;; (require 'org-node-fakeroam)

;; (-all-p #'org-node-link-p (apply #'append (hash-table-values org-node--dest<>links)))

(ert-deftest org-node/test-split-refs-field ()
  (setq org-node-parser--paths-types nil)
  (let ((result
         (org-node-parser--split-refs-field
          (concat " \"[cite:@citekey abcd ; @citekey2 cdefgh;@citekey3]\""
                  " \"[[citep:&citekey4 abcd ; &citekey5 cdefgh;&citekey6]]\""
                  " \"[[https://gnu.org/A Link With Spaces/index2.htm]]\""
                  " [[https://gnu.org/A Link With Spaces/index.htm][baz]]"
                  " https://gnu.org [cite:&citekey7]  @foo &bar "
                  " info:with%20escaped%20spaces"))))
    (should (--all-p (member it result)
                     '("@citekey"
                       "@citekey4"
                       "@citekey7"
                       "@foo"
                       "@bar"
                       "with escaped spaces"
                       "//gnu.org/A Link With Spaces/index.htm"
                       "//gnu.org/A Link With Spaces/index2.htm"
                       "//gnu.org")))
    (should (equal "https" (cdr (assoc "//gnu.org/A Link With Spaces/index.htm"
                                       org-node-parser--paths-types))))
    (should (equal "https" (cdr (assoc "//gnu.org"
                                       org-node-parser--paths-types))))
    (should (equal nil (cdr (assoc "@citekey"
                                   org-node-parser--paths-types))))
    (should (equal nil (cdr (assoc "citekey"
                                   org-node-parser--paths-types))))))

(ert-deftest org-node/test-time-format-hacks ()
  (let ((fmt "Wild%Y--%m%dexample%H%M%S-"))
    (should (equal (org-node-extract-ymd
                    (format-time-string fmt '(26300 36406 109008 201000))
                    fmt)
                   "2024-08-14"))))

(ert-deftest org-node/test-parsing-testfile2.org ()
  (org-node--scan-targeted
   (list (file-name-concat (file-name-directory (locate-library "org-node"))
                           "test"
                           "testfile2.org")))
  (org-node-cache-ensure t)
  (let ((node (gethash "bb02315f-f329-4566-805e-1bf17e6d892d" org-node--id<>node)))
    (should (equal (org-node-get-olp node) nil))
    (should (equal (org-node-get-file-title node) "Title"))
    (should (equal (org-node-get-todo node) "CUSTOMDONE"))
    (should (equal (org-node-get-scheduled node) "<2024-06-17 Mon>")))
  (let ((node (gethash "d28cf9b9-d546-46b0-8615-9880a4d2463d" org-node--id<>node)))
    (should (equal (org-node-get-olp node) '("1st-level" "TODO 2nd-level, invalid todo state")))
    (should (equal (org-node-get-title node) "3rd-level, has ID"))
    (should (equal (org-node-get-todo node) nil))))

(ert-deftest org-node/test-having-multiple-id-dirs ()
  (mkdir "/tmp/org-node/test1" t)
  (mkdir "/tmp/org-node/test2" t)
  (write-region "" nil "/tmp/org-node/test2/emptyfile.org")
  (write-region "" nil "/tmp/org-node/test2/emptyfile2.org")
  (write-region "" nil "/tmp/org-node/test1/emptyfile3.org")
  (let ((org-id-locations (make-hash-table :test #'equal))
        (org-node--file<>mtime.elapsed (make-hash-table :test #'equal))
        (org-node-extra-id-dirs '("/tmp/org-node/test1/"
                                  "/tmp/org-node/test2/"))
        (org-node-ask-directory nil))
    (should (equal (car (org-node--root-dirs (org-node-list-files)))
                   "/tmp/org-node/test2/"))))

(defun org-node-test-file (file)
  "Look for FILE in test/ directory or in default directory."
  (let ((loaded-org-node (find-library-name "org-node")))
    (or (and loaded-org-node
             (let ((file (file-name-concat
                          (file-name-directory (file-truename loaded-org-node))
                          "test" file)))
               (if (file-exists-p file) file)))
        file)))

(ert-deftest org-node/test-goto-random ()
  (require 'seq)
  (org-node--scan-targeted (list (org-node-test-file "testfile2.org")))
  (org-node-cache-ensure t)
  (let ((node (seq-random-elt (hash-table-values org-node--id<>node))))
    (org-node--goto node)
    (should (equal (point) (org-node-get-pos node)))
    (should (equal (abbreviate-file-name (buffer-file-name))
                   (org-node-get-file-path node)))))

(ert-deftest org-node/test-split-into-n-sublists ()
  (should (equal 4 (length (org-node--split-into-n-sublists
                            '(a v e e) 7))))
  (should (equal 1 (length (org-node--split-into-n-sublists
                            '(a v e e) 1))))
  (should (equal 4 (length (org-node--split-into-n-sublists
                            '(a v e e q l fk k k ki i o r r r r r r  r g g gg)
                            4)))))

(ert-deftest org-node/test-file-naming ()
  (let ((org-node-datestamp-format "")
        (org-node-slug-fn #'org-node-slugify-for-web))
    (should (equal (funcall org-node-slug-fn "19 Foo bar Baz 588")
                   "19-foo-bar-baz-588")))
  (should (equal (org-node--make-regexp-for-time-format "%Y%M%d")
                 "^[[:digit:]]+"))
  (should (equal (org-node--make-regexp-for-time-format "%A%Y%M%d-")
                 "^[[:alpha:]]+[[:digit:]]+-")))

(ert-deftest org-node/test-various ()
  (let ((org-node-ask-directory "/tmp/org-node/test/")
        ;; NOTE you should manually test the other creation-fns
        (org-node-creation-fn #'org-node-new-file))
    (delete-directory org-node-ask-directory t)
    ;; (should-error (org-node-create "New node" "not-an-uuid1234"))
    (mkdir org-node-ask-directory t)
    (org-node-create "New node" "not-an-uuid1234")
    (org-node-cache-ensure t)
    (let ((node (gethash "not-an-uuid1234" org-node--id<>node)))
      (org-node--goto node)
      (should (file-equal-p default-directory org-node-ask-directory))
      (should (equal (org-node-get-id node) (org-entry-get nil "ID" t)))
      (should (equal (org-node-get-id node) "not-an-uuid1234"))
      (should (equal (org-node-get-title node) "New node"))
      (should (equal (org-node-get-file-title node) "New node"))
      (should (equal (org-node-get-file-title-or-basename node) "New node")))
    (let ((org-node-prefer-with-heading nil))
      (org-node-create "A top-level heading" "not-an-uuid5678")
      (org-node-cache-ensure t)
      (let ((node (gethash "not-an-uuid5678" org-node--id<>node))
            (expected-filename
             (concat (format-time-string org-node-datestamp-format)
                     (funcall org-node-slug-fn "A top-level heading")
                     ".org")))
        (should (equal (org-node-get-title node) "A top-level heading"))
        (should (equal (org-node-get-file-title node) nil))
        (should (equal (org-node-get-file-title-or-basename node)
                       expected-filename))))))

;;; org-node-test.el ends here
