;;; org-node-test.el -*- lexical-binding: t; -*-

(require 'ert)
(require 'dash)
(require 'org-node)
(require 'org-node-roam)
(require 'org-node-worker)
(require 'org-node-backlink)

;; TODO Maybe org-ref &keys can easily be supported, on same principle as the
;;      org 9.5 @keys.  See org-ref manual for syntax.  Basically worker.el
;;      just need some more branches on merged-re and plain-re matches.
;;
;;      In short, what's relevant to us: org-ref v3 citations always match
;;      plain-re because they are URIs starting with citep:, citet:, citealp:
;;      and many others, but it's the old story where you have to match any-re
;;      first in case it is bracketed, with spaces inside.  Like [[citep:See
;;      &kitchin-2015-examp page 2]].  Anyway, then just extract each &key.
;;      Finally, have to consult some org-ref variable first to get all the
;;      defined URI types.
;;
;;      Wow!  I just combine the lessons I learned for supporting bracketed
;;      link with spaces, for extracting @citekeys, and for consulting an
;;      org-super-links variable!  Easy peasy.
(ert-deftest org-node-test--parse-refs ()
  (let ((result (org-node-worker--split-refs-field
                 "[cite:@citeKey abcd ; @citeKey2 cdefgh] @foo [[https://gnu.org/A Link With Spaces/index.htm][baz]] https://gnu.org ")))
    (should (--all-p (member it result)
                     '("@citeKey2"
                       "@citeKey"
                       "@foo"
                       "https://gnu.org/A Link With Spaces/index.htm"
                       "https://gnu.org")))))

(ert-deftest org-node-test--oldata-fns ()
  (let ((olp '((3730 "A subheading" 2 "33dd")
               (2503 "A top heading" 1 "d3rh")
               (1300 "A sub-subheading" 3 "d3csae")
               (1001 "A subheading" 2 "d3")
               (199 "Another top heading" 1)
               (123 "First heading in file is apparently third-level" 3))))
    (should (equal (org-node-worker--pos->olp olp 1300)
                   '("Another top heading" "A subheading")))
    (should (equal (org-node-worker--pos->olp olp 2503)
                   nil))
    (should-error (org-node-worker--pos->olp olp 2500))
    (should (equal (org-node-worker--pos->parent-id olp 1300 nil)
                   "d3"))))

(ert-deftest org-node-test--parse-testfile2.org ()
  (org-node--scan-targeted (list "testfile2.org"))
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

(ert-deftest org-node-test--multiple-id-dirs ()
  (mkdir "/tmp/org-node/test1" t)
  (mkdir "/tmp/org-node/test2" t)
  (write-region "" nil "/tmp/org-node/test2/emptyfile.org")
  (write-region "" nil "/tmp/org-node/test2/emptyfile2.org")
  (write-region "" nil "/tmp/org-node/test1/emptyfile3.org")
  (let ((org-id-locations (make-hash-table :test #'equal))
        (org-node-extra-id-dirs '("/tmp/org-node/test1/"
                                  "/tmp/org-node/test2/"))
        (org-node-ask-directory nil))
    (should (equal (car (org-node--root-dirs (org-node-files)))
                   "/tmp/org-node/test2/"))))

(ert-deftest org-node-test--goto-random ()
  (require 'seq)
  (org-node--scan-targeted (list "testfile2.org"))
  (org-node-cache-ensure t)
  (let ((node (seq-random-elt (hash-table-values org-node--id<>node))))
    (org-node--goto node)
    (should (equal (point) (org-node-get-pos node)))
    (should (equal (abbreviate-file-name (buffer-file-name))
                   (org-node-get-file-path node)))))

(ert-deftest org-node-test--split-file-list ()
  (should (equal 4 (length (org-node--split-into-n-sublists
                            '(a v e e) 7))))
  (should (equal 1 (length (org-node--split-into-n-sublists
                            '(a v e e) 1))))
  (should (equal 4 (length (org-node--split-into-n-sublists
                            '(a v e e q l fk k k ki i o r r r r r r r r r r g g g g g gg)
                            4)))))

(ert-deftest org-node-test--various ()
  (let ((org-node-ask-directory "/tmp/org-node/test/")
        ;; NOTE you should manually test the other creation-fns
        (org-node-creation-fn #'org-node-new-file))
    (delete-directory org-node-ask-directory t)
    ;; (should-error (org-node--create "New node" "not-an-uuid1234"))
    (mkdir org-node-ask-directory t)
    (org-node--create "New node" "not-an-uuid1234")
    (org-node-cache-ensure t)
    (let ((node (gethash "not-an-uuid1234" org-node--id<>node)))
      (org-node--goto node)
      (should (file-equal-p default-directory org-node-ask-directory))
      (should (equal (org-node-get-id node) (org-entry-get nil "ID" t)))
      (should (equal (org-node-get-id node) "not-an-uuid1234"))
      (should (equal (org-node-get-title node) "New node"))
      (should (equal (org-node-get-file-title node) "New node"))
      (should (equal (org-node-get-file-title-or-basename node) "New node")))
    (let ((org-node-prefer-file-level-nodes nil))
      (org-node--create "A top-level heading" "not-an-uuid5678")
      (org-node-cache-ensure t)
      (let ((node (gethash "not-an-uuid5678" org-node--id<>node))
            (expected-filename (funcall org-node-filename-fn "A top-level heading")))
        (should (equal (org-node-get-title node) "A top-level heading"))
        (should (equal (org-node-get-file-title node) nil))
        (should (equal (org-node-get-file-title-or-basename node)
                       expected-filename))))))

(provide 'org-node-test)

;;; org-node-test.el ends here
