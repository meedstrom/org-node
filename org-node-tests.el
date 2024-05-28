;;; -*- lexical-binding: t; -*-

;; Should test:
;; - That all nodes have all required properties non-nil (e.g. :title)
;; ...

(require 'buttercup)

(describe "Test pos->parent-id"
          (it ""
              (let ((olp '((373 "A subheading" 2 "33dd")
                           (250 "A top heading" 1 "d3")
                           (199 "Another top heading" 1)
                           (123 "First heading in the file is apparently third-level" 3))))
                (expect (org-node-worker--pos->parent-id

                         373
                         nil)
                        :to-be "d3"))))


;; Hm... actually should error when passed an assoc not exist

(describe "Test oldata fns"
          :var ((olp '((3730 "A subheading" 2 "33dd")
                       (2503 "A top heading" 1 "d3rh")
                       (1300 "A sub-subheading" 3 "d3csae")
                       (1001 "A subheading" 2 "d3")
                       (199 "Another top heading" 1)
                       (123 "First heading in file is apparently third-level" 3))))
          (it "Test pos->olp"
              (expect (org-node-worker--pos->olp olp 1300)
                      :to-equal '("Another top heading" "A subheading"))
              (expect (org-node-worker--pos->olp olp 2503)
                      :to-equal nil)
              (expect (org-node-worker--pos->olp olp 2500)
                      :to-throw))

          (it "Test pos->parent-id"
              (expect (org-node-worker--pos->parent-id olp 1300 nil)
                      :to-equal "d3")))


(describe "Various functions"
          (it ""
              (expect (length (org-node--split-into-n-sublists
                               '(a v e e) 7))
                      :to-be 7)
              (expect (length (remq nil (org-node--split-into-n-sublists
                                         '(a v e e) 7)))
                      :to-be 1)
              (expect (length
                       (org-node--split-into-n-sublists
                        '(a v e e q l fk k k ki i o r r r r r r r r r r g g g g g gg)
                        4))
                      :to-be 4)))

(describe "Parse testfile2.org"
          (if (gethash "bb02315f-f329-4566-805e-1bf17e6d892d" org-nodes)
              (org-node-cache-rescan-file nil "testfile2.org")
            (org-node-cache--collect (list "testfile2.org")))
          (it "Etc"
              (let ((node (gethash "bb02315f-f329-4566-805e-1bf17e6d892d" org-nodes)))
                (expect (org-node-get-olp node)
                        :to-equal nil)
                (expect (org-node-get-file-title node)
                        :to-equal "Title")
                (expect (org-node-get-todo node)
                        :to-equal "TODO"))
              (let ((node (gethash "d28cf9b9-d546-46b0-8615-9880a4d2463d" org-nodes)))
                (expect (org-node-get-olp node)
                        :to-equal '("1st-level" "2nd-level")))))
