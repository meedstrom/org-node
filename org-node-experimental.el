;;; org-node-experimental.el -*- lexical-binding: t; -*-

;;; Commentary:
;;
;; Advices that instruct org-node to collect outline paths.  They make it
;; about 50% slower to rebuild the cache.
;;
;; To enable:
;;
;; (advice-add 'org-node-cache--scan :override #'org-node-experimental--scan)
;; (advice-add 'org-node-cache-reset :before #'org-node-experimental--clear-extra-hash-tables)

;;; Code:

(defvar org-node-experimental--files-with-file-level-nodes
  (make-hash-table :size 4000 :test #'equal)
  "Table keyed on filepaths, where each value is t.")

(defvar org-node-experimental--files
  (make-hash-table :size 4000 :test #'equal)
  "Table keyed on filepaths, where each value is an alist of the form:
((LINE-NUM . ID)
 (LINE-NUM . ID)
 ...)")

(defvar org-node-experimental--file-outlines
  (make-hash-table :size 4000 :test #'equal)
  "Table keyed on filepaths, where each value is an alist of the form:
((LINE-NUM . OUTLINE-LEVEL)
 (LINE-NUM . OUTLINE-LEVEL)
 ...)

Used by `org-node-experimental--line->olpath'.")

(defun org-node-experimental--collect-file-level-nodes (target)
  "Scan TARGET (a file or directory) for file-level ID nodes."
  (let ((rg-result
         (apply #'org-node-cache--program-output "rg"
                `("--multiline"
                  "--ignore-case"
                  "--with-filename"
                  "--line-number"
                  "--max-count" "1"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2\f$3\f$4\f$5\f$6\f$7--veryIntelligentSeparator--"
                  ,@org-node-cache-extra-rg-args
                  ,org-node-cache--file-level-re
                  ,target))))
    (dolist (file-head (string-split rg-result "--veryIntelligentSeparator--\n" t))
      (let ((splits (string-split file-head "\f")))
        (let ((file:lnum (string-split (nth 0 splits) ":" t))
              ($1 (nth 1 splits))
              ($2 (nth 2 splits))
              ($3 (nth 3 splits))
              ($4 (nth 4 splits))
              ($5 (nth 5 splits))
              ($6 (nth 6 splits))
              ($7 (nth 7 splits)))
          (let ((node (list :title $7
                            :level 0
                            :line-number 1
                            :exclude (not (string-blank-p $4))
                            :tags (string-split $6 ":" t)
                            :file-path (car file:lnum)
                            :id $2
                            :aliases (org-node-cache--aliases->list $3)
                            :roam-refs (string-split $5 " " t)
                            :backlink-ids (org-node-cache--backlinks->list $1))))
            (push (cons 1 (plist-get node :id))
                  (gethash (car file:lnum) org-node-experimental--files))
            (puthash $2 node org-nodes))
          (puthash (car file:lnum) t
                   org-node-experimental--files-with-file-level-nodes))))))

(defun org-node-experimental--calc-file-outlines (target)
  "Must run after collect-file-level-nodes."
  (let ((file nil)
        (rg-result (apply #'org-node-cache--program-output "rg"
                          `("--with-filename"
                            "--line-number"
                            "--only-matching"
                            ,@org-node-cache-extra-rg-args
                            "^\\*+ "
                            ,target))))
    (dolist (line (split-string rg-result "\n" t))
      (let ((parts (split-string line ":")))
        (let ((lnum (string-to-number (nth 1 parts)))
              (level (length (substring (nth 2 parts) 0 -1))))
          (unless (equal file (nth 0 parts))
            (setq file (nth 0 parts))
            ;; If the file has a file level node, add level 0
            (when (gethash file org-node-experimental--files-with-file-level-nodes)
              (push (cons 1 0)
                    (gethash file org-node-experimental--file-outlines))))
          (push (cons lnum level)
                (gethash file org-node-experimental--file-outlines)))))))

;; (org-node-cache-peek org-node-experimental--file-outlines)

(defun org-node-experimental--collect-subtree-nodes (target)
  "Scan TARGET (a file or directory) for subtree ID nodes."
  (let ((rg-result
         (apply #'org-node-cache--program-output "rg"
                `("--multiline"
                  "--with-filename"
                  "--line-number"
                  "--only-matching"
                  "--replace"
                  "\f$1\f$2\f$3\f$4\f$5\f$6\f$7\f$8\f$9--veryIntelligentSeparator--"
                  ,@org-node-cache-extra-rg-args
                  ,(org-node-cache--calc-subtree-re)
                  ,target))))
    (dolist (subtree (string-split rg-result "--veryIntelligentSeparator--\n" t))
      (let ((splits (string-split subtree "\f")))
        (let ((file:lnum (string-split (nth 0 splits) ":" t))
              ($1 (nth 1 splits))
              ($2 (nth 2 splits))
              ($3 (nth 3 splits))
              ($4 (nth 4 splits))
              ($5 (nth 5 splits))
              ($6 (nth 6 splits))
              ($7 (nth 7 splits))
              ($8 (nth 8 splits))
              ($9 (nth 9 splits)))
          (let ((node (list :title $3
                            :is-subtree t
                            :pseudo-olpath
                            (org-node-experimental--line->olpath
                             (gethash (car file:lnum)
                                      org-node-experimental--file-outlines)
                             (string-to-number (cadr file:lnum)))
                            :level (length $1)
                            :line-number (string-to-number (cadr file:lnum))
                            :exclude (not (string-blank-p $8))
                            :tags (string-split $4 ":" t)
                            :todo (unless (string-blank-p $2) $2)
                            :file-path (car file:lnum)
                            :id $6
                            :aliases (org-node-cache--aliases->list $7)
                            :roam-refs (string-split $9 " " t)
                            :backlink-ids (org-node-cache--backlinks->list $5))))
            (push (cons (plist-get node :line-number) (plist-get node :id))
                  (gethash (car file:lnum) org-node-experimental--files))
            (puthash $6 node org-nodes)))))))

(defun org-node-experimental--line->olpath (outline-tree-as-alist line)
  "Given LINE number, return a list of line numbers corresponding to
where the ancestor subtrees are.  The current subtree is included.

Argument OUTLINE-TREE-AS-ALIST must be of the form
 ((1 . 0)
  (23 . 3)
  (50 . 1)
  (73 . 2)
  ...)

where the car represents a line number and the cdr represents the
outline depth i.e. the number of asterisks in the heading at that
line."
  (let* (reversed
         ;; Drop all the info below LINE
         (relevant (cl-loop for cell in (reverse outline-tree-as-alist)
                            if (> line (car cell))
                            do (push cell reversed)
                            else return t))
         (curr-level (cdr (car reversed)))
         (path (list (car (car reversed)))))
    ;; Work backwards towards the top of the file
    (cl-loop for cell in reversed
             when (> curr-level (cdr cell))
             do (setq curr-level (cdr cell))
             (push (car cell) path))
    path))

;; TODO: Use this to infer inherited tags, properties, all that good stuff
(defun org-node-experimental--olpath->ids (file pseudo-olpath)
  "Operating on a PSEUDO-OLPATH as generated by `org-node-experimental--line->olpath',
transform it into a list of IDs in FILE."
  (let ((nodes-in-file (gethash file org-node-experimental--files)))
    (cl-loop for lnum in pseudo-olpath
             as known = (assoc lnum nodes-in-file)
             when known collect (cdr known))))

(defun org-node-experimental--clear-extra-hash-tables ()
  (clrhash org-node-experimental--file-outlines)
  (clrhash org-node-experimental--files-with-file-level-nodes)
  (clrhash org-node-experimental--files))

(defun org-node-experimental--scan (target)
  (org-node-experimental--collect-file-level-nodes target)
  (org-node-experimental--calc-file-outlines target)
  (org-node-experimental--collect-subtree-nodes target))

;; Alternative approach using affixation/annotation
;; (defun org-node-experimental--read ()
;;   (let ((completion-extra-properties
;;          (list :affixation-function
;;                (lambda (completions)
;;                  (cl-loop
;;                   for c in completions
;;                   for node = (gethash c org-node-collection)
;;                   collect (list c
;;                                 (concat (plist-get node :file-path) " -> ")
;;                                 nil))))))
;;     (gethash (completing-read "Node: " org-node-collection
;;                               () () () 'org-node-hist)
;;              org-node-collection)))
;; ;; (org-node-read)


(provide 'org-node-experimental)

;;; org-node-experimental.el ends here
