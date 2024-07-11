;;; org-node-backlink.el -*- lexical-binding: t; -*-

;; FIXME I believe that when you remove a link, then
;;       `org-node-cache-rescan-file' is not enough to remove the link from
;;       the link table.  It gets corrected only on reset.

(require 'org-node-common)
(require 'org-node-cache)
(require 'cl-macs)

;;;###autoload
(let (warned-once)
  (defun org-node-backlinks-mode (&rest args)
    (unless warned-once
      (setq warned-once t)
      (run-with-timer .1 nil #'display-warning 'org-node
                      "Your config may have misspelled `org-node-backlink-mode' as `org-node-backlinks-mode'"))
    (apply #'org-node-backlink-mode args)))

(define-globalized-minor-mode org-node-backlink-global-mode
  org-node-backlink-mode
  (lambda ()
    (when (derived-mode-p 'org-mode)
      (org-node-backlink-mode))))

;;;###autoload
(define-minor-mode org-node-backlink-mode
  "Keep :BACKLINKS: properties updated."
  :group 'org-node
  (if org-node-backlink-mode
      (progn
        (add-hook 'org-roam-post-node-insert-hook
                  #'org-node-backlink--add-in-target nil t)
        (add-hook 'org-node-insert-link-hook
                  #'org-node-backlink--add-in-target nil t)
        (add-hook 'after-change-functions
                  #'org-node-backlink--flag-change nil t)
        (add-hook 'before-save-hook
                  #'org-node-backlink--fix-changed-parts-of-buffer nil t)
        ;; We won't clean up this advice when the mode is turned off, because
        ;; it's a buffer-local mode and advices cannot be buffer-local, but
        ;; it's OK because it's made to no-op where the mode is inactive.
        (advice-add 'org-insert-link :after
                    #'org-node-backlink--add-in-target)
        (org-node-cache-ensure 'must-async))
    (remove-hook 'org-roam-post-node-insert-hook
                 #'org-node-backlink--add-in-target t)
    (remove-hook 'org-node-insert-link-hook
                 #'org-node-backlink--add-in-target t)
    (remove-hook 'after-change-functions
                 #'org-node-backlink--flag-change t)
    (remove-hook 'before-save-hook
                 #'org-node-backlink--fix-changed-parts-of-buffer t)))

(defvar org-node-backlink--fix-ctr 0)
(defvar org-node-backlink--files-to-fix nil)

(defun org-node-backlink-regret ()
  "Visit all `org-id-locations' and remove :BACKLINKS: property."
  (interactive)
  (org-node-backlink-fix-all 'remove))

(defun org-node-backlink-fix-all (&optional remove?)
  "Add :BACKLINKS: property to all nodes known to `org-id-locations'.
Optional argument REMOVE? t means remove them instead, the same
as the user command \\[org-node-backlink-regret].

Can be quit midway through and resumed later.  With
\\[universal-argument], start over instead of resuming."
  (interactive)
  (when (or (null org-node-backlink--files-to-fix) current-prefix-arg)
    ;; Start over
    (org-node-cache-ensure t t)
    (setq org-node-backlink--files-to-fix
          (-uniq (hash-table-values org-id-locations))))
  (when (or (not (= 0 org-node-backlink--fix-ctr)) ;; resume interrupted
            (and
             (y-or-n-p (format "Edit the %d files found in `org-id-locations'?"
                               (length org-node-backlink--files-to-fix)))
             (y-or-n-p (string-fill "You understand that this may trigger your auto git-commit systems and similar because many files are about to be edited and saved?"
                                    fill-column))))
    ;; Do 1000 at a time, because Emacs cries about opening too many file
    ;; buffers in one loop
    (let (;;(coding-system-for-read org-node-perf-assume-coding-system)
          (file-name-handler-alist nil))
      (dotimes (_ 1000)
        (when-let ((file (pop org-node-backlink--files-to-fix)))
          (message "Adding/updating :BACKLINKS:... (you may quit and resume anytime) (%d) %s"
                   (cl-incf org-node-backlink--fix-ctr) file)
          (org-node--with-quick-file-buffer file
            (org-node-backlink--fix-whole-buffer remove?)))))
    (if org-node-backlink--files-to-fix
        ;; Keep going
        (run-with-timer 1 nil #'org-node-backlink-fix-all remove?)
      ;; Reset
      (setq org-node-backlink--fix-ctr 0))))

(defun org-node-backlink--fix-whole-buffer (&optional remove?)
  (save-excursion
    (goto-char (point-min))
    (let ((case-fold-search t))
      (while (re-search-forward "^[[:space:]]*:id: " nil t)
        (org-node-backlink--fix-subtree-here remove?)))))

(defun org-node-backlink--fix-subtree-here (&optional remove?)
  "Assume point is at an :ID: line, after the \":ID:\" substring,
then update the :BACKLINKS: property in this entry.  With arg
REMOVE, remove it instead."
  (if remove?
      (org-entry-delete nil "BACKLINKS")
    (skip-chars-forward "[:space:]")
    (let* ((id (buffer-substring-no-properties
                (point) (+ (point) (skip-chars-forward "^ \n"))))
           (node (gethash id org-node--node-by-id)))
      ;; The node may not yet have been scanned by org-node, because it was
      ;; created just now, in a capture buffer which triggered a save hook
      ;; which led us here.  There's probably an async process looking at it
      ;; right now but we're slightly too early.  In fact, I wager this happens
      ;; every time, so backlinks are always added using slightly outdated
      ;; metadata. TODO: Maybe don't run on save hook but rather on
      ;; handle-finished-job?
      (when node
        ;; Make the full string to which the :BACKLINKS: property should be set
        (let* ((reflinks (org-node-get-reflinks node))
               (backlinks (gethash id org-node--backlinks-by-id))
               ;; (cites (--keep (gethash it org-node--cites-by-citekey)
               ;;                (org-node-get-refs node)))
               (combined
                (thread-last
                  (append reflinks backlinks)
                  (--map (plist-get it :origin))
                  (-uniq)
                  (-non-nil) ;; REVIEW why can there be nils?
                  (-sort #'string-lessp)
                  ;; At this point we have a sorted list of ids of
                  ;; every node that links to here.  Now format them as links.
                  (--map (org-link-make-string
                          (concat "id:" it)
                          (org-node-get-title
                           (or (gethash it org-node--node-by-id)
                               (error "ID in backlink tables not known to main org-nodes table: %s"
                                      it)))))))
               (links-string (string-join combined "  ")))
          (if combined
              (unless (equal links-string (org-entry-get nil "BACKLINKS"))
                (org-entry-put nil "BACKLINKS" links-string))
            (org-entry-delete nil "BACKLINKS")))))))

(defun org-node-backlink--fix-changed-parts-of-buffer ()
  "Look for areas flagged by `org-node-backlink--flag-change' and
run `org-node-backlink--fix-subtree-here' at each affected
subtree.  For a huge file, this is much faster than using
`org-node-backlink--fix-whole-buffer'."
  (when org-node-backlink-mode
    ;; Catch any error because this runs at `before-save-hook' which MUST fail
    ;; gracefully and let the user save anyway
    (condition-case err
        (save-excursion
          (without-restriction
            ;; Iterate over each change-region.  Algorithm borrowed from
            ;; `ws-butler-map-changes', odd but elegant.  Worth knowing that if
            ;; you tell Emacs to search for text that has a given text-property
            ;; with a nil value, that's the same as searching for text without
            ;; that property at all.  So if position START is in some
            ;; unmodified area -- "org-node-flag" is effectively valued at nil
            ;; -- this way of calling `text-property-not-all' means search
            ;; forward until it is t.  Then calling it again has the opposite
            ;; effect, searching until it is nil again.
            (let ((start (point-min-marker))
                  (end (make-marker))
                  (case-fold-search t)
                  prop)
              (while (< start (point-max))
                (setq prop (get-text-property start 'org-node-flag))
                (set-marker end (or (text-property-not-all
                                     start (point-max) 'org-node-flag prop)
                                    (point-max)))
                (when prop
                  (goto-char start)
                  ;; START and END delineate an area where changes were
                  ;; detected, but the area rarely envelops the current
                  ;; subtree's property drawer, likely placed long before
                  ;; START, so search back for it
                  (save-excursion
                    (let ((id-here (org-entry-get nil "ID" t)))
                      (and id-here
                           (re-search-backward
                            (concat "^[[:space:]]*:id: *"
                                    (regexp-quote id-here))
                            nil t)
                           (re-search-forward ":id: *" (pos-eol))
                           (org-node-backlink--fix-subtree-here))))
                  ;; ...and if the change-area is massive, spanning multiple
                  ;; subtrees (like after a big yank), update each subtree
                  (while (and (< (point) end)
                              (re-search-forward
                               "^[[:space:]]*:id: " end t))
                    (org-node-backlink--fix-subtree-here))
                  (remove-text-properties start end 'org-node-flag))
                ;; This change-area dealt with, move on
                (set-marker start (marker-position end)))
              (set-marker start nil)
              (set-marker end nil))))
      (( error debug )
       (message "org-node: Updating backlinks ran into an issue: %S" err)
       (remove-text-properties (point-min) (point-max) 'org-node-flag)
       ;; Provide backtrace even tho we don't signal an error
       (when debug-on-error
         (message "org-node: Printing backtrace")
         (backtrace))))))

(defun org-node-backlink--flag-change (beg end _)
  "Add text property `org-node-flag' to region between BEG and END.

Designed to run on `after-change-functions', in which case this
effectively flags all areas where text is added/changed/deleted."
  (when (derived-mode-p 'org-mode)
    (with-silent-modifications
      (put-text-property beg end 'org-node-flag t))))


;;; Link-insertion advice
;;
;; This logic is independent from the save-hook
;; `org-node-backlink--fix-changed-parts-of-buffer', because that operates on
;; the file being saved -- in other words, making the file navel-gaze its own
;; content to see if it looks correct according to current links tables.
;; Technically, that would be enough to result in correct backlinks everywhere
;; if you just run it on all files, and that's more-or-less how
;; `org-node-backlink-fix-all' works, but we don't want to run it all the time.
;;
;; By contrast, the below code does not look up tables, just reacts to the
;; exact link being inserted, which has two benefits:
;;
;; 1. You can observe backlinks appearing in realtime before a buffer is saved
;;
;; 2. It's actually necessary, because a link being inserted does not mean we
;;    should check the current file but rather visit and edit the target file.
;;    If we didn't have the below code, we'd have save the current buffer (in
;;    order to update tables) and then open the target file and run
;;    `org-node-backlink--fix-whole-buffer', which can easily take a while for
;;    a big target file.

;; TODO Report when it has members
(defvar org-node-backlink--fails nil
  "List of IDs that could not be resolved.")

(defun org-node-backlink--add-in-target (&rest _)
  "For known link at point, leave a backlink in the target node."
  (unless (derived-mode-p 'org-mode)
    (error "Called in non-org buffer"))
  (when org-node-backlink-mode
    (org-node-cache-ensure)
    (let ((elm (org-element-context)))
      (let ((path (org-element-property :path elm))
            (type (org-element-property :type elm))
            id file)
        (when (and path type)
          ;; REVIEW Here is actually interesting space to consider merging
          ;; some code for backlinks and reflinks by just considering ids
          ;; themselves as a kind of ref...
          (if (equal "id" type)
              ;; Classic backlink
              (progn
                (setq id path)
                (setq file (org-id-find-id-file id)))
            ;; "Reflink"
            (setq id (gethash (concat type ":" path) org-node--id-by-ref))
            (setq file (ignore-errors
                         (org-node-get-file-path
                          (gethash id org-node--node-by-id)))))
          (when (null file)
            (push id org-node-backlink--fails))
          (when (and id file)
            (org-node-backlink--add-in-target-1 file id)))))))

(defun org-node-backlink--add-in-target-1 (target-file target-id)
  (let ((case-fold-search t)
        (src-id (org-entry-get nil "ID" t)))
    (when (and src-id (not (equal src-id target-id)))
      (let ((src-title
             (save-excursion
               (without-restriction
                 (re-search-backward (concat "^[ \t]*:id: +" src-id))
                 (or (org-get-heading t t t t)
                     (cadar (org-collect-keywords '("TITLE")))
                     (file-name-nondirectory buffer-file-name))))))
        ;; The link needs to be recorded immediately to ensure that later on,
        ;; `org-node-backlink--fix-changed-parts-of-buffer' will not remove the
        ;; backlink we just added.  Ditto for node at point, if it hasn't been
        ;; scanned yet.
        (org-node-cache--dirty-ensure-node-known)
        (let ((org-node--imminent-recovery-msg
               "Org-node going to add a backlink to the target of the link you just inserted, but it's likely you will first get a prompt to recover an auto-save file, ready? "))
          (org-node--with-quick-file-buffer target-file
            (org-node-backlink--add-at target-id src-title src-id)))))))

(defun org-node-backlink--add-at (target-id src-title src-id)
  (goto-char (point-min))
  (if (not (re-search-forward
            (concat "^[ \t]*:id: +" (regexp-quote target-id))
            nil t))
      (push target-id org-node-backlink--fails)
    (when (get-text-property (point) 'read-only)
      ;; If for some reason the search landed us in a transclude region or...
      ;; Note that `org-entry-put' inhibits read-only, so it wouldn't have
      ;; signaled any error.
      (error "org-node: Property drawer seems to be read-only at %d in %s"
             (point) (buffer-name)))
    (let ((current-backlinks-value (org-entry-get nil "BACKLINKS"))
          (src-link (org-link-make-string (concat "id:" src-id) src-title))
          new-value)
      (if current-backlinks-value
          ;; Build a temp list to check we don't add the same link twice.  There
          ;; is an Org builtin `org-entry-add-to-multivalued-property', but we
          ;; cannot use it since the link descriptions may contain spaces.
          (let ((links (split-string (replace-regexp-in-string
                                      "]][[:space:]]+\\[\\["
                                      "]]\f[["
                                      (string-trim current-backlinks-value))
                                     "\f")))
            (dolist (dup (--filter (string-search src-id it) links))
              (setq links (remove dup links)))
            (push src-link links)
            (when (-any-p #'null links)
              (org-node--die "nils in %S" links))
            ;; Enforce deterministic order to prevent unnecessarily reordering
            ;; every time a node is linked that already has the backlink
            (sort links #'string-lessp)
            ;; Two spaces between links help them look distinct
            (setq new-value (string-join links "  ")))
        (setq new-value src-link))
      (unless (equal current-backlinks-value new-value)
        ;; TODO don't inhibit all modification hooks, just
        ;;      inhibit `org-node-backlink--flag-change'
        (let ((inhibit-modification-hooks t))
          (org-entry-put nil "BACKLINKS" new-value))
        ;; (org-node-cache--scan-targeted (list src-file))
        ;; TODO Placeholder to prevent deleting the just-added backlink when
        ;;      saving the "wrong" buffer first Although
        ;;      `org-node-backlink--fix-rescanned-file-buffers' is a better fix
        ;; (push (list :src-id src-id) (gethash target-id org-node--backlinks-by-id))
        ))))

;; (defun org-node-backlink--fix-rescanned-file-buffers (files)
;;   "Designed for `org-node-rescan-hook'.

;; This replaces an older design where
;; `org-node-backlink--fix-changed-parts-of-buffer' ran directly on
;; before-save-hook.  That design sometimes caused backlinks to
;; disappear when the target buffer was saved before the source
;; buffer, because the tables were not up to date.  This instead
;; waits until the tables knows more about the saved buffer."
;;   (dolist (file files)
;;     (let ((buf (find-buffer-visiting file)))
;;       (when buf
;;         (with-current-buffer buf
;;           (org-node-backlink--fix-changed-parts-of-buffer))))))

(provide 'org-node-backlink)

;;; org-node-backlink.el ends here
