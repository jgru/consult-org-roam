;;; org-roam-consult.el --- Consult integration for org-roam  -*- lexical-binding: t; -*-

;;; Commentary:
;; Copyright (C) 2022 jgru

;;; Code:

(require 'org-roam)
(require 'consult)

(defcustom org-roam-consult-grep-func #'consult-ripgrep
  "Function for searching files."
   :type 'function)

(defun org-roam-consult-search (&optional initial)
  "Search org-roam directory using consult-ripgrep with live-preview.
With an option for INITIAL input when called non-interactively."
  (interactive)
  (if initial
      (funcall org-roam-consult-grep-func org-roam-directory (format "%s" initial))
    (funcall org-roam-consult-grep-func org-roam-directory)))

(defun org-roam-consult--select-file (&optional prompt list)
  "Wrapper around `consult--read' to select an org-roam file.
Offers candidates withing `org-roam-directory', or from LIST when
supplied. Can take a PROMPT argument."
  (let* ((files (if list list
                  (org-roam-list-files)))
         (prompt (if prompt prompt
                 "Select File: ")))
     (consult--read
      files
      :prompt prompt
      :sort t
      :require-match t
      :state (consult--file-preview))))

(defun org-roam-consult--ids-to-files (ids)
  "Take a bunch of IDS of org-roam-nodes and convert those into file paths."
  (mapcar #'(lambda (id)
              (org-roam-node-file (org-roam-node-from-id (car id))))
         ids))

(defun org-roam-consult-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((node (org-roam-node-at-point))
         (ids (org-roam-db-query
            [:select [source]
                     :from links
                     :where (= dest $s1)
                     :and (= type "id")]
            (org-roam-node-id (org-roam-node-at-point)))))
    (if ids
        (find-file (org-roam-consult--select-file "Backlinks: " (org-roam-consult--ids-to-files ids)))
      (user-error "No backlinks found"))))

(defun org-roam-consult-file-find ()
  "Find org-roam node with preview."
  (interactive "")
  (find-file (org-roam-consult--select-file "Node: ")))

;; Completing-read interface using consult
;; Override `org-roam-node-read' so that each an every completing function resorts to consult
;; https://github.com/org-roam/org-roam/blob/f50d6e7376b3ba603fb4df1d95204ca5f3cc3ca8/org-roam-node.el#L510
(defun org-roam-node-read (&optional initial-input filter-fn sort-fn require-match prompt)
  "Read and return an `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match.
PROMPT is a string to show at the beginning of the mini-buffer, defaulting to \"Node: \""
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn))
         (prompt (or prompt "Node: "))
         (node
          (consult--read
              nodes
              :prompt prompt
              :initial initial-input
              :predicate filter-fn
              :sort sort-fn
              :require-match t
              :category 'org-roam-node
              :annotate (lambda (title)
                              (funcall org-roam-node-annotation-function
                                       (get-text-property 0 'node title)))
              :history org-roam-node-history
              :state (org-roam-consult--node-preview)
              :preview-key 'any)))
    (or (cdr (assoc node nodes))
        (org-roam-node-create :title node))))

;; Minimally adapted from https://github.com/minad/consult/blob/8547e336142e74449b59a6b018e3c96a2b205fd2/consult.el#L1103
(defun org-roam-consult--temporary-nodes ()
  "Return a function to open nodes temporarily."
  (let* ((new-buffers)
         (old-buffers (buffer-list))
         (dir default-directory))
    (lambda (&optional node)
      (if node
          (let ((default-directory dir)
                (inhibit-message t)
                (enable-dir-local-variables nil)
                (enable-local-variables (and enable-local-variables :safe))
                (non-essential t)
                (name
                 (org-roam-node-file (get-text-property 0 'node node))))
            (or
             ;; get-file-buffer is only a small optimization here. It
             ;; may not find the actual buffer, for directories it
             ;; returns nil instead of returning the Dired buffer.
             (get-file-buffer name)
             ;; file-attributes may throw permission denied error
             (when-let* ((attrs (ignore-errors (file-attributes name)))
                         (size (file-attribute-size attrs)))
               (if (> size consult-preview-max-size)
                      (prog1 nil
                        (message "File `%s' (%s) is too large for preview"
                                 name (file-size-human-readable size)))
                 (cl-letf* (((default-value 'find-file-hook)
                             (seq-remove (lambda (x)
                                           (memq x consult-preview-excluded-hooks))
                                         (default-value 'find-file-hook)))
                            (buf (find-file-noselect
                                  name 'nowarn
                                  (> size consult-preview-raw-size))))
                   ;; Only add new buffer if not already in the list
                   (unless (or (memq buf new-buffers) (memq buf old-buffers))
                     (push buf new-buffers)
                     ;; Only keep a few buffers alive
                     (while (> (length new-buffers) consult-preview-max-count)
                       (consult--kill-clean-buffer (car (last new-buffers)))
                       (setq new-buffers (nbutlast new-buffers))))
                   buf)))))
        (mapc #'consult--kill-clean-buffer new-buffers)))))

;; Taken from https://github.com/minad/consult/blob/8547e336142e74449b59a6b018e3c96a2b205fd2/consult.el#L3265
;; and adapted to use `org-roam-consult--temporary-nodes'
(defun org-roam-consult--node-preview ()
  "Create preview function for nodes."
  (let ((open (org-roam-consult--temporary-nodes))
        (preview (consult--buffer-preview)))
    (lambda (cand restore)
      (if restore
          (progn
            (funcall preview nil t)
            (funcall open))
        (funcall preview (and cand (funcall open cand)) nil)))))

(provide 'org-roam-consult)
;;; org-roam-consult.el ends here
