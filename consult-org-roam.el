;;; consult-org-roam.el --- Consult integration for org-roam  -*- lexical-binding: t; -*-
;; Copyright (C) 2022 jgru

;; Author: jgru <https://github.com/jgru>
;; Created: March 3rd, 2022
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/jgru/consult-org-roam
;; Package-Requires: ((emacs "27.1") (org-roam "2.2.0") (consult "0.16"))

;;; Commentary:

;; This is a set of functions to use org-roam with consult.
;; This packages replaces org-roam's own completing read functions
;; with equivalent versions utilizing consult's internal API. By doing so,
;; one gains all advantages of consult which enhances Emacs' own
;; completing-read funcionality.

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(require 'org-roam)
(require 'consult)

;; ============================================================================
;;;; Customize definitions
;; ============================================================================

(defgroup consult-org-roam nil
  "Consult interface for org-roam."
  :group 'org
  :group 'convenience
  :prefix "consult-org-roam-")

(defcustom consult-org-roam-grep-func #'consult-ripgrep
  "Function for searching files."
   :type 'function
   :group 'consult-org-roam)

(defcustom consult-org-roam-no-preview-functions
  '()
  "List of functions for which previews should not be rendered."
  :group 'consult-org-roam
  :type '(repeat function))

;; ============================================================================
;;;; Functions
;; ============================================================================

(defun consult-org-roam-search (&optional initial)
  "Search org-roam directory using `consult-ripgrep' with live-preview.
With an option for INITIAL input when called non-interactively."
  (interactive)
  (if initial
      (funcall consult-org-roam-grep-func org-roam-directory (format "%s" initial))
    (funcall consult-org-roam-grep-func org-roam-directory)))

(defun consult-org-roam--select-file (&optional prompt list)
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
     :preview-key (consult-org-roam--preview-functions)
     :state (consult--file-preview))))

(defun consult-org-roam--ids-to-files (ids)
  "Take a bunch of IDS of org-roam-nodes and convert those into file paths."
  (mapcar (lambda (id)
              (org-roam-node-file (org-roam-node-from-id (car id))))
         ids))

;;;###autoload
(defun consult-org-roam-backlinks ()
  "Select from list of all notes that link to the current note."
  (interactive)
  (let* ((node (org-roam-node-at-point))
         (ids (org-roam-db-query
            [:select [source]
                     :from links
                     :where (= dest $s1)
                     :and (= type "id")]
            (if node
                (org-roam-node-id (org-roam-node-at-point))
              (user-error "Buffer does not contain org-roam-nodes")))))
    (if ids
        (find-file (consult-org-roam--select-file "Backlinks: " (consult-org-roam--ids-to-files ids)))
      (user-error "No backlinks found"))))

;;;###autoload
(defun consult-org-roam-forward-links ()
  "Select a forward link contained in the current buffer."
  (interactive)
  (let ((id-links nil))
    (org-element-map (org-element-parse-buffer) 'link
  (lambda (link)
    (when (string= (org-element-property :type link) "id")
      ;; Use add-to-list to avoid duplicates
      (add-to-list 'id-links
                   ;; wrap each link in a list to be conformant
                   ;; with the format expected by consult-org-roam--ids-to-files
                   (list (org-element-property :path link))))))
    (if id-links
        (find-file (consult-org-roam--select-file "Links: " (consult-org-roam--ids-to-files id-links)))
      (user-error "No forward links found"))))

;;;###autoload
(defun consult-org-roam-file-find ()
  "Find org-roam node with preview."
  (interactive "")
  (find-file (consult-org-roam--select-file "Node: ")))

;; Completing-read interface using consult. Override
;; `org-roam-node-read' so that each an every completing function
;; resorts to consult
(defun consult-org-roam-node-read (&optional initial-input filter-fn sort-fn
                                     require-match prompt)
  "Read and return an `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial minibuffer prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
SORT-FN is a function to sort nodes. See `org-roam-node-read-sort-by-file-mtime'
for an example sort function.
If REQUIRE-MATCH, the minibuffer prompt will require a match.
PROMPT is a string to show at the beginning of the mini-buffer,
defaulting to \"Node: \""
  (let* ((nodes (org-roam-node-read--completions filter-fn sort-fn))
         (prompt (or prompt "Node: "))
         (node (consult--read
                nodes
                :prompt prompt
                :initial initial-input
                :predicate filter-fn
                :sort sort-fn
                :require-match require-match
                :category 'org-roam-node
                ;;:history 'org-roam-node-history
                :annotate (lambda (title)
                                (funcall org-roam-node-annotation-function
                                         (get-text-property 0 'node title)))
                :state (consult-org-roam--node-preview)
                :preview-key (consult-org-roam--preview-functions)
                ;; uses the DEFAULT argument of alist-get to return input in case the input is not found as key.
                :lookup (lambda (_ candidates input)(alist-get input candidates input nil #'equal)))))
    (if (org-roam-node-p node) (progn node)
        (progn (org-roam-node-create :title node)))))

(defun consult-org-roam--temporary-nodes ()
  "Return a function to open nodes temporarily."
  (let* ((new-buffers)
         (old-buffers (buffer-list))
         (dir default-directory))
    (lambda (&optional node)
      (if node
          ;; Preview only, when it is an org-roam node
          (when (org-roam-node-p node)
            (let ((default-directory dir)
                  (inhibit-message t)
                  (enable-dir-local-variables nil)
                  (enable-local-variables (and enable-local-variables :safe))
                  (non-essential t)
                  (name (org-roam-node-file node)))
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
                     buf))))))
        ;; clean buffers, even if it is not an org-roam-node
        (mapc #'consult--kill-clean-buffer new-buffers)))))

(defun consult-org-roam--node-preview ()
  "Create preview function for nodes."
  (let ((open (consult-org-roam--temporary-nodes))
        (preview (consult--buffer-preview)))
    (lambda (cand restore)
      (if restore
          (progn
            (funcall preview nil t)
            (funcall open))
        (funcall preview (and cand (funcall open cand)) nil)))))

;; Completing-read interface for references using consult. Override
;; `org-roam-ref-read' so that each an every completing function
;; regarding refs resorts to consult
(defun consult-org-roam-ref-read (&optional initial-input filter-fn)
  "Read a ref and return its `org-roam-node' with the help of consult.
INITIAL-INPUT is the initial prompt value.
FILTER-FN is a function to filter out nodes: it takes an `org-roam-node',
and when nil is returned the node will be filtered out.
filtered out."
  (let* ((refs (org-roam-ref-read--completions))
         (refs (cl-remove-if-not (lambda (n)
                                   (if filter-fn (funcall filter-fn (cdr n)) t)) refs))
         (ref (consult--read
                refs
                :prompt "Refs: "
                :initial initial-input
                :predicate filter-fn
                :require-match t
                :category 'org-roam-ref
                :history 'org-roam-ref-history
                :annotate (lambda (r) (funcall org-roam-ref-annotation-function r))
                :state (consult-org-roam--node-preview)
                :preview-key (consult-org-roam--preview-functions)
                :lookup #'consult--lookup-cdr)))
    (progn ref)))

(defun consult-org-roam--preview-functions ()
  "Check wether the calling function should be previewd or not."
  (when (not (member this-command consult-org-roam-no-preview-functions))
    consult-preview-key))

(make-variable-buffer-local
  (defvar consult-org-roam-mode nil
    "Toggle consult-org-roam-mode to integrate consult in org-roam."))

(add-to-list 'minor-mode-alist '(consult-org-roam " consult-org-roam"))

;;;###autoload
(defun consult-org-roam-mode (&optional ARG)
  "Toggle `consult-org-roam-mode' to integrate consult with org-roam.
By enabling `consult-org-roam-mode' the functions `org-roam-node-read' and
`org-roam-ref-read' are overriden by consults-org-roam's equivalents. Optional
argument ARG indicates whether the mode should be enabled or disabled."
  (interactive (list 'toggle))
  (setq consult-org-roam-mode
        (if (eq ARG 'toggle)
            (not consult-org-roam-mode)
          (> ARG 0)))

  ;; Take some action when enabled or disabled
  (if consult-org-roam-mode
      (progn
        (advice-add #'org-roam-node-read :override #'consult-org-roam-node-read)
        (advice-add #'org-roam-ref-read :override #'consult-org-roam-ref-read)
        (message "Consult integration for org-roam enabled"))
    (progn
      (advice-remove #'org-roam-node-read #'consult-org-roam-node-read)
      (advice-remove #'org-roam-ref-read #'consult-org-roam-ref-read)
      (message "Consult integration for org-roam disabled"))))

(provide 'consult-org-roam)
;;; consult-org-roam.el ends here
