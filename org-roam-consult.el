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
  "Take a bunch of IDS of org-roam and convert those into file paths."
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

(provide 'org-roam-consult)
;;; org-roam-consult.el ends here
