;;; consult-org-roam-buffer.el --- Consult-buffer integration for org-roam  -*- lexical-binding: t; -*-
;; Copyright (C) 2022 jgru

;; Author: apc <https://github.com/apc> and jgru <https://github.com/jgru>
;; Created: October 7th, 2022
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Version: 0.1
;; Homepage: https://github.com/jgru/consult-org-roam
;; Package-Requires: ((emacs "27.1") (org-roam "2.2.0") (consult "0.16"))

;;; Commentary:

;; This is a set of functions to add a custom source to consult-buffer
;; for org-roam notes.

;;; Code:

;; ============================================================================
;;;; Dependencies
;; ============================================================================

(if (locate-library "org-roam")
    (require 'org-roam)
  (error "Org-roam not found!"))
(require 'consult)

;; ============================================================================
;;;; Customize definitions
;; ============================================================================

(defgroup consult-org-roam-buffer nil
  "Consult buffer source for org-roam."
  :group 'org
  :group 'convenience
  :prefix "consult-org-roam-buffer-")

(defcustom consult-org-roam-buffer-narrow-key ?n
  "Narrow key for consult-buffer"
  :type 'key
  :group 'consult-org-roam-buffer)

;; ============================================================================
;;;; Functions
;; ============================================================================

(defun consult-org-roam-buffer--state ()
  (let ((preview (consult--buffer-preview)))
    (lambda
      (action cand)
      (funcall preview action
        (consult-org-roam-buffer--with-title cand))
      (when
        (and cand
          (eq action 'return))
        (consult--buffer-action
          (consult-org-roam-buffer--with-title cand))))))

(defun consult-org-roam-buffer--get-title (buffer)
  "Get title of org-roam BUFFER."
  (if (org-roam-buffer-p buffer)
    (let* ((title
             (with-current-buffer buffer
               (org-roam-db--file-title)))
            (filename (buffer-file-name buffer))
            (fhash (consult-org-roam-db--file-hash filename)))
      (if fhash
        (concat title " [" (substring fhash 0 7) "]")
        (concat title " [not persisted]")))))

(defun consult-org-roam-db--file-hash (fname)
  "Retrieve the hash of FNAME from org-roam's db "
  (let* ((fhash (org-roam-db-query
                  [:select [hash]
                    :from files
                    :where (like file $s1)
                    ]
                  (concat "%" fname))))
    (car (car fhash))))

(defun consult-org-roam-buffer--add-title (buffer)
  "Build a cons consisting of the BUFFER title and the BUFFER name"
  (cons (consult-org-roam-buffer--get-title buffer) buffer))

(defun consult-org-roam-buffer--update-open-buffer-list ()
  "Generate an alist of the form `(TITLE . BUF)’, where TITLE is the title of an open org-roam buffer"
  (setq org-roam-buffer-open-buffer-list
    (mapcar #'consult-org-roam-buffer--add-title
      (org-roam-buffer-list))))

(defun consult-org-roam-buffer--with-title (title)
  "Find buffer name with TITLE from among the list of open org-roam buffers"
  (consult-org-roam-buffer--update-open-buffer-list)
  (cdr (assoc title org-roam-buffer-open-buffer-list)))

(autoload 'org-roam-buffer-list "org-roam")
(add-to-list 'consult-buffer-sources 'org-roam-buffer-source 'append)

(defun consult-org-roam-buffer--get-roam-bufs ()
  "Return list of currently open org-roam buffers"
  (consult--buffer-query
    :sort 'visibility
    :as #'consult-org-roam-buffer--get-title
    :filter t
    :predicate (lambda (buf) (org-roam-buffer-p buf))))

(defvar org-roam-buffer-source
  `(:name     "Org-roam"
     :hidden   nil
     :narrow   ,consult-org-roam-buffer-narrow-key
     :category org-roam-buffer
     :annotate ,(lambda (cand)
                  (f-filename
                    (buffer-file-name
                      (consult-org-roam-buffer--with-title cand))))
     :state    ,#'consult-org-roam-buffer--state
     :items    ,#'consult-org-roam-buffer--get-roam-bufs))

(add-to-list 'consult-buffer-sources 'org-roam-buffer-source 'append)

;; Customize consult--source-buffer to show org-roam buffers only in
;; their dedicated section
(consult-customize
  consult--source-buffer
  :items (lambda ()
           (consult--buffer-query
             :sort 'visibility
             :as #'buffer-name
             :predicate (lambda (buf) (not (org-roam-buffer-p buf))))))

;;; consult-org-roam-buffer.el ends here