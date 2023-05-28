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

(require 'org-roam)
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

(defcustom consult-org-roam-buffer-enabled t
  "Boolean to enable/disable consult-org-raom-buffer"
  :type 'boolean
  :group 'consult-org-roam-buffer)

(defcustom consult-org-roam-buffer-after-buffers nil
  "If non-nil, display org-roam buffers right after non-org-roam buffers.
  Otherwise, display org-roam buffers after any other visible default
  source")

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
            (bufname (buffer-name buffer))
            (fhash (consult-org-roam-db--file-hash bufname)))
      (if fhash
        (progn
          ;; Add hash to differentiate between notes with identical
          ;; titles but make it invisible to not disturb the user
          (add-text-properties 0 (length fhash) '(invisible t) fhash)
          (concat title fhash))
        ;; Handle edge cases where the org-roam buffer has not yet
        ;; been written to disk (and DB)
        (if (string-match-p (regexp-quote "CAPTURE") bufname)
          (concat title " [Capture]")
        (concat title " [File not saved]"))))))

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
  "Build a cons consisting of the BUFFER title and the BUFFER name."
  (cons (consult-org-roam-buffer--get-title buffer) buffer))

(defun consult-org-roam--remove-capture-dups (buffer-list)
  "Remove CAPTURE-duplicates from BUFFER-LIST."
  (let ((out-list buffer-list))
    (dolist (x buffer-list)
      (when  (string-match-p (regexp-quote "CAPTURE-") (buffer-name x))
        (setq out-list (delete x buffer-list))
        ))
    out-list))

(defun consult-org-roam--buffer-list-without-dups ()
  "Return a list of all org-roam-buffers without duplicates.
If an org-roam-capture is in progress, there will be duplicate
buffers in the list returned by `org-roam-buffer-list';
therefore, we need to filter the duplicate buffers (prefixed
with 'CAPTURE-') out of this list."
  (consult-org-roam--remove-capture-dups (org-roam-buffer-list)))

(defun consult-org-roam-buffer--update-open-buffer-list (buffer-list)
  "Generate an alist of the form `(TITLE . BUF) from BUFFER-LIST’.
Generate an alist of the form `(TITLE . BUF)’ where TITLE is the
title of an open org-roam buffer."
  (setq org-roam-buffer-open-buffer-list
    (mapcar #'consult-org-roam-buffer--add-title buffer-list)))

(defun consult-org-roam-buffer--with-title (title)
  "Find buffer name with TITLE from among the list of open org-roam buffers."
  (cdr (assoc title org-roam-buffer-open-buffer-list)))

(defun consult-org-roam-buffer--get-roam-bufs ()
  "Return list of currently open org-roam buffers."
  (let ((buffer-list-no-dups (consult-org-roam--buffer-list-without-dups)))
    (consult-org-roam-buffer--update-open-buffer-list buffer-list-no-dups)
    (consult--buffer-query
    :sort 'visibility
    :as #'consult-org-roam-buffer--get-title
    :filter t
    :predicate (lambda (buf)
                   (member buf buffer-list-no-dups)))))

;; Define source for consult-buffer
(defvar org-roam-buffer-source
  `(:name     "Org-roam"
     :hidden   nil
     :narrow   ,consult-org-roam-buffer-narrow-key
     :category org-roam-buffer
     :annotate ,(lambda (cand)
                  (file-name-nondirectory
                    (buffer-name
                      (consult-org-roam-buffer--with-title cand))))
     :state    ,#'consult-org-roam-buffer--state
     :items    ,#'consult-org-roam-buffer--get-roam-bufs))

(defun consult-org-roam-buffer-setup ()
  "Setup consult-org-roam-buffer functionality.
Setup consult-org-roam-buffer functionality by adding
org-roam-buffer-source to consult-buffer-sources and customizing
consult--source-buffer."
  ;; Remove org-roam-buffer-source to avoid duplicate
  (consult-org-roam-buffer-teardown)
  (consult-org-roam-buffer--customize-source-buffer t)
  (if consult-org-roam-buffer-after-buffers
    (let* ((idx (cl-position 'consult--source-buffer
                  consult-buffer-sources :test 'equal))
            (tail (nthcdr idx consult-buffer-sources)))
      (setcdr
        (nthcdr (1- idx) consult-buffer-sources)
        (append (list 'org-roam-buffer-source) tail)))
    (add-to-list 'consult-buffer-sources 'org-roam-buffer-source 'append)))

(defun consult-org-roam-buffer-teardown ()
  "Remove org-roam-buffer-source from consult-buffer-sources."
  (setq consult-buffer-sources
    (delete 'org-roam-buffer-source consult-buffer-sources))
  (consult-org-roam-buffer--customize-source-buffer nil))

(defun consult-org-roam-buffer--customize-source-buffer (is-remove)
  "Customize consult--source-buffer to show or hide depending on IS-REMOVE.
Customize consult--source-buffer to either show or hide
org-roam-buffers in regular consult-buffer section, which is
controlled by IS-REMOVE.."
  (if is-remove
    (consult-customize
      consult--source-buffer
      :items (lambda ()
               (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :predicate (lambda (buf) (not (org-roam-buffer-p buf))))))
    (consult-customize
      consult--source-buffer
      :items (lambda ()
               (consult--buffer-query
                 :sort 'visibility
                 :as #'buffer-name
                 :predicate nil
                 )))))

(provide 'consult-org-roam-buffer)
;;; consult-org-roam-buffer.el ends here
