;;; verbiste-ui-improvements.el --- UI improvements for verbiste.el -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>
;; Keywords: languages, french, italian, conjugation

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This file provides UI improvements for verbiste.el, fixing issues with
;; button highlighting, improving read-only buffer feedback, and enhancing
;; the random verbs browsing experience.

;;; Code:

(require 'verbiste)

;; Customization

(defcustom verbiste-random-verbs-count 10
  "Number of random verbs to display in browse-random-verbs."
  :type 'integer
  :group 'verbiste)

(defcustom verbiste-display-mode-line-indicator t
  "Whether to display a mode line indicator for verbiste buffers."
  :type 'boolean
  :group 'verbiste)

;; Improved button creation

(defun verbiste--make-button (text action &optional help-echo)
  "Create a button with TEXT that triggers ACTION when clicked.
HELP-ECHO is optional tooltip text."
  (let ((start (point)))
    (insert text)
    (make-button start (point)
                 'action action
                 'help-echo (or help-echo (format "Click to %s" text))
                 'follow-link t)))

(defun verbiste--make-verb-button (verb action &optional text help-echo)
  "Create a button for VERB that triggers ACTION.
Optional TEXT overrides the button label.
Optional HELP-ECHO provides tooltip text."
  (verbiste--make-button (or text verb)
                        (lambda (_) (funcall action verb))
                        help-echo))

;; Improved buffer setup

(defun verbiste--setup-ui-buffer (title)
  "Set up a verbiste UI buffer with TITLE.
Returns the buffer."
  (let ((buffer-name (format "*Verbiste %s*" title)))
    (with-current-buffer (get-buffer-create buffer-name)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (propertize (concat title "\n") 'face 'bold))
        (insert (propertize
                 "Navigate with TAB or mouse. This is a read-only buffer.\n\n"
                 'face 'font-lock-comment-face)))
      
      ;; Mode line indicator
      (when verbiste-display-mode-line-indicator
        (setq mode-line-format 
              (append mode-line-format
                      (list (propertize " [Verbiste]" 'face 'bold)))))
      
      (special-mode)
      (current-buffer))))

;; Enhanced random verbs function

(defun verbiste-browse-random-verbs-improved (&optional count)
  "Display a buffer with random verbs that can be explored interactively.
COUNT specifies the number of verbs to show (defaults to verbiste-random-verbs-count)."
  (interactive (list verbiste-random-verbs-count))
  (let* ((verb-count (or count verbiste-random-verbs-count))
         (all-verbs (verbiste--get-all-french-verbs))
         (total-count (length all-verbs))
         (verbs (verbiste--get-random-verbs verb-count)))
    (if verbs
        (with-current-buffer (verbiste--setup-ui-buffer "Random Verbs")
          (insert (format "Showing %d of %d total French verbs\n\n" 
                         (min verb-count total-count) total-count))
          
          ;; Add buttons for each verb
          (dolist (verb verbs)
            (insert "- ")
            ;; Make the verb itself clickable
            (verbiste--make-verb-button verb #'verbiste-display-similar-verbs
                                       nil "Click to see similar verbs")
            (insert " ")
            ;; Add conjugation button
            (verbiste--make-verb-button verb #'verbiste-french-conjugation
                                       "[conjugate]" "Click to conjugate this verb")
            (insert "\n"))
          
          ;; Add refresh and show more buttons
          (insert "\n")
          (verbiste--make-button "[Refresh with new random verbs]"
                                (lambda (_) (verbiste-browse-random-verbs-improved)))
          
          (insert "  ")
          (verbiste--make-button "[Show more verbs]"
                                (lambda (_) 
                                  (verbiste-browse-random-verbs-improved 
                                   (* 2 verb-count))))
          
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "No verb clusters data available"))))

;; Function to get all French verbs
(defun verbiste--get-all-french-verbs ()
  "Get list of all French verbs from the dictionary."
  (let ((verbs nil)
        (clusters (verbiste--load-verb-clusters)))
    (if clusters
        (setq verbs (mapcar (lambda (entry)
                             (let ((verb (car entry)))
                               (if (symbolp verb) (symbol-name verb) verb)))
                           clusters))
      ;; Fallback: try to load from XML if clusters not available
      (unless verbs
        (when verbiste--french-verbs-cache
          (setq verbs (mapcar #'car verbiste--french-verbs-cache)))
        (unless verbs
          (when (file-readable-p (expand-file-name "french_verbs_list.txt" verbiste-data-dir))
            (with-temp-buffer
              (insert-file-contents (expand-file-name "french_verbs_list.txt" verbiste-data-dir))
              (setq verbs (split-string (buffer-string) "\n" t)))))))
    verbs))

;; Run tests to verify UI improvements

(defun verbiste-test-ui-improvements ()
  "Test the UI improvements."
  (interactive)
  ;; Run both versions for comparison
  (verbiste-browse-random-verbs)
  (verbiste-browse-random-verbs-improved)
  (message "UI tests completed. Compare the two buffers to see improvements."))

(provide 'verbiste-ui-improvements)
;;; verbiste-ui-improvements.el ends here