;;; screenshot.el --- Generate text screenshots of verbiste.el UI -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;;; Commentary:

;; This script generates text-based screenshots of verbiste.el UI.
;; It's meant to be run from the command line like:
;; emacs --script screenshot.el

;;; Code:

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Add current directory to load path
(add-to-list 'load-path default-directory)

;; Create screenshots directory if it doesn't exist
(unless (file-directory-p "screenshots")
  (make-directory "screenshots"))

;; Load verbiste
(require 'verbiste)

;; Set up environment
(setq verbiste-use-xml-directly t)
(verbiste-mode 1)

;; Capture random verbs UI
(defun capture-buffer-to-file (buffer-name file-name)
  "Capture BUFFER-NAME content to FILE-NAME."
  (with-current-buffer buffer-name
    (write-region (point-min) (point-max) file-name))
  (message "Captured %s to %s" buffer-name file-name))

(defun generate-all-screenshots ()
  "Generate all screenshots."
  (message "Generating screenshots...")
  
  ;; Random verbs browser
  (verbiste-browse-random-verbs)
  (sit-for 1)
  (capture-buffer-to-file "*Verbiste Random Verbs*" "screenshots/random-verbs.txt")
  
  ;; Conjugation
  (verbiste-french-conjugation "parler")
  (sit-for 1)
  (capture-buffer-to-file "*Verbiste French Conjugation*" "screenshots/conjugation.txt")
  
  ;; Similar verbs
  (verbiste-display-similar-verbs "parler")
  (sit-for 1)
  (capture-buffer-to-file "*Verbiste Similar Verbs*" "screenshots/similar-verbs.txt")
  
  ;; Installation check
  (verbiste-check-installation)
  (sit-for 1)
  (capture-buffer-to-file "*Verbiste Installation Check*" "screenshots/installation-check.txt")
  
  ;; Generate README with screenshots
  (generate-readme-with-screenshots)
  
  (message "All screenshots generated successfully!"))

(defun generate-readme-with-screenshots ()
  "Generate a README with screenshots embedded."
  (with-temp-buffer
    (insert "# Verbiste.el UI Screenshots\n\n")
    (insert "## Random Verbs Browser\n\n")
    (insert "```\n")
    (insert-file-contents "screenshots/random-verbs.txt")
    (goto-char (point-max))
    (insert "```\n\n")
    
    (insert "## Verb Conjugation\n\n")
    (insert "```\n")
    (insert-file-contents "screenshots/conjugation.txt")
    (goto-char (point-max))
    (insert "```\n\n")
    
    (insert "## Similar Verbs\n\n")
    (insert "```\n")
    (insert-file-contents "screenshots/similar-verbs.txt")
    (goto-char (point-max))
    (insert "```\n\n")
    
    (insert "## Installation Check\n\n")
    (insert "```\n")
    (insert-file-contents "screenshots/installation-check.txt")
    (goto-char (point-max))
    (insert "```\n")
    
    (write-region (point-min) (point-max) "screenshots/README.md")))

;; Run the screenshot generation
(generate-all-screenshots)

(message "Screenshots generated in the screenshots/ directory")

(kill-emacs 0)

;;; screenshot.el ends here