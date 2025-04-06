;;; verbiste.el --- Interface to Verbiste French/Italian verb conjugation -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>
;; Version: 0.1
;; Package-Requires: ((emacs "25.1"))
;; Keywords: languages, french, italian, conjugation
;; URL: https://github.com/aygp-dr/verbiste.el

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides an Emacs interface to the Verbiste
;; French verb conjugation library, with Italian support planned for future releases.
;;
;; Features:
;;  - Conjugate French verbs in all tenses and moods
;;  - Find infinitive forms of conjugated French verbs
;;  - Check Verbiste installation status
;;  - Convenient minor mode with keybindings
;;
;; Basic usage:
;;
;;   (require 'verbiste)
;;
;;   ;; Optional: enable the minor mode globally
;;   (verbiste-mode 1)
;;
;;   ;; Commands
;;   M-x verbiste-french-conjugation
;;   M-x verbiste-french-deconjugation
;;   M-x verbiste-check-installation
;;
;; Prerequisites:
;;  - The Verbiste package must be installed on your system
;;  - French conjugator and deconjugator executables must be available
;;  - XML data files should be accessible at the configured location

;;; Code:

(require 'cl-lib)
(require 'xml)

;; Customization

(defgroup verbiste nil
  "Interface to Verbiste French/Italian verb conjugation."
  :group 'languages
  :prefix "verbiste-")

(defcustom verbiste-data-dir "/usr/local/share/verbiste-0.1"
  "Directory where Verbiste XML files are located."
  :type 'directory
  :group 'verbiste)

(defcustom verbiste-french-conjugator-path "french-conjugator"
  "Path to the French conjugator executable."
  :type 'string
  :group 'verbiste)

(defcustom verbiste-french-deconjugator-path "french-deconjugator"
  "Path to the French deconjugator executable."
  :type 'string
  :group 'verbiste)

(defcustom verbiste-use-xml-directly nil
  "If non-nil, parse XML directly instead of using command-line tools.
This is more efficient but requires the XML files to be accessible."
  :type 'boolean
  :group 'verbiste)

;; Internal variables

(defvar verbiste--french-verbs-cache nil
  "Cache of French verbs and their templates.")

(defvar verbiste--italian-verbs-cache nil
  "Cache of Italian verbs and their templates.")

(defvar verbiste--french-templates-cache nil
  "Cache of French conjugation templates.")

(defvar verbiste--italian-templates-cache nil
  "Cache of Italian conjugation templates.")

;; Helper functions

(defun verbiste--ensure-executable (program)
  "Check if PROGRAM executable exists."
  (or (executable-find program)
      (progn
        (message "Cannot find %s. Please install verbiste." program)
        nil)))

(defun verbiste--shell-command-to-string (command)
  "Execute shell COMMAND and return its output as a string."
  (with-temp-buffer
    (call-process-shell-command command nil t)
    (buffer-string)))

(defun verbiste--load-xml-file (filename)
  "Load XML from FILENAME."
  (let ((file (expand-file-name filename verbiste-data-dir)))
    (if (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (xml-parse-region (point-min) (point-max)))
      (error "Cannot read file: %s" file))))

;; Main Functions for Command-line Interface

(defun verbiste-french-conjugation (verb)
  "Conjugate the French VERB."
  (interactive "sFrench verb: ")
  (if (not (verbiste--ensure-executable verbiste-french-conjugator-path))
      (message "French conjugator not found")
    (let ((output (verbiste--shell-command-to-string
                   (format "%s %s" verbiste-french-conjugator-path (shell-quote-argument verb)))))
      (with-current-buffer (get-buffer-create "*Verbiste French Conjugation*")
        (erase-buffer)
        (insert (format "Conjugation of French verb: %s\n\n" verb))
        (insert output)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

(defun verbiste-french-deconjugation (verb)
  "Find infinitive form of conjugated French VERB."
  (interactive "sConjugated French verb: ")
  (if (not (verbiste--ensure-executable verbiste-french-deconjugator-path))
      (message "French deconjugator not found")
    (let ((output (verbiste--shell-command-to-string
                   (format "%s %s" verbiste-french-deconjugator-path (shell-quote-argument verb)))))
      (with-current-buffer (get-buffer-create "*Verbiste French Deconjugation*")
        (erase-buffer)
        (insert (format "Deconjugation of French verb: %s\n\n" verb))
        (insert output)
        (goto-char (point-min))
        (display-buffer (current-buffer))))))

;; TODO: Add Italian verb functions when they become available

;; XML parsing implementation (for future direct parsing instead of using command-line tools)

(defun verbiste--load-french-verbs ()
  "Load French verbs and templates from XML."
  (unless verbiste--french-verbs-cache
    (let* ((xml (verbiste--load-xml-file "verbs-fr.xml"))
           (verbs (xml-get-children (car xml) 'v)))
      (setq verbiste--french-verbs-cache
            (mapcar (lambda (v)
                      (cons (xml-get-attribute v 'i)
                            (xml-get-attribute v 't)))
                    verbs))))
  verbiste--french-verbs-cache)

;; Future implementation for direct XML parsing

;; Keymap

(defvar verbiste-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c v f c") 'verbiste-french-conjugation)
    (define-key map (kbd "C-c v f d") 'verbiste-french-deconjugation)
    ;; TODO: Add Italian keybindings when implemented
    map)
  "Keymap for Verbiste mode.")

;; Mode definition (for potential future use)

(define-minor-mode verbiste-mode
  "Minor mode for Verbiste verb conjugation."
  :lighter " Verb"
  :keymap verbiste-mode-map
  :global t)

;; Installation check

(defun verbiste-check-installation ()
  "Check if verbiste is properly installed."
  (interactive)
  (let ((fr-conj (executable-find verbiste-french-conjugator-path))
        (fr-deconj (executable-find verbiste-french-deconjugator-path))
        (fr-verbs (file-readable-p (expand-file-name "verbs-fr.xml" verbiste-data-dir)))
        (fr-conj-xml (file-readable-p (expand-file-name "conjugation-fr.xml" verbiste-data-dir))))
    (with-current-buffer (get-buffer-create "*Verbiste Installation Check*")
      (erase-buffer)
      (insert "Verbiste Installation Check\n\n")
      (insert (format "French conjugator: %s\n" (if fr-conj "Found" "Not found")))
      (insert (format "French deconjugator: %s\n" (if fr-deconj "Found" "Not found")))
      (insert (format "French verbs XML: %s\n" (if fr-verbs "Found" "Not found")))
      (insert (format "French conjugation XML: %s\n" (if fr-conj-xml "Found" "Not found")))
      (insert "\nOverall status: ")
      (if (and fr-conj fr-deconj fr-verbs fr-conj-xml)
          (insert "Ready for use\n")
        (insert "Installation incomplete\n"))
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

(provide 'verbiste)
;;; verbiste.el ends here