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
;;  - Browse random verbs with interactive UI
;;  - Explore similar verbs based on embedding clusters
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
;;   M-x verbiste-display-similar-verbs
;;   M-x verbiste-browse-random-verbs
;;
;; Prerequisites:
;;  - The Verbiste package must be installed on your system
;;  - French conjugator and deconjugator executables must be available
;;  - XML data files should be accessible at the configured location

;;; Code:

(require 'cl-lib)
(require 'xml)
(require 'json)

;; Customization

(defgroup verbiste nil
  "Interface to Verbiste French/Italian verb conjugation."
  :group 'languages
  :prefix "verbiste-")

(defcustom verbiste-data-dir 
  (expand-file-name "data" 
                   (file-name-directory (or load-file-name buffer-file-name)))
  "Directory where Verbiste XML files are located.
Defaults to the `data' subdirectory in the verbiste.el package directory."
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

(defvar verbiste--verb-clusters-cache nil
  "Cache of French verb clusters loaded from JSON.")

(defcustom verbiste-clusters-file
  (expand-file-name "french_verb_clusters.json" verbiste-data-dir)
  "File containing French verb clusters data."
  :type 'file
  :group 'verbiste)

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
    (define-key map (kbd "C-c v s") 'verbiste-display-similar-verbs)
    (define-key map (kbd "C-c v r") 'verbiste-browse-random-verbs)
    ;; TODO: Add Italian keybindings when implemented
    map)
  "Keymap for Verbiste mode.")

;; Mode definition (for potential future use)

(define-minor-mode verbiste-mode
  "Minor mode for Verbiste verb conjugation."
  :lighter " Verb"
  :keymap verbiste-mode-map
  :global t)

;; Verb clusters

(defun verbiste--load-verb-clusters ()
  "Load French verb clusters from JSON file."
  (unless verbiste--verb-clusters-cache
    (if (file-readable-p verbiste-clusters-file)
        (with-temp-buffer
          (insert-file-contents verbiste-clusters-file)
          (setq verbiste--verb-clusters-cache (json-read)))
      (message "Cannot read verb clusters file: %s" verbiste-clusters-file)))
  verbiste--verb-clusters-cache)

(defun verbiste--get-similar-verbs (verb)
  "Get list of verbs similar to VERB from clusters data."
  (let ((clusters (verbiste--load-verb-clusters)))
    (when clusters
      (let ((similar-verbs (assoc-default verb clusters)))
        (when similar-verbs
          (mapcar (lambda (item)
                    (cons (assoc-default 'verb item)
                          (assoc-default 'similarity item)))
                  similar-verbs))))))

(defun verbiste--get-random-verbs (count)
  "Get COUNT random verbs from the clusters data."
  (let ((clusters (verbiste--load-verb-clusters)))
    (when clusters
      (let ((verb-list (mapcar #'car clusters)))
        (if (<= (length verb-list) count)
            verb-list
          (cl-loop repeat count
                   for random-index = (random (length verb-list))
                   for verb = (nth random-index verb-list)
                   collect verb))))))

(defun verbiste-display-similar-verbs (verb)
  "Display verbs similar to VERB in a buffer."
  (interactive "sFrench verb: ")
  (let ((similar-verbs (verbiste--get-similar-verbs verb)))
    (if similar-verbs
        (with-current-buffer (get-buffer-create "*Verbiste Similar Verbs*")
          (erase-buffer)
          (insert (format "Verbs similar to '%s':\n\n" verb))
          (dolist (v similar-verbs)
            (insert (format "%-20s (similarity: %.4f)\n" (car v) (cdr v))))
          (goto-char (point-min))
          (display-buffer (current-buffer)))
      (message "No similar verbs found for '%s'" verb))))

(defun verbiste-browse-random-verbs ()
  "Display a buffer with random verbs that can be explored."
  (interactive)
  (let ((verbs (verbiste--get-random-verbs 10)))
    (if verbs
        (with-current-buffer (get-buffer-create "*Verbiste Random Verbs*")
          (erase-buffer)
          (insert "Random French Verbs\n\n")
          (dolist (verb verbs)
            (insert (format "- %s\n" verb)))
          (insert "\nClick on a verb to see similar verbs, or to conjugate it.\n")
          (goto-char (point-min))
          
          ;; Add buttons for each verb
          (save-excursion
            (goto-char (point-min))
            (forward-line 2) ;; Skip header
            (dolist (verb verbs)
              (beginning-of-line)
              (forward-char 2) ;; Skip "- "
              (let ((start (point)))
                (forward-word)
                (make-button start (point)
                             'action (lambda (_)
                                       (verbiste-display-similar-verbs verb))
                             'help-echo "Click to see similar verbs"
                             'follow-link t))
              (insert " [conjugate]")
              (make-button (- (point) 12) (point)
                           'action (lambda (_)
                                     (verbiste-french-conjugation verb))
                           'help-echo "Click to conjugate this verb"
                           'follow-link t)
              (forward-line 1)))
          
          (special-mode) ;; Make buffer read-only with navigation keys
          (display-buffer (current-buffer)))
      (message "No verb clusters data available"))))

;; Installation check

(defun verbiste-check-installation ()
  "Check if verbiste is properly installed."
  (interactive)
  (let ((fr-conj (executable-find verbiste-french-conjugator-path))
        (fr-deconj (executable-find verbiste-french-deconjugator-path))
        (fr-verbs (file-readable-p (expand-file-name "verbs-fr.xml" verbiste-data-dir)))
        (fr-conj-xml (file-readable-p (expand-file-name "conjugation-fr.xml" verbiste-data-dir)))
        (clusters (file-readable-p verbiste-clusters-file))
        (use-local-files verbiste-use-xml-directly))
    (with-current-buffer (get-buffer-create "*Verbiste Installation Check*")
      (erase-buffer)
      (insert "Verbiste Installation Check\n\n")
      
      ;; Check for command-line tools (only needed if not using XML directly)
      (unless use-local-files
        (insert "Command-line tools:\n")
        (insert (format "  French conjugator: %s\n" (if fr-conj "Found" "Not found")))
        (insert (format "  French deconjugator: %s\n\n" (if fr-deconj "Found" "Not found"))))
      
      ;; Always check for XML files
      (insert "Data files:\n")
      (insert (format "  French verbs XML: %s\n" (if fr-verbs "Found" "Not found")))
      (insert (format "  French conjugation XML: %s\n" (if fr-conj-xml "Found" "Not found")))
      (insert (format "  Verb clusters data: %s\n\n" (if clusters "Found" "Not found")))
      
      ;; Evaluate overall status
      (insert "Mode configuration:\n")
      (insert (format "  XML direct parsing: %s\n\n" (if use-local-files "Enabled" "Disabled")))
      
      (insert "Overall status: ")
      (if (or 
           ;; If using direct XML parsing
           (and use-local-files fr-verbs fr-conj-xml)
           ;; If using command-line tools
           (and (not use-local-files) fr-conj fr-deconj))
          (insert "Ready for use\n")
        (insert "Installation incomplete\n"))
      
      ;; Add usage suggestion if XML files are available but tools aren't
      (when (and (not use-local-files) fr-verbs fr-conj-xml (not (and fr-conj fr-deconj)))
        (insert "\nSuggestion: Enable direct XML parsing with (setq verbiste-use-xml-directly t)\n"))
        
      (goto-char (point-min))
      (display-buffer (current-buffer)))))

;; Initialize the package
(defun verbiste--init ()
  "Initialize verbiste package."
  ;; Load verb clusters if available
  (when (file-readable-p verbiste-clusters-file)
    (verbiste--load-verb-clusters))
  
  ;; If local XML files are available but command-line tools aren't,
  ;; use direct XML parsing by default
  (let ((fr-verbs (file-readable-p (expand-file-name "verbs-fr.xml" verbiste-data-dir)))
        (fr-conj-xml (file-readable-p (expand-file-name "conjugation-fr.xml" verbiste-data-dir)))
        (fr-conj (executable-find verbiste-french-conjugator-path))
        (fr-deconj (executable-find verbiste-french-deconjugator-path)))
    (when (and fr-verbs fr-conj-xml (not (and fr-conj fr-deconj)))
      (setq verbiste-use-xml-directly t))))

;; Initialize when loaded
(eval-after-load 'verbiste '(verbiste--init))

(provide 'verbiste)
;;; verbiste.el ends here