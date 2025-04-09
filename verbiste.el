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
  (or
   ;; First try: data/ in the package directory
   (let ((pkg-dir (and load-file-name (file-name-directory load-file-name))))
     (when pkg-dir
       (let ((data-dir (expand-file-name "data" pkg-dir)))
         (when (file-directory-p data-dir)
           data-dir))))
   
   ;; Second try: data/ in current directory
   (let ((data-dir (expand-file-name "data" default-directory)))
     (when (file-directory-p data-dir)
       data-dir))
   
   ;; Last resort: Use the system verbiste directory
   "/usr/local/share/verbiste-0.1")
  "Directory where Verbiste XML files are located.
Automatically detects the location of data files, with fallbacks."
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
  (or
   ;; Try in verbiste-data-dir
   (let ((clusters-path (expand-file-name "french_verb_clusters.json" verbiste-data-dir)))
     (when (file-exists-p clusters-path)
       clusters-path))
   
   ;; Try in current directory
   (let ((local-path (expand-file-name "french_verb_clusters.json" default-directory)))
     (when (file-exists-p local-path)
       local-path))
   
   ;; Default fallback (will be used if file doesn't exist)
   (expand-file-name "french_verb_clusters.json" verbiste-data-dir))
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
  "Load XML from FILENAME.
Tries to find the file in verbiste-data-dir, current directory, 
or predefined sample files."
  (let* ((file-in-data-dir (expand-file-name filename verbiste-data-dir))
         (file-in-current-dir (expand-file-name filename default-directory))
         (sample-file (expand-file-name 
                      (concat (file-name-sans-extension filename) "-sample.xml")
                      default-directory))
         (file (cond
                ((file-readable-p file-in-data-dir) file-in-data-dir)
                ((file-readable-p file-in-current-dir) file-in-current-dir)
                ((file-readable-p sample-file) sample-file)
                (t nil))))
    (if file
        (with-temp-buffer
          (insert-file-contents file)
          (message "Loaded XML file: %s" file)
          (xml-parse-region (point-min) (point-max)))
      (error "Cannot find readable XML file: %s (tried %s, %s, and %s)"
             filename file-in-data-dir file-in-current-dir sample-file))))

;; Main Functions for Command-line Interface

(defun verbiste-french-conjugation (verb)
  "Conjugate the French VERB and show results with navigation buttons."
  (interactive "sFrench verb: ")
  (if (not (and (not verbiste-use-xml-directly)
                (not (verbiste--ensure-executable verbiste-french-conjugator-path))))
      (let ((output (if verbiste-use-xml-directly
                      (verbiste--get-conjugation-from-xml verb)
                    (verbiste--shell-command-to-string
                     (format "%s %s" verbiste-french-conjugator-path (shell-quote-argument verb))))))
        (with-current-buffer (get-buffer-create "*Verbiste French Conjugation*")
          (erase-buffer)
          (insert (format "Conjugation of French verb: %s\n\n" verb))
          (insert output)
          
          ;; Add navigation buttons
          (goto-char (point-min))
          (end-of-line)
          (insert " ")
          (insert "[similar verbs]")
          (make-button (- (point) 15) (point)
                       'action (lambda (_)
                                 (verbiste-display-similar-verbs verb))
                       'help-echo "View similar verbs"
                       'follow-link t)
          
          (insert " [random verbs]")
          (make-button (- (point) 14) (point)
                       'action (lambda (_)
                                 (verbiste-browse-random-verbs))
                       'help-echo "Browse random verbs"
                       'follow-link t)
          
          (goto-char (point-min))
          ;; Make buffer read-only but allow button clicks
          (special-mode)
          (display-buffer (current-buffer))))
    (message "French conjugator not found, enable direct XML parsing with verbiste-use-xml-directly")))

;; Function to get conjugation from XML when command-line tools are not available
(defun verbiste--get-conjugation-from-xml (verb)
  "Get conjugation of VERB directly from XML files.
Returns a formatted string with the conjugation tables."
  ;; This is a placeholder - in a real implementation, we would:
  ;; 1. Load and parse the XML files
  ;; 2. Find the verb's template
  ;; 3. Format the conjugation in a readable way
  
  ;; For now, we'll just return a simple message
  (format "Direct XML parsing not fully implemented yet.\nUse command-line tools for complete conjugation tables.\n\nVerb: %s\nLanguage: French" verb))

(defun verbiste-french-deconjugation (verb)
  "Find infinitive form of conjugated French VERB.
Adds interactive navigation buttons for the result."
  (interactive "sConjugated French verb: ")
  (if (not (and (not verbiste-use-xml-directly)
                (not (verbiste--ensure-executable verbiste-french-deconjugator-path))))
    (let ((output (if verbiste-use-xml-directly
                      (verbiste--get-deconjugation-from-xml verb)
                    (verbiste--shell-command-to-string
                     (format "%s %s" verbiste-french-deconjugator-path (shell-quote-argument verb))))))
      (with-current-buffer (get-buffer-create "*Verbiste French Deconjugation*")
        (erase-buffer)
        (insert (format "Deconjugation of French verb: %s\n\n" verb))
        (insert output)
        
        ;; Try to extract the infinitive(s) from the output
        (goto-char (point-min))
        (let ((infinitives nil))
          (while (re-search-forward "infinitive: \\(.*\\)$" nil t)
            (push (match-string 1) infinitives))
          
          ;; Add navigation buttons for each found infinitive
          (when infinitives
            (goto-char (point-max))
            (insert "\n\nActions:\n")
            (dolist (inf infinitives)
              (insert (format "- %s: " inf))
              (insert "[conjugate]")
              (make-button (- (point) 11) (point)
                           'action (lambda (_)
                                     (verbiste-french-conjugation inf))
                           'help-echo "Conjugate this verb"
                           'follow-link t)
              
              (insert " [similar verbs]")
              (make-button (- (point) 15) (point)
                           'action (lambda (_)
                                     (verbiste-display-similar-verbs inf))
                           'help-echo "View similar verbs"
                           'follow-link t)
              (insert "\n"))))
        
        ;; Add random verbs button
        (goto-char (point-max))
        (insert "\n[Browse random verbs]")
        (make-button (- (point) 19) (point)
                     'action (lambda (_)
                               (verbiste-browse-random-verbs))
                     'help-echo "Browse random verbs"
                     'follow-link t)
        
        (goto-char (point-min))
        (special-mode) ;; Make buffer read-only with navigation keys
        (display-buffer (current-buffer))))
    (message "French deconjugator not found, enable direct XML parsing with verbiste-use-xml-directly")))

;; Function to get deconjugation from XML when command-line tools are not available
(defun verbiste--get-deconjugation-from-xml (verb)
  "Get possible infinitive forms of VERB directly from XML files.
Returns a formatted string with the deconjugation results."
  ;; This is a placeholder - in a real implementation, we would:
  ;; 1. Load and parse the XML files
  ;; 2. Search through all possible conjugations to find matches
  ;; 3. Return the infinitive forms
  
  ;; For now, we'll just return a simple message
  (format "Direct XML parsing not fully implemented yet.\nUse command-line tools for complete deconjugation.\n\nConjugated verb: %s\nLanguage: French" verb))

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
        (progn
          (message "Loading clusters from %s..." verbiste-clusters-file)
          (condition-case err
              (with-temp-buffer
                (insert-file-contents verbiste-clusters-file)
                (message "File loaded, parsing JSON...")
                (let ((json-object-type 'alist)
                      (json-array-type 'list))
                  (setq verbiste--verb-clusters-cache (json-read))
                  (message "JSON parsed successfully. Found %d verb entries."
                           (length verbiste--verb-clusters-cache))))
            (error 
             (message "Error parsing clusters file: %s" (error-message-string err))
             nil)))
      (message "Cannot read verb clusters file: %s" verbiste-clusters-file)))
  verbiste--verb-clusters-cache)

(defun verbiste--get-similar-verbs (verb)
  "Get list of verbs similar to VERB from clusters data."
  (let ((clusters (verbiste--load-verb-clusters)))
    (when clusters
      (let* ((verb-symbol (intern verb))
             (verb-entry (assoc verb-symbol clusters))
             (similar-verbs (when verb-entry (cdr verb-entry))))
        (when similar-verbs
          (mapcar (lambda (item)
                    (cons (cdr (assoc 'verb item))
                          (cdr (assoc 'similarity item))))
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
  "Display verbs similar to VERB in a buffer.
Allows clicking on verbs to navigate the similarity graph."
  (interactive "sFrench verb: ")
  (let ((similar-verbs (verbiste--get-similar-verbs verb)))
    (if similar-verbs
        (with-current-buffer (get-buffer-create "*Verbiste Similar Verbs*")
          (erase-buffer)
          (insert (format "Verbs similar to '%s':\n\n" verb))
          
          ;; Add [conjugate] button for the main verb
          (insert "[conjugate]")
          (make-button (- (point) 11) (point)
                       'action (lambda (_)
                                 (verbiste-french-conjugation verb))
                       'help-echo "Click to conjugate this verb"
                       'follow-link t)
          (insert "\n\n")
          
          ;; Add clickable similar verbs with conjugation buttons
          (dolist (v similar-verbs)
            (let ((sim-verb (car v))
                  (sim-score (cdr v)))
              ;; Make the verb itself clickable
              (let ((start (point)))
                (insert (format "%-20s" sim-verb))
                (make-button start (- (point) (- 20 (length sim-verb)))
                             'action (lambda (_)
                                       (verbiste-display-similar-verbs sim-verb))
                             'help-echo "Click to see similar verbs"
                             'follow-link t))
              
              ;; Add similarity score
              (insert (format "(similarity: %.4f) " sim-score))
              
              ;; Add conjugation button
              (insert "[conjugate]")
              (make-button (- (point) 11) (point)
                           'action (lambda (_)
                                     (verbiste-french-conjugation sim-verb))
                           'help-echo "Click to conjugate this verb"
                           'follow-link t)
              (insert "\n")))
          
          ;; Add back link to browse random verbs
          (insert "\n[Browse random verbs]")
          (make-button (- (point) 19) (point)
                       'action (lambda (_)
                                 (verbiste-browse-random-verbs))
                       'help-echo "Click to browse random verbs"
                       'follow-link t)
          
          (goto-char (point-min))
          (special-mode) ;; Make buffer read-only with navigation keys
          (display-buffer (current-buffer)))
      (message "No similar verbs found for '%s'" verb))))

(defun verbiste-browse-random-verbs ()
  "Display a buffer with random verbs that can be explored interactively.
Clicking on verbs navigates to the similar verbs view, allowing for
graph-like exploration of verb relationships."
  (interactive)
  (let ((verbs (verbiste--get-random-verbs 10)))
    (if verbs
        (with-current-buffer (get-buffer-create "*Verbiste Random Verbs*")
          (erase-buffer)
          (insert "Random French Verbs\n\n")
          (insert "Click on a verb to see similar verbs and navigate the similarity graph.\n")
          (insert "Press TAB to navigate between buttons, ENTER to activate them.\n\n")
          
          ;; Add buttons for each verb
          (dolist (verb verbs)
            (insert "- ")
            ;; Make the verb itself clickable
            (let ((start (point)))
              (insert verb)
              (make-button start (point)
                           'action (lambda (_)
                                     (verbiste-display-similar-verbs verb))
                           'help-echo "Click to see similar verbs"
                           'follow-link t))
            
            ;; Add conjugation button
            (insert " [conjugate]")
            (make-button (- (point) 11) (point)
                         'action (lambda (_)
                                   (verbiste-french-conjugation verb))
                         'help-echo "Click to conjugate this verb"
                         'follow-link t)
            (insert "\n"))
          
          ;; Add refresh button
          (insert "\n[Refresh with new random verbs]")
          (make-button (- (point) 28) (point)
                       'action (lambda (_)
                                 (verbiste-browse-random-verbs))
                       'help-echo "Click to get new random verbs"
                       'follow-link t)
          
          (goto-char (point-min))
          (special-mode) ;; Make buffer read-only with navigation keys
          (local-set-key (kbd "<tab>") 'forward-button)
          (local-set-key (kbd "<backtab>") 'backward-button)
          (display-buffer (current-buffer)))
      (message "No verb clusters data available"))))

;; Installation check

(defun verbiste-check-installation ()
  "Check if verbiste is properly installed."
  (interactive)
  (let* ((fr-conj (executable-find verbiste-french-conjugator-path))
         (fr-deconj (executable-find verbiste-french-deconjugator-path))
         (fr-verbs-path (expand-file-name "verbs-fr.xml" verbiste-data-dir))
         (fr-conj-path (expand-file-name "conjugation-fr.xml" verbiste-data-dir))
         (fr-verbs (file-readable-p fr-verbs-path))
         (fr-conj-xml (file-readable-p fr-conj-path))
         (clusters-path verbiste-clusters-file)
         (clusters (file-readable-p clusters-path))
         (use-local-files verbiste-use-xml-directly))
    (with-current-buffer (get-buffer-create "*Verbiste Installation Check*")
      (erase-buffer)
      (insert "Verbiste Installation Check\n\n")
      
      ;; Directory information
      (insert "Directory Information:\n")
      (insert (format "  verbiste-data-dir: %s\n" verbiste-data-dir))
      (insert (format "  Current directory: %s\n" default-directory))
      (insert (format "  Package directory: %s\n\n" 
                     (or (and load-file-name (file-name-directory load-file-name)) "unknown")))
      
      ;; Check for command-line tools (only needed if not using XML directly)
      (unless use-local-files
        (insert "Command-line tools:\n")
        (insert (format "  French conjugator: %s\n" (if fr-conj "Found" "Not found")))
        (insert (format "  French deconjugator: %s\n\n" (if fr-deconj "Found" "Not found"))))
      
      ;; Always check for XML files
      (insert "Data files:\n")
      (insert (format "  French verbs XML: %s (%s)\n" 
                     (if fr-verbs "Found" "Not found") fr-verbs-path))
      (insert (format "  French conjugation XML: %s (%s)\n" 
                     (if fr-conj-xml "Found" "Not found") fr-conj-path))
      (insert (format "  Verb clusters data: %s (%s)\n\n" 
                     (if clusters "Found" "Not found") clusters-path))
      
      ;; Try fallback paths
      (unless (and fr-verbs fr-conj-xml clusters)
        (insert "Checking alternative locations:\n")
        (dolist (file '(("verbs-fr.xml" . "French verbs XML")
                        ("conjugation-fr.xml" . "French conjugation XML")
                        ("french_verb_clusters.json" . "Verb clusters data")))
          (let* ((filename (car file))
                 (desc (cdr file))
                 (in-current-dir (expand-file-name filename default-directory))
                 (sample-file (expand-file-name 
                              (concat (file-name-sans-extension filename) "-sample.xml")
                              default-directory)))
            (insert (format "  %s:\n" desc))
            (insert (format "    In current dir: %s (%s)\n" 
                           (if (file-readable-p in-current-dir) "Found" "Not found")
                           in-current-dir))
            (when (string-match-p "\\.xml$" filename)
              (insert (format "    Sample file: %s (%s)\n" 
                             (if (file-readable-p sample-file) "Found" "Not found")
                             sample-file)))))
        (insert "\n"))
      
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
  
  ;; Setup debug message
  (message "Verbiste initializing...")
  (message "  verbiste-data-dir: %s" verbiste-data-dir)
  (message "  Current dir: %s" default-directory)
  (let ((pkg-dir (and load-file-name (file-name-directory load-file-name))))
    (message "  Package dir: %s" (or pkg-dir "unknown")))
  
  ;; Check clusters file existence
  (message "  Clusters file: %s (%s)" 
           verbiste-clusters-file
           (if (file-readable-p verbiste-clusters-file) "readable" "not readable"))
  
  ;; Load verb clusters if available
  (when (file-readable-p verbiste-clusters-file)
    (message "Loading verb clusters...")
    (verbiste--load-verb-clusters)
    (message "Verb clusters loaded."))
  
  ;; Check XML files existence
  (let ((fr-verbs-path (expand-file-name "verbs-fr.xml" verbiste-data-dir))
        (fr-conj-path (expand-file-name "conjugation-fr.xml" verbiste-data-dir)))
    (message "  French verbs XML: %s (%s)"
             fr-verbs-path
             (if (file-readable-p fr-verbs-path) "readable" "not readable"))
    (message "  French conjugation XML: %s (%s)"
             fr-conj-path
             (if (file-readable-p fr-conj-path) "readable" "not readable"))
    
    ;; If local XML files are available but command-line tools aren't,
    ;; use direct XML parsing by default
    (let ((fr-verbs (file-readable-p fr-verbs-path))
          (fr-conj-xml (file-readable-p fr-conj-path))
          (fr-conj (executable-find verbiste-french-conjugator-path))
          (fr-deconj (executable-find verbiste-french-deconjugator-path)))
      
      ;; Auto-enable direct XML parsing if needed
      (when (and fr-verbs fr-conj-xml (not (and fr-conj fr-deconj)))
        (setq verbiste-use-xml-directly t)
        (message "  Auto-enabling direct XML parsing."))
      
      (message "Verbiste initialized. XML direct parsing: %s" 
               (if verbiste-use-xml-directly "enabled" "disabled")))))

;; Initialize when loaded
(eval-after-load 'verbiste '(verbiste--init))

(provide 'verbiste)
;;; verbiste.el ends here