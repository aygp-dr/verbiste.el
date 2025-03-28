;;; ob-verbiste.el --- Org Babel functions for verbiste evaluation -*- lexical-binding: t -*-

;; Copyright (C) 2025 Jason Walsh

;; Author: Jason Walsh <your.email@example.com>
;; Keywords: languages, tools
;; Homepage: https://github.com/username/verbiste.el
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (org "9.5"))

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Provides org-babel integration for verbiste, allowing execution of
;; verbiste code blocks in org files with babel.
;;
;; This enables:
;; - French verb conjugation in org-mode documents
;; - Direct generation of conjugation tables
;; - Tangling to .vb files
;; - Detangling from .vb files
;;
;; Add the following to your init file:
;;
;; (with-eval-after-load 'org
;;   (require 'ob-verbiste)
;;   (add-to-list 'org-babel-load-languages '(verbiste . t)))

;;; Code:
(require 'ob)
(require 'verbiste)

(defvar org-babel-default-header-args:verbiste
  '((:results . "table")
    (:exports . "both"))
  "Default header arguments for verbiste code blocks.")

(defvar org-babel-tangle-lang-exts '(("verbiste" . "vb"))
  "File extension for verbiste tangled files.")

(defun org-babel-execute:verbiste (body params)
  "Execute a block of verbiste code with org-babel.
This function is called by `org-babel-execute-src-block'.

BODY contains the verbiste code (verb) to conjugate.
PARAMS contains the parameters specified in the org-babel header.

Returns a formatted conjugation table."
  (let* ((verb (string-trim body))
         (tenses (cdr (assq :tenses params)))
         (format (or (cdr (assq :format params)) "table"))
         (template (cdr (assq :template params)))
         (info-only (cdr (assq :info params)))
         (lang (or (cdr (assq :language params)) "fr"))
         (result-params (cdr (assq :result-params params)))
         (conjugation-data nil))
    
    ;; Generate the conjugation data
    (if template
        ;; Use template parameter to conjugate a custom verb
        (setq conjugation-data (verbiste-conjugate-with-template verb template))
      ;; Use the standard conjugation function
      (setq conjugation-data (verbiste-conjugate verb)))
    
    ;; Filter by requested tenses if specified
    (when tenses
      (let ((tense-list (split-string tenses ",")))
        (setq conjugation-data 
              (verbiste--filter-tenses conjugation-data tense-list))))
    
    ;; Format the results based on the format parameter and requested output
    (cond
     (info-only
      (verbiste--format-verb-info verb conjugation-data))
     ((string= format "compact")
      (verbiste--format-compact-table verb conjugation-data))
     (t
      (verbiste--format-full-table verb conjugation-data)))))

(defun verbiste--filter-tenses (conjugation-data tense-list)
  "Filter CONJUGATION-DATA to include only tenses in TENSE-LIST."
  (let ((filtered-data '())
        (tense-mode-map '(("indicatif présent" . (indicative present))
                          ("indicatif imparfait" . (indicative imperfect))
                          ("indicatif futur" . (indicative future))
                          ("indicatif passé simple" . (indicative simple-past))
                          ("subjonctif présent" . (subjunctive present))
                          ("subjonctif imparfait" . (subjunctive imperfect))
                          ("conditionnel présent" . (conditional present))
                          ("impératif présent" . (imperative present))
                          ("participe présent" . (participle present))
                          ("participe passé" . (participle past)))))
    
    ;; Loop through requested tenses and find matching data
    (dolist (tense-str (mapcar #'string-trim tense-list))
      (let* ((tense-key (assoc-string tense-str tense-mode-map t))
             (mode (caar (cdr tense-key)))
             (tense (cadar (cdr tense-key))))
        (when tense-key
          (let ((tense-data (verbiste--get-tense-data conjugation-data mode tense)))
            (when tense-data
              (push tense-data filtered-data))))))
    
    (nreverse filtered-data)))

(defun verbiste--get-tense-data (conjugation-data mode tense)
  "Extract specific MODE and TENSE data from CONJUGATION-DATA."
  ;; This function would extract the specific tense data from the conjugation data
  ;; Simplified version for demonstration
  (list mode tense (format "Data for %s %s" mode tense)))

(defun verbiste--format-verb-info (verb data)
  "Format verb information for VERB with DATA."
  '(("Property" "Value")
    hline
    ("Infinitive" "venir")
    ("Template" "ven:ir")
    ("Group" "3rd group")
    ("Irregular" "Yes")
    ("Has \"être\" aux" "Yes")))

(defun verbiste--format-compact-table (verb data)
  "Format VERB with DATA as a compact org table."
  ;; Simplified for demonstration
  '(("Mode" "Tense" "je" "tu" "il/elle" "nous" "vous" "ils/elles")
    hline
    ("Indicatif" "Présent" "vais" "vas" "va" "allons" "allez" "vont")))

(defun verbiste--format-full-table (verb data)
  "Format VERB with DATA as a full org table."
  ;; Simplified for demonstration
  '(("Mode" "Tense" "je" "tu" "il/elle" "nous" "vous" "ils/elles")
    hline
    ("Indicatif" "Présent" "parle" "parles" "parle" "parlons" "parlez" "parlent")
    ("Indicatif" "Imparfait" "parlais" "parlais" "parlait" "parlions" "parliez" "parlaient")))

;; Tangling and detangling support
(defun org-babel-tangle-verbiste (verb params file)
  "Write VERB code block defined by PARAMS to FILE."
  (let ((base-name (file-name-sans-extension file))
        (verb-name (string-trim verb))
        (template (cdr (assq :template params)))
        (tenses (cdr (assq :tenses params)))
        (format (cdr (assq :format params))))
    
    ;; Create the .vb file content
    (with-temp-buffer
      (insert (format "# Verb: %s\n" verb-name))
      (when template
        (insert (format "# Template: %s\n" template)))
      (insert "# Group: 1\n")
      (insert "#\n")
      (insert "# This file was generated from verbiste.org\n")
      (insert "# Edit and detangle to update the source\n\n")
      (insert verb-name)
      (insert "\n\n# Options:\n")
      (when tenses
        (insert (format "# :tenses \"%s\"\n" tenses)))
      (when format
        (insert (format "# :format \"%s\"\n" format)))
      
      ;; Write to file
      (write-region (point-min) (point-max) file))
    
    ;; Return the file path for confirmation
    file))

(defun org-babel-detangle-verbiste (file)
  "Detangle a verbiste .vb FILE back to its org source."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    
    ;; Extract verb name and options
    (let ((verb nil)
          (options '()))
      
      ;; Find the verb name (first non-comment, non-blank line)
      (while (and (not verb) (not (eobp)))
        (forward-line)
        (beginning-of-line)
        (if (looking-at "^\\([^#\n].*\\)$")
            (setq verb (match-string 1))))
      
      ;; Find options (lines starting with "# :")
      (goto-char (point-min))
      (while (re-search-forward "^# :\\(.*\\)" nil t)
        (push (match-string 1) options))
      
      ;; Return the extracted data
      (list verb (nreverse options)))))

;; Register verbiste as an org-babel language
(add-to-list 'org-babel-tangle-lang-exts '("verbiste" . "vb"))

;; Create a structure template for verbiste blocks
(with-eval-after-load 'org-tempo
  (add-to-list 'org-structure-template-alist '("v" . "src verbiste")))

;; Navigation functions for verbiste blocks
(defun ob-verbiste-next-block ()
  "Move to the next verbiste code block."
  (interactive)
  (org-babel-next-src-block nil)
  (while (and (not (eobp))
              (not (string= (org-element-property :language (org-element-at-point)) "verbiste")))
    (org-babel-next-src-block nil)))

(defun ob-verbiste-prev-block ()
  "Move to the previous verbiste code block."
  (interactive)
  (org-babel-previous-src-block nil)
  (while (and (not (bobp))
              (not (string= (org-element-property :language (org-element-at-point)) "verbiste")))
    (org-babel-previous-src-block nil)))

(defun ob-verbiste-list-blocks ()
  "List all verbiste blocks in the current buffer."
  (interactive)
  (let ((blocks '())
        (current-point (point)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^[ \t]*#\\+begin_src[ \t]+verbiste" nil t)
        (let* ((element (org-element-at-point))
               (begin (org-element-property :begin element))
               (value (org-element-property :value element)))
          (push (list begin (string-trim value)) blocks))))
    
    (if blocks
        (let* ((formatted (mapcar (lambda (b) 
                                    (format "%d: %s" (car b) (cadr b))) 
                                  (nreverse blocks)))
               (selection (completing-read "Jump to verb: " formatted nil t)))
          (when selection
            (goto-char (car (nth (string-to-number (car (split-string selection ":"))) blocks)))))
      (message "No verbiste blocks found"))))

;; Key bindings for verbiste navigation
(org-defkey org-babel-map (kbd "v n") 'ob-verbiste-next-block)
(org-defkey org-babel-map (kbd "v p") 'ob-verbiste-prev-block)
(org-defkey org-babel-map (kbd "v l") 'ob-verbiste-list-blocks)
(org-defkey org-babel-map (kbd "v c") 'verbiste)

(provide 'ob-verbiste)
;;; ob-verbiste.el ends here
