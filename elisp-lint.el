;;; elisp-lint.el --- Comprehensive linting for Emacs Lisp libraries -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "25.1") (package-lint "0.18") (checkdoc "0.1"))
;; Keywords: lisp, tools, maint
;; URL: https://github.com/aygp-dr/verbiste.el

;; This file is not part of GNU Emacs.

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

;; This package provides a comprehensive linting setup for Emacs Lisp
;; libraries, bringing together multiple checkers to verify:
;;
;; - Header conventions (checkdoc)
;; - Package metadata (package-lint and flycheck-package)
;; - Code style and formatting
;; - Documentation strings
;; - License headers
;;
;; Usage:
;;
;; 1. Call `elisp-lint-setup' to configure the linting environment
;; 2. Use `elisp-lint-buffer' to check the current buffer
;; 3. Use `elisp-lint-batch-files' from the command line to lint multiple files
;;
;; Command-line usage:
;;   emacs -Q --batch -l elisp-lint.el -f elisp-lint-batch-files FILE1 FILE2 ...

;;; Code:

(require 'checkdoc)
(require 'cl-lib)
(require 'package-lint nil t)  ; Optional dependency
(require 'flycheck nil t)      ; Optional dependency

(defgroup elisp-lint nil
  "Comprehensive linting for Emacs Lisp files."
  :group 'tools
  :prefix "elisp-lint-")

(defcustom elisp-lint-ignored-validators nil
  "List of validators to ignore.
Valid values are symbols: 'package-lint, 'checkdoc, 'byte-compile."
  :type '(repeat (choice (const :tag "Package Lint" package-lint)
                        (const :tag "CheckDoc" checkdoc)
                        (const :tag "Byte Compile" byte-compile)))
  :group 'elisp-lint)

(defcustom elisp-lint-header-requirements
  '(("Package-Requires" . "Required dependencies")
    ("Keywords" . "Search keywords")
    ("Author" . "Author name")
    ("URL" . "Package URL")
    ("Version" . "Package version"))
  "Required headers for Emacs Lisp files.
Each element is a cons cell (HEADER . DESCRIPTION)."
  :type '(repeat (cons (string :tag "Header name")
                       (string :tag "Description")))
  :group 'elisp-lint)

(defcustom elisp-lint-license-requirements
  '("This program is free software"
    "GNU General Public License")
  "Strings that should appear in the license section."
  :type '(repeat string)
  :group 'elisp-lint)

(defun elisp-lint-get-github-user-info ()
  "Get GitHub user information using GitHub CLI if available."
  (when (executable-find "gh")
    (with-temp-buffer
      (when (= 0 (call-process "gh" nil t nil "api" "user"))
        (let ((json-string (buffer-string)))
          (when (and json-string (not (string= json-string "")))
            (let ((json-object-type 'alist)
                  (json-array-type 'list)
                  (json-key-type 'string))
              (json-read-from-string json-string))))))))

(defun elisp-lint-header-check (buffer)
  "Check BUFFER for required headers."
  (with-current-buffer buffer
    (let ((issues '()))
      (save-excursion
        (goto-char (point-min))
        ;; Check first line format
        (unless (looking-at ";;; .+\\.el --- .+ \\(-\\*- lexical-binding: t \\(;\\|-\\*-\\)\\|\\)")
          (push "First line should be of the form: \";;; package.el --- Description\"" issues))
        
        ;; Check for required headers
        (dolist (header elisp-lint-header-requirements)
          (goto-char (point-min))
          (unless (re-search-forward (concat "^;; " (car header) ": ") nil t)
            (push (format "Missing required header: %s (%s)"
                          (car header) (cdr header))
                  issues)))
        
        ;; Check license section
        (goto-char (point-min))
        (let ((license-found t))
          (dolist (license-text elisp-lint-license-requirements)
            (unless (search-forward license-text nil t)
              (setq license-found nil)
              (push (format "Missing license text: %s" license-text) issues)))
          
          (unless license-found
            (push "License section may be incomplete or missing" issues)))
        
        ;; Check Commentary and Code markers
        (goto-char (point-min))
        (unless (re-search-forward "^;;; Commentary:" nil t)
          (push "Missing \";;; Commentary:\" section" issues))
        
        (goto-char (point-min))
        (unless (re-search-forward "^;;; Code:" nil t)
          (push "Missing \";;; Code:\" section" issues))
        
        ;; Check for proper footer
        (goto-char (point-max))
        (unless (re-search-backward ";;; .+\\.el ends here" nil t)
          (push "Missing proper footer: \";;; filename.el ends here\"" issues))
        
        ;; Check for provide statement
        (goto-char (point-min))
        (let ((file-name (file-name-sans-extension
                          (file-name-nondirectory buffer-file-name))))
          (unless (re-search-forward (format "(provide '%s)" file-name) nil t)
            (push (format "Missing (provide '%s) statement" file-name) issues))))
      
      (nreverse issues))))

(defun elisp-lint-run-package-lint (buffer)
  "Run package-lint on BUFFER if available."
  (when (and (fboundp 'package-lint-buffer)
             (not (memq 'package-lint elisp-lint-ignored-validators)))
    (with-current-buffer buffer
      (let ((package-lint-results (package-lint-buffer)))
        (mapcar (lambda (lint-item)
                  (format "package-lint: %s:%d:%d: %s"
                          (buffer-name)
                          (nth 0 lint-item) ; line
                          (nth 1 lint-item) ; column
                          (nth 3 lint-item))) ; message
                package-lint-results)))))

(defun elisp-lint-run-checkdoc (buffer)
  "Run checkdoc on BUFFER."
  (when (not (memq 'checkdoc elisp-lint-ignored-validators))
    (with-current-buffer buffer
      (let ((checkdoc-arguments-in-order-flag nil)
            (checkdoc-verb-check-experimental-flag nil)
            (checkdoc-force-docstrings-flag nil)
            (checkdoc-spellcheck-documentation-flag nil)
            (checkdoc-results '()))
        (save-excursion
          (goto-char (point-min))
          (let ((checkdoc-diagnostic-buffer
                 (get-buffer-create "*Checkdoc Status*")))
            (with-current-buffer checkdoc-diagnostic-buffer
              (erase-buffer))
            (checkdoc-current-buffer t)
            (with-current-buffer checkdoc-diagnostic-buffer
              (goto-char (point-min))
              (while (not (eobp))
                (push (buffer-substring-no-properties
                       (line-beginning-position) (line-end-position))
                      checkdoc-results)
                (forward-line 1)))
            (kill-buffer checkdoc-diagnostic-buffer)))
        (nreverse checkdoc-results)))))

(defun elisp-lint-check-trailing-whitespace (buffer)
  "Check BUFFER for trailing whitespace."
  (with-current-buffer buffer
    (let ((whitespace-issues '()))
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward "[ \t]+$" nil t)
          (push (format "Trailing whitespace at line %d"
                        (line-number-at-pos (point)))
                whitespace-issues)))
      (nreverse whitespace-issues))))

(defun elisp-lint-byte-compile (buffer)
  "Byte compile BUFFER to check for warnings and errors."
  (when (not (memq 'byte-compile elisp-lint-ignored-validators))
    (with-current-buffer buffer
      (let ((byte-compile-results '())
            (byte-compile-dest-file-function (lambda (_) (make-temp-file "elisp-lint" nil ".elc")))
            (file (buffer-file-name))
            (byte-compile-log-buffer "*Compile-Log*"))
        (when file
          (with-current-buffer (get-buffer-create byte-compile-log-buffer)
            (erase-buffer))
          (byte-compile-file file)
          (with-current-buffer byte-compile-log-buffer
            (goto-char (point-min))
            (while (not (eobp))
              (let ((line (buffer-substring-no-properties
                           (line-beginning-position) (line-end-position))))
                (when (string-match "^\\(.+\\):\\([0-9]+\\):\\([0-9]+\\):[ \t]\\(.+\\)" line)
                  (push line byte-compile-results)))
              (forward-line 1)))
          (kill-buffer byte-compile-log-buffer))
        (nreverse byte-compile-results)))))

;;;###autoload
(defun elisp-lint-buffer ()
  "Lint the current buffer.
Runs a series of checks on the buffer and lists the results."
  (interactive)
  (let ((buffer (current-buffer))
        (header-issues (elisp-lint-header-check (current-buffer)))
        (package-lint-issues (elisp-lint-run-package-lint (current-buffer)))
        (checkdoc-issues (elisp-lint-run-checkdoc (current-buffer)))
        (whitespace-issues (elisp-lint-check-trailing-whitespace (current-buffer)))
        (byte-compile-issues (elisp-lint-byte-compile (current-buffer)))
        (result-buffer (get-buffer-create "*Elisp-Lint Results*")))
    
    (with-current-buffer result-buffer
      (erase-buffer)
      (insert "* Elisp Lint Results for " (buffer-name buffer) "\n\n")
      
      (when header-issues
        (insert "** Header Issues\n\n")
        (dolist (issue header-issues)
          (insert "- " issue "\n"))
        (insert "\n"))
      
      (when package-lint-issues
        (insert "** Package Lint Issues\n\n")
        (dolist (issue package-lint-issues)
          (insert "- " issue "\n"))
        (insert "\n"))
      
      (when checkdoc-issues
        (insert "** Checkdoc Issues\n\n")
        (dolist (issue checkdoc-issues)
          (insert "- " issue "\n"))
        (insert "\n"))
      
      (when whitespace-issues
        (insert "** Whitespace Issues\n\n")
        (dolist (issue whitespace-issues)
          (insert "- " issue "\n"))
        (insert "\n"))
      
      (when byte-compile-issues
        (insert "** Byte Compile Issues\n\n")
        (dolist (issue byte-compile-issues)
          (insert "- " issue "\n"))
        (insert "\n"))
      
      (if (and (null header-issues)
               (null package-lint-issues)
               (null checkdoc-issues)
               (null whitespace-issues)
               (null byte-compile-issues))
          (insert "No issues found. ✓\n")
        (insert (format "Found %d issue(s).\n"
                        (+ (length header-issues)
                           (length package-lint-issues)
                           (length checkdoc-issues)
                           (length whitespace-issues)
                           (length byte-compile-issues)))))
      
      (org-mode)
      (goto-char (point-min)))
    
    (display-buffer result-buffer)))

;;;###autoload
(defun elisp-lint-setup ()
  "Set up elisp-lint with flycheck if available."
  (interactive)
  ;; Load flycheck-package if available
  (when (and (fboundp 'flycheck-mode)
             (require 'flycheck-package nil t))
    (eval-after-load 'flycheck
      '(flycheck-package-setup)))
  
  ;; Set up standard checkers
  (when (fboundp 'flycheck-mode)
    (add-hook 'emacs-lisp-mode-hook 'flycheck-mode))
  
  ;; Add our custom key bindings
  (define-key emacs-lisp-mode-map (kbd "C-c C-v") 'elisp-lint-buffer)
  
  (message "Elisp-lint setup complete"))

;;;###autoload
(defun elisp-lint-create-package-template ()
  "Create a template for a new Emacs Lisp package in the current buffer."
  (interactive)
  (let* ((buffer-file-name (or buffer-file-name
                              (read-file-name "Save as: " nil nil nil)))
         (file-name (file-name-nondirectory buffer-file-name))
         (base-name (file-name-sans-extension file-name))
         (gh-user (elisp-lint-get-github-user-info))
         (user-name (if gh-user
                        (or (alist-get "name" gh-user nil nil 'string=)
                            user-full-name)
                      user-full-name))
         (user-email (if gh-user
                         (or (alist-get "email" gh-user nil nil 'string=)
                             user-mail-address)
                       user-mail-address))
         (github-username (if gh-user
                             (alist-get "login" gh-user nil nil 'string=)
                           "yourusername"))
         (year (format-time-string "%Y")))
    
    (erase-buffer)
    (insert (format ";;; %s --- Description -*- lexical-binding: t -*-

;; Copyright (C) %s %s

;; Author: %s <%s>
;; Version: 0.1.0
;; Package-Requires: ((emacs \"25.1\"))
;; Keywords: convenience
;; URL: https://github.com/%s/%s

;; This file is not part of GNU Emacs.

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

;; Description of the package.

;;; Code:

;; Your code here

(provide '%s)
;;; %s ends here
" 
            file-name year user-name user-name user-email
            github-username base-name base-name file-name))
    
    (goto-char (point-min))
    (search-forward "Description")
    (backward-word)
    (set-mark (point))
    (forward-word)
    (delete-region (mark) (point))
    (message "Created package template")))

;;;###autoload
(defun elisp-lint-batch-files ()
  "Run linting on files specified on the command line.
For use in batch mode only."
  (unless noninteractive
    (error "This function can only be used in batch mode"))
  
  (let ((exit-code 0)
        (files command-line-args-left))
    (dolist (file files)
      (princ (format "Checking %s\n" file))
      (let* ((buffer (find-file-noselect file))
             (header-issues (elisp-lint-header-check buffer))
             (package-lint-issues (elisp-lint-run-package-lint buffer))
             (checkdoc-issues (elisp-lint-run-checkdoc buffer))
             (whitespace-issues (elisp-lint-check-trailing-whitespace buffer))
             (byte-compile-issues (elisp-lint-byte-compile buffer))
             (issues (append header-issues package-lint-issues
                             checkdoc-issues whitespace-issues byte-compile-issues)))
        
        (when issues
          (setq exit-code 1)
          (princ "Issues found:\n")
          (dolist (issue issues)
            (princ (format "- %s\n" issue)))
          (princ "\n"))
        
        (kill-buffer buffer)))
    
    (kill-emacs exit-code)))

(provide 'elisp-lint)
;;; elisp-lint.el ends here