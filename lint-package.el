;;; lint-package.el --- Simple linter for package.el packages -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>

;;; Commentary:

;; Simple linter script for package.el packages.

;;; Code:
(require 'package)

(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'package-lint)
  (package-refresh-contents)
  (package-install 'package-lint))

(require 'package-lint)

;; Process command line arguments, skipping options like -l/--load
(let ((files '())
      (args command-line-args)
      (errors-found nil))
  
  ;; Skip program name
  (setq args (cdr args))
  
  ;; Parse args, skipping options
  (while args
    (let ((arg (car args)))
      (if (string-match "^-" arg)
          (setq args (if (member arg '("-l" "--load"))
                         (cddr args)
                       (cdr args)))
        (push arg files)
        (setq args (cdr args)))))
  
  ;; Reverse to get original order
  (setq files (nreverse files))
  
  ;; Process each file
  (dolist (file files)
    (when (and (file-exists-p file)
               (string-match-p "\\.el$" file))
      (message "Checking %s..." file)
      (with-current-buffer (find-file-noselect file)
        (emacs-lisp-mode)
        ;; Run package-lint
        (condition-case err
            (let ((issues (package-lint-current-buffer)))
              (if (and issues (> (length issues) 0))
                  (progn
                    (message "Issues found in %s:" file)
                    (dolist (issue issues)
                      (message "  Line %d: %s" 
                               (or (nth 0 issue) 0)
                               (or (nth 3 issue) "Unknown issue")))
                    (setq errors-found t))
                (message "No issues found in %s" file)))
          (error (message "Error checking %s: %s" file (error-message-string err)))))))
  
  ;; Exit with status code
  (kill-emacs (if errors-found 1 0)))

;;; lint-package.el ends here