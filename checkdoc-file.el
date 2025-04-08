;;; checkdoc-file.el --- Run checkdoc on Emacs Lisp files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>

;;; Commentary:

;; Simple script to run checkdoc on Emacs Lisp files.

;;; Code:
(require 'checkdoc)

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
      (message "Checking documentation in %s..." file)
      (with-current-buffer (find-file-noselect file)
        (emacs-lisp-mode)
        ;; Run checkdoc
        (condition-case err
            (let ((checkdoc-diagnostic-buffer "*Checkdoc Status*"))
              (let ((found-errors (checkdoc-current-buffer t)))
                (message "Documentation check complete for %s" file)
                (when found-errors
                  (setq errors-found t))))
          (error (message "Error checking %s: %s" file (error-message-string err)))))))
  
  ;; Exit with status code
  (kill-emacs (if errors-found 1 0)))

;;; checkdoc-file.el ends here