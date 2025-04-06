;;; lint-file.el --- Simple lint checker for Emacs Lisp files -*- lexical-binding: t -*-

;; Copyright (C) 2025 Aidan Pace

;; Author: Aidan Pace <apace@defrecord.com>

;;; Commentary:
;;
;; Very simple linter for Emacs Lisp files. Just checks that the file
;; can be byte-compiled without errors.

;;; Code:

(dolist (file command-line-args-left)
  (when (and (file-exists-p file)
             (string-match "\\.el$" file))
    (message "Linting %s..." file)
    (with-current-buffer (find-file-noselect file)
      (emacs-lisp-mode)
      (condition-case err
          (progn
            (byte-compile-file file)
            (message "No errors in %s" file))
        (error
         (message "Error in %s: %s" file (error-message-string err)))))))

(provide 'lint-file)
;;; lint-file.el ends here