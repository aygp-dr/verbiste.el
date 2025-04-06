#!/usr/bin/env bash
# Script to generate text-based screenshots of verbiste.el UI

# Create screenshots directory
mkdir -p screenshots

# Ensure verbiste.el is compiled
echo "Compiling verbiste.el..."
emacs -Q -batch -f batch-byte-compile verbiste.el

# Generate screenshots
echo "Generating screenshots..."

# Define a function to capture buffer content
emacs -Q --eval "
(progn
  (add-to-list 'load-path \"$(pwd)\")
  (require 'verbiste)
  (setq verbiste-use-xml-directly t)
  (verbiste-mode 1)
  
  ;; Function to capture buffer to file
  (defun capture-buffer (buffer-name file-name)
    (with-current-buffer buffer-name
      (write-region (point-min) (point-max) file-name))
    (message \"Captured %s to %s\" buffer-name file-name))
  
  ;; Random verbs browser
  (verbiste-browse-random-verbs)
  (sit-for 1)
  (capture-buffer \"*Verbiste Random Verbs*\" \"screenshots/random-verbs.txt\")
  
  ;; Conjugation
  (verbiste-french-conjugation \"parler\")
  (sit-for 1)
  (capture-buffer \"*Verbiste French Conjugation*\" \"screenshots/conjugation.txt\")
  
  ;; Similar verbs
  (verbiste-display-similar-verbs \"parler\")
  (sit-for 1)
  (capture-buffer \"*Verbiste Similar Verbs*\" \"screenshots/similar-verbs.txt\")
  
  ;; Installation check
  (verbiste-check-installation)
  (sit-for 1)
  (capture-buffer \"*Verbiste Installation Check*\" \"screenshots/installation-check.txt\")
  
  ;; Generate README with screenshots
  (with-temp-buffer
    (insert \"# Verbiste.el UI Screenshots\\n\\n\")
    (insert \"## Random Verbs Browser\\n\\n\")
    (insert \"\\`\\`\\`\\n\")
    (insert-file-contents \"screenshots/random-verbs.txt\")
    (goto-char (point-max))
    (insert \"\\`\\`\\`\\n\\n\")
    
    (insert \"## Verb Conjugation\\n\\n\")
    (insert \"\\`\\`\\`\\n\")
    (insert-file-contents \"screenshots/conjugation.txt\")
    (goto-char (point-max))
    (insert \"\\`\\`\\`\\n\\n\")
    
    (insert \"## Similar Verbs\\n\\n\")
    (insert \"\\`\\`\\`\\n\")
    (insert-file-contents \"screenshots/similar-verbs.txt\")
    (goto-char (point-max))
    (insert \"\\`\\`\\`\\n\\n\")
    
    (insert \"## Installation Check\\n\\n\")
    (insert \"\\`\\`\\`\\n\")
    (insert-file-contents \"screenshots/installation-check.txt\")
    (goto-char (point-max))
    (insert \"\\`\\`\\`\\n\")
    
    (write-region (point-min) (point-max) \"screenshots/README.md\"))
  
  (message \"Screenshots generated in screenshots/ directory\")
  (kill-emacs)
)"

echo "Screenshots generated in the screenshots/ directory"
echo "You can view them with 'cat screenshots/*.txt' or see screenshots/README.md"