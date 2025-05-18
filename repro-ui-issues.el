;; Script to reproduce verbiste.el UI issues
;; Run with: emacs -nw --batch -l repro-ui-issues.el

(require 'ert)

;; Load verbiste.el
(add-to-list 'load-path default-directory)
(require 'verbiste)

;; Enable debug messages
(setq debug-on-error t)

(defun capture-buffer-state (buffer-name)
  "Capture the state of BUFFER-NAME, including button positions and text properties."
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (let ((content (buffer-string))
            (button-positions '())
            (property-regions '()))
        
        ;; Collect button positions
        (goto-char (point-min))
        (while (setq next-button (next-button (point)))
          (let ((start (button-start next-button))
                (end (button-end next-button))
                (label (buffer-substring-no-properties 
                        (button-start next-button) 
                        (button-end next-button))))
            (push (list start end label) button-positions)
            (goto-char (button-end next-button))))
        
        ;; Collect face property regions
        (goto-char (point-min))
        (let ((pos (point-min))
              (limit (point-max))
              last-face current-face region-start)
          (while (< pos limit)
            (setq current-face (get-text-property pos 'face))
            (if (eq current-face last-face)
                (setq pos (1+ pos))
              ;; Face changed
              (when (and last-face region-start)
                (push (list region-start (1- pos) last-face) property-regions))
              (setq last-face current-face
                    region-start pos
                    pos (1+ pos))))
          ;; Add the last region
          (when (and last-face region-start)
            (push (list region-start pos last-face) property-regions)))
        
        ;; Return collected data
        (list :content content
              :buttons (nreverse button-positions)
              :faces (nreverse property-regions))))))

(defun log-state-to-file (state filename)
  "Log captured buffer STATE to FILENAME."
  (with-temp-file filename
    (insert "Buffer Content:\n")
    (insert "===============\n")
    (insert (plist-get state :content))
    (insert "\n\nButtons:\n")
    (insert "========\n")
    (dolist (button (plist-get state :buttons))
      (insert (format "Button at %d-%d: \"%s\"\n" 
                      (nth 0 button) (nth 1 button) (nth 2 button))))
    (insert "\n\nText Properties (Faces):\n")
    (insert "=======================\n")
    (dolist (region (plist-get state :faces))
      (when (nth 2 region) ; Only log non-nil faces
        (insert (format "Face \"%s\" at %d-%d\n" 
                        (nth 2 region) (nth 0 region) (nth 1 region)))))))

;; Run tests and save results
(progn
  ;; Test verbiste-browse-random-verbs
  (message "Testing verbiste-browse-random-verbs...")
  (verbiste-browse-random-verbs)
  (let ((state (capture-buffer-state "*Verbiste Random Verbs*")))
    (log-state-to-file state "random-verbs-buffer-state.txt"))
  
  ;; Test verbiste-french-conjugation
  (message "Testing verbiste-french-conjugation...")
  (verbiste-french-conjugation "parler")
  (let ((state (capture-buffer-state "*Verbiste French Conjugation*")))
    (log-state-to-file state "french-conjugation-buffer-state.txt"))
  
  ;; Test verbiste-display-similar-verbs
  (message "Testing verbiste-display-similar-verbs...")
  (verbiste-display-similar-verbs "parler")
  (let ((state (capture-buffer-state "*Verbiste Similar Verbs*")))
    (log-state-to-file state "similar-verbs-buffer-state.txt"))
  
  (message "Tests completed. Results saved to *-buffer-state.txt files.")
  (kill-emacs 0))