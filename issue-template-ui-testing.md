# UI Testing in Emacs Packages

## Current Issues with verbiste.el UI

When running `verbiste-browse-random-verbs`, the following UI issues are observed:

1. **Read-only buffers without clear indication** - Buffers created by UI functions use `special-mode` which makes them read-only, but doesn't clearly indicate this to users.
2. **Incomplete verb list** - Only 10 random verbs are shown with no indication that this is a subset or how to see more.
3. **Inconsistent button highlighting** - Action links like "[conjugate]" have text that's not fully highlighted due to fixed character count calculations in button creation code.

## Reproducing the Issues

### In Interactive Mode
```elisp
(require 'verbiste)
(verbiste-browse-random-verbs)
;; Observe the buffer and try to navigate with TAB between buttons
```

### In Batch Mode
```bash
# Run the reproduction script
emacs --batch -l repro-ui-issues.el

# Examine the output files for button highlighting issues
cat random-verbs-buffer-state.txt
```

## Testing Approaches for Emacs UI Features

### Automated Testing Options

#### 1. ERT with Buffer State Verification
```elisp
(ert-deftest test-verbiste-buttons ()
  "Test that buttons in random verbs buffer are properly created."
  (cl-letf (((symbol-function 'verbiste--get-random-verbs)
             (lambda (_) '("parler" "finir"))))
    (verbiste-browse-random-verbs)
    (with-current-buffer "*Verbiste Random Verbs*"
      ;; Test button at position
      (goto-char (point-min))
      (search-forward "parler")
      (should (button-at (point)))
      ;; Test the entire button text has button face
      (search-forward "[conjugate]")
      (backward-char 1)
      (should (eq (get-text-property (- (point) 5) 'face) 'button)))))
```

#### 2. Process Communication Testing
```bash
#!/usr/bin/expect -f
# Example expect script for UI testing
spawn emacs -nw -Q
expect "*scratch*"
send "(add-to-list 'load-path \".\")\r"
send "(require 'verbiste)\r"
send "(verbiste-browse-random-verbs)\r"
expect "*Verbiste Random Verbs*"
# Capture output/state for verification
```

#### 3. Visual Regression Testing
- Capture buffer contents with text properties
- Generate visual representation in text format
- Compare against expected output
- Can be done with custom Emacs Lisp functions

### Interactive Testing Options

#### 1. Direct User Testing
- Create test scenarios with step-by-step instructions
- Have users record observations about UI behavior
- Collect screenshots of the interface

#### 2. Instrumented UI
- Add debugging indicators for button boundaries
- Display button property information on hover
- Add keybinding to inspect text properties at point

## Proposed Solutions

### For Button Highlighting Issues
```elisp
;; BEFORE: Unreliable fixed character count
(insert " [conjugate]")
(make-button (- (point) 11) (point)
             'action (lambda (_) (verb-action))
             'follow-link t)

;; AFTER: Reliable position tracking
(let ((start (point)))
  (insert " [conjugate]")
  (make-button start (point)
               'action (lambda (_) (verb-action))
               'follow-link t))
```

### For Read-Only Status Indication
```elisp
(defun verbiste--setup-ui-buffer (title)
  "Set up a verbiste UI buffer with TITLE."
  (with-current-buffer (get-buffer-create (format "*Verbiste %s*" title))
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert title "\n\n")
      (insert "This buffer is in special-mode (read-only). "
              "Use TAB to navigate between buttons.\n\n"))
    (special-mode)
    (current-buffer)))
```

### For Limited Verb List
```elisp
(defcustom verbiste-random-verbs-count 10
  "Number of random verbs to display in browse-random-verbs."
  :type 'integer
  :group 'verbiste)

;; Show pagination controls
(let* ((all-verbs (verbiste--get-all-verbs))
       (total (length all-verbs))
       (page-verbs (verbiste--get-random-verbs verbiste-random-verbs-count)))
  ;; Display info about how many are shown
  (insert (format "Showing %d of %d total verbs\n\n" 
                  (length page-verbs) total)))
```

## Questions for Discussion

1. What are best practices for testing Emacs UI components?
2. How can we ensure consistent button highlighting and text properties?
3. What automation approaches work best for Emacs package testing?
4. Should UI testing be part of CI/CD pipelines for Emacs packages?