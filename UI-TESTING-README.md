# UI Testing for verbiste.el

This directory contains tools and resources for testing the UI components of verbiste.el.

## Issue Overview

When running `verbiste-browse-random-verbs` and other UI functions, the following issues occur:
1. Buffers are read-only without clear indication to users
2. Only a partial list of verbs is shown (limited to 10)
3. Action buttons have inconsistent highlighting (text not fully highlighted)

## Testing Tools

### Reproduction Script

`repro-ui-issues.el` - Reproduces the UI issues in batch mode and captures detailed buffer state information including button positions and text properties.

Run with:
```bash
emacs --batch -l repro-ui-issues.el
```

### Automated Testing

`test-ui-modes.sh` - Comprehensive test script that:
1. Runs tests in batch mode with `--batch`
2. Runs interactive tests with `expect` if available
3. Generates detailed reports and an issue template

Run with:
```bash
./test-ui-modes.sh
```

### Proposed Improvements

`verbiste-ui-improvements.el` - Implementation of fixes for the UI issues:
1. Better button creation with position markers
2. Improved buffer setup with clear read-only indicators
3. Enhanced random verbs function with count information

Test the improvements with:
```elisp
(require 'verbiste-ui-improvements)
(verbiste-test-ui-improvements)
```

## Testing Approaches for Emacs UI

### Batch Mode Testing

Emacs batch mode can be used for non-interactive testing:
```bash
emacs --batch -l test-script.el
```

This works well for testing function execution and basic output, but doesn't fully simulate user interaction with UI elements.

### Expect-Based Testing

The `expect` tool can be used to script interactive sessions:
```bash
#!/usr/bin/expect -f
spawn emacs -nw -Q
expect "*scratch*"
send "(require 'verbiste)\r"
send "(verbiste-browse-random-verbs)\r"
# Test UI interaction
```

This allows testing actual keyboard interaction in a terminal environment.

### ERT Unit Testing

For testing specific UI components:
```elisp
(ert-deftest test-buttons ()
  (let ((mock-data '("verb1" "verb2")))
    (cl-letf (((symbol-function 'verbiste--get-random-verbs)
               (lambda (_) mock-data)))
      (verbiste-browse-random-verbs)
      (with-current-buffer "*Verbiste Random Verbs*"
        ;; Test UI properties
        ))))
```

## Best Practices for UI Testing in Emacs

1. **Mock External Dependencies**
   - Use `cl-letf` to substitute function implementations
   - Create controlled test data sets

2. **Capture Text Properties**
   - Emacs UI often relies on text properties for buttons and faces
   - Verify property placement and ranges

3. **Test Navigation**
   - Verify TAB navigation between buttons
   - Test button activation with `push-button`

4. **Combination Approach**
   - Use batch mode for quick regression testing
   - Use expect for interactive testing
   - Use ERT for targeted component tests

## GitHub Issue

See `issue-template-ui-testing.md` for a detailed description of the issues and proposed testing approaches that can be used to create a GitHub issue.