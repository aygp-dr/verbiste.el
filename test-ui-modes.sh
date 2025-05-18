#!/bin/bash
# Script to test verbiste.el UI in both batch and interactive modes

set -e

echo "Testing verbiste.el UI issues..."
REPO_DIR="$(pwd)"

# Create output directory
mkdir -p "$REPO_DIR/ui-test-results"

# Part 1: Run in batch mode (non-interactive)
echo "Running in batch mode..."
emacs --batch -l "$REPO_DIR/repro-ui-issues.el" 2>&1 | tee "$REPO_DIR/ui-test-results/batch-output.log"

# Move result files
mv *-buffer-state.txt "$REPO_DIR/ui-test-results/" 2>/dev/null || true

# Part 2: Generate an Expect script for interactive testing
cat > "$REPO_DIR/ui-test-results/verbiste-expect.exp" << 'EOF'
#!/usr/bin/expect -f
# Automated interactive testing of verbiste.el UI

set timeout 10
spawn emacs -nw -Q

# Wait for Emacs to start
expect "*scratch*"

# Load verbiste
send "(add-to-list 'load-path \"$env(REPO_DIR)\")\r"
send "(require 'verbiste)\r"
send "(setq debug-on-error t)\r"

# Test browse random verbs
send "(verbiste-browse-random-verbs)\r"
expect "*Verbiste Random Verbs*"
# Take screenshot or capture state
send "\C-x\C-w"
expect "Write file:"
send "$env(REPO_DIR)/ui-test-results/random-verbs-buffer.txt\r"

# Navigate through buttons to test highlighting
send "\t"  ;# Move to first button
expect "*Verbiste Random Verbs*"
sleep 0.5
send "\t"  ;# Move to second button
expect "*Verbiste Random Verbs*"
sleep 0.5

# Test verb conjugation
send "\C-x b *scratch*\r"
expect "*scratch*"
send "(verbiste-french-conjugation \"parler\")\r"
expect "*Verbiste French Conjugation*"
send "\C-x\C-w"
expect "Write file:"
send "$env(REPO_DIR)/ui-test-results/conjugation-buffer.txt\r"

# Test similar verbs
send "\C-x b *scratch*\r"
expect "*scratch*"
send "(verbiste-display-similar-verbs \"parler\")\r"
expect "*Verbiste Similar Verbs*"
send "\C-x\C-w"
expect "Write file:"
send "$env(REPO_DIR)/ui-test-results/similar-verbs-buffer.txt\r"

# Capture some screenshots with UI elements (terminal only)
send "\C-x b *Verbiste Random Verbs*\r"
expect "*Verbiste Random Verbs*"
system "echo 'Screenshot of random verbs buffer:' >> $env(REPO_DIR)/ui-test-results/terminal-ui.txt"
system "echo '-------------------------------' >> $env(REPO_DIR)/ui-test-results/terminal-ui.txt"
system "cat /dev/tty >> $env(REPO_DIR)/ui-test-results/terminal-ui.txt"
system "echo '\n\n' >> $env(REPO_DIR)/ui-test-results/terminal-ui.txt"

# Exit Emacs
send "\C-x\C-c"
expect eof
EOF

# Make the expect script executable
chmod +x "$REPO_DIR/ui-test-results/verbiste-expect.exp"

# Run Expect script if available
if command -v expect &> /dev/null; then
    echo "Running interactive test with Expect..."
    export REPO_DIR
    "$REPO_DIR/ui-test-results/verbiste-expect.exp" | tee "$REPO_DIR/ui-test-results/expect-output.log"
else
    echo "Expect not found. Skipping interactive test."
    echo "To install expect: sudo apt-get install expect"
fi

# Part 3: Generate GitHub issue template
cat > "$REPO_DIR/ui-test-results/ui-issue-template.md" << 'EOF'
## UI Issues in verbiste.el

### Description

When using `verbiste-browse-random-verbs` and other UI functions in verbiste.el, the following issues are observed:

1. Buffers are read-only by design (using `special-mode`), but there's no clear indication to users
2. The random verbs list is limited to 10 verbs instead of showing the full list
3. Action "links" (buttons) have some text that's not fully highlighted

### Reproduction Steps

1. Load verbiste.el: `(require 'verbiste)`
2. Run the command: `(verbiste-browse-random-verbs)`
3. Observe the buffer and button highlighting
4. Navigate through buttons with TAB key

### Expected Behavior

1. Clear indication that buffers are intentionally read-only
2. Either full list of verbs or clear pagination controls
3. Fully highlighted action buttons with consistent styling

### Actual Behavior

1. Read-only buffers without explanation
2. Only 10 random verbs shown with no indication of the full list size
3. Inconsistent button highlighting where text is partially highlighted

### Diagnostic Information

<details>
<summary>Test Results</summary>

See attached files in ui-test-results/ directory for detailed button and text property analysis.

</details>

### Proposed Testing Approaches

Several approaches could be used to test and fix these UI issues:

#### Automated Testing Options

1. **ERT with Buffer State Verification**
   - Use ERT to test function execution
   - Capture and verify buffer state, button positions and text properties
   - Good for regression testing

2. **Process Communication Testing**
   - Use Emacs --batch with expect scripts
   - Run UI operations and capture results
   - Useful for continuous integration

3. **Visual Regression Testing**
   - Capture buffer content with text properties
   - Generate visual representations of buffers
   - Compare against expected output

#### Recommended Fixes

1. For read-only buffers:
   - Add mode-line indicator or header text explaining read-only status
   - Consider `view-mode` instead of `special-mode` for better user experience

2. For incomplete verb list:
   - Add pagination controls
   - Make the limit customizable
   - Show total count of available verbs

3. For button highlighting:
   - Replace fixed character count calculations with position markers
   - Store button start/end positions dynamically
   - Use a helper function for consistent button creation
EOF

echo "Testing completed. Results and issue template saved to ui-test-results/"