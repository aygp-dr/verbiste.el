.PHONY: all compile install test clean check-deps help verbiste-json verbiste-org verbiste-all clean-json clean-org

# Variables
EMACS = emacs
BATCH = $(EMACS) -Q -batch
ELCFILES = verbiste.elc
TESTFILES = verbiste-tests.el

# Directory definitions
VERBISTE_XML_DIR = /usr/local/share/verbiste-0.1
DATA_DIR = ./data

# Default target shows help
.DEFAULT_GOAL := help

# Main targets
all: compile                          # Build everything
compile: $(ELCFILES)                  # Compile Emacs Lisp files
%.elc: %.el
	$(BATCH) -f batch-byte-compile $

install:                              # Show installation instructions
	@echo "To install, add this to your Emacs init file:"
	@echo "(add-to-list 'load-path \"$(shell pwd)\")"
	@echo "(require 'verbiste)"

check-deps:                           # Check if Verbiste dependencies are installed
	@echo "Checking Verbiste dependencies..."
	@if command -v french-conjugator >/dev/null 2>&1; then echo "french-conjugator: Found"; else echo "french-conjugator: Not found"; fi
	@if command -v french-deconjugator >/dev/null 2>&1; then echo "french-deconjugator: Found"; else echo "french-deconjugator: Not found"; fi
	@if [ -f $(VERBISTE_XML_DIR)/verbs-fr.xml ]; then echo "verbs-fr.xml: Found"; else echo "verbs-fr.xml: Not found"; fi
	@if [ -f $(VERBISTE_XML_DIR)/conjugation-fr.xml ]; then echo "conjugation-fr.xml: Found"; else echo "conjugation-fr.xml: Not found"; fi

test:                                 # Run tests
	$(BATCH) -l ert -l verbiste.el -l $(TESTFILES) -f ert-run-tests-batch-and-exit

# Directory targets
$(DATA_DIR):
	mkdir -p $@

# JSON conversion
$(DATA_DIR)/%.json: $(VERBISTE_XML_DIR)/%.xml | $(DATA_DIR)  # Convert XML to JSON
	$(EMACS) --batch \
		--eval "(require 'json)" \
		--eval "(require 'xml)" \
		--eval "(setq xml-data (with-temp-buffer \
					(insert-file-contents \"$<\") \
					(xml-parse-region (point-min) (point-max))))" \
		--eval "(setq json-data (json-encode xml-data))" \
		--eval "(with-temp-file \"$@\" \
					(insert json-data) \
					(json-pretty-print-buffer))" \
		--eval "(message \"Converted $< to $@\")"

# Org generation
$(DATA_DIR)/%.org: $(DATA_DIR)/%.json              # Generate org docs from JSON
	$(EMACS) --batch \
		--eval "(require 'json)" \
		--eval "(require 'org)" \
		--eval "(let ((json-str (with-temp-buffer \
					(insert-file-contents \"$<\") \
					(buffer-string)))) \
				(with-temp-file \"$@\" \
					(insert \"#+TITLE: Verbiste Data: $(notdir $<)\n\") \
					(insert \"#+PROPERTY: header-args:json :tangle $<\n\n\") \
					(insert \"* Verbiste $(notdir $<) Structure\n\n\") \
					(insert \"#+begin_src json\n\") \
					(insert json-str) \
					(insert \"#+end_src\n\n\") \
					(insert \"* Usage Examples\n\n\") \
					(insert \"#+begin_src emacs-lisp :results output\n\") \
					(insert \"(let ((json-data (json-read-file \\\"$<\\\")))\\n\") \
					(insert \"  (message \\\"Total entries: %d\\\" (length json-data)))\\n\") \
					(insert \"#+end_src\")))"

verbiste-json: $(DATA_DIR)/verbs-fr.json $(DATA_DIR)/conjugation-fr.json $(DATA_DIR)/verbs-it.json $(DATA_DIR)/conjugation-it.json  # Convert all XML to JSON
verbiste-org: $(DATA_DIR)/verbs-fr.org $(DATA_DIR)/conjugation-fr.org $(DATA_DIR)/verbs-it.org $(DATA_DIR)/conjugation-it.org        # Generate all org docs
verbiste-all: verbiste-json verbiste-org           # Run both conversions

# Clean targets
clean: clean-json clean-org                        # Clean everything
	rm -f $(ELCFILES)
	rm -f *~

clean-json:                                        # Remove JSON files
	rm -f $(DATA_DIR)/*.json

clean-org:                                         # Remove org files
	rm -f $(DATA_DIR)/*.org

# Auto-help function
help:                                              # Show this help
	@echo "Verbiste.el Makefile"
	@echo "===================="
	@echo
	@echo "Available targets:"
	@awk '/^[a-zA-Z0-9_-]+:/ { \
		helpMessage = match(lastLine, /^# (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")); \
			helpMessage = substr(lastLine, RSTART + 2, RLENGTH); \
			printf "  %-20s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
