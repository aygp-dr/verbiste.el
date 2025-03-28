# Verbiste.el - Emacs interface to Verbiste French/Italian verb conjugation
# GNUmakefile - For use with gmake (GNU Make)

.PHONY: all build compile install test clean check-deps help lint package-lint package checkdoc verbiste-json verbiste-org verbiste-all dist

# Variables
EMACS = emacs
BATCH = $(EMACS) -Q -batch
PYTHON = poetry run python3
ELFILES = verbiste.el ob-verbiste.el
ELCFILES = $(ELFILES:.el=.elc)
TESTFILES = verbiste-tests.el
PKG_VERSION = $(shell grep -o "Version: [0-9.]*" verbiste.el | cut -d' ' -f2)
PKG_NAME = verbiste-$(PKG_VERSION)

# Directory definitions
VERBISTE_XML_DIR = /usr/local/share/verbiste-0.1
DATA_DIR = ./data
DIST_DIR = ./dist

# Default target shows help
.DEFAULT_GOAL := help

# Main targets
all: compile verbiste-json        # Build everything
build: compile                    # Alias for compile

compile: $(ELCFILES)              # Compile Emacs Lisp files

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

install:                          # Show installation instructions
	@echo "To install, add this to your Emacs init file:"
	@echo "(add-to-list 'load-path \"$(CURDIR)\")"
	@echo "(require 'verbiste)"

$(VERBISTE_XML_DIR)/verbs-fr.xml:
	@echo "ERROR: verbs-fr.xml not found at $(VERBISTE_XML_DIR)"
	@echo "Install Verbiste or set VERBISTE_XML_DIR to correct location"
	@false

$(VERBISTE_XML_DIR)/conjugation-fr.xml:
	@echo "ERROR: conjugation-fr.xml not found at $(VERBISTE_XML_DIR)"
	@echo "Install Verbiste or set VERBISTE_XML_DIR to correct location"
	@false

check-deps: $(VERBISTE_XML_DIR)/verbs-fr.xml $(VERBISTE_XML_DIR)/conjugation-fr.xml
	@echo "Checking Verbiste dependencies..."
	@which french-conjugator || echo "french-conjugator: Not found"
	@which french-deconjugator || echo "french-deconjugator: Not found"
	@echo "All dependencies verified."

test:                             # Run tests
	$(BATCH) -l ert -l verbiste.el -l $(TESTFILES) -f ert-run-tests-batch-and-exit

lint: package-lint checkdoc       # Run all linters

package-lint:                     # Check package headers with package-lint
	$(BATCH) \
	  --eval "(require 'package)" \
	  --eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
	  --eval "(package-initialize)" \
	  --eval "(unless (package-installed-p 'package-lint) (package-refresh-contents) (package-install 'package-lint))" \
	  --eval "(require 'package-lint)" \
	  --eval "(dolist (file '($(ELFILES))) (package-lint-batch-and-exit file))"

checkdoc:                         # Check documentation with checkdoc
	$(BATCH) \
	  --eval "(require 'checkdoc)" \
	  --eval "(dolist (file '($(ELFILES))) (checkdoc-file file))"

# Directory targets
$(DATA_DIR):
	mkdir -p $@

$(DIST_DIR):
	mkdir -p $@

# JSON conversion
$(DATA_DIR)/%.json: $(VERBISTE_XML_DIR)/%.xml | $(DATA_DIR)
	$(PYTHON) -m verbiste_tools.xml2json $< $@

# Org generation
$(DATA_DIR)/%.org: $(DATA_DIR)/%.json
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

JSON_FILES = $(DATA_DIR)/verbs-fr.json $(DATA_DIR)/conjugation-fr.json \
	     $(DATA_DIR)/verbs-it.json $(DATA_DIR)/conjugation-it.json

ORG_FILES = $(DATA_DIR)/verbs-fr.org $(DATA_DIR)/conjugation-fr.org \
	    $(DATA_DIR)/verbs-it.org $(DATA_DIR)/conjugation-it.org

verbiste-json: $(JSON_FILES)      # Convert all XML to JSON
verbiste-org: $(ORG_FILES)        # Generate all org docs
verbiste-all: verbiste-json verbiste-org # Run both conversions

package: $(ELFILES) $(TESTFILES)   # Create package suitable for submission to MELPA
	@echo "Package files verified for MELPA submission"
	@echo "To submit to MELPA, send a pull request to:"
	@echo "  https://github.com/melpa/melpa"

dist: clean compile test lint | $(DIST_DIR)  # Create distribution package
	tar -cf $(DIST_DIR)/$(PKG_NAME).tar $(ELFILES) $(ELCFILES) $(TESTFILES) README.org CLAUDE.org GNUmakefile examples.org
	@echo "Package created at $(DIST_DIR)/$(PKG_NAME).tar"

# Clean targets
clean: clean-json clean-org       # Clean everything
	rm -f $(ELCFILES)
	rm -f *~

clean-json:                       # Remove JSON files
	rm -f $(DATA_DIR)/*.json

clean-org:                        # Remove org files
	rm -f $(DATA_DIR)/*.org

# Help function
help:                             # Show this help
	@echo "Verbiste.el GNUmakefile"
	@echo "======================="
	@echo
	@echo "Available targets:"
	@sed -n 's/^\([a-zA-Z0-9_-]\+\):.*\(#.*\)$$/  \1\2/p' $(MAKEFILE_LIST) | column -t -s '#'
	@echo
	@echo "Note: This is a GNUmakefile designed for gmake, which offers better"
	@echo "      compatibility across OSX and Linux than standard make."


data/french_verbs_list.txt: verbiste_extract.xsl /usr/local/share/verbiste-0.1/verbs-fr.xml # Show a simple list of all verbs
	xml tr $^ > $@

PYTHON := poetry run python3

data/french_verb_clusters.json: generate_verb_clusters.py data/french_verbs_list.txt # Verb -> List[Verb] of similar
	$(PYTHON) $^ > $@
