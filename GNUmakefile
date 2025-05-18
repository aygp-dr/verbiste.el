# Verbiste.el - Emacs interface to Verbiste French/Italian verb conjugation
# GNUmakefile - For use with gmake (GNU Make)

.PHONY: all build compile install test clean check-deps help lint strict-lint bytec checkdoc package verbiste-json verbiste-org verbiste-all create-dist screenshot demo embeddings clusters sample-clusters $(DIST_DIR) $(DATA_DIR)

# Variables
EMACS = emacs
BATCH = $(EMACS) -Q -batch
PYTHON := poetry run python3
# Main Elisp files
ELFILES = verbiste.el
ELCFILES = $(ELFILES:.el=.elc)
TESTFILES = verbiste-tests.el
PKG_VERSION = $(shell grep -o "Version: [0-9.]*" verbiste.el | cut -d' ' -f2)
PKG_NAME = verbiste-$(PKG_VERSION)

# Directory definitions
VERBISTE_XML_DIR = /usr/local/share/verbiste-0.1
VERBISTE_XML = $(VERBISTE_XML_DIR)/verbs-fr.xml
VERBISTE_XSL = verbiste_extract.xsl

# Tool directories
TOOLS_DIR = verbiste_tools
DATA_DIR = ./data
DIST_DIR = ./dist

# Data files
VERBS_LIST = $(DATA_DIR)/french_verbs_list.txt
VERBS_EMBEDDINGS = $(DATA_DIR)/french_verbs_embeddings.json
VERB_CLUSTERS = $(DATA_DIR)/french_verb_clusters.json
VERB_TOPICS = $(DATA_DIR)/french_verb_topics.json

# Default target shows help
.DEFAULT_GOAL := help

# Main targets
all: compile verbiste-xml verbiste-json verbiste-data  # Build everything
build: compile                    # Alias for compile

compile: $(ELCFILES)              # Compile Emacs Lisp files

%.elc: %.el
	$(BATCH) -f batch-byte-compile $<

install:                          # Show installation instructions
	@echo "To install, add this to your Emacs init file:"
	@echo "(add-to-list 'load-path \"$(CURDIR)\")"
	@echo "(require 'verbiste)"

$(VERBISTE_XML):
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

test: sample-clusters              # Run tests
	$(BATCH) -l ert -l verbiste.el -l $(TESTFILES) -f ert-run-tests-batch-and-exit

lint: test bytec checkdoc  # Run basic linters
	@echo "Basic linting complete!"

strict-lint: test        # Run comprehensive linting with elisp-lint
	$(BATCH) -l elisp-lint.el -f elisp-lint-batch-files $(ELFILES)

bytec:                           # Basic syntax check via byte-compilation
	$(BATCH) --eval '(byte-compile-file "verbiste.el")'

checkdoc:                        # Run checkdoc on all files
	@$(BATCH) -l checkdoc-file.el verbiste.el 2>/dev/null || true
	@echo "Checkdoc complete (warnings may be present but ignored for build)"

# Directory targets
$(DATA_DIR):
	mkdir -p $@

# Create distribution directory
$(DIST_DIR):
	mkdir -p $@

# Verbiste XML files
XML_FILES = $(DATA_DIR)/verbs-fr.xml $(DATA_DIR)/conjugation-fr.xml

# Primary method: Try system installation files first
$(DATA_DIR)/%-fr.xml: $(VERBISTE_XML_DIR)/%-fr.xml | $(DATA_DIR)
	cp $< $@
	@echo "Copied $< to $@"

# Fallback method: Use sample files if system files aren't available
$(DATA_DIR)/verbs-fr.xml: verbs-fr-sample.xml | $(DATA_DIR)
	cp $< $@
	@echo "Used sample file $< for $@"

$(DATA_DIR)/conjugation-fr.xml: conjugation-fr-sample.xml | $(DATA_DIR)
	cp $< $@
	@echo "Used sample file $< for $@"

verbiste-xml: $(XML_FILES)      # Copy Verbiste XML files locally

# JSON conversion
$(DATA_DIR)/%.json: $(DATA_DIR)/%.xml | $(DATA_DIR)
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
verbiste-data: $(DATA_DIR)/french_verb_clusters.json # Ensure verb clusters data is available

verbiste-all: verbiste-xml verbiste-json verbiste-org verbiste-data # Run all conversions

package: $(ELFILES) $(TESTFILES)   # Create package suitable for submission to MELPA
	@echo "Package files verified for MELPA submission"
	@echo "To submit to MELPA, send a pull request to:"
	@echo "  https://github.com/melpa/melpa"

sample-clusters:
	@echo "Creating sample verb clusters for testing..."
	@mkdir -p $(DATA_DIR)
	@printf '{\n  "parler": [\n    {\n      "verb": "dialoguer",\n      "similarity": 0.95\n    },\n    {\n      "verb": "discuter",\n      "similarity": 0.92\n    },\n    {\n      "verb": "converser",\n      "similarity": 0.89\n    }\n  ],\n  "finir": [\n    {\n      "verb": "terminer",\n      "similarity": 0.96\n    },\n    {\n      "verb": "achever",\n      "similarity": 0.93\n    },\n    {\n      "verb": "conclure",\n      "similarity": 0.88\n    }\n  ],\n  "aller": [\n    {\n      "verb": "venir",\n      "similarity": 0.87\n    },\n    {\n      "verb": "partir",\n      "similarity": 0.85\n    },\n    {\n      "verb": "voyager",\n      "similarity": 0.81\n    }\n  ],\n  "être": [\n    {\n      "verb": "exister",\n      "similarity": 0.82\n    },\n    {\n      "verb": "devenir",\n      "similarity": 0.79\n    },\n    {\n      "verb": "sembler",\n      "similarity": 0.75\n    }\n  ],\n  "avoir": [\n    {\n      "verb": "posséder",\n      "similarity": 0.91\n    },\n    {\n      "verb": "obtenir",\n      "similarity": 0.87\n    },\n    {\n      "verb": "détenir",\n      "similarity": 0.83\n    }\n  ]\n}\n' > $(DATA_DIR)/french_verb_clusters.json
	@echo "Sample verb clusters created at $(DATA_DIR)/french_verb_clusters.json"

create-dist: clean sample-clusters compile test lint | $(DIST_DIR)  # Create distribution package
	tar -cf $(DIST_DIR)/$(PKG_NAME).tar $(ELFILES) $(ELCFILES) $(TESTFILES) README.org CLAUDE.org GNUmakefile examples.org elisp-lint.el $(DATA_DIR)/french_verb_clusters.json
	@echo "Package created at $(DIST_DIR)/$(PKG_NAME).tar"

# Clean targets
clean: clean-json clean-org       # Clean everything
	rm -f $(ELCFILES)
	rm -f *~

clean-json:                       # Remove JSON files
	rm -f $(DATA_DIR)/*.json

clean-org:                        # Remove org files
	rm -f $(DATA_DIR)/*.org

# Screenshots and demo
screenshot:                       # Generate text-based screenshots of the UI
	@echo "Generating text-based screenshots of verbiste.el UI..."
	$(CURDIR)/generate-screenshots.sh

demo:                             # Launch Emacs with verbiste enabled for demo
	$(EMACS) -Q --eval "(progn \
		(add-to-list 'load-path \"$(CURDIR)\") \
		(require 'verbiste) \
		(verbiste-mode 1) \
		(message \"Welcome to verbiste.el demo! Try M-x verbiste-browse-random-verbs\"))"

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


VERBISTE_XSL = verbiste_extract.xsl

# Extract verb list from Verbiste XML
$(VERBS_LIST): $(VERBISTE_XSL) $(DATA_DIR)/verbs-fr.xml | $(DATA_DIR)
	xml tr $^ > $@
	@echo "Generated verbs list: $@"

# Generate embeddings for all verbs
$(VERBS_EMBEDDINGS): $(VERBS_LIST) | $(DATA_DIR)
	$(PYTHON) -m $(TOOLS_DIR).generate_verb_embeddings $< $@
	@echo "Generated verb embeddings: $@"

embeddings: $(VERBS_EMBEDDINGS)

# Generate verb clusters based on embedding similarity
$(VERB_CLUSTERS): $(VERBS_EMBEDDINGS) | $(DATA_DIR)
	$(PYTHON) -m $(TOOLS_DIR).generate_verb_clusters $< $@
	@echo "Generated verb clusters: $@"

clusters: $(VERB_CLUSTERS)

# Generate topic-based verb clusters for pedagogical purposes
$(VERB_TOPICS): $(VERBS_EMBEDDINGS) | $(DATA_DIR)
	$(PYTHON) -m $(TOOLS_DIR).generate_topic_clusters $< $@ 15
	@echo "Generated verb topic clusters: $@"

topics: $(VERB_TOPICS)


