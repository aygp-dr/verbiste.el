# verbiste.el

Emacs interface to the Verbiste French verb conjugation library.

## Overview

This package provides a convenient interface to the Verbiste library for French verb conjugation directly within Emacs. It allows users to conjugate verbs in all tenses and moods, right from their editor.

## Features

- Conjugate French verbs in all tenses and moods
- Find infinitive forms of conjugated French verbs
- Check installation status to ensure proper setup
- Convenient minor mode with keybindings
- Support for direct XML parsing (optional, for better performance)

## Installation

### Prerequisites

#### FreeBSD
```bash
sudo pkg install verbiste
```

#### Debian/Ubuntu
```bash
sudo apt-get install elpa-verbiste verbiste
```

### Manual Installation

1. Clone this repository:
   ```bash
   git clone https://github.com/aygp-dr/verbiste.el.git
   ```

2. Add the following to your Emacs init file:
   ```elisp
   (add-to-list 'load-path "/path/to/verbiste.el")
   (require 'verbiste)
   ```

## Usage

### Basic Commands

- `M-x verbiste-french-conjugation`: Conjugate a French verb
- `M-x verbiste-french-deconjugation`: Find the infinitive form of a conjugated French verb
- `M-x verbiste-check-installation`: Check if Verbiste is properly installed
- `M-x verbiste-mode`: Toggle Verbiste minor mode for verb conjugation
- `M-x verbiste-display-similar-verbs`: Display verbs similar to the given verb
- `M-x verbiste-browse-random-verbs`: Show a list of random verbs with interactive buttons

### Keybindings

When `verbiste-mode` is enabled:

| Key       | Function                      |
|-----------|-------------------------------|
| C-c v f c | verbiste-french-conjugation   |
| C-c v f d | verbiste-french-deconjugation |
| C-c v s   | verbiste-display-similar-verbs |
| C-c v r   | verbiste-browse-random-verbs  |

## Development

### For Elisp code

```bash
# Compile elisp files
gmake compile

# Run tests
gmake test

# Lint code
gmake lint

# Create distribution package
gmake dist
```

### For supporting Python tools

```bash
# Install with Poetry
poetry install

# Format Python code
poetry run black verbiste_tools

# Check types
poetry run mypy verbiste_tools

# Lint code
poetry run ruff verbiste_tools
```

## License

GPL v2 or later, consistent with Verbiste itself.