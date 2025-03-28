# verbiste-tools

Support scripts for verbiste.el package

## Installation

```bash
# Install with Poetry
poetry install
```

## Available Tools

### xml2json

Converts Verbiste XML files to JSON format with a cleaner structure:

```bash
poetry run xml2json /path/to/verbs-fr.xml /path/to/output/verbs-fr.json
```

### cluster_verbs

Clusters French verbs based on conjugation pattern similarity:

```bash
poetry run cluster_verbs \
  --verbs /path/to/verbs-fr.json \
  --conjugation /path/to/conjugation-fr.json \
  --output ./data/clusters \
  --clusters 15 \
  --method kmeans
```

## Development

```bash
# Format code
poetry run black verbiste_tools

# Run tests
poetry run pytest

# Check types
poetry run mypy verbiste_tools

# Lint code
poetry run ruff verbiste_tools
```