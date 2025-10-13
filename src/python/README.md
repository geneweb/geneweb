# GeneWeb Python

Python implementation of GeneWeb with GED2GWB converter for genealogical data processing.

## Quick Start

```bash
# Install
make install-dev

# Convert GEDCOM to pickle database
python3 -m ged2gwb input.ged --output database.pkl

# Load database
python3 -m ged2gwb --load database.pkl
```

## Project Structure

```
src/python/
├── ged2gwb/          # GED2GWB converter
├── gedcom/           # GEDCOM 5.5.1 parser
├── lib/db_pickle/    # Pickle database system
├── pyproject.toml    # Package configuration
└── Makefile         # Project automation
```

## Development

```bash
# Setup
make dev-setup

# Test
make test

# Demo
make demo

# Clean
make clean
```

## Features

- **GEDCOM 5.5.1** parsing and validation
- **Pickle serialization** with gzip compression
- **Search indexing** for fast lookups
- **30 CLI options** matching GeneWeb documentation
- **Comprehensive testing** with real GEDCOM files

## Requirements

- Python 3.10+
- See `requirements.txt` for dependencies

---

**GeneWeb Python** - Professional genealogical data processing
