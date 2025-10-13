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

### Setup & Installation

```bash
# Create virtual environment
make venv

# Install in development mode
make install-dev

# Install development tools (ruff, pytest, etc.)
make install-tools
```

### Code Quality & Testing

```bash
# Format code with ruff
make format

# Fix linting issues automatically
make fix

# Run all checks (format + lint + test)
make check

# Run tests only
make test

# Run specific test types
make test-unit
make test-integration
make test-concrete

# Check code quality status
make status
make issues
```

### Advanced Development

```bash
# Complete code cleanup (format + fix + lint)
make clean-code

# Fix all issues including unsafe fixes
make fix-all

# Run tests with coverage
make test-coverage

# Generate documentation
make docs
```

### Utilities

```bash
# Demo with sample files
make demo
make demo-uk

# Clean temporary files
make clean
make clean-all

# Remove virtual environment
make clean-venv
```

## Features

- **GEDCOM 5.5.1** parsing and validation
- **Pickle serialization** with gzip compression
- **Search indexing** for fast lookups
- **30 CLI options** matching GeneWeb documentation
- **Comprehensive testing** with real GEDCOM files
- **Automated code quality** with ruff and pytest
- **Virtual environment management** with Makefile
- **Professional development workflow**

## Code Quality Tools

### Ruff (Linting & Formatting)

- **Fast Python linter** and formatter
- **Automatic fixes** for common issues
- **Zero configuration** required
- **Compatible with black, isort, flake8**

### Pytest (Testing)

- **129 comprehensive tests** (unit, integration, concrete)
- **Real GEDCOM file testing** with sample.ged and uk.ged
- **Coverage reporting** with html output
- **Professional test structure** with proper assertions

### Development Workflow

```bash
# 1. Start development
make dev-setup

# 2. Make changes to code
# ... edit files ...

# 3. Quick quality check
make fix

# 4. Run tests
make test

# 5. Full verification
make check

# 6. Clean up
make clean
```

## Troubleshooting & Maintenance

### Common Issues

```bash
# Check current status
make status

# View remaining issues
make issues

# Fix all fixable issues
make fix-all

# Clean everything and start fresh
make clean-all
make dev-setup
```

### Performance Monitoring

```bash
# Run tests with coverage
make test-coverage

# Check specific test categories
make test-unit      # Fast unit tests
make test-concrete  # Real file tests
```

### Environment Management

```bash
# Recreate virtual environment
make clean-venv
make venv
make install-dev

# Update dependencies
make install-tools
```

## Requirements

- Python 3.10+
- See `requirements.txt` for dependencies

## Available Makefile Targets

| Target             | Description                            |
| ------------------ | -------------------------------------- |
| `venv`             | Create virtual environment             |
| `install-dev`      | Install in development mode            |
| `install-tools`    | Install development tools              |
| `format`           | Format code with ruff                  |
| `fix`              | Auto-fix linting issues                |
| `fix-all`          | Fix all issues (including unsafe)      |
| `clean-code`       | Complete code cleanup                  |
| `check`            | Run all checks (format + lint + test)  |
| `check-ruff`       | Run checks without mypy                |
| `test`             | Run all tests                          |
| `test-unit`        | Run unit tests only                    |
| `test-integration` | Run integration tests only             |
| `test-concrete`    | Run concrete tests with real files     |
| `test-coverage`    | Run tests with coverage report         |
| `status`           | Show linting status                    |
| `issues`           | Show remaining issues                  |
| `demo`             | Run demo with sample.ged               |
| `demo-uk`          | Run demo with uk.ged                   |
| `clean`            | Clean temporary files                  |
| `clean-all`        | Clean everything including **pycache** |
| `clean-venv`       | Remove virtual environment             |

---

**GeneWeb Python** - Professional genealogical data processing
