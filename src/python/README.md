# GeneWeb Python Tools

Python tools for GeneWeb genealogy software, including GEDCOM parsing, database utilities, and conversion tools.

## 🚀 Quick Start

```bash
# Install in development mode
cd src/python
pip install -e .

# Run tests
make test

# Run all checks
make check
```

## 📦 Components

- **ged2gwb**: GEDCOM to GeneWeb converter
- **gedcom**: GEDCOM file parser
- **lib.db_pickle**: Database pickle utilities for GeneWeb

## 🛠️ Development

### Prerequisites

- Python 3.8+
- pip
- make (optional, for using Makefile)

### Setup

```bash
# Clone repository
git clone https://github.com/geneweb/geneweb.git
cd geneweb/src/python

# Create virtual environment
python -m venv .venv
source .venv/bin/activate  # On Windows: .venv\Scripts\activate

# Install in development mode
pip install -e .

# Install development tools
make install-tools
```

### Available Commands

```bash
# Setup
make setup          # Complete development setup
make venv           # Create virtual environment
make install        # Install package
make install-tools  # Install dev tools

# Testing
make test           # Run all tests
make test-ged2gwb   # Run ged2gwb tests
make test-gedcom    # Run GEDCOM tests
make test-lib       # Run lib tests
make test-coverage  # Run with coverage

# Development
make format         # Format code with ruff
make lint           # Run linting (ruff + mypy)
make fix            # Auto-fix linting issues
make check          # Format + lint + test

# Utilities
make clean          # Clean temporary files
make demo           # Run demo with sample.ged
make demo-uk        # Run demo with uk.ged
```

## 🧪 Testing

```bash
# Run all tests
make test

# Run specific test categories
make test-ged2gwb
make test-gedcom
make test-lib

# Run with coverage
make test-coverage
```

## 📋 CI/CD

### GitHub Actions Workflows

1. **geneweb-python.yml**: Full Integration Pipeline

   - Runs on master/dev branches
   - Full GeneWeb OCaml build + Python tests
   - Code quality checks (ruff, mypy)
   - Comprehensive testing with Makefile
   - Demo runs with sample GEDCOM files

2. **python-deploy.yml**: Deployment (optional)
   - Runs on tags `python-v*` (e.g., `python-v1.0.0`)
   - Builds Python package
   - Publishes to PyPI
   - Creates GitHub release

### Deployment

To deploy a new version:

```bash
# Create and push a tag
git tag python-v1.0.0
git push origin python-v1.0.0

# This will trigger the deployment workflow
```

## 🔧 Configuration

### pyproject.toml

The project uses `pyproject.toml` for configuration:

- **Build system**: setuptools
- **Code formatting**: ruff
- **Type checking**: mypy
- **Testing**: pytest
- **Coverage**: pytest-cov

### Makefile

The Makefile provides convenient commands for development:

- Uses virtual environment (`.venv/`)
- Consistent command interface
- Colorized output
- Error handling

## 📚 Usage Examples

### GEDCOM to GeneWeb Conversion

```python
from ged2gwb import convert_gedcom_to_geneweb

# Convert GEDCOM file
result = convert_gedcom_to_geneweb('family.ged', 'output.pkl')
print(f"Converted {result.individuals} individuals and {result.families} families")
```

### GEDCOM Parsing

```python
from gedcom import GedcomParser

# Parse GEDCOM file
parser = GedcomParser('family.ged')
individuals = parser.get_individuals()
families = parser.get_families()
```

### Database Operations

```python
from lib.db_pickle import PickleBase

# Load database
db = PickleBase('database.pkl')
individuals = db.get_individuals()
```
