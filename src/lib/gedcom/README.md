# GEDCOM Parser Module

A robust Python implementation for parsing and validating GEDCOM 5.5.1 files.

## Overview

This module provides comprehensive GEDCOM file processing capabilities including parsing, validation, and export functionality. It handles all standard GEDCOM record types and provides detailed error reporting.

## Key Features

- **GEDCOM 5.5.1 Compliance** - Full support for standard GEDCOM format
- **Comprehensive Validation** - Structure and semantic validation
- **Error Handling** - Detailed error reporting with line numbers
- **Type Safety** - Strongly typed data models
- **Export Support** - Convert parsed data back to GEDCOM format

## Quick Start

```python
from lib.gedcom.parser import create_parser

# Parse a GEDCOM file
parser = create_parser()
database = parser.parse_file('family.ged')

# Access parsed data
print(f"Found {len(database.individuals)} individuals")
print(f"Found {len(database.families)} families")

# Access individual data
for xref, individual in database.individuals.items():
    print(f"{individual.primary_name} ({individual.sex})")
```

## Core Components

### Parser (`parser.py`)

Main parser orchestrating the parsing process.

```python
from lib.gedcom.parser import create_parser

parser = create_parser()
database = parser.parse_file('family.ged')
```

### Models (`models.py`)

Data models representing GEDCOM structures.

```python
from lib.gedcom.models import GedcomDatabase, GedcomIndividual

# Access individuals
for individual in database.individuals.values():
    print(individual.primary_name)
```

### Validators (`validators.py`)

Validation logic for structure and semantic checks.

```python
from lib.gedcom.validators import StructureValidator, SemanticValidator

validator = StructureValidator()
errors = validator.validate(database)
```

### Tokenizer (`tokenizer.py`)

Converts raw GEDCOM content into structured tokens.

## Data Models

### GedcomIndividual

Represents a person in the GEDCOM file.

```python
individual = database.individuals['@I1@']
print(individual.primary_name)  # "John /Smith/"
print(individual.sex)           # "M" or "F"
print(individual.birth)         # GedcomEvent object
```

### GedcomFamily

Represents a family unit.

```python
family = database.families['@F1@']
print(family.husband)  # Individual XREF
print(family.wife)     # Individual XREF
print(family.children) # List of child XREFs
```

### GedcomEvent

Represents life events (birth, death, marriage, etc.).

```python
event = individual.birth
print(event.date)    # "15 MAR 1980"
print(event.place)   # "New York, NY"
```

## Validation

The module provides two levels of validation:

### Structure Validation

Checks GEDCOM format compliance.

```python
from lib.gedcom.validators import StructureValidator

validator = StructureValidator()
errors = validator.validate(database)
for error in errors:
    print(f"Line {error.line}: {error.message}")
```

### Semantic Validation

Checks data consistency and logic.

```python
from lib.gedcom.validators import SemanticValidator

validator = SemanticValidator()
warnings = validator.validate(database)
for warning in warnings:
    print(f"Warning: {warning.message}")
```

## Error Handling

The module provides comprehensive error handling:

```python
from lib.gedcom.exceptions import GedcomParseError, GedcomValidationError

try:
    database = parser.parse_file('invalid.ged')
except GedcomParseError as e:
    print(f"Parse error: {e.message}")
    print(f"Line: {e.line}")
except GedcomValidationError as e:
    print(f"Validation error: {e.message}")
```

## Export

Convert parsed data back to GEDCOM format:

```python
from lib.gedcom.exporter import GedcomExporter

exporter = GedcomExporter()
gedcom_content = exporter.export(database)
```

## Sample Files

The `ged/` directory contains sample GEDCOM files for testing:

- `sample.ged` - Small test file (3 individuals)
- `uk.ged` - Large test file (2,322 individuals)
- `names.ged` - Name variations testing
- `pres2020.ged` - Presidential genealogy

## Testing

Run the test suite:

```bash
python3 -m lib.gedcom.tests.test_parser
python3 -m lib.gedcom.tests.test_models
```

## API Reference

### Parser Functions

- `create_parser()` - Create a new GEDCOM parser
- `parse_file(path)` - Parse a GEDCOM file
- `parse_content(content)` - Parse GEDCOM content from string

### Validation Functions

- `StructureValidator.validate(database)` - Validate structure
- `SemanticValidator.validate(database)` - Validate semantics

### Export Functions

- `GedcomExporter.export(database)` - Export to GEDCOM format
