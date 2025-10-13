# GED2GWB Conversion Tool

A Python implementation of the GeneWeb `ged2gwb` tool for converting GEDCOM files to GeneWeb pickle databases.

## Overview

This tool provides a complete conversion pipeline from GEDCOM 5.5.1 files to GeneWeb pickle databases, with support for all standard GEDCOM features and comprehensive error handling.

## Key Features

- **GEDCOM 5.5.1 Support** - Full compatibility with standard GEDCOM format
- **High Performance** - Efficient conversion with search index building
- **Comprehensive Validation** - Data integrity checks and warnings
- **Scalable** - Handles large files (tested with 2,000+ individuals)
- **CLI Interface** - Command-line tool with all standard options

## Quick Start

### Command Line Usage

```bash
# Basic conversion
python3 -m ged2gwb family.ged family

# With options
python3 -m ged2gwb family.ged family --compress --verbose

# Load existing database
python3 -m ged2gwb --load family.pkl
```

### Programmatic Usage

```python
from ged2gwb.converters.gedcom_to_geneweb import GedcomToGenewebConverter
from ged2gwb.utils.options import ConversionOptions

# Create conversion options
options = ConversionOptions(
    input_file="family.ged",
    output_file="family.pkl",
    compress=True,
    verbose=True
)

# Convert
converter = GedcomToGenewebConverter(options)
result = converter.convert()

# Access converted data
geneweb_data = result['geneweb_data']
print(f"Converted {geneweb_data.persons_count} persons")
```

## Command Line Options

### Basic Options

- `gedcom_file` - Input GEDCOM file path
- `database_name` - Output database name (optional with --load)

### Conversion Options

- `--compress` - Compress output with gzip
- `--force` - Overwrite existing files
- `--verbose` - Enable verbose output

### Charset Options

- `--charset CHARSET` - Input charset (UTF-8, ANSEL, etc.)
- `--dates-dm` - Use DD/MM date format
- `--efn` - Extract first names
- `--epn` - Extract public names

### Validation Options

- `--no-consistency-check` - Skip consistency checks
- `--lf` - Check living flag
- `--ls` - Check living status

### Other Options

- `--load` - Load and display existing database
- `--base-dir DIR` - Set base directory
- `--reorg` - Reorganize data

## Conversion Process

The conversion process includes several stages:

1. **Parse GEDCOM** - Parse input file with validation
2. **Convert Data** - Transform GEDCOM structures to GeneWeb models
3. **Build Indexes** - Create search indexes for fast lookups
4. **Validate** - Check data consistency and report warnings
5. **Save** - Serialize to pickle format with optional compression

### Example Output

```
*** saving persons array
*** saving ascends array
*** saving unions array
*** saving families array
*** saving couples array
*** saving descends array
*** saving strings array
*** create name index
*** create strings of sname
*** create strings of fname
*** create string index
*** create surname index
*** create first name index
*** ok
```

## Data Conversion

### Individual Records

GEDCOM individuals are converted to `GenPerson` objects:

```python
# GEDCOM individual
@I1@ INDI
1 NAME John /Smith/
1 SEX M
1 BIRT
2 DATE 15 MAR 1980

# Converted to GenPerson
person = GenPerson(
    first_name="John",
    surname="Smith",
    sex=Sex.MALE,
    birth=Date(year=1980, month=3, day=15)
)
```

### Family Records

GEDCOM families are converted to `GenFamily` and relationship objects:

```python
# GEDCOM family
@F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@

# Converted to GeneWeb structures
family = GenFamily(relation=RelationKind.MARRIED)
couple = GenCouple(father=1, mother=2)
descend = GenDescend(children=[3])
```

## Error Handling

The tool provides comprehensive error handling:

### Validation Warnings

- Sex inconsistencies
- Death before baptism
- Advanced age at death
- Family structure issues

### Error Reporting

- File not found errors
- Invalid GEDCOM format
- Database name validation
- Permission errors

## Performance

### Benchmarks

- **Small files** (3 individuals): ~0.0007s
- **Large files** (2,322 individuals): ~0.2s
- **Search operations**: O(1) with indexes
- **Memory usage**: Efficient with pickle serialization

### Scalability

The tool has been tested with:

- 3 individuals (sample.ged)
- 2,322 individuals (uk.ged)
- 1,115 families
- 91 sources

## Examples

See the `examples/` directory for comprehensive examples:

- `test_with_uk_ged.py` - Test with large GEDCOM file
- `compare_files.py` - Performance comparison between files

## Testing

Run the test suite:

```bash
python3 -m lib.ged2gwb.tests.run_tests
```

## API Reference

### Core Classes

- `Ged2GwbConverter` - Main conversion class
- `GedcomToGenewebConverter` - GEDCOM to GeneWeb converter
- `ConversionOptions` - Conversion configuration

### CLI Interface

- `Ged2GwbCLI` - Command-line interface
- `main()` - Entry point function

### Utility Functions

- `validate_database_name(name)` - Validate output names
- `build_indexes(data)` - Build search indexes
