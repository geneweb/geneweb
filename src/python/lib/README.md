# GeneWeb Python Library

A comprehensive Python implementation of the GeneWeb genealogical database system, providing GEDCOM parsing, data conversion, and high-performance search capabilities.

## Overview

This library consists of four main modules that work together to provide a complete genealogical data management solution:

- **`gedcom/`** - GEDCOM 5.5.1 parsing and validation
- **`db_pickle/`** - Pickle-based database storage with search indexes
- **`ged2gwb/`** - GEDCOM to GeneWeb conversion tool

## Quick Start

```python
from lib.gedcom.parser import create_parser
from lib.ged2gwb.converters.gedcom_to_geneweb import GedcomToGenewebConverter
from lib.ged2gwb.utils.options import ConversionOptions

# Parse GEDCOM file
parser = create_parser()
gedcom_db = parser.parse_file('family.ged')

# Convert to GeneWeb format
options = ConversionOptions(input_file='family.ged')
converter = GedcomToGenewebConverter(options)
result = converter.convert(gedcom_db)

# Use search indexes
geneweb_data = result['geneweb_data']
persons = geneweb_data.search_persons_by_surname("Smith")
```

## Module Documentation

### [GEDCOM Module](gedcom/README.md)

Parse and validate GEDCOM 5.5.1 files with comprehensive error handling.

### [Pickle Database Module](db_pickle/README.md)

High-performance pickle-based storage with search indexes and fast lookups.

### [GED2GWB Tool](ged2gwb/README.md)

Command-line tool for converting GEDCOM files to GeneWeb pickle databases.

## Features

- **GEDCOM 5.5.1 Compliance** - Full support for standard GEDCOM format
- **High Performance** - O(1) search operations with indexed lookups
- **Scalable** - Handles thousands of individuals efficiently
- **Type Safe** - Comprehensive type hints and validation
- **Extensible** - Modular architecture for easy customization

## Requirements

- Python 3.8+
- No external dependencies (pure Python implementation)

## License

This project is part of the GeneWeb ecosystem and follows the same licensing terms.
