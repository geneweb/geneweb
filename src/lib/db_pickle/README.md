# Pickle Database Module

High-performance pickle-based storage system for genealogical data with advanced search capabilities.

## Overview

This module provides a complete database solution for genealogical data using Python's pickle serialization. It includes search indexes, data validation, and high-performance operations optimized for large datasets.

## Key Features

- **High Performance** - O(1) search operations with indexed lookups
- **Scalable** - Handles thousands of individuals efficiently
- **Search Indexes** - Fast name and content searches
- **Data Integrity** - Comprehensive validation and error handling
- **Pickle Storage** - Efficient serialization with optional compression

## Quick Start

```python
from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.io.writer import PickleWriter
from lib.db_pickle.io.reader import PickleReader

# Create database
data = PickleBaseData()

# Add persons
data.persons[1] = GenPerson(first_name="John", surname="Smith", sex=Sex.MALE)

# Build search indexes
data.build_indexes()

# Search
results = data.search_persons_by_surname("Smith")

# Save database
writer = PickleWriter()
writer.save_database(data, "family.pkl", compress=True)

# Load database
reader = PickleReader()
loaded_data = reader.load_database("family.pkl")
```

## Core Components

### Database (`database/`)

Core database classes and operations.

- `base_data.py` - Main database container with search indexes
- `base.py` - Database interface and operations
- `base_func.py` - Functional operations and queries

### Models (`models/`)

Data models for genealogical entities.

- `person.py` - Person records with events and relationships
- `family.py` - Family records and relationships
- `events.py` - Date and event handling
- `relations.py` - Family relationship structures

### I/O (`io/`)

Input/output operations for database persistence.

- `writer.py` - Save database to pickle files
- `reader.py` - Load database from pickle files

## Database Operations

### Creating and Managing Data

```python
from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.core.enums import Sex

# Create database
data = PickleBaseData()

# Add persons
data.persons[1] = GenPerson(
    first_name="John",
    surname="Smith",
    sex=Sex.MALE,
    birth=Date(year=1980, month=3, day=15)
)

# Add families
data.families[1] = GenFamily(relation=RelationKind.MARRIED)
data.couples[1] = GenCouple(father=1, mother=2)
data.descends[1] = GenDescend(children=[3, 4])
```

### Search Operations

```python
# Build search indexes
data.build_indexes()

# Search by first name (partial match)
johns = data.search_persons_by_first_name("John")

# Search by surname
smiths = data.search_persons_by_surname("Smith")

# Search by full name
john_smith = data.search_persons_by_full_name("John Smith")

# Search strings
places = data.search_strings_by_content("New York")
```

### Performance Features

The module provides several performance optimizations:

- **Search Indexes** - Pre-built indexes for fast lookups
- **Partial Matching** - Find "John" in "John Michael"
- **Case Insensitive** - Search works regardless of case
- **Batch Operations** - Efficient bulk operations

## Data Persistence

### Saving Data

```python
from lib.db_pickle.io.writer import PickleWriter

writer = PickleWriter()

# Save without compression
writer.save_database(data, "family.pkl")

# Save with compression
writer.save_database(data, "family.pkl.gz", compress=True)
```

### Loading Data

```python
from lib.db_pickle.io.reader import PickleReader

reader = PickleReader()

# Load database
data = reader.load_database("family.pkl")

# Check if compressed
if reader.is_compressed("family.pkl.gz"):
    data = reader.load_database("family.pkl.gz")
```

## Advanced Features

### Family Tree Operations

```python
# Find descendants
def find_descendants(data, person_id, max_generations=10):
    descendants = []
    to_process = [person_id]

    for generation in range(max_generations):
        current_generation = to_process.copy()
        to_process = []

        for person in current_generation:
            # Find families where this person is a parent
            for family_id, couple in data.couples.items():
                if couple.father == person or couple.mother == person:
                    if family_id in data.descends:
                        children = data.descends[family_id].children
                        descendants.extend(children)
                        to_process.extend(children)

    return descendants
```

### Data Validation

```python
# Validate data integrity
def validate_database(data):
    errors = []

    # Check for orphaned references
    for family_id, couple in data.couples.items():
        if couple.father not in data.persons:
            errors.append(f"Family {family_id} references non-existent father")
        if couple.mother not in data.persons:
            errors.append(f"Family {family_id} references non-existent mother")

    return errors
```

## Examples

See the `examples/` directory for comprehensive usage examples:

- `simple_search_example.py` - Basic search operations
- `descendants_search_example.py` - Advanced genealogical queries

## Performance Characteristics

- **Search Time** - O(1) for indexed searches
- **Memory Usage** - Efficient storage with pickle
- **Scalability** - Tested with 2,000+ individuals
- **Index Building** - Fast index construction

## API Reference

### Database Classes

- `PickleBaseData` - Main database container
- `PickleBase` - Database interface
- `PickleBaseFunc` - Functional operations

### Search Methods

- `search_persons_by_first_name(name)` - Search by first name
- `search_persons_by_surname(surname)` - Search by surname
- `search_persons_by_full_name(full_name)` - Search by full name
- `search_strings_by_content(content)` - Search strings

### I/O Classes

- `PickleWriter` - Save database to files
- `PickleReader` - Load database from files
