
# GeneWeb Python Implementation

[![Python](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![License](https://img.shields.io/badge/license-MIT-green.svg)](LICENSE)

Complete Python implementation of the GeneWeb genealogical database system. This package provides a robust API for managing genealogical data with support for complex family relationships, events, and searches.

## 📚 Table of Contents

- [Features](#features)
- [Installation](#installation)
- [Quick Start](#quick-start)
- [Architecture](#architecture)
- [Core Components](#core-components)
- [Usage Examples](#usage-examples)
- [API Reference](#api-reference)
- [Contributing](#contributing)

---

## ✨ Features

- **Complete Type System**: Strongly-typed indexed IDs (Iper, Ifam, Istr)
- **Rich Data Models**: Person, Family, Events, Dates, Titles, Relations
- **Flexible Collections**: Functional collection API with map, fold, filter operations
- **Name Processing**: Advanced name normalization and search utilities
- **Database Operations**: Full CRUD operations with patching support
- **Search Indexing**: Fast binary search for persons and families
- **GEDCOM Support**: Import/Export genealogical data
- **High-Level Wrappers**: Convenient Person and Family wrapper classes

---

## 📦 Installation

```bash
# Clone the repository
git clone https://github.com/yourusername/geneweb-python.git
cd geneweb-python

# Install in development mode
pip install -e .

# Or install dependencies
pip install -r requirements.txt
```

---

## 🚀 Quick Start

```python
from geneweb import (
    GenPerson, GenFamily, GenCouple, GenDescend,
    Sex, RelationKind, Date
)

# Create a person
person = GenPerson(
    first_name=1,
    surname=2,
    occ=0,
    sex=Sex.MALE,
    birth=Date(year=1980, month=3, day=15),
    key_index=100
)

# Create a family
family = GenFamily(
    marriage=Date(year=2005, month=6, day=20),
    relation=RelationKind.MARRIED,
    fam_index=50
)

# Create relationships
couple = GenCouple(father=100, mother=101)
children = GenDescend(children=[102, 103, 104])

print(f"Person: {person.sex.value}, born {person.birth.year}")
print(f"Family: {len(children.children)} children")
```

---

## 🏗️ Architecture

```
geneweb/
├── core/                   # Core types and enumerations
│   ├── types.py           # Iper, Ifam, Istr
│   ├── enums.py           # Sex, Access, RelationKind, etc.
│   └── indexed.py         # Indexed protocol
│
├── models/                 # Data models
│   ├── person.py          # GenPerson
│   ├── family.py          # GenFamily
│   ├── relations.py       # GenAscend, GenUnion, etc.
│   └── events.py          # Date, Event, Title
│
├── collections/            # Collection utilities
│   ├── collection.py      # Collection class
│   └── marker.py          # Marker class
│
├── database/               # Database layer
│   ├── base.py            # Base class
│   ├── base_data.py       # BaseData
│   ├── base_func.py       # BaseFunc
│   ├── record_access.py   # RecordAccess
│   └── search_index.py    # SearchIndex
│
├── wrappers/               # High-level wrappers
│   ├── person.py          # Person wrapper
│   └── family.py          # Family wrapper
│
├── utils/                  # Utilities
│   ├── names.py           # Name processing
│   ├── binary_search.py   # Search algorithms
│   ├── cache.py           # Caching
│   └── validation.py      # Validation
│
├── io/                     # Input/Output
│   ├── file_reader.py     # File reading
│   ├── file_writer.py     # File writing
│   ├── file_manager.py    # File management
│   └── gedcom_parser.py   # GEDCOM parser
│
├── operations/             # Database operations
│   ├── creation.py        # Create operations
│   ├── deletion.py        # Delete operations
│   └── search.py          # Search operations
│
└── exceptions/             # Custom exceptions
    ├── errors.py          # Error classes
    └── validation_exceptions.py
```

---

## 🔧 Core Components

### 1. Indexed Types

```python
from geneweb import Iper, Ifam, Istr

# Person ID
person_id = 42
is_valid = not Iper.is_dummy(person_id)

# Family ID
family_id = 10
dummy_family = Ifam.dummy()

# String ID
string_id = 5
empty_string = Istr.empty()
```

### 2. Enumerations

```python
from geneweb import Sex, Access, DeathType, RelationKind

# Sex
sex = Sex.MALE  # MALE, FEMALE, NEUTER

# Access level
access = Access.PUBLIC  # PUBLIC, PRIVATE, FRIEND

# Death status
death = DeathType.DEAD  # NOT_DEAD, DEAD, DEAD_YOUNG, etc.

# Relation type
relation = RelationKind.MARRIED  # MARRIED, NOT_MARRIED, ENGAGED, etc.
```

### 3. Data Models

#### Person

```python
from geneweb import GenPerson, Sex, Date, Access

person = GenPerson(
    first_name=1,           # String ID
    surname=2,              # String ID
    occ=0,                  # Occurrence number
    sex=Sex.MALE,
    birth=Date(year=1980, month=3, day=15),
    birth_place=3,
    death=Date(year=2050),
    access=Access.PUBLIC,
    occupation=4,
    image=5,
    key_index=100
)
```

#### Family

```python
from geneweb import GenFamily, RelationKind, DivorceStatus, Date

family = GenFamily(
    marriage=Date(year=2005, month=6, day=20),
    marriage_place=10,
    marriage_note=11,
    marriage_src=12,
    relation=RelationKind.MARRIED,
    divorce=DivorceStatus.NOT_DIVORCED,
    witnesses=[],
    fam_index=50
)
```

#### Relations

```python
from geneweb import GenAscend, GenUnion, GenCouple, GenDescend, Relation

# Parents
ascend = GenAscend(parents=10, consang=0.0)

# Families as parent
union = GenUnion(family=[50, 51])

# Father and mother
couple = GenCouple(father=100, mother=101)

# Children
descend = GenDescend(children=[102, 103, 104])

# Adoptive parents
adoption = Relation(
    father=200,
    mother=201,
    source=10,
    relation_type="rparent_adoption"
)
```

#### Dates and Events

```python
from geneweb import Date, Event, Title

# Date with precision
exact_date = Date(year=1990, month=5, day=15, precision="exact")
about_date = Date(year=1990, precision="about")
before_date = Date(year=2000, precision="before")

# Event
baptism = Event(
    name="Baptism",
    date=exact_date,
    place=15,
    note=20,
    witnesses=[(300, "witness"), (301, "godfather")]
)

# Title
title = Title(
    name=30,
    title="Duke",
    place=40,
    date_start=Date(year=1800),
    date_end=Date(year=1850),
    nth=3
)
```

### 4. Collections

```python
from geneweb import Collection, Marker

# Create collection
data = [1, 2, 3, 4, 5]
collection = Collection(len(data), lambda i: data[i])

# Iterate
collection.iter(lambda x: print(x))

# Fold (reduce)
total = collection.fold(lambda acc, x: acc + x, 0)
product = collection.fold(lambda acc, x: acc * x, 1)

# Map
squared = collection.map(lambda x: x ** 2)

# Marker for tracking
marker = Marker(lambda x: x, collection, False)
marker.set(0, True)
is_marked = marker.get(0)
```

### 5. Name Utilities

```python
from geneweb import NameUtils

name = "Jean-François de La Fontaine"

# Normalize (lowercase, keep accents)
normalized = NameUtils.normalize(name)
# Output: "jean-françois de la fontaine"

# Crush (remove accents, special chars)
crushed = NameUtils.crush_lower(name)
# Output: "jeanfrancoisdelafontaine"

# Split first names
first_names = NameUtils.split_fname(name)
# Output: ["jean", "francois"]

# Split surnames
surnames = NameUtils.split_sname("de La Fontaine")
# Output: ["de", "la", "fontaine"]

# Name index for sorting
index = NameUtils.name_index(name)
```

### 6. Database Operations

```python
from geneweb import Base, BaseData, BaseFunc
from pathlib import Path

# Open database
base = Base(Path("/path/to/database"))

# Access data
persons = base.data.persons
families = base.data.families

# Get person by ID
person = persons.get(100)

# Get family by ID
family = families.get(50)

# Search by name
results = base.func.persons_of_name("Doe")

# Get person by key
person = base.func.person_of_key(1, 2, 0)  # first_name, surname, occ

# Commit changes
base.commit_patches()
```

### 7. High-Level Wrappers

```python
from geneweb import Person, Family, Base

base = Base(Path("/path/to/database"))

# Person wrapper
person = Person(base, 100)

# Access properties
name = person.get_first_name()
surname = person.get_surname()
birth = person.get_birth()
sex = person.get_sex()

# Get relations
parents = person.get_parents()
children = person.get_children()
spouses = person.get_spouses()

# Family wrapper
family = Family(base, 50)

# Access properties
father = family.get_father()
mother = family.get_mother()
children = family.get_children()
marriage = family.get_marriage()
```

### 8. Binary Search

```python
from geneweb import BinarySearch

data = [1, 3, 5, 7, 9, 11, 13, 15]

# Find first element >= value
index = BinarySearch.lower_bound(data, 8, lambda x: x)
# Returns: 4 (element 9)

# Find first element matching predicate
result = BinarySearch.find_first(data, lambda x: x > 10)
# Returns: Some(11)

# Find range of matching elements
start, end = BinarySearch.find_range(data, lambda x: 5 <= x <= 11)
# Returns: (2, 5)
```

---

## 📖 Usage Examples

### Example 1: Build a Family Tree

```python
from geneweb import (
    GenPerson, GenFamily, GenCouple, GenDescend, GenAscend, GenUnion,
    Sex, RelationKind, Date
)

# Create grandparents
grandfather = GenPerson(
    first_name=1, surname=2, occ=0,
    sex=Sex.MALE,
    birth=Date(year=1920),
    key_index=1
)

grandmother = GenPerson(
    first_name=3, surname=4, occ=0,
    sex=Sex.FEMALE,
    birth=Date(year=1922),
    key_index=2
)

# Create parents family
parents_family = GenFamily(
    marriage=Date(year=1945),
    relation=RelationKind.MARRIED,
    fam_index=1
)

parents_couple = GenCouple(father=1, mother=2)
parents_children = GenDescend(children=[3, 4])

# Create father
father = GenPerson(
    first_name=5, surname=2, occ=0,
    sex=Sex.MALE,
    birth=Date(year=1950),
    key_index=3
)

father_ascend = GenAscend(parents=1)  # Points to parents_family
father_union = GenUnion(family=[2])   # His own family

# Create child
child = GenPerson(
    first_name=7, surname=2, occ=0,
    sex=Sex.MALE,
    birth=Date(year=1980),
    key_index=5
)

child_ascend = GenAscend(parents=2)  # Points to father's family
```

### Example 2: Search and Filter

```python
from geneweb import Base, NameUtils, Collection
from pathlib import Path

base = Base(Path("/path/to/database"))

# Search by name
surname = "Doe"
results = base.func.persons_of_name(surname)

# Filter by birth year
persons = base.data.persons
birth_year = 1980

matches = []
def check_birth(i):
    person = persons.get(i)
    if person and person.birth and person.birth.year == birth_year:
        matches.append(i)

Collection(
    persons.length(),
    lambda i: i
).iter(check_birth)

print(f"Found {len(matches)} persons born in {birth_year}")
```

### Example 3: Name Processing

```python
from geneweb import NameUtils

# Process various name formats
names = [
    "Jean-François de La Fontaine",
    "Mary O'Brien-Smith",
    "José María García López",
]

for name in names:
    print(f"Original: {name}")
    print(f"Normalized: {NameUtils.normalize(name)}")
    print(f"Crushed: {NameUtils.crush_lower(name)}")
    print(f"Index: {NameUtils.name_index(name)}")
    print()
```

### Example 4: Import from GEDCOM

```python
from geneweb import GedcomParser
from pathlib import Path

# Parse GEDCOM file
parser = GedcomParser()
data = parser.parse_file(Path("family.ged"))

# Access parsed data
for person in data.persons:
    print(f"{person.first_name} {person.surname}")
    if person.birth:
        print(f"  Born: {person.birth.year}")
```

---

## 📚 API Reference

### Core Types

- **Iper**: Person index type
  - `dummy() -> int`: Get dummy value
  - `is_dummy(i: int) -> bool`: Check if dummy
  - `to_string(i: int) -> str`: Convert to string

- **Ifam**: Family index type (same methods as Iper)
- **Istr**: String index type (same methods plus `empty()`)

### Enumerations

- **Sex**: MALE, FEMALE, NEUTER
- **Access**: PUBLIC, PRIVATE, FRIEND
- **DeathType**: NOT_DEAD, DEAD, DEAD_YOUNG, DEAD_DONT_KNOW_WHEN, DONT_KNOW_IF_DEAD
- **BurialType**: UNKNOWN, BURIED, CREMATED
- **RelationKind**: MARRIED, NOT_MARRIED, ENGAGED, NO_SEXES_CHECK_NOT_MARRIED, NO_MENTION
- **DivorceStatus**: NOT_DIVORCED, DIVORCED, SEPARATED

### Data Models

All models are dataclasses with type-safe fields. See individual model documentation for complete field lists.

### Collections

- **Collection[T]**: Generic collection type
  - `get(i: int) -> Optional[T]`
  - `iter(f: Callable[[T], None]) -> None`
  - `fold(f: Callable[[A, T], A], init: A) -> A`
  - `map(f: Callable[[T], U]) -> Collection[U]`

- **Marker[K, V]**: Boolean marker for collections
  - `get(key: K) -> bool`
  - `set(key: K, value: bool) -> None`

### Utilities

- **NameUtils**: Name processing
  - `normalize(name: str) -> str`
  - `crush_lower(name: str) -> str`
  - `split_fname(name: str) -> List[str]`
  - `split_sname(name: str) -> List[str]`
  - `name_index(name: str) -> int`

- **BinarySearch**: Search algorithms
  - `lower_bound(arr: List[T], value: U, key: Callable) -> int`
  - `find_first(arr: List[T], pred: Callable) -> Optional[T]`
  - `find_range(arr: List[T], pred: Callable) -> Tuple[int, int]`

---

## 🧪 Testing

```bash
# Run all tests
python test.py

# Run demo
python demo.py

# Generate test report
python test.py > test_results.txt
```

---

## 🤝 Contributing

Contributions are welcome! Please:

1. Fork the repository
2. Create a feature branch
3. Add tests for new features
4. Ensure all tests pass
5. Submit a pull request

---

## 📄 License

MIT License - see LICENSE file for details

---

## 📞 Support

- **Issues**: [GitHub Issues](https://github.com/yourusername/geneweb-python/issues)
- **Documentation**: [Full Documentation](https://geneweb-python.readthedocs.io)
- **Community**: [Discussion Forum](https://github.com/yourusername/geneweb-python/discussions)

---

## 🙏 Acknowledgments

- Original GeneWeb project by Daniel de Rauglaudre
- OCaml GeneWeb implementation contributors
- Python genealogy community

---

**Version**: 1.0.0
**Last Updated**: 2024
**Author**: Your Name
