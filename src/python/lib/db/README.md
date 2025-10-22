# Database Save Structure

## Directory Layout

```
bases/
└── database_name.msgpack/
    ├── base                    # Main data file (MessagePack)
    ├── access                  # Access permissions (MessagePack)
    ├── persons                 # Person index (MessagePack)
    ├── families                # Family index (MessagePack)
    ├── strings                 # String index (MessagePack)
    ├── notes_d/                # Notes directory
    ├── wiznotes/               # Wizard notes directory
    └── metadata.json           # Database metadata (JSON)
```

## File Contents

### `base` - Main Data File

- **Format**: MessagePack binary
- **Content**: All genealogical data (persons, families, relations, strings)
- **Header**: Magic number, version, counts, timestamps

### `access` - Access File

- **Format**: MessagePack
- **Content**: Read/write permissions, user access levels

### `persons` - Person Index

- **Format**: MessagePack
- **Content**: Person data indexed by ID, name, surname, first name

### `families` - Family Index

- **Format**: MessagePack
- **Content**: Family data indexed by ID, marriage date, husband, wife

### `strings` - String Index

- **Format**: MessagePack
- **Content**: String data indexed by ID and content

### `notes_d/` - Notes Directory

- **Format**: Directory
- **Content**: Individual note files

### `wiznotes/` - Wizard Notes Directory

- **Format**: Directory
- **Content**: Wizard-generated note files

### `metadata.json` - Database Metadata

- **Format**: JSON
- **Content**: Database name, version, statistics, timestamps

## Data Serialization

### Person Object

```python
{
    "first_name": str,
    "surname": str,
    "occ": int,
    "sex": str,
    "birth": Date | None,
    "death": Date | None,
    "baptism": Date | None,
    "burial": Date | None,
    "titles": [Title],
    "events": [Event],
    "notes": str,
    "sources": str,
    "key_index": int
}
```

### Family Object

```python
{
    "marriage": Date | None,
    "marriage_place": str,
    "marriage_src": str,
    "marriage_note": str,
    "relation": str,
    "divorce": str,
    "divorce_date": Date | None,
    "divorce_place": str,
    "divorce_src": str,
    "divorce_note": str,
    "events": [Event],
    "notes": str,
    "sources": str,
    "fam_index": int
}
```

### Date Object

```python
{
    "year": int | None,
    "month": int | None,
    "day": int | None,
    "precision": str,
    "calendar": str
}
```

## Index Structure

### Name Indexes

- `first_name_index`: First name → [Person IDs]
- `surname_index`: Surname → [Person IDs]
- `full_name_index`: Full name → [Person IDs]
- `string_content_index`: Content → [String IDs]

### Relation Indexes

- `ascends`: Person ID → Ascendancy data
- `unions`: Person ID → Union data
- `couples`: Family ID → Couple data
- `descends`: Family ID → Descendancy data

## Usage

### Save Database

```python
writer = MessagePackWriter("bases")
db_path = writer.write_database(data, "database_name")
```

### Load Database

```python
reader = MessagePackReader("bases")
db = reader.load_database("database_name")
```

## Key Features

- **Modular**: Separate files for different data types
- **Indexed**: Fast search by name, date, relation
- **Compatible**: Similar to OCaml `.gwb` format
- **Secure**: MessagePack format (safer than Pickle)
- **Portable**: Cross-language compatibility
