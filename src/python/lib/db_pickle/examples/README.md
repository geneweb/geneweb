# Search Examples for db_pickle

This directory contains examples demonstrating how to use the search functionality in `db_pickle`.

## Examples

### 1. Simple Search (`simple_search_example.py`)

Basic demonstration of search indexes:

- Search by first name
- Search by surname
- Search by full name
- Search strings by content
- Case-insensitive search

```bash
python3 src/lib/db_pickle/examples/simple_search_example.py
```

### 2. Descendants Search (`descendants_search_example.py`)

Advanced genealogical queries:

- Find descendants of a person
- Find ancestors of a person
- Build family trees
- Complex genealogical queries
- Performance comparisons

```bash
python3 src/lib/db_pickle/examples/descendants_search_example.py
```

### 3. GEDCOM Conversion Tests (`../ged2gwb/examples/`)

Complete workflow examples:

- `test_with_uk_ged.py`: Test with large GEDCOM file (2,322 persons)
- `compare_files.py`: Compare performance between small and large files

```bash
python3 src/lib/ged2gwb/examples/test_with_uk_ged.py
python3 src/lib/ged2gwb/examples/compare_files.py
```

## Search Methods

### Person Search

```python
# Search by first name (case-insensitive)
persons = data.search_persons_by_first_name("Jean")

# Search by surname (case-insensitive)
persons = data.search_persons_by_surname("Dupont")

# Search by full name (case-insensitive)
persons = data.search_persons_by_full_name("Jean Dupont")
```

### String Search

```python
# Search strings by content (case-insensitive)
strings = data.search_strings_by_content("Paris")
```

### Building Indexes

```python
# Build all search indexes
data.build_indexes()

# Build indexes without verbose output
data.build_indexes(verbose=False)
```

## Performance

The search indexes provide significant performance improvements:

- **Linear search**: O(n) - slow for large datasets
- **Indexed search**: O(1) - instant lookup
- **Typical improvement**: 10-100x faster depending on dataset size

## Index Types

1. **`first_name_index`**: Maps first names to person IDs
2. **`surname_index`**: Maps surnames to person IDs
3. **`full_name_index`**: Maps full names to person IDs
4. **`string_content_index`**: Maps string content to string IDs

All indexes are case-insensitive and automatically maintained when `build_indexes()` is called.
