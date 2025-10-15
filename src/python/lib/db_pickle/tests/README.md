# db_pickle Test Suite

Comprehensive test suite for the db_pickle module, covering all aspects of the pickle-based genealogical database system.

## Test Structure

### Test Files

- **`test_models.py`** - Tests for data models (GenPerson, GenFamily, GenCouple, GenDescend, Date)
- **`test_database.py`** - Tests for database operations (PickleBaseData)
- **`test_io.py`** - Tests for I/O operations (PickleWriter, PickleReader)
- **`test_search.py`** - Tests for search functionality
- **`test_performance.py`** - Performance and scalability tests
- **`test_integration.py`** - End-to-end integration tests
- **`conftest.py`** - Pytest fixtures and configuration
- **`run_tests.py`** - Test runner script

### Test Categories

#### 1. Model Tests (`test_models.py`)

- Person creation and validation
- Family relationship models
- Date handling and validation
- Model equality and comparison
- Edge cases and error handling

#### 2. Database Tests (`test_database.py`)

- Database creation and initialization
- Data addition and retrieval
- Search index building
- Search operations (by name, surname, full name)
- Database validation
- Statistics calculation
- Database copying and clearing

#### 3. I/O Tests (`test_io.py`)

- Saving databases (compressed and uncompressed)
- Loading databases
- File format detection
- Error handling for corrupted files
- Directory creation
- Large dataset I/O operations

#### 4. Search Tests (`test_search.py`)

- Exact and partial name matching
- Case insensitive searches
- Special character handling
- Performance with large datasets
- Empty database searches
- String content searches

#### 5. Performance Tests (`test_performance.py`)

- Large dataset creation performance
- Index building performance
- Search performance benchmarks
- Memory usage optimization
- Concurrent operation performance
- Database copy performance

#### 6. Integration Tests (`test_integration.py`)

- Complete genealogical workflows
- Save/load roundtrip testing
- Large dataset workflows
- Error handling workflows
- Multiple database instances
- Database copy and modification

## Running Tests

### Run All Tests

```bash
cd src/python/lib/db_pickle/tests
python run_tests.py
```

### Run Specific Test Files

```bash
# Model tests
python -m pytest test_models.py -v

# Database tests
python -m pytest test_database.py -v

# I/O tests
python -m pytest test_io.py -v

# Search tests
python -m pytest test_search.py -v

# Performance tests
python -m pytest test_performance.py -v

# Integration tests
python -m pytest test_integration.py -v
```

### Run with Coverage

```bash
python -m pytest . --cov=../db_pickle --cov-report=html
```

## Test Fixtures

### Available Fixtures

- **`sample_person_data`** - Sample person data for testing
- **`sample_family_data`** - Sample family data for testing
- **`empty_database`** - Empty database instance
- **`populated_database`** - Database with sample data
- **`large_database`** - Database with 100 persons for performance testing
- **`temp_file`** - Temporary file for I/O testing
- **`temp_compressed_file`** - Temporary compressed file for I/O testing

### Using Fixtures

```python
def test_example(populated_database, temp_file):
    """Example test using fixtures."""
    db = populated_database
    # Test with populated database

    # Use temp_file for I/O operations
    writer = PickleWriter()
    writer.save_database(db, temp_file)
```

## Test Data

### Sample Data Structure

The test suite uses a consistent sample dataset:

- **4 Persons**: John Smith, Jane Smith, Alice Smith, Bob Smith
- **1 Family**: Married couple with 2 children
- **Relationships**: Father-Mother-Children structure
- **Dates**: Birth dates for all persons
- **Search Data**: Various name combinations for testing

### Large Dataset

Performance tests use a dataset with:

- **100 Persons**: Named Person0-Person99
- **10 Surnames**: Surname0-Surname9 (10 persons each)
- **Mixed Genders**: Alternating male/female
- **Birth Dates**: 1900-1999 range

## Assertions and Validations

### Common Assertions

```python
# Data integrity
assert len(db.persons) == expected_count
assert person.first_name == "John"
assert person.sex == Sex.MALE

# Search results
results = db.search_persons_by_surname("Smith")
assert len(results) == expected_count
assert person_id in results

# I/O operations
assert os.path.exists(file_path)
assert reader.is_compressed(file_path) == expected

# Performance
assert duration < max_duration
assert memory_usage < max_memory
```

### Validation Tests

- **Data Integrity**: All references are valid
- **Search Accuracy**: Results match expected criteria
- **I/O Reliability**: Save/load preserves all data
- **Performance**: Operations complete within time limits
- **Error Handling**: Appropriate errors for invalid inputs

## Continuous Integration

The test suite is designed to run in CI environments:

- **Fast Execution**: Most tests complete in < 1 second
- **Deterministic**: No random data or timing dependencies
- **Isolated**: Tests don't interfere with each other
- **Comprehensive**: Covers all major functionality
- **Maintainable**: Clear structure and documentation

## Debugging Tests

### Verbose Output

```bash
python -m pytest test_database.py -v -s
```

### Specific Test

```bash
python -m pytest test_database.py::TestPickleBaseData::test_add_person -v
```

### Debug Mode

```bash
python -m pytest test_database.py --pdb
```

### Coverage Report

```bash
python -m pytest . --cov=../db_pickle --cov-report=term-missing
```

## Test Maintenance

### Adding New Tests

1. **Choose appropriate test file** based on functionality
2. **Use existing fixtures** when possible
3. **Follow naming convention**: `test_<functionality>`
4. **Add docstring** explaining test purpose
5. **Include both positive and negative cases**

### Updating Tests

1. **Update assertions** when behavior changes
2. **Add new test cases** for new features
3. **Remove obsolete tests** for deprecated functionality
4. **Update fixtures** if data structure changes
