# GED2GWB Test Suite

This directory contains a comprehensive test suite for the ged2gwb tool

## Test Structure

The test suite is organized into several categories:

### 1. Unit Tests

- **`test_unit_converter.py`**: Tests for the core `Ged2GwbConverter` class
- **`test_unit_options.py`**: Tests for the `ConversionOptions` class and argument parsing

### 2. Regression Tests

- **`test_regression.py`**: Tests to ensure previously working functionality continues to work
- Verifies that bug fixes remain effective
- Tests basic conversion, charset handling, name processing, and error handling

### 3. Integration Tests

- **`test_integration.py`**: Tests that verify all components work together correctly
- Tests complete workflows, option combinations, and realistic data scenarios

### 4. Option Verification Tests

- **`test_option_verification.py`**: Detailed verification of each command-line option
- Tests individual options and their combinations
- Verifies that options produce expected results with detailed assertions

### 5. Performance Tests

- **`test_performance.py`**: Tests for performance characteristics
- Tests with different file sizes (small, medium, large)
- Tests compression performance and memory usage
- Tests concurrent conversions and error handling performance

## Running Tests

### Run All Tests

```bash
cd src/python
python3 -m ged2gwb.tests.run_tests
```

### Run Individual Test Suites

```bash
# Unit tests
python3 -m ged2gwb.tests.test_unit_converter
python3 -m ged2gwb.tests.test_unit_options

# Regression tests
python3 -m ged2gwb.tests.test_regression

# Integration tests
python3 -m ged2gwb.tests.test_integration

# Option verification tests
python3 -m ged2gwb.tests.test_option_verification

# Performance tests
python3 -m ged2gwb.tests.test_performance
```

## Test Features

### Professional Test Structure

- All tests are written in English
- Clear, descriptive test names and documentation
- Proper setup and teardown methods
- Comprehensive error handling

### Detailed Verification

- Each option is tested individually
- Option combinations are tested
- Results are verified with detailed assertions
- Performance characteristics are measured

### Test Data Management

- Tests use temporary directories
- Test data is cleaned up after each test
- Realistic GEDCOM content is generated for testing

### Error Testing

- Invalid input handling
- Error message verification
- Performance under error conditions

## Test Categories

### Unit Tests

- Test individual components in isolation
- Mock dependencies where appropriate
- Fast execution
- High coverage of individual functions

### Integration Tests

- Test complete workflows
- Test component interactions
- Test with realistic data
- Verify end-to-end functionality

### Regression Tests

- Ensure existing functionality continues to work
- Test previously fixed bugs
- Verify backward compatibility
- Test with various data sizes

### Option Verification Tests

- Test each command-line option individually
- Test option combinations
- Verify expected behavior
- Test edge cases and error conditions

### Performance Tests

- Test with different data sizes
- Measure execution time
- Test memory usage
- Test concurrent operations
- Test compression performance

## Test Results

Each test suite provides:

- Pass/fail status for each test
- Execution time
- Detailed error messages for failures
- Summary statistics

## Dependencies

The test suite requires:

- Python 3.7+
- `psutil` (for memory usage testing)
- Standard library modules (tempfile, pickle, time, etc.)

## Contributing

When adding new tests:

1. Follow the existing naming conventions
2. Use descriptive test names
3. Include proper documentation
4. Ensure tests are isolated and repeatable
5. Add appropriate assertions
6. Clean up test data after execution
