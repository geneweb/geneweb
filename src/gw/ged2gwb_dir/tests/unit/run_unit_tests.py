#!/usr/bin/env python3
"""
Unit test runner for GED2GWB module.

Runs all unit tests and provides a comprehensive report.
"""

import sys
import time
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent))

def run_unit_tests():
    """Run all unit tests."""
    print("GED2GWB Unit Tests")
    print("==================")
    print()

    start_time = time.time()

    # Test results
    results = {
        'cli': False,
        'converter': False,
        'options': False
    }

    # Run CLI tests
    print("1. Running CLI Unit Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.unit.test_cli import main as test_cli
        results['cli'] = test_cli() == 0
    except Exception as e:
        print(f"ERROR: CLI unit tests failed with exception: {e}")
        results['cli'] = False

    print()

    # Run Converter tests
    print("2. Running Converter Unit Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.unit.test_converter import main as test_converter
        results['converter'] = test_converter() == 0
    except Exception as e:
        print(f"ERROR: Converter unit tests failed with exception: {e}")
        results['converter'] = False

    print()

    # Run Options tests
    print("3. Running Options Unit Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.unit.test_options import main as test_options
        results['options'] = test_options() == 0
    except Exception as e:
        print(f"ERROR: Options unit tests failed with exception: {e}")
        results['options'] = False

    # Calculate results
    end_time = time.time()
    duration = end_time - start_time

    passed = sum(results.values())
    total = len(results)

    print()
    print("Unit Test Results Summary")
    print("=" * 50)
    print(f"Duration: {duration:.2f} seconds")
    print()

    for test_name, passed_test in results.items():
        status = "PASS" if passed_test else "FAIL"
        print(f"{test_name.capitalize():12} {status}")

    print()
    print(f"Overall: {passed}/{total} test suites passed")

    if passed == total:
        print("SUCCESS: All unit tests passed! GED2GWB components are working correctly.")
        return 0
    else:
        print("FAILURE: Some unit tests failed. Please check the implementation.")
        return 1

def main():
    """Main unit test runner entry point."""
    return run_unit_tests()

if __name__ == '__main__':
    sys.exit(main())
