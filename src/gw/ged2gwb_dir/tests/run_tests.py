#!/usr/bin/env python3
"""
Main test runner for GED2GWB module.

Runs all tests and provides a comprehensive test report.
"""

import sys
import time
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

def run_test_suite():
    """Run the complete test suite."""
    print("GED2GWB Test Suite")
    print("=" * 50)
    print()

    start_time = time.time()

    # Test results
    results = {
        'options': False,
        'conversion': False,
        'integration': False,
        'concrete': False
    }

    # Run option tests
    print("1. Running Options Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.test_options import main as test_options
        results['options'] = test_options() == 0
    except Exception as e:
        print(f"ERROR: Options tests failed with exception: {e}")
        results['options'] = False

    print()

    # Run conversion tests
    print("2. Running Conversion Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.test_conversion import main as test_conversion
        results['conversion'] = test_conversion() == 0
    except Exception as e:
        print(f"ERROR: Conversion tests failed with exception: {e}")
        results['conversion'] = False

    print()

    # Run integration tests
    print("3. Running Integration Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.test_integration import main as test_integration
        results['integration'] = test_integration() == 0
    except Exception as e:
        print(f"ERROR: Integration tests failed with exception: {e}")
        results['integration'] = False

    print()

    # Run concrete tests
    print("4. Running Concrete Tests...")
    print("-" * 30)
    try:
        from lib.ged2gwb.tests.test_concrete import main as test_concrete
        results['concrete'] = test_concrete() == 0
    except Exception as e:
        print(f"ERROR: Concrete tests failed with exception: {e}")
        results['concrete'] = False

    # Calculate results
    end_time = time.time()
    duration = end_time - start_time

    passed = sum(results.values())
    total = len(results)

    print()
    print("Test Results Summary")
    print("=" * 50)
    print(f"Duration: {duration:.2f} seconds")
    print()

    for test_name, passed_test in results.items():
        status = "PASS" if passed_test else "FAIL"
        print(f"{test_name.capitalize():12} {status}")

    print()
    print(f"Overall: {passed}/{total} test suites passed")

    if passed == total:
        print("SUCCESS: All tests passed! GED2GWB is ready for use.")
        return 0
    else:
        print("FAILURE: Some tests failed. Please check the implementation.")
        return 1

def main():
    """Main test runner entry point."""
    return run_test_suite()

if __name__ == '__main__':
    sys.exit(main())
