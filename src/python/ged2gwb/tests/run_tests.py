#!/usr/bin/env python3
"""
Main test runner for ged2gwb.

This script executes all test suites in a professional manner.
"""

import sys
import os
import time
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))


def run_unit_converter_tests():
    """Run unit tests for the converter."""
    print("=== Unit Tests - Converter ===")

    try:
        from ged2gwb.tests.test_unit_converter import run_unit_tests

        return run_unit_tests()
    except Exception as e:
        print(f"ERROR: Failed to run unit converter tests: {e}")
        return 1


def run_unit_options_tests():
    """Run unit tests for the options."""
    print("=== Unit Tests - Options ===")

    try:
        from ged2gwb.tests.test_unit_options import run_unit_tests

        return run_unit_tests()
    except Exception as e:
        print(f"ERROR: Failed to run unit options tests: {e}")
        return 1


def run_regression_tests():
    """Run regression tests."""
    print("=== Regression Tests ===")

    try:
        from ged2gwb.tests.test_regression import run_regression_tests

        return run_regression_tests()
    except Exception as e:
        print(f"ERROR: Failed to run regression tests: {e}")
        return 1


def run_integration_tests():
    """Run integration tests."""
    print("=== Integration Tests ===")

    try:
        from ged2gwb.tests.test_integration import run_integration_tests

        return run_integration_tests()
    except Exception as e:
        print(f"ERROR: Failed to run integration tests: {e}")
        return 1


def run_option_verification_tests():
    """Run option verification tests."""
    print("=== Option Verification Tests ===")

    try:
        from ged2gwb.tests.test_option_verification import run_option_verification_tests

        return run_option_verification_tests()
    except Exception as e:
        print(f"ERROR: Failed to run option verification tests: {e}")
        return 1


def run_performance_tests():
    """Run performance tests."""
    print("=== Performance Tests ===")

    try:
        from ged2gwb.tests.test_performance import run_performance_tests

        return run_performance_tests()
    except Exception as e:
        print(f"ERROR: Failed to run performance tests: {e}")
        return 1


def main():
    """Main function to run all test suites."""
    print("GED2GWB - Comprehensive Test Suite")
    print("=" * 60)
    print(f"Python version: {sys.version}")
    print(f"Working directory: {os.getcwd()}")
    print(f"Test execution time: {time.strftime('%Y-%m-%d %H:%M:%S')}")
    print()

    # Execute all test suites
    test_suites = [
        ("Unit Tests - Converter", run_unit_converter_tests),
        ("Unit Tests - Options", run_unit_options_tests),
        ("Regression Tests", run_regression_tests),
        ("Integration Tests", run_integration_tests),
        ("Option Verification Tests", run_option_verification_tests),
        ("Performance Tests", run_performance_tests),
    ]

    results = []
    total_time = 0

    for test_name, test_func in test_suites:
        print(f"\n{'=' * 60}")
        print(f"EXECUTING: {test_name}")
        print(f"{'=' * 60}")

        start_time = time.time()
        result = test_func()
        end_time = time.time()

        execution_time = end_time - start_time
        total_time += execution_time

        status = "PASS" if result == 0 else "FAIL"
        results.append(
            {
                "name": test_name,
                "status": status,
                "time": execution_time,
                "result": result,
            }
        )

        print(f"\n{status}: {test_name} - {execution_time:.2f}s")

    # Final report
    print(f"\n{'=' * 60}")
    print("FINAL REPORT")
    print(f"{'=' * 60}")

    passed = sum(1 for r in results if r["status"] == "PASS")
    total = len(results)

    print(f"Test suites executed: {total}")
    print(f"Test suites passed: {passed}")
    print(f"Test suites failed: {total - passed}")
    print(f"Success rate: {(passed / total) * 100:.1f}%")
    print(f"Total execution time: {total_time:.2f}s")

    print(f"\nDetailed results:")
    print(f"{'Test Suite':<30} {'Status':<8} {'Time (s)':<10}")
    print(f"{'-' * 50}")

    for result in results:
        print(f"{result['name']:<30} {result['status']:<8} {result['time']:<10.2f}")

    if passed == total:
        print(f"\nSUCCESS: All test suites passed!")
        return 0
    else:
        print(f"\nFAILURE: {total - passed} test suite(s) failed.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
