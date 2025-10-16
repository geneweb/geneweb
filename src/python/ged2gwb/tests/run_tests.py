#!/usr/bin/env python3
"""
Main test runner for ged2gwb.

This script executes all test suites using pytest in a professional manner.
"""

import sys
import os
import time
import subprocess
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))


def run_pytest_tests(test_path, test_name):
    """Run pytest tests for a specific path."""
    print(f"=== {test_name} ===")

    try:
        # Use absolute path for test file
        test_file = Path(__file__).parent / test_path
        if not test_file.exists():
            print(f"ERROR: Test file not found: {test_file}")
            return 1

        result = subprocess.run(
            [sys.executable, '-m', 'pytest', str(test_file), '-v', '--tb=short'],
            capture_output=True,
            text=True,
            cwd=Path(__file__).parent.parent.parent.parent
        )

        if result.returncode == 0:
            # Count passed tests
            test_count = len([line for line in result.stdout.split('\n')
                            if 'PASSED' in line or 'FAILED' in line])
            print(f"✓ {test_name}: {test_count} tests passed")
            return 0
        else:
            print(f"✗ {test_name}: Tests failed")
            print(result.stdout)
            if result.stderr:
                print("STDERR:", result.stderr)
            return 1

    except Exception as e:
        print(f"ERROR: Failed to run {test_name}: {e}")
        return 1


def run_unit_tests():
    """Run all unit tests."""
    print("=== Unit Tests ===")

    unit_tests = [
        ("unit/test_converter.py", "Converter Unit Tests"),
        ("unit/test_options.py", "Options Unit Tests"),
        ("unit/test_cli.py", "CLI Unit Tests"),
    ]

    results = []
    for test_file, test_name in unit_tests:
        result = run_pytest_tests(test_file, test_name)
        results.append(result)

    return 0 if all(r == 0 for r in results) else 1


def run_integration_tests():
    """Run integration tests."""
    return run_pytest_tests("test_integration.py", "Integration Tests")


def run_regression_tests():
    """Run regression tests."""
    return run_pytest_tests("test_regression.py", "Regression Tests")


def run_option_verification_tests():
    """Run option verification tests."""
    return run_pytest_tests("test_option_verification.py", "Option Verification Tests")


def run_performance_tests():
    """Run performance tests."""
    return run_pytest_tests("test_performance.py", "Performance Tests")


def run_concrete_tests():
    """Run concrete tests with real files."""
    return run_pytest_tests("test_concrete.py", "Concrete Tests")


def run_conversion_tests():
    """Run conversion tests."""
    return run_pytest_tests("test_conversion.py", "Conversion Tests")


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
        ("Unit Tests", run_unit_tests),
        ("Integration Tests", run_integration_tests),
        ("Regression Tests", run_regression_tests),
        ("Option Verification Tests", run_option_verification_tests),
        ("Performance Tests", run_performance_tests),
        ("Concrete Tests", run_concrete_tests),
        ("Conversion Tests", run_conversion_tests),
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
