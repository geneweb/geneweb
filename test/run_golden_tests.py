#!/usr/bin/env python3

import sys
import os
sys.path.append(os.path.dirname(__file__))

from golden_master_runner import GoldenMasterTester

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 run_golden_tests.py <binary_name>")
        print("Example: python3 run_golden_tests.py ged2gwb")
        print("\nAvailable test files:")

        # List available test files in scena directory
        scena_dir = os.path.join(os.path.dirname(__file__), "scena")
        if os.path.exists(scena_dir):
            for file in os.listdir(scena_dir):
                if file.endswith('.json'):
                    binary_name = file[:-5]  # Remove .json extension
                    print(f"  - {binary_name}")

        sys.exit(1)

    binary_name = sys.argv[1]

    # Check if test file exists
    test_file = f"test/scena/{binary_name}.json"
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        print("Available test files:")
        scena_dir = "test/scena"
        if os.path.exists(scena_dir):
            for file in os.listdir(scena_dir):
                if file.endswith('.json'):
                    available_binary = file[:-5]
                    print(f"  - {available_binary}")
        sys.exit(1)

    print(f"Running Golden Master tests for {binary_name}...")
    print(f"Using test file: {test_file}")

    tester = GoldenMasterTester(binary_name)
    success = tester.run_all_tests()

    if success:
        print(f"✅ All tests passed for {binary_name}")
        sys.exit(0)
    else:
        print(f"❌ Some tests failed for {binary_name}")
        sys.exit(1)

if __name__ == "__main__":
    main()
