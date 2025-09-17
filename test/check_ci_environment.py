#!/usr/bin/env python3

import os
import subprocess
import sys

def check_ci_environment():
    """Vérifie que l'environnement CI est prêt pour les tests Golden Master"""

    print("🔍 Checking CI environment for Golden Master tests...")

    issues = []

    # 1. Check OCaml binary
    ocaml_binary = "distribution/gw/ged2gwb"
    if os.path.exists(ocaml_binary) and os.access(ocaml_binary, os.X_OK):
        print(f"✅ OCaml binary: {ocaml_binary}")

        # Test it
        try:
            result = subprocess.run([ocaml_binary, "-help"],
                                  capture_output=True, text=True, timeout=5)
            if "Usage:" in result.stdout:
                print("✅ OCaml binary responds correctly")
            else:
                issues.append("OCaml binary doesn't show usage")
        except Exception as e:
            issues.append(f"OCaml binary test failed: {e}")
    else:
        issues.append(f"OCaml binary not found or not executable: {ocaml_binary}")

    # 2. Check Python implementation
    python_impl = "python/ged2gwb/ged2gwb.py"
    if os.path.exists(python_impl):
        print(f"✅ Python implementation: {python_impl}")

        # Test it
        try:
            result = subprocess.run(["python3", python_impl, "-help"],
                                  capture_output=True, text=True, timeout=5)
            if "Usage:" in result.stdout:
                print("✅ Python implementation responds correctly")
            else:
                issues.append("Python implementation doesn't show usage")
        except Exception as e:
            issues.append(f"Python implementation test failed: {e}")
    else:
        issues.append(f"Python implementation not found: {python_impl}")

    # 3. Check test files
    test_file = "test/scena/ged2gwb.json"
    if os.path.exists(test_file):
        print(f"✅ Test file: {test_file}")

        # Check test file content
        try:
            import json
            with open(test_file) as f:
                tests = json.load(f)
            print(f"✅ Found {len(tests)} test cases")
        except Exception as e:
            issues.append(f"Test file parsing failed: {e}")
    else:
        issues.append(f"Test file not found: {test_file}")

    # 4. Check fixtures
    fixtures_dir = "test/fixtures"
    if os.path.exists(fixtures_dir):
        fixtures = [f for f in os.listdir(fixtures_dir) if f.endswith('.ged')]
        if fixtures:
            print(f"✅ Test fixtures: {len(fixtures)} files")
        else:
            issues.append("No GEDCOM fixtures found")
    else:
        issues.append(f"Fixtures directory not found: {fixtures_dir}")

    # 5. Test the actual Golden Master runner
    runner = "test/run_golden_tests.py"
    if os.path.exists(runner):
        print(f"✅ Golden Master runner: {runner}")
    else:
        issues.append(f"Golden Master runner not found: {runner}")

    # Summary
    print(f"\n{'='*50}")
    if issues:
        print("❌ Issues found:")
        for issue in issues:
            print(f"  - {issue}")
        return 1
    else:
        print("✅ CI environment is ready for Golden Master tests!")
        return 0

if __name__ == "__main__":
    sys.exit(check_ci_environment())
