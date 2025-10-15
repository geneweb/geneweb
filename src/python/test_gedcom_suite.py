#!/usr/bin/env python3
"""
Test script to verify all GEDCOM tests.
"""

import sys
import subprocess
import time
from pathlib import Path

def run_test_module(module_name, description):
    """Execute a test module and return the result."""
    print(f"\n{'='*60}")
    print(f"TEST: {description}")
    print(f"Module: {module_name}")
    print(f"{'='*60}")

    start_time = time.time()

    try:
        result = subprocess.run(
            [sys.executable, '-m', module_name],
            capture_output=True,
            text=True,
            check=True,
            cwd=str(Path(__file__).parent)
        )

        end_time = time.time()
        execution_time = end_time - start_time

        print(f"✅ SUCCESS: {description}")
        print(f"⏱️  Execution time: {execution_time:.3f}s")
        print(f"📊 Output lines: {len(result.stdout.splitlines())}")

        return True, execution_time

    except subprocess.CalledProcessError as e:
        end_time = time.time()
        execution_time = end_time - start_time

        print(f"❌ FAILED: {description}")
        print(f"⏱️  Execution time: {execution_time:.3f}s")
        print(f"🔍 Error code: {e.returncode}")
        print(f"📝 Error output:")
        print(e.stderr)

        return False, execution_time

def main():
    """Main function to execute all GEDCOM tests."""
    print("🧪 GEDCOM Test Suite Verification")
    print("=" * 60)
    print(f"Python version: {sys.version}")
    print(f"Working directory: {Path.cwd()}")
    print(f"Test execution time: {time.strftime('%Y-%m-%d %H:%M:%S')}")

    # List of GEDCOM tests to execute
    test_modules = [
        ("gedcom.tests.test_parser", "GEDCOM Parser Tests"),
        ("gedcom.tests.test_models", "GEDCOM Models Tests"),
        ("gedcom.tests.test_exporter", "GEDCOM Exporter Tests"),
        ("gedcom.tests.test_advanced_functionality", "GEDCOM Advanced Functionality Tests"),
    ]

    results = []
    total_time = 0

    for module_name, description in test_modules:
        success, execution_time = run_test_module(module_name, description)
        results.append({
            'module': module_name,
            'description': description,
            'success': success,
            'time': execution_time
        })
        total_time += execution_time

    # Final report
    print(f"\n{'='*60}")
    print("📋 FINAL REPORT")
    print(f"{'='*60}")

    passed = sum(1 for r in results if r['success'])
    total = len(results)

    print(f"📊 Tests executed: {total}")
    print(f"✅ Tests passed: {passed}")
    print(f"❌ Tests failed: {total - passed}")
    print(f"📈 Success rate: {(passed/total)*100:.1f}%")
    print(f"⏱️  Total execution time: {total_time:.3f}s")

    print(f"\n📝 Detailed results:")
    print(f"{'Test Module':<40} {'Status':<8} {'Time (s)':<10}")
    print("-" * 60)

    for result in results:
        status = "✅ PASS" if result['success'] else "❌ FAIL"
        print(f"{result['description']:<40} {status:<8} {result['time']:<10.3f}")

    if passed == total:
        print(f"\n🎉 SUCCESS: All GEDCOM tests passed!")
        return 0
    else:
        print(f"\n💥 FAILURE: {total - passed} test(s) failed.")
        return 1

if __name__ == "__main__":
    sys.exit(main())
