#!/usr/bin/env python3
"""
Dynamic test discovery script for Python modules.
"""

import os
import sys
import subprocess
from pathlib import Path


def discover_test_directories():
    """Discover all test directories in the current directory tree."""
    test_dirs = []

    # Directories to exclude from test discovery
    exclude_dirs = {'__pycache__', 'build', 'dist', 'htmlcov', '.pytest_cache', '.venv', 'examples', 'scripts', 'bin', 'etc', 'lang', 'cnt'}

    for root, dirs, files in os.walk('.'):
        # Skip hidden directories and common non-test directories
        dirs[:] = [d for d in dirs if not d.startswith('.') and d not in exclude_dirs]

        # Skip if this directory should be excluded
        if any(excluded in root for excluded in exclude_dirs):
            continue

        # Look for test files
        if any(f.startswith('test_') and f.endswith('.py') for f in files):
            test_dirs.append(root)
        # Look for tests directory
        elif 'tests' in dirs:
            test_dirs.append(os.path.join(root, 'tests'))

    return sorted(set(test_dirs))


def run_dynamic_tests():
    """Run tests for all discovered modules dynamically."""
    print("\033[0;34mRunning dynamic test discovery...\033[0m")

    # Discover all test directories
    test_dirs = discover_test_directories()

    total_tests = 0
    failed_modules = []

    # Group test directories by their parent module
    modules = {}
    for test_dir in test_dirs:
        # Extract module name from test directory path
        if test_dir.startswith('./'):
            test_dir = test_dir[2:]

        if '/' in test_dir:
            module = test_dir.split('/')[0]
        else:
            module = test_dir

        if module not in modules:
            modules[module] = []
        modules[module].append(test_dir)

    # Run tests for each module
    for module, dirs in sorted(modules.items()):
        print(f"\033[0;34mTesting {module} module...\033[0m")

        module_tests = 0
        for test_dir in dirs:
            try:
                result = subprocess.run(
                    [sys.executable, '-m', 'pytest', test_dir, '-v', '--tb=short'],
                    capture_output=True,
                    text=True
                )

                if result.returncode == 0:
                    # Count passed tests
                    test_count = len([line for line in result.stdout.split('\n')
                                    if 'PASSED' in line or 'FAILED' in line])
                    module_tests += test_count
                else:
                    print(f"\033[0;31m✗ {module}: Tests failed in {test_dir}\033[0m")
                    failed_modules.append(f"{module}({test_dir})")

            except Exception as e:
                print(f"\033[0;31m✗ {module}: Error in {test_dir} - {e}\033[0m")
                failed_modules.append(f"{module}({test_dir})")

        if module_tests > 0:
            print(f"\033[0;32m✓ {module}: {module_tests} tests passed\033[0m")
            total_tests += module_tests
        else:
            print(f"\033[0;33mNo tests found in {module}\033[0m")

    print(f"\n\033[0;34mTotal tests run: {total_tests}\033[0m")

    if failed_modules:
        print(f"\033[0;31mFailed modules: {', '.join(failed_modules)}\033[0m")
        return 1
    else:
        print(f"\033[0;32m✓ All modules passed!\033[0m")
        return 0


def main():
    """Main function."""
    if len(sys.argv) > 1 and sys.argv[1] == 'discover':
        # Just discover and list test directories
        test_dirs = discover_test_directories()
        for test_dir in test_dirs:
            print(test_dir)
    else:
        # Run dynamic tests
        return run_dynamic_tests()


if __name__ == '__main__':
    sys.exit(main())
