#!/usr/bin/env python3
"""
Simple test runner for db_pickle module.
"""

import sys
import subprocess
from pathlib import Path

# Add the src/python directory to the Python path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

def main():
    """Run all tests."""
    print("Running db_pickle tests...")

    # Run all tests with pytest
    result = subprocess.run(
        "python -m pytest . -v --tb=short",
        shell=True,
        cwd=Path(__file__).parent
    )

    return result.returncode

if __name__ == "__main__":
    sys.exit(main())
