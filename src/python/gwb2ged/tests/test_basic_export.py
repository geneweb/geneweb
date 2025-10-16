#!/usr/bin/env python3
"""
Simple test to understand gwb2ged OCaml behavior
"""

import sys
from tools.test_utils import *

def test_basic_export():
    """Test basic export without options - minimal compliance checks."""
    gwb2ged_path = get_absolute_path("distribution/gw/gwb2ged")
    test_db = get_absolute_path("src/python/gwb2ged/sample.gwb")

    if not check_file_exists(gwb2ged_path):
        return None
    if not check_file_exists(test_db):
        return None
    cmd = f"{gwb2ged_path} {test_db}"
    returncode, _, stderr = run_command(cmd)
    if returncode != 0:
        print(f"FAIL: Export failed: {stderr}")
        return None

    lines = stderr.splitlines()
    # Minimal checks
    has_head = any(line.endswith("HEAD") or " HEAD" in line for line in lines)
    has_trlr = any(line.endswith("TRLR") or " TRLR" in line for line in lines)
    has_utf8 = any("CHAR UTF-8" in line for line in lines)
    has_version = any("VERS 5.5.1" in line for line in lines)

    if not (has_head and has_trlr and has_utf8 and has_version):
        print("FAIL: GEDCOM minimal compliance checks failed")
        return None

    return stderr

if __name__ == "__main__":
    stdout = test_basic_export()
    if stdout is None:
        print("FAIL")
        sys.exit(1)
    print(stdout)
    sys.exit(0)
