#!/usr/bin/env python3
"""
Test script to verify all ged2gwb options are implemented.

This script tests that all options from the original ged2gwb documentation
are properly implemented in our Python version.
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / "python"))

from ged2gwb.cli.main import Ged2GwbCLI


def test_all_options():
    """Test that all options from the documentation are implemented."""

    # Options from https://geneweb.github.io/binaries/ged2gwb.html
    expected_options = [
        # Charset
        "--charset",
        # Date interpretation
        "--dates-dm",
        "--dates-md",
        "--no-nd",
        "--tnd",
        # Source handling
        "--ds",
        # Name processing
        "--efn",
        "--epn",
        "--no-efn",
        "--no-epn",
        "--fne",
        "--lf",
        "--ls",
        "--us",
        # Particles
        "--particles",
        # Relation status
        "--rs-no-mention",
        # Privacy
        "--no-pit",
        # Pictures
        "--nopicture",
        # Undefined death interval
        "--udi",
        # Untreated tags
        "--uin",
        # Output options
        "--force",
        "-f",
        "--output",
        "-o",
        # Logging
        "--log",
        "--verbose",
        "-v",
        "--trackid",
        # Consistency
        "--no-consistency-check",
        "-nc",
    ]

    cli = Ged2GwbCLI()
    parser = cli.create_parser()

    # Get all action options
    all_options = set()
    for action in parser._actions:
        if action.option_strings:
            all_options.update(action.option_strings)

    print("=== GED2GWB Options Verification ===\n")

    # Check which options are implemented
    implemented = []
    missing = []

    for option in expected_options:
        if option in all_options:
            implemented.append(option)
        else:
            missing.append(option)

    print(f"PASS: Implemented options ({len(implemented)}):")
    for option in sorted(implemented):
        print(f"   {option}")

    if missing:
        print(f"\nFAIL: Missing options ({len(missing)}):")
        for option in sorted(missing):
            print(f"   {option}")
    else:
        print(
            f"\nSUCCESS: All {len(expected_options)} expected options are implemented!"
        )

    # Show help to verify
    print(f"\nFull help output:")
    print("=" * 50)
    parser.print_help()

    assert len(missing) == 0, f"Missing options: {missing}"


def test_option_validation():
    """Test option validation logic."""
    print("\n=== Option Validation Tests ===\n")

    from pathlib import Path

    from ged2gwb.utils.options import ConversionOptions

    # Test conflicting options
    try:
        options = ConversionOptions(
            input_file=Path("test.ged"), dates_dm=True, dates_md=True
        )
        print("FAIL: Should have failed with conflicting date options")
        raise AssertionError("Test failed")
    except ValueError as e:
        print(f"PASS: Correctly caught conflicting date options: {e}")

    try:
        options = ConversionOptions(input_file=Path("test.ged"), efn=True, no_efn=True)
        print("FAIL: Should have failed with conflicting efn options")
        raise AssertionError("Test failed")
    except ValueError as e:
        print(f"PASS: Correctly caught conflicting efn options: {e}")

    try:
        options = ConversionOptions(input_file=Path("test.ged"), epn=True, no_epn=True)
        print("FAIL: Should have failed with conflicting epn options")
        raise AssertionError("Test failed")
    except ValueError as e:
        print(f"PASS: Correctly caught conflicting epn options: {e}")

    # Test valid options
    try:
        options = ConversionOptions(
            input_file=Path("test.ged"),
            dates_dm=True,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
            udi=(80, 120),
        )
        print("PASS: Valid options created successfully")
    except Exception as e:
        print(f"FAIL: Valid options failed: {e}")
        raise AssertionError("Test failed")
    # Test passed successfully


def main():
    """Main test function."""
    print("Testing GED2GWB options implementation...\n")

    # Test option implementation
    options_ok = test_all_options()

    # Test option validation
    validation_ok = test_option_validation()

    if options_ok and validation_ok:
        print("\nSUCCESS: All tests passed! GED2GWB implementation is complete.")
        return 0
    else:
        print("\nFAILURE: Some tests failed. Please check the implementation.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
