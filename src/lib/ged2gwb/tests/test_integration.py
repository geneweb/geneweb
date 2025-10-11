#!/usr/bin/env python3
"""
Integration tests for GED2GWB.

Tests the complete workflow from GEDCOM parsing to pickle serialization.
"""

import sys
import tempfile
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from lib.ged2gwb.cli.main import Ged2GwbCLI
from lib.ged2gwb.core.converter import Ged2GwbConverter
from lib.ged2gwb.utils.options import ConversionOptions

def create_complex_gedcom() -> Path:
    """Create a complex GEDCOM file for testing."""
    complex_gedcom = """0 HEAD
1 SOUR Test Complex
1 CHAR UTF-8
1 GEDC
2 VERS 5.5.1
1 NOTE This is a test GEDCOM file
0 @I1@ INDI
1 NAME Jean-Pierre /Dupont/
2 GIVN Jean-Pierre
2 SURN Dupont
1 SEX M
1 BIRT
2 DATE 15 MAR 1980
2 PLAC Paris, France
2 SOUR @S1@
1 DEAT
2 DATE 2020
2 PLAC Paris, France
1 FAMS @F1@
1 FAMS @F2@
0 @I2@ INDI
1 NAME Marie-Claire /Martin/
2 GIVN Marie-Claire
2 SURN Martin
1 SEX F
1 BIRT
2 DATE 20 JUN 1985
2 PLAC Lyon, France
1 FAMS @F1@
0 @I3@ INDI
1 NAME Sophie /Dupont/
2 GIVN Sophie
2 SURN Dupont
1 SEX F
1 BIRT
2 DATE 10 JAN 2012
2 PLAC Paris, France
1 FAMC @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
1 MARR
2 DATE 10 SEP 2010
2 PLAC Paris, France
2 SOUR @S1@
1 DIV
2 DATE 2015
0 @F2@ FAM
1 HUSB @I1@
1 MARR
2 DATE 2018
2 PLAC Paris, France
0 @S1@ SOUR
1 TITL Test Source
1 AUTH Test Author
1 PUBL Test Publisher
0 TRLR
"""

    # Create temporary file
    temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.ged', delete=False)
    temp_file.write(complex_gedcom)
    temp_file.close()

    return Path(temp_file.name)

def test_cli_help():
    """Test CLI help output."""
    print("=== Testing CLI Help ===\n")

    cli = Ged2GwbCLI()
    parser = cli.create_parser()

    # Test help output
    help_output = parser.format_help()
    print("PASS: CLI help generated successfully")
    print(f"Help length: {len(help_output)} characters")

    # Check for key options in help
    key_options = ['--charset', '--dates-dm', '--efn', '--epn', '--lf', '--ls']
    for option in key_options:
        if option in help_output:
            print(f"PASS: Found {option} in help")
        else:
            print(f"FAIL: Missing {option} in help")
            return False

    return True

def test_cli_parsing():
    """Test CLI argument parsing."""
    print("\n=== Testing CLI Parsing ===\n")

    cli = Ged2GwbCLI()

    # Test valid arguments
    test_args = [
        'test.ged',
        '--charset', 'ANSEL',
        '--dates-dm',
        '--efn', '--epn',
        '--lf', '--ls',
        '--udi', '80-120',
        '--compress',
        '--verbose',
        '-o', 'output.pkl'
    ]

    try:
        # This would normally be called by the CLI
        parser = cli.create_parser()
        args = parser.parse_args(test_args)
        print("PASS: CLI parsing successful")
        print(f"   Charset: {args.charset}")
        print(f"   Dates DM: {args.dates_dm}")
        print(f"   EFN: {args.efn}")
        print(f"   EPN: {args.epn}")
        print(f"   LF: {args.lf}")
        print(f"   LS: {args.ls}")
        print(f"   UDI: {args.udi}")
        print(f"   Compress: {args.compress}")
        print(f"   Verbose: {args.verbose}")
        print(f"   Output: {args.output}")
    except Exception as e:
        print(f"FAIL: CLI parsing failed: {e}")
        return False

    return True

def test_end_to_end_conversion():
    """Test complete end-to-end conversion."""
    print("\n=== Testing End-to-End Conversion ===\n")

    # Create complex GEDCOM
    gedcom_file = create_complex_gedcom()
    output_file = Path("test-integration.pkl")

    try:
        # Create options
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
            charset="UTF-8",
            dates_dm=True,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
            udi=(80, 120),
            compress=True,
            verbose=True
        )

        # Create converter
        converter = Ged2GwbConverter(options)

        # Validate input
        converter.validate_input()
        print("PASS: Input validation passed")

        # Get conversion info
        info = converter.get_conversion_info()
        print(f"INFO: Conversion info: {info}")

        # Perform conversion
        stats = converter.convert()
        print("PASS: End-to-end conversion completed successfully")
        print(f"   Input: {stats['input_file']}")
        print(f"   Output: {stats['output_file']}")
        print(f"   Format: {stats['format']}")
        print(f"   Compressed: {stats['compressed']}")
        print(f"   File size: {stats['file_size']:,} bytes")
        print(f"   Individuals: {stats['individuals_count']}")
        print(f"   Families: {stats['families_count']}")
        print(f"   Time: {stats.get('serialization_time', 0):.3f}s")

        # Verify output file exists and has content (use actual file path from stats)
        actual_output_file = Path(stats['file_path'])
        if actual_output_file.exists() and actual_output_file.stat().st_size > 0:
            print("PASS: Output file created with content")
        else:
            print("FAIL: Output file missing or empty")
            return False

        return True

    except Exception as e:
        print(f"FAIL: End-to-end conversion failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        # Cleanup
        if gedcom_file.exists():
            gedcom_file.unlink()
        # Clean up both possible output files
        if output_file.exists():
            output_file.unlink()
        if 'actual_output_file' in locals() and actual_output_file.exists():
            actual_output_file.unlink()

def test_error_scenarios():
    """Test various error scenarios."""
    print("\n=== Testing Error Scenarios ===\n")

    # Test with non-existent input file
    try:
        options = ConversionOptions(
            input_file=Path("nonexistent.ged"),
            output_file=Path("test.pkl")
        )
        converter = Ged2GwbConverter(options)
        converter.validate_input()
        print("FAIL: Should have failed with non-existent file")
        return False
    except FileNotFoundError:
        print("PASS: Correctly handled non-existent input file")

    # Test with invalid UDI format
    try:
        options = ConversionOptions(
            input_file=Path("test.ged"),
            output_file=Path("test.pkl"),
            udi="invalid-format"
        )
        print("FAIL: Should have failed with invalid UDI format")
        return False
    except ValueError:
        print("PASS: Correctly handled invalid UDI format")

    return True

def main():
    """Main integration test function."""
    print("Running GED2GWB integration tests...\n")

    tests = [
        test_cli_help,
        test_cli_parsing,
        test_end_to_end_conversion,
        test_error_scenarios
    ]

    passed = 0
    total = len(tests)

    for test in tests:
        try:
            if test():
                passed += 1
        except Exception as e:
            print(f"FAIL: Test {test.__name__} failed with exception: {e}")
            import traceback
            traceback.print_exc()

    print(f"\n=== Integration Test Results ===")
    print(f"Passed: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All integration tests passed!")
        return 0
    else:
        print("FAILURE: Some integration tests failed.")
        return 1

if __name__ == '__main__':
    sys.exit(main())
