#!/usr/bin/env python3
"""
Test script for GED2GWB conversion functionality.

Tests the actual conversion process with sample GEDCOM files.
"""

import sys
import tempfile
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from lib.ged2gwb.core.converter import Ged2GwbConverter
from lib.ged2gwb.utils.options import ConversionOptions

def create_sample_gedcom() -> Path:
    """Create a sample GEDCOM file for testing."""
    sample_gedcom = """0 HEAD
1 SOUR Test
1 CHAR UTF-8
1 GEDC
2 VERS 5.5.1
0 @I1@ INDI
1 NAME Jean /Dupont/
2 GIVN Jean
2 SURN Dupont
1 SEX M
1 BIRT
2 DATE 15 MAR 1980
2 PLAC Paris, France
1 FAMS @F1@
0 @I2@ INDI
1 NAME Marie /Martin/
2 GIVN Marie
2 SURN Martin
1 SEX F
1 BIRT
2 DATE 20 JUN 1985
2 PLAC Lyon, France
1 FAMS @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 10 SEP 2010
2 PLAC Paris, France
0 TRLR
"""

    # Create temporary file
    temp_file = tempfile.NamedTemporaryFile(mode='w', suffix='.ged', delete=False)
    temp_file.write(sample_gedcom)
    temp_file.close()

    return Path(temp_file.name)

def test_basic_conversion():
    """Test basic GEDCOM to pickle conversion."""
    print("=== Testing Basic Conversion ===\n")

    # Create sample GEDCOM
    gedcom_file = create_sample_gedcom()
    output_file = Path("test-output.pkl")

    try:
        # Create options
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
            compress=False  # No compression for easier testing
      )

        # Create converter
        converter = Ged2GwbConverter(options)

        # Test validation
        converter.validate_input()
        print("PASS: Input validation passed")

        # Perform conversion
        stats = converter.convert()
        print("PASS: Conversion completed successfully")
        print(f"   File size: {stats['file_size']:,} bytes")
        print(f"   Individuals: {stats['individuals_count']}")
        print(f"   Families: {stats['families_count']}")

        # Verify output file exists
        if output_file.exists():
            print("PASS: Output file created")
        else:
            print("FAIL: Output file not found")
            return False

        return True

    except Exception as e:
        print(f"FAIL: Conversion failed: {e}")
        return False
    finally:
        # Cleanup
        if gedcom_file.exists():
            gedcom_file.unlink()
        if output_file.exists():
            output_file.unlink()

def test_compressed_conversion():
    """Test conversion with compression."""
    print("\n=== Testing Compressed Conversion ===\n")

    # Create sample GEDCOM
    gedcom_file = create_sample_gedcom()
    output_file = Path("test-output.pkl.gz")

    try:
        # Create options with compression
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
            compress=True
        )

        # Create converter
        converter = Ged2GwbConverter(options)

        # Perform conversion
        stats = converter.convert()
        print("PASS: Compressed conversion completed successfully")
        print(f"   File size: {stats['file_size']:,} bytes")
        print(f"   Compressed: {stats['compressed']}")
        actual_output_file = Path(stats['file_path'])
        if actual_output_file.exists():
            print("PASS: Compressed output file created")
        else:
            print("FAIL: Compressed output file not found")
            return False

        return True

    except Exception as e:
        print(f"FAIL: Compressed conversion failed: {e}")
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

def test_name_processing_options():
    """Test conversion with name processing options."""
    print("\n=== Testing Name Processing Options ===\n")

    # Create sample GEDCOM
    gedcom_file = create_sample_gedcom()
    output_file = Path("test-name-processing.pkl")

    try:
        # Create options with name processing
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
            compress=False
        )

        # Create converter
        converter = Ged2GwbConverter(options)

        # Perform conversion
        stats = converter.convert()
        print("PASS: Name processing conversion completed successfully")
        print(f"   Options applied: efn={options.efn}, epn={options.epn}, lf={options.lf}, ls={options.ls}")

        return True

    except Exception as e:
        print(f"FAIL: Name processing conversion failed: {e}")
        return False
    finally:
        # Cleanup
        if gedcom_file.exists():
            gedcom_file.unlink()
        if output_file.exists():
            output_file.unlink()

def test_error_handling():
    """Test error handling with invalid inputs."""
    print("\n=== Testing Error Handling ===\n")

    # Test with non-existent file
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
        print("PASS: Correctly caught non-existent file error")

    # Test with existing output file (without force)
    try:
        # Create a dummy output file
        dummy_output = Path("dummy_output.pkl")
        dummy_output.touch()

        options = ConversionOptions(
            input_file=create_sample_gedcom(),
            output_file=dummy_output,
            force=False
        )
        converter = Ged2GwbConverter(options)
        converter.validate_input()
        print("FAIL: Should have failed with existing output file")
        return False
    except FileExistsError:
        print("PASS: Correctly caught existing output file error")
    finally:
        if dummy_output.exists():
            dummy_output.unlink()

    return True

def main():
    """Main test function."""
    print("Testing GED2GWB conversion functionality...\n")

    tests = [
        test_basic_conversion,
        test_compressed_conversion,
        test_name_processing_options,
        test_error_handling
    ]

    passed = 0
    total = len(tests)

    for test in tests:
        try:
            if test():
                passed += 1
        except Exception as e:
            print(f"FAIL: Test {test.__name__} failed with exception: {e}")

    print(f"\n=== Test Results ===")
    print(f"Passed: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All conversion tests passed!")
        return 0
    else:
        print("FAILURE: Some conversion tests failed.")
        return 1

if __name__ == '__main__':
    sys.exit(main())
