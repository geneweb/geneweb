#!/usr/bin/env python3
"""
Test script for GED2GWB conversion functionality.

Tests the actual conversion process with sample GEDCOM files.
"""

import sys
import tempfile
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent / "python"))

from ged2gwb.core.converter import Ged2GwbConverter
from ged2gwb.utils.options import ConversionOptions


def verify_msgpack_database_structure(output_dir: Path) -> bool:
    """Verify that all required MessagePack database files and directories exist."""
    required_files = [
        "base",           # Main database file
        "base.acc",       # Access file
        "names.inx",      # Names index
        "fnames.inx",     # First names index
        "fnames.dat",     # First names data
        "snames.inx",     # Surnames index
        "snames.dat",     # Surnames data
        "strings.inx",    # Strings index
        "nb_persons",     # Number of persons
        "notes",          # Notes file
        "particles.txt",  # Particles file
        "patches",        # Patches file
        "synchro_patches", # Synchronization patches
    ]

    required_dirs = [
        "notes_d",        # Notes directory
        "wiznotes",       # Wiznotes directory
    ]

    missing_files = []
    for file_name in required_files:
        if not (output_dir / file_name).exists():
            missing_files.append(file_name)

    missing_dirs = []
    for dir_name in required_dirs:
        if not (output_dir / dir_name).exists() or not (output_dir / dir_name).is_dir():
            missing_dirs.append(dir_name)

    if missing_files:
        print(f"FAIL: Missing required files: {missing_files}")
        return False

    if missing_dirs:
        print(f"FAIL: Missing required directories: {missing_dirs}")
        return False

    print("PASS: All required MessagePack database files created")
    return True


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
    temp_file = tempfile.NamedTemporaryFile(mode="w", suffix=".ged", delete=False)
    temp_file.write(sample_gedcom)
    temp_file.close()

    return Path(temp_file.name)


def test_basic_conversion():
    """Test basic GEDCOM to MessagePack conversion."""
    print("=== Testing Basic Conversion ===\n")

    # Create sample GEDCOM
    gedcom_file = create_sample_gedcom()
    output_file = Path("test-output.msgpack")

    try:
        # Create options
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
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
        # Verify output directory exists (MessagePack creates directories)
        output_dir = Path(stats["file_path"])
        if output_dir.exists() and output_dir.is_dir():
            print("PASS: Output directory created")

            # Verify all required MessagePack database files exist
            if not verify_msgpack_database_structure(output_dir):
                raise AssertionError("MessagePack database structure verification failed")
        else:
            print("FAIL: Output directory not found")
            raise AssertionError("Test failed")
        # Test passed successfully
    except Exception as e:
        print(f"FAIL: Conversion failed: {e}")
        raise AssertionError("Test failed")
    finally:
        # Cleanup
        if gedcom_file.exists():
            gedcom_file.unlink()
        if output_file.exists():
            if output_file.is_dir():
                import shutil
                shutil.rmtree(output_file)
            else:
                output_file.unlink()


def test_name_processing_options():
    """Test conversion with name processing options."""
    print("\n=== Testing Name Processing Options ===\n")

    # Create sample GEDCOM
    gedcom_file = create_sample_gedcom()
    output_file = Path("test-name-processing.msgpack")

    try:
        # Create options with name processing
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=output_file,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
        )

        # Create converter
        converter = Ged2GwbConverter(options)

        # Perform conversion
        stats = converter.convert()
        print("PASS: Name processing conversion completed successfully")
        print(
            f"   Options applied: efn={options.efn}, epn={options.epn}, lf={options.lf}, ls={options.ls}"
        )

        # Test passed successfully
    except Exception as e:
        print(f"FAIL: Name processing conversion failed: {e}")
        raise AssertionError("Test failed")
    finally:
        # Cleanup
        if gedcom_file.exists():
            gedcom_file.unlink()
        if output_file.exists():
            if output_file.is_dir():
                import shutil
                shutil.rmtree(output_file)
            else:
                output_file.unlink()


def test_error_handling():
    """Test error handling with invalid inputs."""
    print("\n=== Testing Error Handling ===\n")

    # Test with non-existent file
    try:
        options = ConversionOptions(
            input_file=Path("nonexistent.ged"), output_file=Path("test.pkl")
        )
        converter = Ged2GwbConverter(options)
        converter.validate_input()
        print("FAIL: Should have failed with non-existent file")
        raise AssertionError("Test failed")
    except FileNotFoundError:
        print("PASS: Correctly caught non-existent file error")

    # Test with existing output file (without force)
    try:
        # Create a dummy output file
        dummy_output = Path("dummy_output.pkl")
        dummy_output.touch()

        options = ConversionOptions(
            input_file=create_sample_gedcom(), output_file=dummy_output, force=False
        )
        converter = Ged2GwbConverter(options)
        converter.validate_input()
        print("FAIL: Should have failed with existing output file")
        raise AssertionError("Test failed")
    except FileExistsError:
        print("PASS: Correctly caught existing output file error")
    finally:
        if dummy_output.exists():
            dummy_output.unlink()

    # Test passed successfully


def main():
    """Main test function."""
    print("Testing GED2GWB conversion functionality...\n")

    tests = [
        test_basic_conversion,
        test_name_processing_options,
        test_error_handling,
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


if __name__ == "__main__":
    sys.exit(main())
