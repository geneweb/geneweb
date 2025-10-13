#!/usr/bin/env python3
"""
Concrete tests for GED2GWB using real GEDCOM files.

These tests use actual GEDCOM files and verify real functionality.
"""

import sys
import tempfile
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from lib.ged2gwb.cli.main import Ged2GwbCLI
from lib.ged2gwb.core.converter import Ged2GwbConverter
from lib.ged2gwb.utils.options import ConversionOptions
from lib.db_pickle.io.reader import PickleReader
from lib.db_pickle import create_pickle_base_func

def test_with_sample_ged():
    """Test conversion with the real sample.ged file."""
    print("=== Testing with sample.ged ===\n")

    # Try multiple possible paths
    possible_paths = [
        Path("lib/gedcom/ged/sample.ged"),
        Path("src/lib/gedcom/ged/sample.ged"),
        Path("../lib/gedcom/ged/sample.ged"),
        Path("../../lib/gedcom/ged/sample.ged")
    ]

    sample_ged = None
    for path in possible_paths:
        if path.exists():
            sample_ged = path
            break

    if not sample_ged:
        print("SKIP: sample.ged not found in any expected location")
        return False

    output_file = Path("test_sample_concrete.pkl")

    try:
        # Create options
        options = ConversionOptions(
            input_file=sample_ged,
            output_file=output_file,
            compress=True,
            verbose=True
        )

        # Convert
        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        print(f"PASS: Conversion completed")
        print(f"   Input: {stats['input_file']}")
        print(f"   Output: {stats['output_file']}")
        print(f"   File size: {stats['file_size']:,} bytes")
        print(f"   Individuals: {stats['individuals_count']}")
        print(f"   Families: {stats['families_count']}")

        # Verify output file
        actual_file = Path(stats['file_path'])
        if actual_file.exists() and actual_file.stat().st_size > 0:
            print("PASS: Output file created with content")
        else:
            print("FAIL: Output file missing or empty")
            return False

        # Load and verify data
        reader = PickleReader(verbose=False)
        data = reader.load_database(actual_file)
        func = create_pickle_base_func(data)

        print(f"PASS: Database loaded successfully")
        print(f"   Persons: {func.nb_of_persons()}")
        print(f"   Families: {func.nb_of_families()}")
        print(f"   Real persons: {func.nb_of_real_persons()}")

        # Test search functionality
        if func.nb_of_persons() > 0:
            # Get first person
            persons = list(data.persons.items())
            if persons:
                first_person = persons[0][1]
                print(f"PASS: First person: {first_person.first_name} {first_person.surname}")

                # Test search by first name
                search_results = data.search_persons_by_first_name(first_person.first_name)
                if search_results:
                    print(f"PASS: Search by first name found {len(search_results)} results")
                else:
                    print("FAIL: Search by first name found no results")
                    return False

        return True

    except Exception as e:
        print(f"FAIL: Conversion failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        # Cleanup
        if output_file.exists():
            output_file.unlink()
        if actual_file.exists():
            actual_file.unlink()

def test_with_uk_ged():
    """Test conversion with the real uk.ged file."""
    print("\n=== Testing with uk.ged ===\n")

    # Try multiple possible paths
    possible_paths = [
        Path("lib/gedcom/ged/uk.ged"),
        Path("src/lib/gedcom/ged/uk.ged"),
        Path("../lib/gedcom/ged/uk.ged"),
        Path("../../lib/gedcom/ged/uk.ged")
    ]

    uk_ged = None
    for path in possible_paths:
        if path.exists():
            uk_ged = path
            break

    if not uk_ged:
        print("SKIP: uk.ged not found in any expected location")
        return False

    output_file = Path("test_uk_concrete.pkl")

    try:
        # Create options
        options = ConversionOptions(
            input_file=uk_ged,
            output_file=output_file,
            compress=True,
            verbose=True
        )

        # Convert
        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        print(f"PASS: Conversion completed")
        print(f"   Input: {stats['input_file']}")
        print(f"   Output: {stats['output_file']}")
        print(f"   File size: {stats['file_size']:,} bytes")
        print(f"   Individuals: {stats['individuals_count']}")
        print(f"   Families: {stats['families_count']}")

        # Verify output file
        actual_file = Path(stats['file_path'])
        if actual_file.exists() and actual_file.stat().st_size > 0:
            print("PASS: Output file created with content")
        else:
            print("FAIL: Output file missing or empty")
            return False

        # Load and verify data
        reader = PickleReader(verbose=False)
        data = reader.load_database(actual_file)
        func = create_pickle_base_func(data)

        print(f"PASS: Database loaded successfully")
        print(f"   Persons: {func.nb_of_persons()}")
        print(f"   Families: {func.nb_of_families()}")
        print(f"   Real persons: {func.nb_of_real_persons()}")

        # Test search functionality with common names
        common_names = ["John", "Mary", "Robert", "Elizabeth", "William"]
        found_names = 0

        for name in common_names:
            results = data.search_persons_by_first_name(name)
            if results:
                found_names += 1
                print(f"PASS: Found {len(results)} persons named '{name}'")

        if found_names > 0:
            print(f"PASS: Search functionality working - found {found_names} common names")
        else:
            print("WARN: No common names found in search")

        return True

    except Exception as e:
        print(f"FAIL: Conversion failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        # Cleanup
        if output_file.exists():
            output_file.unlink()
        if actual_file.exists():
            actual_file.unlink()

def test_cli_functionality():
    """Test CLI functionality with real files."""
    print("\n=== Testing CLI Functionality ===\n")

    # Try multiple possible paths
    possible_paths = [
        Path("lib/gedcom/ged/sample.ged"),
        Path("src/lib/gedcom/ged/sample.ged"),
        Path("../lib/gedcom/ged/sample.ged"),
        Path("../../lib/gedcom/ged/sample.ged")
    ]

    sample_ged = None
    for path in possible_paths:
        if path.exists():
            sample_ged = path
            break

    if not sample_ged:
        print("SKIP: sample.ged not found in any expected location")
        return False

    output_file = Path("test_cli_concrete.pkl")

    actual_file = None
    try:
        cli = Ged2GwbCLI()

        # Test help
        parser = cli.create_parser()
        help_text = parser.format_help()
        if "--charset" in help_text and "--efn" in help_text:
            print("PASS: CLI help contains expected options")
        else:
            print("FAIL: CLI help missing expected options")
            return False

        # Test argument parsing
        test_args = [
            str(sample_ged),
            "--output", str(output_file),
            "--compress",
            "--verbose",
            "--efn", "--epn",
            "--lf", "--ls"
        ]

        args = parser.parse_args(test_args)
        if (str(args.gedcom_file) == str(sample_ged) and
            str(args.output) == str(output_file)):
            print("PASS: CLI argument parsing successful")
        else:
            print(f"FAIL: CLI argument parsing failed")
            print(f"   Expected gedcom_file: {str(sample_ged)}")
            print(f"   Got gedcom_file: {args.gedcom_file}")
            print(f"   Expected output: {str(output_file)}")
            print(f"   Got output: {args.output}")
            return False

        # Test conversion through CLI
        options = ConversionOptions.from_args(args)
        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        print(f"PASS: CLI conversion completed")
        print(f"   File size: {stats['file_size']:,} bytes")

        # Verify output
        actual_file = Path(stats['file_path'])
        if actual_file.exists():
            print("PASS: CLI output file created")
        else:
            print("FAIL: CLI output file not created")
            return False

        return True

    except Exception as e:
        print(f"FAIL: CLI test failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        # Cleanup
        if output_file.exists():
            output_file.unlink()
        if actual_file and actual_file.exists():
            actual_file.unlink()

def test_load_functionality():
    """Test the --load functionality."""
    print("\n=== Testing Load Functionality ===\n")

    # Try multiple possible paths
    possible_paths = [
        Path("lib/gedcom/ged/sample.ged"),
        Path("src/lib/gedcom/ged/sample.ged"),
        Path("../lib/gedcom/ged/sample.ged"),
        Path("../../lib/gedcom/ged/sample.ged")
    ]

    sample_ged = None
    for path in possible_paths:
        if path.exists():
            sample_ged = path
            break

    if not sample_ged:
        print("SKIP: sample.ged not found in any expected location")
        return False

    output_file = Path("test_load_concrete.pkl")

    try:
        # First create a database
        options = ConversionOptions(
            input_file=sample_ged,
            output_file=output_file,
            compress=True
        )

        converter = Ged2GwbConverter(options)
        stats = converter.convert()
        actual_file = Path(stats['file_path'])

        if not actual_file.exists():
            print("FAIL: Could not create test database")
            return False

        # Test load functionality
        cli = Ged2GwbCLI()
        result = cli.load_database(str(actual_file))

        if result is not None:
            print("PASS: Load functionality working")
            print(f"   Database type: {type(result).__name__}")
        else:
            print("FAIL: Load functionality failed")
            return False

        return True

    except Exception as e:
        print(f"FAIL: Load test failed: {e}")
        import traceback
        traceback.print_exc()
        return False
    finally:
        # Cleanup
        if output_file.exists():
            output_file.unlink()
        if actual_file.exists():
            actual_file.unlink()

def test_error_handling():
    """Test error handling with concrete scenarios."""
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
        print("PASS: Correctly handled non-existent file")

    # Test with invalid database name
    try:
        # Try multiple possible paths
        possible_paths = [
            Path("lib/gedcom/ged/sample.ged"),
            Path("src/lib/gedcom/ged/sample.ged"),
            Path("../lib/gedcom/ged/sample.ged"),
            Path("../../lib/gedcom/ged/sample.ged")
        ]

        sample_ged = None
        for path in possible_paths:
            if path.exists():
                sample_ged = path
                break

        if sample_ged:
            options = ConversionOptions(
                input_file=sample_ged,
                output_file=Path("invalid_name_with_underscore.pkl")
            )
            converter = Ged2GwbConverter(options)
            converter.validate_input()
            print("FAIL: Should have failed with invalid database name")
            return False
    except ValueError as e:
        if "forbidden character" in str(e).lower() or "invalid characters" in str(e).lower():
            print("PASS: Correctly rejected invalid database name")
        else:
            print(f"FAIL: Wrong error for invalid database name: {e}")
            return False

    # Test with existing output file (without force)
    existing_file = None
    try:
        # Try multiple possible paths
        possible_paths = [
            Path("lib/gedcom/ged/sample.ged"),
            Path("src/lib/gedcom/ged/sample.ged"),
            Path("../lib/gedcom/ged/sample.ged"),
            Path("../../lib/gedcom/ged/sample.ged")
        ]

        sample_ged = None
        for path in possible_paths:
            if path.exists():
                sample_ged = path
                break

        if sample_ged:
            # Create existing file
            existing_file = Path("existing_output.pkl")
            existing_file.touch()

            options = ConversionOptions(
                input_file=sample_ged,
                output_file=existing_file,
                force=False
            )
            converter = Ged2GwbConverter(options)
            converter.validate_input()
            print("FAIL: Should have failed with existing output file")
            return False
    except FileExistsError:
        print("PASS: Correctly handled existing output file")
    finally:
        if existing_file and existing_file.exists():
            existing_file.unlink()

    return True

def main():
    """Main test function."""
    print("Running GED2GWB concrete tests...\n")

    tests = [
        test_with_sample_ged,
        test_with_uk_ged,
        test_cli_functionality,
        test_load_functionality,
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
            import traceback
            traceback.print_exc()

    print(f"\n=== Concrete Test Results ===")
    print(f"Passed: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All concrete tests passed!")
        return 0
    else:
        print("FAILURE: Some concrete tests failed.")
        return 1

if __name__ == '__main__':
    sys.exit(main())
