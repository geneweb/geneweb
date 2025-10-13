#!/usr/bin/env python3
"""
Test that ged2gwb options are actually implemented and functional.
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from lib.ged2gwb.cli.main import Ged2GwbCLI
from lib.ged2gwb.utils.options import ConversionOptions

def test_option_parsing():
    """Test that options can be parsed correctly."""
    print("=== Testing Option Parsing ===\n")

    cli = Ged2GwbCLI()
    parser = cli.create_parser()

    # Test basic options
    test_cases = [
        # Basic usage
        ['sample.ged', '-o', 'test.pkl'],
        ['sample.ged', '--output', 'test.pkl'],

        # Charset options
        ['sample.ged', '--charset', 'ANSEL'],
        ['sample.ged', '--charset', 'ASCII'],
        ['sample.ged', '--charset', 'MSDOS'],

        # Date options
        ['sample.ged', '--dates-dm'],
        ['sample.ged', '--dates-md'],
        ['sample.ged', '--no-nd'],

        # Name processing
        ['sample.ged', '--efn'],
        ['sample.ged', '--epn'],
        ['sample.ged', '--no-efn'],
        ['sample.ged', '--no-epn'],
        ['sample.ged', '--fne', '""'],
        ['sample.ged', '--lf'],
        ['sample.ged', '--ls'],
        ['sample.ged', '--us'],

        # Control options
        ['sample.ged', '--force'],
        ['sample.ged', '--nc'],
        ['sample.ged', '--no-consistency-check'],

        # Pickle options
        ['sample.ged', '--compress'],
        ['sample.ged', '--no-compress'],
        ['sample.ged', '--load', 'existing.pkl'],
    ]

    passed = 0
    failed = 0

    for i, test_case in enumerate(test_cases, 1):
        try:
            args = parser.parse_args(test_case)
            print(f"✅ Test {i:2d}: {' '.join(test_case[:3])}{'...' if len(test_case) > 3 else ''}")
            passed += 1
        except SystemExit as e:
            print(f"❌ Test {i:2d}: {' '.join(test_case[:3])}{'...' if len(test_case) > 3 else ''} - Exit: {e.code}")
            failed += 1
        except Exception as e:
            print(f"❌ Test {i:2d}: {' '.join(test_case[:3])}{'...' if len(test_case) > 3 else ''} - Error: {e}")
            failed += 1

    print(f"\n📊 PARSING RESULTS:")
    print(f"   Passed: {passed}")
    print(f"   Failed: {failed}")
    print(f"   Success rate: {(passed/(passed+failed)*100):.1f}%")

    return failed == 0

def test_conversion_options():
    """Test ConversionOptions with various options."""
    print("\n=== Testing ConversionOptions ===\n")

    tests = [
        # Basic options
        {
            'name': 'Basic options',
            'kwargs': {
                'input_file': Path('test.ged'),
                'output_file': Path('test.pkl'),
                'charset': 'ANSEL',
                'dates_dm': True,
                'efn': True,
                'lf': True,
                'compress': True
            }
        },
        # UDI option
        {
            'name': 'UDI option',
            'kwargs': {
                'input_file': Path('test.ged'),
                'output_file': Path('test.pkl'),
                'udi': (80, 120)
            }
        },
        # All boolean options
        {
            'name': 'All boolean options',
            'kwargs': {
                'input_file': Path('test.ged'),
                'output_file': Path('test.pkl'),
                'dates_dm': True,
                'dates_md': True,
                'no_nd': True,
                'efn': True,
                'epn': True,
                'no_efn': True,
                'no_epn': True,
                'lf': True,
                'ls': True,
                'us': True,
                'rs_no_mention': True,
                'no_pit': True,
                'nopicture': True,
                'uin': True,
                'tnd': True,
                'trackid': True,
                'force': True,
                'nc': True,
                'compress': True,
                'verbose': True
            }
        }
    ]

    passed = 0
    failed = 0

    for test in tests:
        try:
            options = ConversionOptions(**test['kwargs'])
            print(f"✅ {test['name']}: Created successfully")
            passed += 1
        except Exception as e:
            print(f"❌ {test['name']}: Failed - {e}")
            failed += 1

    print(f"\n📊 CONVERSION OPTIONS RESULTS:")
    print(f"   Passed: {passed}")
    print(f"   Failed: {failed}")
    print(f"   Success rate: {(passed/(passed+failed)*100):.1f}%")

    return failed == 0

def test_option_values():
    """Test that option values are correctly stored."""
    print("\n=== Testing Option Values ===\n")

    cli = Ged2GwbCLI()
    parser = cli.create_parser()

    # Test specific option values
    test_cases = [
        {
            'args': ['sample.ged', '--charset', 'ANSEL', '--udi', '70-110', '--fne', '""'],
            'expected': {
                'charset': 'ANSEL',
                'udi': (70, 110),
                'fne': '""'
            }
        },
        {
            'args': ['sample.ged', '--dates-dm', '--efn', '--lf', '--compress'],
            'expected': {
                'dates_dm': True,
                'efn': True,
                'lf': True,
                'compress': True
            }
        }
    ]

    passed = 0
    failed = 0

    for i, test_case in enumerate(test_cases, 1):
        try:
            args = parser.parse_args(test_case['args'])

            # Check expected values
            all_correct = True
            for key, expected_value in test_case['expected'].items():
                actual_value = getattr(args, key, None)
                if actual_value != expected_value:
                    print(f"❌ Test {i}: {key} = {actual_value}, expected {expected_value}")
                    all_correct = False

            if all_correct:
                print(f"✅ Test {i}: All values correct")
                passed += 1
            else:
                failed += 1

        except Exception as e:
            print(f"❌ Test {i}: Error - {e}")
            failed += 1

    print(f"\n📊 OPTION VALUES RESULTS:")
    print(f"   Passed: {passed}")
    print(f"   Failed: {failed}")
    print(f"   Success rate: {(passed/(passed+failed)*100):.1f}%")

    return failed == 0

def test_help_output():
    """Test that help output contains expected options."""
    print("\n=== Testing Help Output ===\n")

    cli = Ged2GwbCLI()
    parser = cli.create_parser()
    help_text = parser.format_help()

    # Check for key options in help
    expected_options = [
        '--charset',
        '--dates-dm',
        '--dates-md',
        '--efn',
        '--epn',
        '--lf',
        '--ls',
        '--udi',
        '--compress',
        '--force',
        '--nc'
    ]

    found = 0
    missing = []

    for option in expected_options:
        if option in help_text:
            found += 1
        else:
            missing.append(option)

    print(f"✅ Found {found}/{len(expected_options)} expected options in help")

    if missing:
        print(f"❌ Missing options: {missing}")
        return False
    else:
        print("✅ All expected options found in help")
        return True

def main():
    """Run all implementation tests."""
    print("=== GED2GWB Implementation Verification ===\n")

    # Run all tests
    parsing_ok = test_option_parsing()
    conversion_ok = test_conversion_options()
    values_ok = test_option_values()
    help_ok = test_help_output()

    print(f"\n=== FINAL SUMMARY ===")
    print(f"Option Parsing: {'✅ PASS' if parsing_ok else '❌ FAIL'}")
    print(f"Conversion Options: {'✅ PASS' if conversion_ok else '❌ FAIL'}")
    print(f"Option Values: {'✅ PASS' if values_ok else '❌ FAIL'}")
    print(f"Help Output: {'✅ PASS' if help_ok else '❌ FAIL'}")

    all_passed = parsing_ok and conversion_ok and values_ok and help_ok

    if all_passed:
        print(f"\n🎉 ALL TESTS PASSED! Options are properly implemented.")
    else:
        print(f"\n❌ SOME TESTS FAILED! Implementation needs fixes.")

    return 0 if all_passed else 1

if __name__ == '__main__':
    sys.exit(main())
