#!/usr/bin/env python3
"""
Unit tests for GED2GWB CLI module.

Tests the command-line interface functionality.
"""

import sys
import tempfile
from pathlib import Path
from unittest.mock import MagicMock, patch

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent))

from ged2gwb.cli.main import Ged2GwbCLI
from ged2gwb.utils.options import ConversionOptions


class TestGed2GwbCLI:
    """Test cases for Ged2GwbCLI class."""

    def setup_method(self):
        """Set up test fixtures before each test method."""
        self.cli = Ged2GwbCLI()

    def test_create_parser(self):
        """Test parser creation."""
        parser = self.cli.create_parser()
        assert parser is not None
        assert hasattr(parser, "parse_args")

    def test_help_generation(self):
        """Test help text generation."""
        parser = self.cli.create_parser()
        help_text = parser.format_help()

        assert len(help_text) > 0
        assert "ged2gwb" in help_text
        assert "--help" in help_text
        assert "--output" in help_text
        assert "--charset" in help_text

    def test_required_options_present(self):
        """Test that all required options are present."""
        parser = self.cli.create_parser()
        help_text = parser.format_help()

        required_options = [
            "--charset",
            "--dates-dm",
            "--dates-md",
            "--efn",
            "--epn",
            "--lf",
            "--ls",
            "--output",
            "--force",
            "--verbose",
        ]

        for option in required_options:
            assert option in help_text, f"Required option {option} not found in help"

    def test_optional_arguments(self):
        """Test optional arguments handling."""
        parser = self.cli.create_parser()

        # Test with minimal arguments
        args = parser.parse_args(["test.ged"])
        assert str(args.gedcom_file) == "test.ged"
        assert str(args.output) == "base.msgpack"

    def test_all_options_parsing(self):
        """Test parsing of all available options."""
        parser = self.cli.create_parser()

        test_args = [
            "test.ged",
            "--charset",
            "ASCII",
            "--dates-dm",
            "--efn",
            "--epn",
            "--lf",
            "--ls",
            "--udi",
            "80-120",
            "--verbose",
            "--output",
            "output.msgpack",
        ]

        args = parser.parse_args(test_args)

        assert str(args.gedcom_file) == "test.ged"
        assert args.charset == "ASCII"
        assert args.dates_dm is True
        assert args.efn is True
        assert args.epn is True
        assert args.lf is True
        assert args.ls is True
        assert args.udi == "80-120"
        assert args.verbose is True
        assert str(args.output) == "output.msgpack"

    def test_udi_validation(self):
        """Test UDI format validation."""
        parser = self.cli.create_parser()

        # Test valid UDI format
        args = parser.parse_args(["test.ged", "--udi", "80-120"])
        assert args.udi == "80-120"

        # Test invalid UDI format - argparse allows any string
        # Validation happens in ConversionOptions.from_args
        args = parser.parse_args(["test.ged", "--udi", "invalid"])
        assert args.udi == "invalid"

    def test_conflicting_options(self):
        """Test conflicting options detection."""
        parser = self.cli.create_parser()

        # Test conflicting date options - these should be allowed by argparse
        # The validation happens in ConversionOptions, not in argparse
        args = parser.parse_args(["test.ged", "--dates-dm", "--dates-md"])
        assert args.dates_dm is True
        assert args.dates_md is True

    def test_short_options(self):
        """Test short option aliases."""
        parser = self.cli.create_parser()

        args = parser.parse_args(["test.ged", "-o", "output.msgpack", "-v", "-f"])
        assert str(args.output) == "output.msgpack"
        assert args.verbose is True
        assert args.force is True

    def test_load_option(self):
        """Test --load option functionality."""
        parser = self.cli.create_parser()

        args = parser.parse_args(["--load", "database.pkl"])
        assert args.load == "database.pkl"
        assert args.gedcom_file is None

    def test_base_dir_option(self):
        """Test --base-dir option."""
        parser = self.cli.create_parser()

        args = parser.parse_args(["test.ged", "--base-dir", "/tmp"])
        assert args.base_dir == Path("/tmp")

    def test_consistency_check_options(self):
        """Test consistency check options."""
        parser = self.cli.create_parser()

        # Test --no-consistency-check
        args = parser.parse_args(["test.ged", "--no-consistency-check"])
        assert args.no_consistency_check is True

        # Test -nc alias
        args = parser.parse_args(["test.ged", "-nc"])
        assert args.nc is True


class TestConversionOptions:
    """Test cases for ConversionOptions class."""

    def test_from_args(self):
        """Test ConversionOptions creation from parsed arguments."""
        parser = Ged2GwbCLI().create_parser()
        args = parser.parse_args(
            [
                "test.ged",
                "--charset",
                "ASCII",
                "--dates-dm",
                "--efn",
                "--epn",
                "--udi",
                "80-120",
                "--output",
                "output.msgpack",
            ]
        )

        options = ConversionOptions.from_args(args)

        assert options.input_file == Path("test.ged")
        assert options.output_file == Path("output.msgpack")
        assert options.charset == "ASCII"
        assert options.dates_dm is True
        assert options.efn is True
        assert options.epn is True
        assert options.udi == (80, 120)

    def test_default_values(self):
        """Test default values."""
        parser = Ged2GwbCLI().create_parser()
        args = parser.parse_args(["test.ged"])

        options = ConversionOptions.from_args(args)

        assert options.output_file == Path("base.msgpack")
        assert options.charset is None
        assert options.verbose is False
        assert options.force is False

    def test_udi_conversion(self):
        """Test UDI string to tuple conversion."""
        parser = Ged2GwbCLI().create_parser()
        args = parser.parse_args(["test.ged", "--udi", "90-130"])

        options = ConversionOptions.from_args(args)
        assert options.udi == (90, 130)

    def test_boolean_options(self):
        """Test boolean options handling."""
        parser = Ged2GwbCLI().create_parser()
        args = parser.parse_args(
            [
                "test.ged",
                "--efn",
                "--epn",
                "--lf",
                "--ls",
                "--verbose",
                "--force",
            ]
        )

        options = ConversionOptions.from_args(args)

        assert options.efn is True
        assert options.epn is True
        assert options.lf is True
        assert options.ls is True
        assert options.verbose is True
        assert options.force is True


def main():
    """Run all CLI unit tests."""
    print("Running CLI unit tests...")

    # Test Ged2GwbCLI
    cli_tests = TestGed2GwbCLI()
    cli_tests.setup_method()

    test_methods = [method for method in dir(cli_tests) if method.startswith("test_")]

    passed = 0
    total = len(test_methods)

    for test_method in test_methods:
        try:
            getattr(cli_tests, test_method)()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")

    # Test ConversionOptions
    options_tests = TestConversionOptions()
    options_test_methods = [
        method for method in dir(options_tests) if method.startswith("test_")
    ]

    for test_method in options_test_methods:
        try:
            getattr(options_tests, test_method)()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")

    total += len(options_test_methods)

    print(f"\nCLI Unit Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All CLI unit tests passed!")
        return 0
    else:
        print("FAILURE: Some CLI unit tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
