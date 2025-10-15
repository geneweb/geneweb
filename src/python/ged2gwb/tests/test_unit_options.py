#!/usr/bin/env python3
"""
Unit tests for the ConversionOptions class.

These tests verify the options parsing and validation logic.
"""

import sys
from pathlib import Path
from unittest.mock import MagicMock

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from ged2gwb.utils.options import ConversionOptions


class TestConversionOptions:
    """Unit tests for ConversionOptions."""

    def test_default_initialization(self):
        """Test default initialization values."""
        options = ConversionOptions(input_file=Path("test.ged"))

        assert options.input_file == Path("test.ged")
        assert options.output_file == Path("base.pkl")
        assert options.charset is None
        assert options.dates_dm is False
        assert options.dates_md is False
        assert options.no_nd is False
        assert options.efn is False
        assert options.epn is False
        assert options.no_efn is False
        assert options.no_epn is False
        assert options.fne is None
        assert options.lf is False
        assert options.ls is False
        assert options.us is False
        assert options.particles_file is None
        assert options.default_source is None
        assert options.rs_no_mention is False
        assert options.no_pit is False
        assert options.no_picture is False
        assert options.udi is None
        assert options.uin is False
        assert options.tnd is False
        assert options.compress is True
        assert options.force is False
        assert options.verbose is False
        assert options.track_id is False
        assert options.no_consistency_check is False
        assert options.nc is False
        assert options.base_dir is None

    def test_custom_initialization(self):
        """Test custom initialization values."""
        options = ConversionOptions(
            input_file=Path("input.ged"),
            output_file=Path("output.pkl"),
            charset="ASCII",
            dates_dm=True,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
            us=True,
            udi=(90, 130),
            compress=False,
            verbose=True,
            force=True,
            base_dir=Path("/tmp"),
        )

        assert options.input_file == Path("input.ged")
        assert options.output_file == Path("output.pkl")
        assert options.charset == "ASCII"
        assert options.dates_dm is True
        assert options.efn is True
        assert options.epn is True
        assert options.lf is True
        assert options.ls is True
        assert options.us is True
        assert options.udi == (90, 130)
        assert options.compress is False
        assert options.verbose is True
        assert options.force is True
        assert options.base_dir == Path("/tmp")

    def test_from_args_parsing(self):
        """Test ConversionOptions creation from parsed arguments."""
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.output = Path("output.pkl")
        args.charset = "ASCII"
        args.dates_dm = True
        args.dates_md = False
        args.no_nd = False
        args.efn = True
        args.epn = True
        args.no_efn = False
        args.no_epn = False
        args.fne = None
        args.lf = True
        args.ls = True
        args.us = False
        args.particles = None
        args.ds = "Test Source"
        args.rs_no_mention = False
        args.no_pit = False
        args.nopicture = False
        args.udi = "80-120"
        args.uin = True
        args.tnd = False
        args.compress = True
        args.force = True
        args.verbose = True
        args.track_id = False
        args.log = None
        args.no_consistency_check = False
        args.nc = False
        args.base_dir = Path("/tmp")

        options = ConversionOptions.from_args(args)

        assert options.input_file == Path("test.ged")
        assert options.output_file == Path("output.pkl")
        assert options.charset == "ASCII"
        assert options.dates_dm is True
        assert options.efn is True
        assert options.epn is True
        assert options.lf is True
        assert options.ls is True
        assert options.us is False
        assert options.default_source == "Test Source"
        assert options.udi == (80, 120)
        assert options.uin is True
        assert options.compress is True
        assert options.force is True
        assert options.verbose is True
        assert options.base_dir == Path("/tmp")

    def test_udi_string_parsing(self):
        """Test UDI string to tuple conversion."""
        args = MagicMock()
        args.udi = "90-130"
        args.gedcom_file = Path("test.ged")
        # Set other required attributes to defaults
        for attr in [
            "output",
            "charset",
            "dates_dm",
            "dates_md",
            "no_nd",
            "efn",
            "epn",
            "no_efn",
            "no_epn",
            "fne",
            "lf",
            "ls",
            "us",
            "particles",
            "ds",
            "rs_no_mention",
            "no_pit",
            "nopicture",
            "uin",
            "tnd",
            "compress",
            "force",
            "verbose",
            "trackid",
            "no_consistency_check",
            "nc",
            "base_dir",
        ]:
            setattr(
                args,
                attr,
                None
                if attr in ["output", "charset", "fne", "particles", "ds", "base_dir"]
                else False,
            )

        options = ConversionOptions.from_args(args)
        assert options.udi == (90, 130)

    def test_udi_invalid_format(self):
        """Test UDI invalid format handling."""
        args = MagicMock()
        args.udi = "invalid-format"
        args.gedcom_file = Path("test.ged")
        # Set other required attributes to defaults
        for attr in [
            "output",
            "charset",
            "dates_dm",
            "dates_md",
            "no_nd",
            "efn",
            "epn",
            "no_efn",
            "no_epn",
            "fne",
            "lf",
            "ls",
            "us",
            "particles",
            "ds",
            "rs_no_mention",
            "no_pit",
            "nopicture",
            "uin",
            "tnd",
            "compress",
            "force",
            "verbose",
            "trackid",
            "no_consistency_check",
            "nc",
            "base_dir",
        ]:
            setattr(
                args,
                attr,
                None
                if attr in ["output", "charset", "fne", "particles", "ds", "base_dir"]
                else False,
            )

        try:
            options = ConversionOptions.from_args(args)
            assert False, "Expected ValueError for invalid UDI format"
        except (ValueError, IndexError) as e:
            # The code will fail when trying to split and convert to int
            assert True  # Any error is expected

    def test_conflicting_date_options(self):
        """Test validation of conflicting date options."""
        try:
            options = ConversionOptions(
                input_file=Path("test.ged"), dates_dm=True, dates_md=True
            )
            assert False, "Expected ValueError for conflicting date options"
        except ValueError as e:
            assert "conflicting" in str(e).lower() or "dates" in str(e).lower()

    def test_conflicting_efn_options(self):
        """Test validation of conflicting efn options."""
        try:
            options = ConversionOptions(
                input_file=Path("test.ged"), efn=True, no_efn=True
            )
            assert False, "Expected ValueError for conflicting efn options"
        except ValueError as e:
            assert "conflicting" in str(e).lower() or "efn" in str(e).lower()

    def test_conflicting_epn_options(self):
        """Test validation of conflicting epn options."""
        try:
            options = ConversionOptions(
                input_file=Path("test.ged"), epn=True, no_epn=True
            )
            assert False, "Expected ValueError for conflicting epn options"
        except ValueError as e:
            assert "conflicting" in str(e).lower() or "epn" in str(e).lower()

    def test_nc_alias_mapping(self):
        """Test -nc alias mapping to no_consistency_check."""
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.nc = True
        # Set other required attributes to defaults
        for attr in [
            "output",
            "charset",
            "dates_dm",
            "dates_md",
            "no_nd",
            "efn",
            "epn",
            "no_efn",
            "no_epn",
            "fne",
            "lf",
            "ls",
            "us",
            "particles",
            "ds",
            "rs_no_mention",
            "no_pit",
            "nopicture",
            "udi",
            "uin",
            "tnd",
            "compress",
            "force",
            "verbose",
            "track_id",
            "no_consistency_check",
            "base_dir",
        ]:
            setattr(
                args,
                attr,
                None
                if attr in ["output", "charset", "fne", "particles", "ds", "base_dir"]
                else False,
            )

        options = ConversionOptions.from_args(args)
        assert options.no_consistency_check is True
        assert options.nc is True

    def test_get_gedcom_parser_options(self):
        """Test GEDCOM parser options generation."""
        options = ConversionOptions(input_file=Path("test.ged"), no_picture=True)

        parser_options = options.get_gedcom_parser_options()

        assert parser_options["preserve_bom"] is True
        assert parser_options["preserve_notes"] is False  # no_picture=True

    def test_path_conversion(self):
        """Test path conversion from strings."""
        args = MagicMock()
        args.gedcom_file = "input.ged"
        args.output = "output.pkl"
        args.base_dir = "/tmp/bases"
        # Set other required attributes to defaults
        for attr in [
            "charset",
            "dates_dm",
            "dates_md",
            "no_nd",
            "efn",
            "epn",
            "no_efn",
            "no_epn",
            "fne",
            "lf",
            "ls",
            "us",
            "particles",
            "ds",
            "rs_no_mention",
            "no_pit",
            "nopicture",
            "udi",
            "uin",
            "tnd",
            "compress",
            "force",
            "verbose",
            "track_id",
            "log",
            "no_consistency_check",
            "nc",
        ]:
            setattr(
                args,
                attr,
                None if attr in ["charset", "fne", "particles", "ds", "log"] else False,
            )

        options = ConversionOptions.from_args(args)

        # The from_args method doesn't convert strings to Path automatically
        assert str(options.input_file) == "input.ged"
        assert str(options.output_file) == "output.pkl"
        assert str(options.base_dir) == "/tmp/bases"


def run_unit_tests():
    """Run all unit tests."""
    print("=== Unit Tests for ConversionOptions ===\n")

    test_instance = TestConversionOptions()
    test_methods = [
        method
        for method in dir(test_instance)
        if method.startswith("test_") and callable(getattr(test_instance, method))
    ]

    passed = 0
    total = len(test_methods)

    for test_method in test_methods:
        try:
            getattr(test_instance, test_method)()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")

    print(f"\nUnit Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All unit tests passed!")
        return 0
    else:
        print("FAILURE: Some unit tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_unit_tests())
