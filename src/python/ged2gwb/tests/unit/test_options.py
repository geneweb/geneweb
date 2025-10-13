#!/usr/bin/env python3
"""
Unit tests for GED2GWB options module.

Tests the ConversionOptions class and option validation.
"""

import sys
from pathlib import Path
from unittest.mock import MagicMock

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent.parent))

from ged2gwb.utils.options import ConversionOptions


class TestConversionOptions:
    """Test cases for ConversionOptions class."""

    def test_default_initialization(self):
        """Test default initialization values."""
        options = ConversionOptions(input_file=Path("test.ged"))

        assert options.input_file == Path("test.ged")
        assert options.output_file == Path("a.pkl")
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
            udi=(90, 130),
            compress=True,
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
        assert options.udi == (90, 130)
        assert options.compress is True
        assert options.verbose is True
        assert options.force is True
        assert options.base_dir == Path("/tmp")

    def test_from_args(self):
        """Test ConversionOptions creation from parsed arguments."""
        # Mock parsed arguments
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.output = Path("output.pkl")
        args.charset = None
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
        args.ds = None
        args.rs_no_mention = False
        args.no_pit = False
        args.nopicture = False
        args.udi = "80-120"
        args.uin = False
        args.tnd = False
        args.compress = True
        args.force = True
        args.verbose = True
        args.trackid = False
        args.no_consistency_check = False
        args.nc = False
        args.base_dir = Path("/tmp")

        options = ConversionOptions.from_args(args)

        assert options.input_file == Path("test.ged")
        assert options.output_file == Path("output.pkl")
        assert options.charset is None
        assert options.dates_dm is True
        assert options.efn is True
        assert options.epn is True
        assert options.lf is True
        assert options.ls is True
        assert options.udi == (80, 120)
        assert options.compress is True
        assert options.force is True
        assert options.verbose is True
        assert options.base_dir == Path("/tmp")

    def test_udi_conversion(self):
        """Test UDI string to tuple conversion."""
        # Test valid UDI format
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
                (
                    None
                    if attr
                    in ["output", "charset", "fne", "particles", "ds", "base_dir"]
                    else False
                ),
            )

        options = ConversionOptions.from_args(args)
        assert options.udi == (90, 130)

    def test_udi_invalid_format(self):
        """Test UDI invalid format handling."""
        # Test direct creation with invalid UDI format
        try:
            options = ConversionOptions(
                input_file=Path("test.ged"),
                output_file=Path("output.pkl"),
                udi="invalid-format",  # This should cause validation error
            )
            assert False, "Should have raised ValueError for invalid UDI format"
        except ValueError as e:
            # The error should be about UDI format validation
            assert (
                "must be a tuple" in str(e)
                or "Invalid UDI format" in str(e)
                or "udi" in str(e).lower()
            )

    def test_boolean_options_mapping(self):
        """Test boolean options mapping from args."""
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.output = Path("output.pkl")
        args.charset = None
        args.dates_dm = True
        args.dates_md = False
        args.no_nd = True
        args.efn = True
        args.epn = True
        args.no_efn = False
        args.no_epn = False
        args.fne = None
        args.lf = True
        args.ls = True
        args.us = False
        args.particles = None
        args.ds = None
        args.rs_no_mention = True
        args.no_pit = True
        args.nopicture = True
        args.udi = "80-120"
        args.uin = True
        args.tnd = True
        args.compress = True
        args.force = True
        args.verbose = True
        args.trackid = True
        args.no_consistency_check = True
        args.nc = True
        args.base_dir = None

        options = ConversionOptions.from_args(args)

        assert options.dates_dm is True
        assert options.dates_md is False
        assert options.no_nd is True
        assert options.efn is True
        assert options.epn is True
        assert options.no_efn is False
        assert options.no_epn is False
        assert options.lf is True
        assert options.ls is True
        assert options.us is False
        assert options.rs_no_mention is True
        assert options.no_pit is True
        assert options.no_picture is True
        assert options.uin is True
        assert options.tnd is True
        assert options.compress is True
        assert options.force is True
        assert options.verbose is True
        assert options.track_id is True
        assert options.no_consistency_check is True
        assert options.nc is True

    def test_nc_alias_mapping(self):
        """Test -nc alias mapping."""
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.output = Path("output.pkl")
        args.charset = None
        args.dates_dm = False
        args.dates_md = False
        args.no_nd = False
        args.efn = False
        args.epn = False
        args.no_efn = False
        args.no_epn = False
        args.fne = None
        args.lf = False
        args.ls = False
        args.us = False
        args.particles = None
        args.ds = None
        args.rs_no_mention = False
        args.no_pit = False
        args.nopicture = False
        args.udi = "80-120"
        args.uin = False
        args.tnd = False
        args.compress = False
        args.force = False
        args.verbose = False
        args.trackid = False
        args.no_consistency_check = False
        args.nc = True  # This should map to no_consistency_check
        args.base_dir = None

        options = ConversionOptions.from_args(args)

        assert options.no_consistency_check is True
        assert options.nc is True

    def test_path_conversion(self):
        """Test path conversion from strings."""
        args = MagicMock()
        args.gedcom_file = Path("input.ged")
        args.output = Path("output.pkl")
        args.charset = None
        args.dates_dm = False
        args.dates_md = False
        args.no_nd = False
        args.efn = False
        args.epn = False
        args.no_efn = False
        args.no_epn = False
        args.fne = None
        args.lf = False
        args.ls = False
        args.us = False
        args.particles = None
        args.ds = None
        args.rs_no_mention = False
        args.no_pit = False
        args.nopicture = False
        args.udi = "80-120"
        args.uin = False
        args.tnd = False
        args.compress = False
        args.force = False
        args.verbose = False
        args.trackid = False
        args.no_consistency_check = False
        args.nc = False
        args.base_dir = Path("/tmp/bases")

        options = ConversionOptions.from_args(args)

        assert isinstance(options.input_file, Path)
        assert isinstance(options.output_file, Path)
        assert isinstance(options.base_dir, Path)
        assert options.input_file == Path("input.ged")
        assert options.output_file == Path("output.pkl")
        assert options.base_dir == Path("/tmp/bases")

    def test_optional_attributes(self):
        """Test optional attributes handling."""
        args = MagicMock()
        args.gedcom_file = Path("test.ged")
        args.output = None  # Should use default
        args.charset = None
        args.dates_dm = False
        args.dates_md = False
        args.no_nd = False
        args.efn = False
        args.epn = False
        args.no_efn = False
        args.no_epn = False
        args.fne = None
        args.lf = False
        args.ls = False
        args.us = False
        args.particles = None
        args.ds = None
        args.rs_no_mention = False
        args.no_pit = False
        args.nopicture = False
        args.udi = "80-120"
        args.uin = False
        args.tnd = False
        args.compress = False
        args.force = False
        args.verbose = False
        args.trackid = False
        args.no_consistency_check = False
        args.nc = False
        args.base_dir = None

        options = ConversionOptions.from_args(args)

        # Should use default output file
        assert options.output_file == Path("a.pkl")
        assert options.fne is None
        assert options.particles_file is None
        assert options.default_source is None
        assert options.base_dir is None


def main():
    """Run all options unit tests."""
    print("Running Options unit tests...")

    options_tests = TestConversionOptions()
    test_methods = [
        method for method in dir(options_tests) if method.startswith("test_")
    ]

    passed = 0
    total = len(test_methods)

    for test_method in test_methods:
        try:
            getattr(options_tests, test_method)()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")

    print(f"\nOptions Unit Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All options unit tests passed!")
        return 0
    else:
        print("FAILURE: Some options unit tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(main())
