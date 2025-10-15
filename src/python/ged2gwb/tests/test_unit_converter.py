#!/usr/bin/env python3
"""
Unit tests for the Ged2GwbConverter class.

These tests verify the core functionality of the converter in isolation.
"""

import sys
import os
import tempfile
import pickle
from pathlib import Path
from unittest.mock import Mock, patch

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from ged2gwb.core.converter import Ged2GwbConverter
from ged2gwb.utils.options import ConversionOptions


class TestGed2GwbConverter:
    """Unit tests for Ged2GwbConverter."""

    def setup_method(self):
        """Set up test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_dir = Path(self.temp_dir)

    def teardown_method(self):
        """Clean up test environment."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_test_gedcom(self, content: str, filename: str = "test.ged") -> Path:
        """Create a test GEDCOM file."""
        file_path = self.test_dir / filename
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(content)
        return file_path

    def test_converter_initialization(self):
        """Test converter initialization with valid options."""
        gedcom_file = self.create_test_gedcom("0 HEAD\n1 GEDC\n2 VERS 5.5.1\n0 TRLR\n")
        options = ConversionOptions(input_file=gedcom_file)

        converter = Ged2GwbConverter(options)

        assert converter.options == options
        assert converter.parser is not None
        assert converter.gedcom_converter is not None

    def test_converter_initialization_with_charset(self):
        """Test converter initialization with charset option."""
        gedcom_file = self.create_test_gedcom("0 HEAD\n1 GEDC\n2 VERS 5.5.1\n0 TRLR\n")
        options = ConversionOptions(input_file=gedcom_file, charset="ASCII")

        converter = Ged2GwbConverter(options)

        assert converter.options.charset == "ASCII"

    def test_convert_simple_individual(self):
        """Test conversion of a simple individual."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Verify conversion was successful
        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 1
        assert result["families_count"] == 0

        # Verify output file was created
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        assert output_file.exists()

        # Load and verify data
        if output_file.suffix == ".gz":
            import gzip

            with gzip.open(output_file, "rb") as f:
                data = pickle.load(f)
        else:
            with open(output_file, "rb") as f:
                data = pickle.load(f)

        assert len(data.persons) == 1
        person = list(data.persons.values())[0]
        assert person.first_name == "John"
        assert person.surname == "Smith"

    def test_convert_with_charset_handling(self):
        """Test conversion with different charsets."""
        # Test with UTF-8 content
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME François /Müller/
1 SEX M
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "output.pkl",
            charset="ANSEL",
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["charset"] == "ANSEL"

    def test_convert_with_name_processing_options(self):
        """Test conversion with name processing options."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean Pierre /de la Roche/
1 SEX M
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "output.pkl",
            efn=True,  # Extract first name only
            lf=True,  # Lowercase first names
            us=True,  # Uppercase surnames
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True

        # Load and verify processed names
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        if output_file.suffix == ".gz":
            import gzip

            with gzip.open(output_file, "rb") as f:
                data = pickle.load(f)
        else:
            with open(output_file, "rb") as f:
                data = pickle.load(f)

        person = list(data.persons.values())[0]
        assert person.first_name == "jean"  # --efn + --lf
        assert person.surname == "DE LA ROCHE"  # --us

    def test_convert_with_compression(self):
        """Test conversion with compression enabled."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Test /Person/
1 SEX M
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "output.pkl",
            compress=True,
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["compressed"] is True

        # Verify compressed file was created
        compressed_file = options.output_file.with_suffix(".pkl.gz")
        assert compressed_file.exists()

    def test_convert_with_family(self):
        """Test conversion with family relationships."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
0 @I2@ INDI
1 NAME Jane /Doe/
1 SEX F
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 2020
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 2
        assert result["families_count"] == 1

    def test_convert_error_handling(self):
        """Test error handling during conversion."""
        # Create invalid GEDCOM file
        gedcom_file = self.create_test_gedcom("Invalid GEDCOM content")
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)

        # Should raise an exception
        try:
            converter.convert()
            assert False, "Expected conversion to fail"
        except Exception as e:
            # Expected to fail
            assert "Error" in str(e) or "Failed" in str(e)

    def test_validate_input_file_not_found(self):
        """Test validation when input file doesn't exist."""
        options = ConversionOptions(
            input_file=Path("nonexistent.ged"), output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)

        try:
            converter.validate_input()
            assert False, "Expected validation to fail"
        except FileNotFoundError:
            # Expected
            pass

    def test_validate_database_name_validation(self):
        """Test database name validation."""
        gedcom_file = self.create_test_gedcom("0 HEAD\n1 GEDC\n2 VERS 5.5.1\n0 TRLR\n")
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "invalid@name.pkl"
        )

        converter = Ged2GwbConverter(options)

        try:
            converter.validate_input()
            assert False, "Expected validation to fail"
        except ValueError as e:
            assert "forbidden character" in str(e)


def run_unit_tests():
    """Run all unit tests."""
    print("=== Unit Tests for Ged2GwbConverter ===\n")

    test_instance = TestGed2GwbConverter()
    test_methods = [
        method
        for method in dir(test_instance)
        if method.startswith("test_") and callable(getattr(test_instance, method))
    ]

    passed = 0
    total = len(test_methods)

    for test_method in test_methods:
        try:
            test_instance.setup_method()
            getattr(test_instance, test_method)()
            test_instance.teardown_method()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")
            test_instance.teardown_method()

    print(f"\nUnit Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All unit tests passed!")
        return 0
    else:
        print("FAILURE: Some unit tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_unit_tests())
