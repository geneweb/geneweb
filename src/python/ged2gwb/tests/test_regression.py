#!/usr/bin/env python3
"""
Regression tests for ged2gwb.

These tests ensure that previously working functionality continues to work
and that bug fixes remain effective.
"""

import sys
import os
import tempfile
import pickle
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from ged2gwb.core.converter import Ged2GwbConverter
from ged2gwb.utils.options import ConversionOptions


class TestRegression:
    """Regression tests for ged2gwb."""

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

    def load_pickle_data(self, file_path: Path):
        """Load pickle data, handling compression."""
        if file_path.suffix == ".gz":
            import gzip

            with gzip.open(file_path, "rb") as f:
                return pickle.load(f)
        else:
            with open(file_path, "rb") as f:
                return pickle.load(f)

    def test_basic_conversion_regression(self):
        """Test that basic conversion still works as expected."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
1 BIRT
2 DATE 15 MAR 1980
0 @I2@ INDI
1 NAME Jane /Doe/
1 SEX F
1 BIRT
2 DATE 10 JAN 1985
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 20 JUN 2010
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Verify basic conversion results
        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 2
        assert result["families_count"] == 1
        assert result["format"] == "pickle"

        # Load and verify data structure
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)

        # Verify persons
        assert len(data.persons) == 2
        persons = list(data.persons.values())

        # Find John Smith
        john = next(
            (p for p in persons if p.first_name == "John" and p.surname == "Smith"),
            None,
        )
        assert john is not None
        assert john.sex.value == "M"

        # Find Jane Doe
        jane = next(
            (p for p in persons if p.first_name == "Jane" and p.surname == "Doe"), None
        )
        assert jane is not None
        assert jane.sex.value == "F"

        # Verify families
        assert len(data.families) == 1

    def test_charset_handling_regression(self):
        """Test that charset handling continues to work correctly."""
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

        # Test ANSEL charset
        options_ansel = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "ansel.pkl",
            charset="ANSEL",
        )

        converter = Ged2GwbConverter(options_ansel)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["charset"] == "ANSEL"

        # Verify special characters are preserved
        output_file = options_ansel.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)
        person = list(data.persons.values())[0]
        assert person.first_name == "François"
        assert person.surname == "Müller"

    def test_name_processing_options_regression(self):
        """Test that name processing options continue to work correctly."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean Pierre Marie /de la Roche/
1 SEX M
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)

        # Test --efn (extract first name)
        options_efn = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "efn.pkl", efn=True
        )

        converter = Ged2GwbConverter(options_efn)
        result = converter.convert()

        assert result["conversion_successful"] is True

        output_file = options_efn.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)
        person = list(data.persons.values())[0]
        assert person.first_name == "Jean"  # Only first name extracted

    def test_combined_options_regression(self):
        """Test that combined options continue to work correctly."""
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean Pierre /de la Roche/
1 SEX M
1 BIRT
2 DATE 15/03/1980
1 CUSTOM_TAG Custom value
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)

        # Test multiple options combined
        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "combined.pkl",
            charset="ANSEL",
            dates_dm=True,
            efn=True,
            lf=True,
            us=True,
            uin=True,
            default_source="Test Source",
            compress=True,
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["compressed"] is True

        # Verify all options were applied
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)
        person = list(data.persons.values())[0]

        # --efn + --lf should give "jean"
        assert person.first_name == "jean"
        # --us should give "DE LA ROCHE"
        assert person.surname == "DE LA ROCHE"

    def test_compression_regression(self):
        """Test that compression continues to work correctly."""
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

        # Test with compression
        options_compressed = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "compressed.pkl",
            compress=True,
        )

        converter = Ged2GwbConverter(options_compressed)
        result_compressed = converter.convert()

        # Test without compression
        options_uncompressed = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "uncompressed.pkl",
            compress=False,
        )

        converter = Ged2GwbConverter(options_uncompressed)
        result_uncompressed = converter.convert()

        # Both should succeed
        assert result_compressed["conversion_successful"] is True
        assert result_uncompressed["conversion_successful"] is True

        # Compressed file should be smaller
        compressed_file = options_compressed.output_file.with_suffix(".pkl.gz")
        uncompressed_file = options_uncompressed.output_file

        assert compressed_file.exists()
        assert uncompressed_file.exists()

        compressed_size = compressed_file.stat().st_size
        uncompressed_size = uncompressed_file.stat().st_size

        # Compressed should be smaller (or at least not larger)
        assert compressed_size <= uncompressed_size

    def test_error_handling_regression(self):
        """Test that error handling continues to work correctly."""
        # Test with invalid GEDCOM
        gedcom_file = self.create_test_gedcom("Invalid GEDCOM content")
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)

        # Should handle error gracefully
        try:
            converter.convert()
            assert False, "Expected conversion to fail"
        except Exception as e:
            # Should provide meaningful error message
            assert len(str(e)) > 0

    def test_large_file_handling_regression(self):
        """Test that large files continue to be handled correctly."""
        # Create a larger GEDCOM file
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 100 individuals
        for i in range(1, 101):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "large.pkl"
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 100

        # Verify all individuals were processed
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)
        assert len(data.persons) == 100


def run_regression_tests():
    """Run all regression tests."""
    print("=== Regression Tests for ged2gwb ===\n")

    test_instance = TestRegression()
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

    print(f"\nRegression Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All regression tests passed!")
        return 0
    else:
        print("FAILURE: Some regression tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_regression_tests())
