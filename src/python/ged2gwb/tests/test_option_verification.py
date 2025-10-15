#!/usr/bin/env python3
"""
Option verification tests for ged2gwb.

These tests verify that each command-line option works correctly
and produces the expected results with detailed verification.
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


class TestOptionVerification:
    """Detailed verification tests for all ged2gwb options."""

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

    def convert_and_verify(
        self, gedcom_content: str, options: ConversionOptions, expected_checks: dict
    ):
        """Convert GEDCOM and verify expected results."""
        gedcom_file = self.create_test_gedcom(gedcom_content)
        options.input_file = gedcom_file

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Verify conversion was successful
        assert result["conversion_successful"] is True, f"Conversion failed: {result}"

        # Load and verify data
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_pickle_data(output_file)

        # Perform expected checks
        for check_name, check_func in expected_checks.items():
            try:
                check_func(data, result)
                print(f"✓ VERIFIED: {check_name}")
            except Exception as e:
                print(f"✗ FAILED: {check_name} - {e}")
                raise

    def test_charset_options(self):
        """Test --charset option with different encodings."""
        print("\n=== Testing Charset Options ===")

        # Test ANSEL charset
        gedcom_content_ansel = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR ANSEL
0 @I1@ INDI
1 NAME François /Müller/
1 SEX M
0 TRLR
"""

        options_ansel = ConversionOptions(
            input_file=Path("dummy.ged"),  # Will be overridden in convert_and_verify
            output_file=self.test_dir / "ansel.pkl",
            charset="ANSEL",
        )

        def verify_ansel(data, result):
            assert result["charset"] == "ANSEL"
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            assert person.first_name == "François"
            assert person.surname == "Müller"

        self.convert_and_verify(
            gedcom_content_ansel, options_ansel, {"ANSEL charset": verify_ansel}
        )

        # Test ASCII charset
        gedcom_content_ascii = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR ASCII
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
0 TRLR
"""

        options_ascii = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "ascii.pkl",
            charset="ASCII",
        )

        def verify_ascii(data, result):
            assert result["charset"] == "ASCII"
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            assert person.first_name == "John"
            assert person.surname == "Smith"

        self.convert_and_verify(
            gedcom_content_ascii, options_ascii, {"ASCII charset": verify_ascii}
        )

        # Test MSDOS charset
        gedcom_content_msdos = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR MSDOS
0 @I1@ INDI
1 NAME José /García/
1 SEX M
0 TRLR
"""

        options_msdos = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "msdos.pkl",
            charset="MSDOS",
        )

        def verify_msdos(data, result):
            assert result["charset"] == "MSDOS"
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            assert person.first_name == "Jos├®"
            assert person.surname == "Garc├¡a"

        self.convert_and_verify(
            gedcom_content_msdos, options_msdos, {"MSDOS charset": verify_msdos}
        )

    def test_date_format_options(self):
        """Test --dates-dm and --dates-md options."""
        print("\n=== Testing Date Format Options ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
1 BIRT
2 DATE 15/03/1980
1 DEAT
2 DATE 10/01/2020
0 TRLR
"""

        # Test --dates-dm
        options_dm = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "dates_dm.pkl",
            dates_dm=True,
        )

        def verify_dates_dm(data, result):
            assert result["dates_dm"] is True
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            # Verify birth date was processed with DM format
            assert person.birth is not None

        self.convert_and_verify(
            gedcom_content, options_dm, {"Dates DM format": verify_dates_dm}
        )

        # Test --dates-md
        options_md = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "dates_md.pkl",
            dates_md=True,
        )

        def verify_dates_md(data, result):
            assert result["dates_md"] is True
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            # Verify birth date was processed with MD format
            assert person.birth is not None

        self.convert_and_verify(
            gedcom_content, options_md, {"Dates MD format": verify_dates_md}
        )

    def test_name_processing_options(self):
        """Test name processing options: --efn, --epn, --lf, --ls, --us."""
        print("\n=== Testing Name Processing Options ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean Pierre Marie /de la Roche/
1 SEX M
0 @I2@ INDI
1 NAME Marie Claire /Dupont/
1 SEX F
0 TRLR
"""

        # Test --efn (extract first name)
        options_efn = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "efn.pkl",
            efn=True,
        )

        def verify_efn(data, result):
            assert result["efn"] is True
            assert len(data.persons) == 2
            persons = list(data.persons.values())

            # First person: Jean Pierre Marie -> Jean
            person1 = persons[0]
            assert person1.first_name == "Jean", (
                f"Expected 'Jean', got '{person1.first_name}'"
            )

            # Second person: Marie Claire -> Marie
            person2 = persons[1]
            assert person2.first_name == "Marie", (
                f"Expected 'Marie', got '{person2.first_name}'"
            )

        self.convert_and_verify(
            gedcom_content, options_efn, {"Extract First Name": verify_efn}
        )

        # Test --epn (extract public name)
        options_epn = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "epn.pkl",
            epn=True,
        )

        def verify_epn(data, result):
            assert result["epn"] is True
            assert len(data.persons) == 2
            persons = list(data.persons.values())

            # Should keep full names
            person1 = persons[0]
            assert person1.first_name == "Jean Pierre Marie"

            person2 = persons[1]
            assert person2.first_name == "Marie Claire"

        self.convert_and_verify(
            gedcom_content, options_epn, {"Extract Public Name": verify_epn}
        )

        # Test --lf (lowercase first names)
        options_lf = ConversionOptions(
            input_file=Path("dummy.ged"), output_file=self.test_dir / "lf.pkl", lf=True
        )

        def verify_lf(data, result):
            assert result["lf"] is True
            assert len(data.persons) == 2
            persons = list(data.persons.values())

            # All first names should be lowercase
            for person in persons:
                assert person.first_name.islower(), (
                    f"Expected lowercase, got '{person.first_name}'"
                )

        self.convert_and_verify(
            gedcom_content, options_lf, {"Lowercase First Names": verify_lf}
        )

        # Test --ls (lowercase surnames with particles)
        options_ls = ConversionOptions(
            input_file=Path("dummy.ged"), output_file=self.test_dir / "ls.pkl", ls=True
        )

        def verify_ls(data, result):
            assert result["ls"] is True
            assert len(data.persons) == 2
            persons = list(data.persons.values())

            # Surnames should be title case with particles lowercase
            person1 = persons[0]
            assert person1.surname == "de la Roche", (
                f"Expected 'de la Roche', got '{person1.surname}'"
            )

            person2 = persons[1]
            assert person2.surname == "Dupont", (
                f"Expected 'Dupont', got '{person2.surname}'"
            )

        self.convert_and_verify(
            gedcom_content, options_ls, {"Lowercase Surnames": verify_ls}
        )

        # Test --us (uppercase surnames)
        options_us = ConversionOptions(
            input_file=Path("dummy.ged"), output_file=self.test_dir / "us.pkl", us=True
        )

        def verify_us(data, result):
            assert result["us"] is True
            assert len(data.persons) == 2
            persons = list(data.persons.values())

            # All surnames should be uppercase
            for person in persons:
                assert person.surname.isupper(), (
                    f"Expected uppercase, got '{person.surname}'"
                )

        self.convert_and_verify(
            gedcom_content, options_us, {"Uppercase Surnames": verify_us}
        )

    def test_combined_name_options(self):
        """Test combinations of name processing options."""
        print("\n=== Testing Combined Name Options ===")

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

        # Test --efn + --lf + --us
        options_combined = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "combined.pkl",
            efn=True,
            lf=True,
            us=True,
        )

        def verify_combined(data, result):
            assert result["efn"] is True
            assert result["lf"] is True
            assert result["us"] is True
            assert len(data.persons) == 1

            person = list(data.persons.values())[0]
            # --efn should extract "Jean", --lf should make it "jean", --us should make surname "DE LA ROCHE"
            assert person.first_name == "jean", (
                f"Expected 'jean', got '{person.first_name}'"
            )
            assert person.surname == "DE LA ROCHE", (
                f"Expected 'DE LA ROCHE', got '{person.surname}'"
            )

        self.convert_and_verify(
            gedcom_content, options_combined, {"Combined Name Options": verify_combined}
        )

    def test_udi_option(self):
        """Test --udi option for undefined individual handling."""
        print("\n=== Testing UDI Option ===")

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
0 TRLR
"""

        options_udi = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "udi.pkl",
            udi=(80, 120),
        )

        def verify_udi(data, result):
            assert result["udi"] == (80, 120)
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            # Verify person was processed with UDI constraints
            assert person.first_name == "John"
            assert person.surname == "Smith"

        self.convert_and_verify(gedcom_content, options_udi, {"UDI Option": verify_udi})

    def test_uin_option(self):
        """Test --uin option for untreated tags in notes."""
        print("\n=== Testing UIN Option ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
1 CUSTOM_TAG Custom value
1 ANOTHER_TAG Another value
0 TRLR
"""

        options_uin = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "uin.pkl",
            uin=True,
        )

        def verify_uin(data, result):
            assert result["uin"] is True
            assert len(data.persons) == 1
            person = list(data.persons.values())[0]
            # Verify custom tags were processed into notes
            assert person.notes is not None
            assert len(person.notes) > 0

        self.convert_and_verify(gedcom_content, options_uin, {"UIN Option": verify_uin})

    def test_compression_option(self):
        """Test --compress option."""
        print("\n=== Testing Compression Option ===")

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

        # Test with compression
        options_compressed = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "compressed.pkl",
            compress=True,
        )

        def verify_compressed(data, result):
            assert result["compressed"] is True
            assert len(data.persons) == 1
            # Verify compressed file was created
            compressed_file = options_compressed.output_file.with_suffix(".pkl.gz")
            assert compressed_file.exists()

        self.convert_and_verify(
            gedcom_content, options_compressed, {"Compression": verify_compressed}
        )

        # Test without compression
        options_uncompressed = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "uncompressed.pkl",
            compress=False,
        )

        def verify_uncompressed(data, result):
            assert result["compressed"] is False
            assert len(data.persons) == 1
            # Verify uncompressed file was created
            assert options_uncompressed.output_file.exists()

        self.convert_and_verify(
            gedcom_content,
            options_uncompressed,
            {"No Compression": verify_uncompressed},
        )

    def test_verbose_option(self):
        """Test --verbose option."""
        print("\n=== Testing Verbose Option ===")

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

        options_verbose = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "verbose.pkl",
            verbose=True,
        )

        def verify_verbose(data, result):
            assert result["verbose"] is True
            assert len(data.persons) == 1
            # Verbose mode should provide more detailed output
            assert result["conversion_successful"] is True

        self.convert_and_verify(
            gedcom_content, options_verbose, {"Verbose Mode": verify_verbose}
        )

    def test_force_option(self):
        """Test --force option."""
        print("\n=== Testing Force Option ===")

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

        options_force = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "force.pkl",
            force=True,
        )

        def verify_force(data, result):
            assert result["force"] is True
            assert len(data.persons) == 1
            # Force mode should allow overwriting existing files
            assert result["conversion_successful"] is True

        self.convert_and_verify(
            gedcom_content, options_force, {"Force Mode": verify_force}
        )

    def test_default_source_option(self):
        """Test --ds option for default source."""
        print("\n=== Testing Default Source Option ===")

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

        options_ds = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "ds.pkl",
            default_source="Test Source",
        )

        def verify_ds(data, result):
            assert result["default_source"] == "Test Source"
            assert len(data.persons) == 1
            # Default source should be applied
            assert result["conversion_successful"] is True

        self.convert_and_verify(
            gedcom_content, options_ds, {"Default Source": verify_ds}
        )

    def test_all_options_combined(self):
        """Test all options working together."""
        print("\n=== Testing All Options Combined ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Jean Pierre Marie /de la Roche/
1 SEX M
1 BIRT
2 DATE 15/03/1980
1 CUSTOM_TAG Custom value
0 @I2@ INDI
1 NAME Marie Claire /Dupont/
1 SEX F
1 BIRT
2 DATE 20/06/1985
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 15/08/2005
0 TRLR
"""

        options_all = ConversionOptions(
            input_file=Path("dummy.ged"),
            output_file=self.test_dir / "all_options.pkl",
            charset="ANSEL",
            dates_dm=True,
            efn=True,
            epn=True,
            lf=True,
            ls=True,
            us=True,
            udi=(80, 120),
            uin=True,
            default_source="Test Source",
            compress=True,
            force=True,
            verbose=True,
        )

        def verify_all_options(data, result):
            # Verify all options were applied
            assert result["charset"] == "ANSEL"
            assert result["dates_dm"] is True
            assert result["efn"] is True
            assert result["epn"] is True
            assert result["lf"] is True
            assert result["ls"] is True
            assert result["us"] is True
            assert result["udi"] == (80, 120)
            assert result["uin"] is True
            assert result["default_source"] == "Test Source"
            assert result["compressed"] is True
            assert result["force"] is True
            assert result["verbose"] is True

            # Verify data was processed correctly
            assert len(data.persons) == 2
            assert len(data.families) == 1

            persons = list(data.persons.values())

            # First person: Jean Pierre Marie /de la Roche/
            person1 = persons[0]
            assert person1.first_name == "jean"  # --efn + --lf
            assert person1.surname == "DE LA ROCHE"  # --us

            # Second person: Marie Claire /Dupont/
            person2 = persons[1]
            assert person2.first_name == "marie"  # --efn + --lf
            assert person2.surname == "DUPONT"  # --us

        self.convert_and_verify(
            gedcom_content, options_all, {"All Options Combined": verify_all_options}
        )


def run_option_verification_tests():
    """Run all option verification tests."""
    print("=== Option Verification Tests for ged2gwb ===\n")

    test_instance = TestOptionVerification()
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

    print(f"\nOption Verification Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All option verification tests passed!")
        return 0
    else:
        print("FAILURE: Some option verification tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_option_verification_tests())
