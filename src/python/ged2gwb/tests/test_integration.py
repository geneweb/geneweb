#!/usr/bin/env python3
"""
Integration tests for ged2gwb.

These tests verify that all components work together correctly
and that the complete workflow functions as expected.
"""

import sys
import os
import tempfile
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

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


from gedcom.exceptions import GedcomParseError


class TestIntegration:
    """Integration tests for ged2gwb."""

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

    def load_msgpack_data(self, file_path: Path):
        """Load MessagePack data from directory."""
        from lib.db.io.msgpack import MessagePackReader

        # MessagePack creates directories, so we need to load from the directory
        if file_path.is_dir():
            reader = MessagePackReader(str(file_path.parent))
            db_name = file_path.stem
            return reader.load_database(db_name)
        else:
            # Fallback for file-based loading
            reader = MessagePackReader(str(file_path.parent))
            db_name = file_path.stem
            return reader.load_database(db_name)

    def test_complete_workflow(self):
        """Test the complete conversion workflow."""
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
1 DEAT
2 DATE 10 JAN 2020
0 @I2@ INDI
1 NAME Jane /Doe/
1 SEX F
1 BIRT
2 DATE 20 JUN 1985
0 @I3@ INDI
1 NAME Bob /Smith/
1 SEX M
1 BIRT
2 DATE 05 SEP 2010
1 FAMC @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
1 MARR
2 DATE 15 AUG 2005
0 TRLR
"""

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.msgpack"
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Verify conversion results
        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 3
        assert result["families_count"] == 1
        assert result["format"] == "messagepack"

        # Verify MessagePack database structure
        output_dir = Path(result["file_path"])
        assert verify_msgpack_database_structure(output_dir), "MessagePack database structure verification failed"

        # Load and verify data
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".msgpack").exists():
            output_file = output_file.with_suffix(".msgpack")

        data = self.load_msgpack_data(output_file)

        # Verify persons
        assert len(data.persons) == 3
        persons = {p.first_name + " " + p.surname: p for p in data.persons.values()}

        assert "John Smith" in persons
        assert "Jane Doe" in persons
        assert "Bob Smith" in persons

        # Verify families
        assert len(data.families) == 1
        family = list(data.families.values())[0]
        assert family is not None

    def test_all_options_integration(self):
        """Test integration with all available options."""
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
1 DEAT
2 DATE 10/01/2020
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

        gedcom_file = self.create_test_gedcom(gedcom_content)

        # Test with all options enabled
        options = ConversionOptions(
            input_file=gedcom_file,
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
            force=True,
            verbose=True,
        )

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Verify conversion was successful
        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 2
        assert result["families_count"] == 1
        assert result["charset"] == "ANSEL"

        # Load and verify processed data
        output_file = options.output_file
        if not output_file.exists() and output_file.with_suffix(".pkl.gz").exists():
            output_file = output_file.with_suffix(".pkl.gz")

        data = self.load_msgpack_data(output_file)

        # Verify name processing options were applied
        persons = list(data.persons.values())

        # First person: Jean Pierre Marie /de la Roche/
        person1 = persons[0]
        assert person1.first_name == "jean"  # --efn + --lf
        assert person1.surname == "DE LA ROCHE"  # --us

        # Second person: Marie Claire /Dupont/
        person2 = persons[1]
        assert person2.first_name == "marie"  # --efn + --lf
        assert person2.surname == "DUPONT"  # --us

    def test_charset_integration(self):
        """Test integration with different charsets."""
        charsets = ["ASCII", "MSDOS", "ANSEL"]

        for charset in charsets:
            # Create appropriate content for each charset
            if charset == "ASCII":
                content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR ASCII
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
0 TRLR
"""
            else:
                content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME François /Müller/
1 SEX M
0 TRLR
"""

            gedcom_file = self.create_test_gedcom(
                content, f"test_{charset.lower()}.ged"
            )
            options = ConversionOptions(
                input_file=gedcom_file,
                output_file=self.test_dir / f"output_{charset.lower()}.pkl",
                charset=charset,
            )

            converter = Ged2GwbConverter(options)
            result = converter.convert()

            assert result["conversion_successful"] is True
            assert result["charset"] == charset


    def test_error_handling_integration(self):
        """Test error handling in the complete workflow."""
        # Test with non-existent file
        options = ConversionOptions(
            input_file=Path("nonexistent.ged"), output_file=self.test_dir / "output.pkl"
        )

        converter = Ged2GwbConverter(options)

        try:
            converter.convert()
            assert False, "Expected conversion to fail"
        except (FileNotFoundError, GedcomParseError):
            pass

        # Test with invalid GEDCOM
        gedcom_file = self.create_test_gedcom("Invalid GEDCOM content")
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "output.msgpack"
        )

        converter = Ged2GwbConverter(options)

        try:
            converter.convert()
            assert False, "Expected conversion to fail"
        except Exception as e:
            # Should provide meaningful error
            assert len(str(e)) > 0

    def test_performance_integration(self):
        """Test performance with realistic data."""
        # Create a realistic GEDCOM file
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 50 individuals
        for i in range(1, 51):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
"""

        # Add 25 families
        for i in range(1, 26):
            husband_id = i * 2
            wife_id = i * 2 + 1
            gedcom_content += f"""0 @F{i}@ FAM
1 HUSB @I{husband_id}@
1 WIFE @I{wife_id}@
1 MARR
2 DATE {i % 28 + 1} JUN {2000 + i}
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "performance.pkl"
        )

        import time

        start_time = time.time()

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        end_time = time.time()
        execution_time = end_time - start_time

        # Verify conversion was successful
        assert result["conversion_successful"] is True
        assert result["individuals_count"] == 50
        assert result["families_count"] == 25

        # Verify performance is reasonable (should complete in under 5 seconds)
        assert execution_time < 5.0, f"Conversion took too long: {execution_time:.2f}s"


def run_integration_tests():
    """Run all integration tests."""
    print("=== Integration Tests for ged2gwb ===\n")

    test_instance = TestIntegration()
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

    print(f"\nIntegration Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All integration tests passed!")
        return 0
    else:
        print("FAILURE: Some integration tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_integration_tests())
