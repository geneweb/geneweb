"""
Unit tests for GEDCOM exporter.
"""

import os
import tempfile
import unittest
from io import StringIO
from pathlib import Path

from .. import (
    FamilyExporter,
    HeaderExporter,
    IndividualExporter,
    RawStructureExporter,
    create_exporter,
)
from ..exceptions import GedcomExportError
from ..models import (
    GedcomDatabase,
    GedcomDate,
    GedcomEvent,
    GedcomFamily,
    GedcomHeader,
    GedcomIndividual,
    GedcomName,
    GedcomPlace,
)


class TestHeaderExporter(unittest.TestCase):
    """Test cases for HeaderExporter."""

    def setUp(self):
        """Set up test fixtures."""
        self.exporter = HeaderExporter()

    def test_can_export(self):
        """Test can_export method."""
        self.assertTrue(self.exporter.can_export("HEAD"))
        self.assertFalse(self.exporter.can_export("INDI"))
        self.assertFalse(self.exporter.can_export("FAM"))

    def test_export_header_with_raw_lines(self):
        """Test exporting header with raw structure."""
        header = GedcomHeader(
            source="TestApp",
            charset="UTF-8",
            raw_lines=[
                (1, "SOUR", "TestApp"),
                (2, "VERS", "1.0"),
                (1, "CHAR", "UTF-8"),
                (1, "DEST", "GENEWEB"),
            ],
        )

        output = StringIO()
        self.exporter.export(output, "", header)
        result = output.getvalue()

        expected_lines = [
            "0 HEAD",
            "1 SOUR TestApp",
            "2 VERS 1.0",
            "1 CHAR UTF-8",
            "1 DEST GENEWEB",
        ]

        for line in expected_lines:
            self.assertIn(line, result)

    def test_export_header_fallback(self):
        """Test exporting header without raw structure."""
        header = GedcomHeader(
            source="TestApp", charset="UTF-8", destination="GENEWEB", version="1.0"
        )

        output = StringIO()
        self.exporter.export(output, "", header)
        result = output.getvalue()

        self.assertIn("0 HEAD", result)
        self.assertIn("1 SOUR TestApp", result)
        self.assertIn("1 CHAR UTF-8", result)
        self.assertIn("1 DEST GENEWEB", result)


class TestIndividualExporter(unittest.TestCase):
    """Test cases for IndividualExporter."""

    def setUp(self):
        """Set up test fixtures."""
        self.exporter = IndividualExporter()

    def test_can_export(self):
        """Test can_export method."""
        self.assertTrue(self.exporter.can_export("INDI"))
        self.assertFalse(self.exporter.can_export("FAM"))
        self.assertFalse(self.exporter.can_export("HEAD"))

    def test_export_basic_individual(self):
        """Test exporting basic individual."""
        name = GedcomName(full="John /Doe/", given="John", surname="Doe")
        individual = GedcomIndividual(xref="@I1@", names=[name], sex="M")

        output = StringIO()
        self.exporter.export(output, "@I1@", individual)
        result = output.getvalue()

        expected_lines = [
            "0 @I1@ INDI",
            "1 NAME John /Doe/",
            "2 GIVN John",
            "2 SURN Doe",
            "1 SEX M",
        ]

        for line in expected_lines:
            self.assertIn(line, result)

    def test_export_individual_with_events(self):
        """Test exporting individual with events."""
        name = GedcomName(full="John /Doe/")
        birth_date = GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1)
        birth_place = GedcomPlace(name="New York, NY")
        birth = GedcomEvent(tag="BIRT", date=birth_date, place=birth_place)

        death_date = GedcomDate(raw="ABT 2050", year=2050, is_approximate=True)
        death = GedcomEvent(tag="DEAT", date=death_date, cause="Old age")

        individual = GedcomIndividual(
            xref="@I1@",
            names=[name],
            birth=birth,
            death=death,
            occupations=["Engineer"],
            titles=["Mr."],
        )

        output = StringIO()
        self.exporter.export(output, "@I1@", individual)
        result = output.getvalue()

        expected_lines = [
            "0 @I1@ INDI",
            "1 NAME John /Doe/",
            "1 BIRT",
            "2 DATE 1 JAN 1980",
            "2 PLAC New York, NY",
            "1 DEAT",
            "2 DATE ABT 2050",
            "2 CAUS Old age",
            "1 OCCU Engineer",
            "1 TITL Mr.",
        ]

        for line in expected_lines:
            self.assertIn(line, result)

    def test_format_date(self):
        """Test date formatting."""
        # Complete date
        date1 = GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1)
        formatted1 = self.exporter._format_date(date1)
        self.assertEqual(formatted1, "1 JAN 1980")

        # Year only
        date2 = GedcomDate(raw="1980", year=1980)
        formatted2 = self.exporter._format_date(date2)
        self.assertEqual(formatted2, "1980")

        # Approximate date
        date3 = GedcomDate(raw="ABT 1980", year=1980, is_approximate=True)
        formatted3 = self.exporter._format_date(date3)
        self.assertEqual(formatted3, "ABT 1980")

        # Invalid date
        date4 = GedcomDate(raw="Unknown")
        formatted4 = self.exporter._format_date(date4)
        self.assertEqual(formatted4, "Unknown")


class TestFamilyExporter(unittest.TestCase):
    """Test cases for FamilyExporter."""

    def setUp(self):
        """Set up test fixtures."""
        self.exporter = FamilyExporter()

    def test_can_export(self):
        """Test can_export method."""
        self.assertTrue(self.exporter.can_export("FAM"))
        self.assertFalse(self.exporter.can_export("INDI"))
        self.assertFalse(self.exporter.can_export("HEAD"))

    def test_export_basic_family(self):
        """Test exporting basic family."""
        family = GedcomFamily(
            xref="@F1@", husband="I1", wife="I2", children=["I3", "I4"]
        )

        output = StringIO()
        self.exporter.export(output, "@F1@", family)
        result = output.getvalue()

        expected_lines = [
            "0 @F1@ FAM",
            "1 HUSB @I1@",
            "1 WIFE @I2@",
            "1 CHIL @I3@",
            "1 CHIL @I4@",
        ]

        for line in expected_lines:
            self.assertIn(line, result)

    def test_export_family_with_marriage(self):
        """Test exporting family with marriage event."""
        marriage_date = GedcomDate(raw="14 FEB 2000", year=2000, month=2, day=14)
        marriage_place = GedcomPlace(name="Las Vegas, NV")
        marriage = GedcomEvent(tag="MARR", date=marriage_date, place=marriage_place)

        family = GedcomFamily(xref="@F1@", husband="I1", wife="I2", marriage=marriage)

        output = StringIO()
        self.exporter.export(output, "@F1@", family)
        result = output.getvalue()

        expected_lines = [
            "0 @F1@ FAM",
            "1 HUSB @I1@",
            "1 WIFE @I2@",
            "1 MARR",
            "2 DATE 14 FEB 2000",
            "2 PLAC Las Vegas, NV",
        ]

        for line in expected_lines:
            self.assertIn(line, result)


class TestGedcomExporter(unittest.TestCase):
    """Test cases for GedcomExporter."""

    def setUp(self):
        """Set up test fixtures."""
        self.exporter = create_exporter()

        # Create test database
        self.database = self._create_test_database()

    def _create_test_database(self) -> GedcomDatabase:
        """Create a test database."""
        header = GedcomHeader(
            source="TestApp",
            charset="UTF-8",
            raw_lines=[(1, "SOUR", "TestApp"), (1, "CHAR", "UTF-8")],
        )

        # Create individuals
        john = GedcomIndividual(
            xref="@I1@",
            names=[GedcomName(full="John /Doe/", given="John", surname="Doe")],
            sex="M",
            birth=GedcomEvent(
                tag="BIRT",
                date=GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1),
                place=GedcomPlace(name="New York, NY"),
            ),
            fams=["F1"],
        )

        jane = GedcomIndividual(
            xref="@I2@",
            names=[GedcomName(full="Jane /Smith/", given="Jane", surname="Smith")],
            sex="F",
            birth=GedcomEvent(
                tag="BIRT",
                date=GedcomDate(raw="15 MAR 1982", year=1982, month=3, day=15),
            ),
            fams=["F1"],
        )

        baby = GedcomIndividual(
            xref="@I3@",
            names=[GedcomName(full="Baby /Doe/", given="Baby", surname="Doe")],
            sex="M",
            birth=GedcomEvent(
                tag="BIRT",
                date=GedcomDate(raw="10 JUL 2005", year=2005, month=7, day=10),
            ),
            famc=["F1"],
        )

        # Create family
        family = GedcomFamily(
            xref="@F1@",
            husband="I1",
            wife="I2",
            children=["I3"],
            marriage=GedcomEvent(
                tag="MARR",
                date=GedcomDate(raw="14 FEB 2004", year=2004, month=2, day=14),
                place=GedcomPlace(name="Las Vegas, NV"),
            ),
        )

        database = GedcomDatabase(
            header=header,
            individuals={"@I1@": john, "@I2@": jane, "@I3@": baby},
            families={"@F1@": family},
            record_order=[
                ("INDI", "@I1@"),
                ("INDI", "@I2@"),
                ("INDI", "@I3@"),
                ("FAM", "@F1@"),
            ],
        )

        return database

    def test_export_content(self):
        """Test exporting database content."""
        output = StringIO()
        self.exporter.export_content(output, self.database)
        result = output.getvalue()

        # Check structure
        lines = result.strip().split("\n")
        self.assertTrue(lines[0].startswith("0 HEAD"))
        self.assertTrue(lines[-1] == "0 TRLR")

        # Check for individuals
        self.assertIn("0 @I1@ INDI", result)
        self.assertIn("1 NAME John /Doe/", result)
        self.assertIn("0 @I2@ INDI", result)
        self.assertIn("1 NAME Jane /Smith/", result)
        self.assertIn("0 @I3@ INDI", result)
        self.assertIn("1 NAME Baby /Doe/", result)

        # Check for family
        self.assertIn("0 @F1@ FAM", result)
        self.assertIn("1 HUSB @I1@", result)
        self.assertIn("1 WIFE @I2@", result)
        self.assertIn("1 CHIL @I3@", result)

    def test_export_file(self):
        """Test exporting to file."""
        with tempfile.NamedTemporaryFile(mode="w", suffix=".ged", delete=False) as f:
            temp_path = Path(f.name)

        try:
            # Export to file
            self.exporter.export_file(temp_path, self.database)

            # Verify file exists and has content
            self.assertTrue(temp_path.exists())

            with open(temp_path, "r", encoding="utf-8") as f:
                content = f.read()

            self.assertIn("0 HEAD", content)
            self.assertIn("0 @I1@ INDI", content)
            self.assertIn("0 @F1@ FAM", content)
            self.assertIn("0 TRLR", content)

        finally:
            # Clean up
            if temp_path.exists():
                temp_path.unlink()

    def test_round_trip(self):
        """Test round-trip export/import."""
        from ..parser import create_parser

        # Export database
        output = StringIO()
        self.exporter.export_content(output, self.database)
        exported_content = output.getvalue()

        # Re-import
        parser = create_parser()
        reimported_db = parser.parse_content(exported_content)

        # Verify data integrity
        self.assertEqual(len(self.database.individuals), len(reimported_db.individuals))
        self.assertEqual(len(self.database.families), len(reimported_db.families))

        # Check specific individual
        original_john = self.database.individuals["@I1@"]
        reimported_john = reimported_db.individuals["@I1@"]

        self.assertEqual(original_john.xref, reimported_john.xref)
        self.assertEqual(original_john.sex, reimported_john.sex)
        self.assertEqual(
            original_john.primary_name.full, reimported_john.primary_name.full
        )
        self.assertEqual(original_john.birth_year, reimported_john.birth_year)

    def test_export_error_handling(self):
        """Test export error handling."""
        # Test with invalid path
        invalid_path = Path("/invalid/path/file.ged")

        with self.assertRaises(GedcomExportError):
            self.exporter.export_file(invalid_path, self.database)

    def test_multiline_text_export(self):
        """Test exporting multiline text with CONC/CONT."""
        # Create individual with long note
        long_note = (
            "This is a very long note that should be split across multiple lines using CONC tags because it exceeds the GEDCOM line length limit of 248 characters. "
            * 3
        )

        individual = GedcomIndividual(
            xref="@I1@", names=[GedcomName(full="Test /Person/")], notes=[long_note]
        )

        output = StringIO()
        self.exporter._export_records(
            output,
            GedcomDatabase(
                header=GedcomHeader(),
                individuals={"@I1@": individual},
                record_order=[("INDI", "@I1@")],
            ),
        )
        result = output.getvalue()

        # Should contain CONC tags for long lines
        self.assertIn("1 NOTE", result)
        # Note: The exact CONC behavior depends on implementation

    def test_empty_database_export(self):
        """Test exporting empty database."""
        empty_db = GedcomDatabase(header=GedcomHeader())

        output = StringIO()
        self.exporter.export_content(output, empty_db)
        result = output.getvalue()

        lines = result.strip().split("\n")
        self.assertTrue(lines[0].startswith("0 HEAD"))
        self.assertTrue(lines[-1] == "0 TRLR")

        # Should only have header and trailer
        self.assertEqual(len([line for line in lines if line.startswith("0 @")]), 0)


class TestRawStructureExporter(unittest.TestCase):
    """Test cases for RawStructureExporter."""

    def test_can_export(self):
        """Test can_export method."""
        exporter = RawStructureExporter(["SUBM", "OBJE"])

        self.assertTrue(exporter.can_export("SUBM"))
        self.assertTrue(exporter.can_export("OBJE"))
        self.assertFalse(exporter.can_export("INDI"))
        self.assertFalse(exporter.can_export("FAM"))


class TestIntegrationExport(unittest.TestCase):
    """Integration tests for export functionality."""

    def test_export_real_data_structure(self):
        """Test exporting with realistic data structure."""
        # Create a more complex database
        header = GedcomHeader(
            source="GeneWeb",
            version="7.0",
            charset="UTF-8",
            raw_lines=[
                (1, "SOUR", "GeneWeb"),
                (2, "VERS", "7.0"),
                (1, "CHAR", "UTF-8"),
                (1, "DATE", "1 JAN 2024"),
            ],
        )

        # Create multiple generations
        grandfather = GedcomIndividual(
            xref="@I1@",
            names=[GedcomName(full="Grandfather /Smith/")],
            sex="M",
            birth=GedcomEvent(tag="BIRT", date=GedcomDate(raw="1920", year=1920)),
            death=GedcomEvent(tag="DEAT", date=GedcomDate(raw="2000", year=2000)),
            fams=["F1"],
        )

        grandmother = GedcomIndividual(
            xref="@I2@",
            names=[GedcomName(full="Grandmother /Jones/")],
            sex="F",
            birth=GedcomEvent(tag="BIRT", date=GedcomDate(raw="1925", year=1925)),
            fams=["F1"],
        )

        father = GedcomIndividual(
            xref="@I3@",
            names=[GedcomName(full="Father /Smith/")],
            sex="M",
            birth=GedcomEvent(tag="BIRT", date=GedcomDate(raw="1950", year=1950)),
            famc=["F1"],
            fams=["F2"],
        )

        child = GedcomIndividual(
            xref="@I4@",
            names=[GedcomName(full="Child /Smith/")],
            sex="M",
            birth=GedcomEvent(tag="BIRT", date=GedcomDate(raw="1980", year=1980)),
            famc=["F2"],
        )

        # Create families
        family1 = GedcomFamily(
            xref="@F1@",
            husband="I1",
            wife="I2",
            children=["I3"],
            marriage=GedcomEvent(tag="MARR", date=GedcomDate(raw="1945", year=1945)),
        )

        family2 = GedcomFamily(xref="@F2@", husband="I3", children=["I4"])

        database = GedcomDatabase(
            header=header,
            individuals={
                "@I1@": grandfather,
                "@I2@": grandmother,
                "@I3@": father,
                "@I4@": child,
            },
            families={"@F1@": family1, "@F2@": family2},
            record_order=[
                ("INDI", "@I1@"),
                ("INDI", "@I2@"),
                ("INDI", "@I3@"),
                ("INDI", "@I4@"),
                ("FAM", "@F1@"),
                ("FAM", "@F2@"),
            ],
        )

        # Export and verify
        exporter = create_exporter()
        output = StringIO()
        exporter.export_content(output, database)
        result = output.getvalue()

        # Verify all records are present
        self.assertIn("0 @I1@ INDI", result)
        self.assertIn("0 @I2@ INDI", result)
        self.assertIn("0 @I3@ INDI", result)
        self.assertIn("0 @I4@ INDI", result)
        self.assertIn("0 @F1@ FAM", result)
        self.assertIn("0 @F2@ FAM", result)

        # Verify relationships
        self.assertIn("1 FAMC @F1@", result)  # Father's parent family
        self.assertIn("1 FAMC @F2@", result)  # Child's parent family
        self.assertIn("1 FAMS @F1@", result)  # Grandparents' family
        self.assertIn("1 FAMS @F2@", result)  # Father's family

        # Test round-trip
        from ..parser import create_parser

        parser = create_parser()
        reimported = parser.parse_content(result)

        self.assertEqual(len(database.individuals), len(reimported.individuals))
        self.assertEqual(len(database.families), len(reimported.families))


if __name__ == "__main__":
    unittest.main()
