"""
Unit tests for GEDCOM parser.
"""

import unittest
from pathlib import Path
from unittest.mock import Mock, patch
import tempfile
import os

from ..parser import GedcomParser, create_parser, IndividualParser, FamilyParser, HeaderParser
from ..models import GedcomDatabase, GedcomIndividual, GedcomFamily, GedcomHeader
from ..tokenizer import GedcomLine
from ..exceptions import GedcomParseError

class TestGedcomParser(unittest.TestCase):
    """Test cases for GedcomParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.parser = create_parser()

    def test_create_parser(self):
        """Test parser creation."""
        parser = create_parser()
        self.assertIsInstance(parser, GedcomParser)
        self.assertIsNotNone(parser.file_reader)
        self.assertIsNotNone(parser.validator)
        self.assertIsNotNone(parser.tokenizer)

    def test_parse_minimal_gedcom(self):
        """Test parsing minimal valid GEDCOM."""
        content = """0 HEAD
1 SOUR Test
1 CHAR UTF-8
0 TRLR"""

        database = self.parser.parse_content(content)

        self.assertIsInstance(database, GedcomDatabase)
        self.assertEqual(database.header.source, "Test")
        self.assertEqual(database.header.charset, "UTF-8")
        self.assertEqual(len(database.individuals), 0)
        self.assertEqual(len(database.families), 0)

    def test_parse_individual(self):
        """Test parsing individual record."""
        content = """0 HEAD
1 SOUR Test
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Doe/
1 SEX M
1 BIRT
2 DATE 1 JAN 1980
2 PLAC New York, USA
0 TRLR"""

        database = self.parser.parse_content(content)

        self.assertEqual(len(database.individuals), 1)
        individual = database.individuals['@I1@']
        self.assertEqual(individual.xref, '@I1@')
        self.assertEqual(len(individual.names), 1)
        self.assertEqual(individual.names[0].full, "John /Doe/")
        self.assertEqual(individual.names[0].given, "John")
        self.assertEqual(individual.names[0].surname, "Doe")
        self.assertEqual(individual.sex, "M")
        self.assertIsNotNone(individual.birth)
        self.assertEqual(individual.birth.tag, "BIRT")
        self.assertIsNotNone(individual.birth.date)
        self.assertEqual(individual.birth.date.year, 1980)
        self.assertEqual(individual.birth.date.month, 1)
        self.assertEqual(individual.birth.date.day, 1)
        self.assertIsNotNone(individual.birth.place)
        self.assertEqual(individual.birth.place.name, "New York, USA")

    def test_parse_family(self):
        """Test parsing family record."""
        content = """0 HEAD
1 SOUR Test
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Doe/
1 SEX M
0 @I2@ INDI
1 NAME Jane /Smith/
1 SEX F
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 1 JUN 2000
2 PLAC Church, City
0 TRLR"""

        database = self.parser.parse_content(content)

        self.assertEqual(len(database.families), 1)
        family = database.families['@F1@']
        self.assertEqual(family.xref, '@F1@')
        self.assertEqual(family.husband, 'I1')
        self.assertEqual(family.wife, 'I2')
        self.assertIsNotNone(family.marriage)
        self.assertEqual(family.marriage.tag, "MARR")
        self.assertEqual(family.marriage.date.year, 2000)
        self.assertEqual(family.marriage.date.month, 6)
        self.assertEqual(family.marriage.date.day, 1)

    def test_parse_file_not_found(self):
        """Test parsing non-existent file."""
        with self.assertRaises(GedcomParseError):
            self.parser.parse_file(Path("non_existent_file.ged"))

    def test_parse_invalid_content(self):
        """Test parsing invalid GEDCOM content."""
        content = "Invalid GEDCOM content"

        with self.assertRaises(GedcomParseError):
            self.parser.parse_content(content)

class TestIndividualParser(unittest.TestCase):
    """Test cases for IndividualParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.parser = IndividualParser()

    def test_can_parse(self):
        """Test can_parse method."""
        self.assertTrue(self.parser.can_parse('INDI'))
        self.assertFalse(self.parser.can_parse('FAM'))
        self.assertFalse(self.parser.can_parse('HEAD'))

    def test_parse_individual_basic(self):
        """Test parsing basic individual."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'SEX', 'M', 3),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsInstance(individual, GedcomIndividual)
        self.assertEqual(individual.xref, '@I1@')
        self.assertEqual(len(individual.names), 1)
        self.assertEqual(individual.names[0].full, 'John /Doe/')
        self.assertEqual(individual.names[0].given, 'John')
        self.assertEqual(individual.names[0].surname, 'Doe')
        self.assertEqual(individual.sex, 'M')
        self.assertEqual(next_index, 3)

    def test_parse_individual_with_events(self):
        """Test parsing individual with events."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'BIRT', '', 3),
            GedcomLine(2, None, 'DATE', '1 JAN 1980', 4),
            GedcomLine(2, None, 'PLAC', 'New York', 5),
            GedcomLine(1, None, 'DEAT', '', 6),
            GedcomLine(2, None, 'DATE', 'ABT 2050', 7),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(individual.birth)
        self.assertEqual(individual.birth.tag, 'BIRT')
        self.assertEqual(individual.birth.date.year, 1980)
        self.assertEqual(individual.birth.place.name, 'New York')

        self.assertIsNotNone(individual.death)
        self.assertEqual(individual.death.tag, 'DEAT')
        self.assertEqual(individual.death.date.year, 2050)
        self.assertTrue(individual.death.date.is_approximate)

    def test_parse_name_formats(self):
        """Test parsing different name formats."""
        # Standard format
        name1 = self.parser._parse_name('John /Doe/')
        self.assertEqual(name1.given, 'John')
        self.assertEqual(name1.surname, 'Doe')

        # With suffix
        name2 = self.parser._parse_name('John /Doe/ Jr.')
        self.assertEqual(name2.given, 'John')
        self.assertEqual(name2.surname, 'Doe')
        self.assertEqual(name2.suffix, 'Jr.')

        # No surname
        name3 = self.parser._parse_name('John')
        self.assertEqual(name3.full, 'John')
        self.assertIsNone(name3.given)
        self.assertIsNone(name3.surname)

    def test_parse_religious_events(self):
        """Test parsing religious events."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'CONF', '', 3),
            GedcomLine(2, None, 'DATE', '15 MAY 1915', 4),
            GedcomLine(1, None, 'BARM', '', 5),
            GedcomLine(2, None, 'DATE', '13 APR 1928', 6),
            GedcomLine(1, None, 'ORDN', '', 7),
            GedcomLine(2, None, 'DATE', '1 JUN 1940', 8),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(individual.confirmation)
        self.assertEqual(individual.confirmation.date.year, 1915)
        self.assertIsNotNone(individual.bar_mitzvah)
        self.assertEqual(individual.bar_mitzvah.date.year, 1928)
        self.assertIsNotNone(individual.ordination)
        self.assertEqual(individual.ordination.date.year, 1940)

    def test_parse_legal_events(self):
        """Test parsing legal events."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'ADOP', '', 3),
            GedcomLine(2, None, 'DATE', '1 JAN 1920', 4),
            GedcomLine(1, None, 'NATU', '', 5),
            GedcomLine(2, None, 'DATE', '15 MAR 1945', 6),
            GedcomLine(1, None, 'WILL', '', 7),
            GedcomLine(2, None, 'DATE', '10 DEC 1990', 8),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(individual.adoption)
        self.assertEqual(individual.adoption.date.year, 1920)
        self.assertIsNotNone(individual.naturalization)
        self.assertEqual(individual.naturalization.date.year, 1945)
        self.assertIsNotNone(individual.will)
        self.assertEqual(individual.will.date.year, 1990)

    def test_parse_migration_events(self):
        """Test parsing migration events."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'EMIG', '', 3),
            GedcomLine(2, None, 'DATE', '1 MAY 1920', 4),
            GedcomLine(2, None, 'PLAC', 'Liverpool, England', 5),
            GedcomLine(1, None, 'IMMI', '', 6),
            GedcomLine(2, None, 'DATE', '15 MAY 1920', 7),
            GedcomLine(2, None, 'PLAC', 'New York, NY', 8),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(individual.emigration)
        self.assertEqual(individual.emigration.date.year, 1920)
        self.assertEqual(individual.emigration.place.name, 'Liverpool, England')
        self.assertIsNotNone(individual.immigration)
        self.assertEqual(individual.immigration.date.year, 1920)
        self.assertEqual(individual.immigration.place.name, 'New York, NY')

    def test_parse_census_events(self):
        """Test parsing multiple census events."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'CENS', '', 3),
            GedcomLine(2, None, 'DATE', '1 APR 1930', 4),
            GedcomLine(2, None, 'PLAC', 'New York, NY', 5),
            GedcomLine(1, None, 'CENS', '', 6),
            GedcomLine(2, None, 'DATE', '1 APR 1940', 7),
            GedcomLine(2, None, 'PLAC', 'New York, NY', 8),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertEqual(len(individual.census), 2)
        self.assertEqual(individual.census[0].date.year, 1930)
        self.assertEqual(individual.census[1].date.year, 1940)

    def test_parse_retirement_event(self):
        """Test parsing retirement event."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'RETI', '', 3),
            GedcomLine(2, None, 'DATE', '1 JAN 1985', 4),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(individual.retirement)
        self.assertEqual(individual.retirement.date.year, 1985)

    def test_parse_generic_event(self):
        """Test parsing generic EVEN event."""
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'EVEN', '', 3),
            GedcomLine(2, None, 'TYPE', 'Military Service', 4),
            GedcomLine(2, None, 'DATE', '1 JAN 1942', 5),
        ]

        individual, next_index = self.parser.parse(lines, 0)

        self.assertEqual(len(individual.events), 1)
        self.assertEqual(individual.events[0].tag, 'EVEN')
        self.assertEqual(individual.events[0].date.year, 1942)

class TestFamilyParser(unittest.TestCase):
    """Test cases for FamilyParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.parser = FamilyParser()

    def test_can_parse(self):
        """Test can_parse method."""
        self.assertTrue(self.parser.can_parse('FAM'))
        self.assertFalse(self.parser.can_parse('INDI'))
        self.assertFalse(self.parser.can_parse('HEAD'))

    def test_parse_family_basic(self):
        """Test parsing basic family."""
        lines = [
            GedcomLine(0, '@F1@', 'FAM', '', 1),
            GedcomLine(1, None, 'HUSB', '@I1@', 2),
            GedcomLine(1, None, 'WIFE', '@I2@', 3),
            GedcomLine(1, None, 'CHIL', '@I3@', 4),
            GedcomLine(1, None, 'CHIL', '@I4@', 5),
        ]

        family, next_index = self.parser.parse(lines, 0)

        self.assertIsInstance(family, GedcomFamily)
        self.assertEqual(family.xref, '@F1@')
        self.assertEqual(family.husband, 'I1')
        self.assertEqual(family.wife, 'I2')
        self.assertEqual(len(family.children), 2)
        self.assertIn('I3', family.children)
        self.assertIn('I4', family.children)
        self.assertEqual(next_index, 5)

    def test_parse_family_with_marriage(self):
        """Test parsing family with marriage event."""
        lines = [
            GedcomLine(0, '@F1@', 'FAM', '', 1),
            GedcomLine(1, None, 'HUSB', '@I1@', 2),
            GedcomLine(1, None, 'WIFE', '@I2@', 3),
            GedcomLine(1, None, 'MARR', '', 4),
            GedcomLine(2, None, 'DATE', '15 JUN 2000', 5),
            GedcomLine(2, None, 'PLAC', 'Church, City', 6),
        ]

        family, next_index = self.parser.parse(lines, 0)

        self.assertIsNotNone(family.marriage)
        self.assertEqual(family.marriage.tag, 'MARR')
        self.assertEqual(family.marriage.date.year, 2000)
        self.assertEqual(family.marriage.date.month, 6)
        self.assertEqual(family.marriage.date.day, 15)
        self.assertEqual(family.marriage.place.name, 'Church, City')

class TestHeaderParser(unittest.TestCase):
    """Test cases for HeaderParser."""

    def setUp(self):
        """Set up test fixtures."""
        self.parser = HeaderParser()

    def test_can_parse(self):
        """Test can_parse method."""
        self.assertTrue(self.parser.can_parse('HEAD'))
        self.assertFalse(self.parser.can_parse('INDI'))
        self.assertFalse(self.parser.can_parse('FAM'))

    def test_parse_header(self):
        """Test parsing header."""
        lines = [
            GedcomLine(0, None, 'HEAD', '', 1),
            GedcomLine(1, None, 'SOUR', 'MyApp', 2),
            GedcomLine(2, None, 'VERS', '1.0', 3),
            GedcomLine(1, None, 'CHAR', 'UTF-8', 4),
            GedcomLine(1, None, 'DEST', 'GENEWEB', 5),
            GedcomLine(1, None, 'DATE', '1 JAN 2024', 6),
            GedcomLine(1, None, 'FILE', 'family.ged', 7),
        ]

        header, next_index = self.parser.parse(lines, 0)

        self.assertIsInstance(header, GedcomHeader)
        self.assertEqual(header.source, 'MyApp')
        self.assertEqual(header.charset, 'UTF-8')
        self.assertEqual(header.destination, 'GENEWEB')
        self.assertEqual(header.date, '1 JAN 2024')
        self.assertEqual(header.filename, 'family.ged')
        self.assertEqual(len(header.raw_lines), 6)
        self.assertEqual(next_index, 7)

class TestIntegration(unittest.TestCase):
    """Integration tests for the complete parser."""

    def test_parse_complete_gedcom(self):
        """Test parsing a complete GEDCOM file."""
        content = """0 HEAD
1 SOUR TestApp
2 VERS 1.0
1 CHAR UTF-8
1 DEST GENEWEB
1 DATE 1 JAN 2024
0 @S1@ SUBM
1 NAME Test Submitter
0 @I1@ INDI
1 NAME John /Doe/
2 GIVN John
2 SURN Doe
1 SEX M
1 BIRT
2 DATE 1 JAN 1980
2 PLAC New York, NY, USA
1 OCCU Engineer
1 FAMS @F1@
0 @I2@ INDI
1 NAME Jane /Smith/
1 SEX F
1 BIRT
2 DATE 15 MAR 1982
1 FAMS @F1@
0 @I3@ INDI
1 NAME Baby /Doe/
1 SEX M
1 BIRT
2 DATE 10 JUL 2005
1 FAMC @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
1 MARR
2 DATE 14 FEB 2004
2 PLAC Las Vegas, NV, USA
0 TRLR"""

        parser = create_parser()
        database = parser.parse_content(content)

        # Verify header
        self.assertEqual(database.header.source, "TestApp")
        self.assertEqual(database.header.charset, "UTF-8")

        # Verify individuals
        self.assertEqual(len(database.individuals), 3)
        john = database.individuals['@I1@']
        self.assertEqual(john.primary_name.display_name, "John Doe")
        self.assertEqual(john.sex, "M")
        self.assertEqual(john.birth_year, 1980)
        self.assertEqual(len(john.occupations), 1)
        self.assertEqual(john.occupations[0], "Engineer")

        jane = database.individuals['@I2@']
        self.assertEqual(jane.primary_name.display_name, "Jane Smith")
        self.assertEqual(jane.birth_year, 1982)

        baby = database.individuals['@I3@']
        self.assertEqual(baby.primary_name.display_name, "Baby Doe")
        self.assertEqual(baby.birth_year, 2005)

        # Verify family
        self.assertEqual(len(database.families), 1)
        family = database.families['@F1@']
        self.assertEqual(family.husband, 'I1')
        self.assertEqual(family.wife, 'I2')
        self.assertEqual(len(family.children), 1)
        self.assertEqual(family.children[0], 'I3')
        self.assertEqual(family.marriage_year, 2004)

        # Verify relationships
        self.assertIn('F1', john.fams)
        self.assertIn('F1', jane.fams)
        self.assertIn('F1', baby.famc)

        # Test database helper methods
        children = database.get_children('F1')
        self.assertEqual(len(children), 1)
        self.assertEqual(children[0].xref, '@I3@')

        parents = database.get_parents('@I3@')
        self.assertEqual(len(parents), 2)
        parent_names = {p.primary_name.display_name for p in parents}
        self.assertEqual(parent_names, {"John Doe", "Jane Smith"})

        spouses = database.get_spouses('@I1@')
        self.assertEqual(len(spouses), 1)
        self.assertEqual(spouses[0].primary_name.display_name, "Jane Smith")

if __name__ == '__main__':
    unittest.main()
