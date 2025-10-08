import unittest
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from gedcom.parsers import (
    HeaderParser, IndividualParser, FamilyParser,
    RepositoryParser, MultimediaParser, SubmitterParser,
    NoteParser, SourceParser
)
from gedcom.tokenizer import GedcomLine

class TestParsers(unittest.TestCase):
    def setUp(self):
        self.header_parser = HeaderParser()
        self.individual_parser = IndividualParser()
        self.family_parser = FamilyParser()
        self.repository_parser = RepositoryParser()
        self.multimedia_parser = MultimediaParser()
        self.submitter_parser = SubmitterParser()
        self.note_parser = NoteParser()
        self.source_parser = SourceParser()

    def test_header_parser_can_parse(self):
        self.assertTrue(self.header_parser.can_parse('HEAD'))
        self.assertFalse(self.header_parser.can_parse('INDI'))

    def test_individual_parser_can_parse(self):
        self.assertTrue(self.individual_parser.can_parse('INDI'))
        self.assertFalse(self.individual_parser.can_parse('FAM'))

    def test_family_parser_can_parse(self):
        self.assertTrue(self.family_parser.can_parse('FAM'))
        self.assertFalse(self.family_parser.can_parse('INDI'))

    def test_repository_parser_can_parse(self):
        self.assertTrue(self.repository_parser.can_parse('REPO'))
        self.assertFalse(self.repository_parser.can_parse('INDI'))

    def test_multimedia_parser_can_parse(self):
        self.assertTrue(self.multimedia_parser.can_parse('OBJE'))
        self.assertFalse(self.multimedia_parser.can_parse('INDI'))

    def test_submitter_parser_can_parse(self):
        self.assertTrue(self.submitter_parser.can_parse('SUBM'))
        self.assertFalse(self.submitter_parser.can_parse('INDI'))

    def test_note_parser_can_parse(self):
        self.assertTrue(self.note_parser.can_parse('NOTE'))
        self.assertFalse(self.note_parser.can_parse('INDI'))

    def test_source_parser_can_parse(self):
        self.assertTrue(self.source_parser.can_parse('SOUR'))
        self.assertFalse(self.source_parser.can_parse('INDI'))

    def test_individual_parser_parse(self):
        lines = [
            GedcomLine(0, '@I1@', 'INDI', '', 1),
            GedcomLine(1, None, 'NAME', 'John /Doe/', 2),
            GedcomLine(1, None, 'SEX', 'M', 3),
            GedcomLine(1, None, 'BIRT', '', 4),
            GedcomLine(2, None, 'DATE', '1 JAN 1900', 5),
            GedcomLine(0, '@I2@', 'INDI', '', 6)
        ]

        individual, next_index = self.individual_parser.parse(lines, 0)
        self.assertEqual(next_index, 5)
        self.assertEqual(individual.xref, '@I1@')
        self.assertEqual(len(individual.names), 1)
        self.assertEqual(individual.names[0].full, 'John /Doe/')
        self.assertEqual(individual.sex, 'M')
        self.assertIsNotNone(individual.birth)
        self.assertEqual(individual.birth.date.raw, '1 JAN 1900')

    def test_family_parser_parse(self):
        lines = [
            GedcomLine(0, '@F1@', 'FAM', '', 1),
            GedcomLine(1, None, 'HUSB', '@I1@', 2),
            GedcomLine(1, None, 'WIFE', '@I2@', 3),
            GedcomLine(1, None, 'MARR', '', 4),
            GedcomLine(2, None, 'DATE', '20 JUN 1925', 5),
            GedcomLine(0, '@F2@', 'FAM', '', 6)
        ]

        family, next_index = self.family_parser.parse(lines, 0)
        self.assertEqual(next_index, 5)
        self.assertEqual(family.xref, '@F1@')
        self.assertEqual(family.husband, 'I1')
        self.assertEqual(family.wife, 'I2')
        self.assertIsNotNone(family.marriage)
        self.assertEqual(family.marriage.date.raw, '20 JUN 1925')

if __name__ == '__main__':
    unittest.main()
