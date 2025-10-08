import unittest
import sys
import os
import io
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from lib.gedcom.exporters import (
    HeaderExporter,
    IndividualExporter,
    FamilyExporter,
    RepositoryExporter,
    MultimediaExporter,
    SubmitterExporter,
    NoteExporter,
    SourceExporter,
)
from lib.gedcom.models import (
    GedcomHeader,
    GedcomIndividual,
    GedcomFamily,
    GedcomName,
    GedcomEvent,
    GedcomDate,
)

class TestExporters(unittest.TestCase):
    def setUp(self):
        self.header_exporter = HeaderExporter()
        self.individual_exporter = IndividualExporter()
        self.family_exporter = FamilyExporter()
        self.repository_exporter = RepositoryExporter()
        self.multimedia_exporter = MultimediaExporter()
        self.submitter_exporter = SubmitterExporter()
        self.note_exporter = NoteExporter()
        self.source_exporter = SourceExporter()

    def test_header_exporter_can_export(self):
        self.assertTrue(self.header_exporter.can_export('HEAD'))
        self.assertFalse(self.header_exporter.can_export('INDI'))

    def test_individual_exporter_can_export(self):
        self.assertTrue(self.individual_exporter.can_export('INDI'))
        self.assertFalse(self.individual_exporter.can_export('FAM'))

    def test_family_exporter_can_export(self):
        self.assertTrue(self.family_exporter.can_export('FAM'))
        self.assertFalse(self.family_exporter.can_export('INDI'))

    def test_repository_exporter_can_export(self):
        self.assertTrue(self.repository_exporter.can_export('REPO'))
        self.assertFalse(self.repository_exporter.can_export('INDI'))

    def test_multimedia_exporter_can_export(self):
        self.assertTrue(self.multimedia_exporter.can_export('OBJE'))
        self.assertFalse(self.multimedia_exporter.can_export('INDI'))

    def test_submitter_exporter_can_export(self):
        self.assertTrue(self.submitter_exporter.can_export('SUBM'))
        self.assertFalse(self.submitter_exporter.can_export('INDI'))

    def test_note_exporter_can_export(self):
        self.assertTrue(self.note_exporter.can_export('NOTE'))
        self.assertFalse(self.note_exporter.can_export('INDI'))

    def test_source_exporter_can_export(self):
        self.assertTrue(self.source_exporter.can_export('SOUR'))
        self.assertFalse(self.source_exporter.can_export('INDI'))

    def test_individual_exporter_export(self):
        individual = GedcomIndividual(
            xref='@I1@',
            names=[GedcomName(full='John /Doe/', given='John', surname='Doe')],
            sex='M',
            birth=GedcomEvent(tag='BIRT', date=GedcomDate(raw='1 JAN 1900')),
            occupations=['Engineer'],
            reference_numbers=['12345'],
            multimedia=['M1']
        )

        file = io.StringIO()
        self.individual_exporter.export(file, 'I1', individual)
        output = file.getvalue()

        self.assertIn('0 @I1@ INDI', output)
        self.assertIn('1 NAME John /Doe/', output)
        self.assertIn('1 SEX M', output)
        self.assertIn('1 BIRT', output)
        self.assertIn('2 DATE 1 JAN 1900', output)
        self.assertIn('1 OCCU Engineer', output)
        self.assertIn('1 REFN 12345', output)
        self.assertIn('1 OBJE @M1@', output)

    def test_family_exporter_export(self):
        family = GedcomFamily(
            xref='@F1@',
            husband='I1',
            wife='I2',
            marriage=GedcomEvent(tag='MARR', date=GedcomDate(raw='20 JUN 1925'))
        )

        file = io.StringIO()
        self.family_exporter.export(file, 'F1', family)
        output = file.getvalue()

        self.assertIn('0 @F1@ FAM', output)
        self.assertIn('1 HUSB @I1@', output)
        self.assertIn('1 WIFE @I2@', output)
        self.assertIn('1 MARR', output)
        self.assertIn('2 DATE 20 JUN 1925', output)

    def test_header_exporter_export(self):
        header = GedcomHeader(
            source='TestApp',
            version='1.0',
            destination='TestApp',
            date='1 JAN 2024',
            charset='UTF-8'
        )

        file = io.StringIO()
        self.header_exporter.export(file, '', header)
        output = file.getvalue()

        self.assertIn('0 HEAD', output)
        self.assertIn('1 SOUR TestApp', output)
        self.assertIn('2 VERS 1.0', output)
        self.assertIn('1 DEST TestApp', output)
        self.assertIn('1 DATE 1 JAN 2024', output)
        self.assertIn('1 CHAR UTF-8', output)

if __name__ == '__main__':
    unittest.main()
