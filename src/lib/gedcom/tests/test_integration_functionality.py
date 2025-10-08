import unittest
import sys
import os
import io
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from lib.gedcom import create_parser, create_exporter
from lib.gedcom.parser import GedcomParser
from lib.gedcom.tokenizer import GedcomTokenizer

class TestIntegration(unittest.TestCase):
    def setUp(self):
        self.parser = create_parser()
        self.exporter = create_exporter()

    def test_simple_gedcom_round_trip(self):
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), 'test_simple.ged')
        with open(test_file, 'r', encoding='utf-8') as f:
            content = f.read()

        # Parse
        database = self.parser.parse_content(content)

        # Verify parsing
        self.assertEqual(len(database.individuals), 2)
        self.assertEqual(len(database.families), 1)
        self.assertEqual(len(database.sources), 1)
        self.assertEqual(len(database.notes), 1)
        self.assertEqual(len(database.multimedia), 1)
        self.assertEqual(len(database.repositories), 1)
        self.assertEqual(len(database.submitters), 1)

        # Verify individual details
        ind1 = database.individuals['@I1@']
        self.assertEqual(len(ind1.names), 1)
        self.assertEqual(ind1.names[0].full, 'John /Doe/')
        self.assertEqual(ind1.names[0].given, 'John')
        self.assertEqual(ind1.names[0].surname, 'Doe')
        self.assertEqual(ind1.sex, 'M')
        self.assertIsNotNone(ind1.birth)
        self.assertEqual(ind1.birth.date.raw, '1 JAN 1900')
        self.assertEqual(ind1.birth.place.name, 'New York, NY, USA')
        self.assertIsNotNone(ind1.death)
        self.assertEqual(ind1.death.date.raw, '31 DEC 1980')
        self.assertEqual(len(ind1.occupations), 1)
        self.assertEqual(ind1.occupations[0], 'Engineer')
        self.assertEqual(len(ind1.notes), 1)
        self.assertEqual(ind1.notes[0], 'This is a test individual\nwith multiple lines')
        self.assertEqual(len(ind1.sources), 1)
        self.assertEqual(ind1.sources[0], '@S1@')
        self.assertEqual(len(ind1.multimedia), 1)
        self.assertEqual(ind1.multimedia[0], 'M1')
        self.assertEqual(len(ind1.reference_numbers), 1)
        self.assertEqual(ind1.reference_numbers[0], '12345')
        self.assertEqual(len(ind1.private_links), 1)
        self.assertEqual(ind1.private_links[0], 'https://example.com')

        # Verify family details
        fam1 = database.families['@F1@']
        self.assertEqual(fam1.husband, 'I1')
        self.assertEqual(fam1.wife, 'I2')
        self.assertIsNotNone(fam1.marriage)
        self.assertEqual(fam1.marriage.date.raw, '20 JUN 1925')
        self.assertEqual(fam1.marriage.place.name, 'New York, NY, USA')

        # Export
        file = io.StringIO()
        self.exporter.export_content(file, database)
        exported_content = file.getvalue()

        # Verify export contains expected content
        self.assertIn('0 @I1@ INDI', exported_content)
        self.assertIn('1 NAME John /Doe/', exported_content)
        self.assertIn('1 SEX M', exported_content)
        self.assertIn('1 BIRT', exported_content)
        self.assertIn('2 DATE 1 JAN 1900', exported_content)
        self.assertIn('2 PLAC New York, NY, USA', exported_content)
        self.assertIn('1 DEAT', exported_content)
        self.assertIn('2 DATE 31 DEC 1980', exported_content)
        self.assertIn('1 OCCU Engineer', exported_content)
        self.assertIn('1 NOTE This is a test individual', exported_content)
        self.assertIn('2 SOUR @S1@', exported_content)
        self.assertIn('1 OBJE @M1@', exported_content)
        self.assertIn('1 REFN 12345', exported_content)
        self.assertIn('1 _LINK https://example.com', exported_content)

        # Round-trip test
        database2 = self.parser.parse_content(exported_content)

        # Verify round-trip conservation
        self.assertEqual(len(database2.individuals), len(database.individuals))
        self.assertEqual(len(database2.families), len(database.families))
        self.assertEqual(len(database2.sources), len(database.sources))
        self.assertEqual(len(database2.notes), len(database.notes))
        self.assertEqual(len(database2.multimedia), len(database.multimedia))
        self.assertEqual(len(database2.repositories), len(database.repositories))
        self.assertEqual(len(database2.submitters), len(database.submitters))

    def test_parser_with_validation_disabled(self):
        # Test parser without validation
        from lib.gedcom.parser import GedcomParser
        from lib.gedcom.tokenizer import GedcomTokenizer

        class NoValidationParser(GedcomParser):
            def parse_content(self, content: str):
                try:
                    lines = self.tokenizer.tokenize(content)
                    database = self._parse_records(lines)
                    self._ensure_stub_sources(database)
                    return database
                except Exception as e:
                    raise Exception(f'Failed to parse content: {e}') from e

        parser = NoValidationParser()

        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), 'test_simple.ged')
        with open(test_file, 'r', encoding='utf-8') as f:
            content = f.read()

        database = parser.parse_content(content)
        self.assertEqual(len(database.individuals), 2)
        self.assertEqual(database.individuals['@I1@'].names[0].full, 'John /Doe/')

    def test_export_with_different_formats(self):
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), 'test_geographic.ged')
        with open(test_file, 'r', encoding='utf-8') as f:
            content = f.read()

        database = self.parser.parse_content(content)

        # Test export to file
        with open('test_export.ged', 'w') as f:
            self.exporter.export_content(f, database)

        # Verify file was created
        self.assertTrue(os.path.exists('test_export.ged'))

        # Clean up
        os.remove('test_export.ged')

if __name__ == '__main__':
    unittest.main()
