import unittest
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from lib.gedcom.models import (
    GedcomDatabase,
    GedcomHeader,
    GedcomIndividual,
    GedcomFamily,
    GedcomEvent,
    GedcomName,
    GedcomPlace,
    GedcomDate,
    GedcomAddress,
    GedcomMap,
    GedcomRepository,
    GedcomMultimedia,
    GedcomSubmitter,
    GedcomNote,
    GedcomSource,
    GedcomSourceCitation,
)

class TestModelsFunctionality(unittest.TestCase):
    def test_gedcom_database_creation(self):
        header = GedcomHeader(source='TestApp')
        database = GedcomDatabase(header=header)
        self.assertEqual(len(database.individuals), 0)
        self.assertEqual(len(database.families), 0)
        self.assertEqual(len(database.sources), 0)
        self.assertEqual(len(database.notes), 0)
        self.assertEqual(len(database.multimedia), 0)
        self.assertEqual(len(database.repositories), 0)
        self.assertEqual(len(database.submitters), 0)
        self.assertIsNotNone(database.header)

    def test_gedcom_header_creation(self):
        header = GedcomHeader(
            source='TestApp',
            version='1.0',
            destination='TestApp',
            date='1 JAN 2024',
            charset='UTF-8'
        )
        self.assertEqual(header.source, 'TestApp')
        self.assertEqual(header.version, '1.0')
        self.assertEqual(header.destination, 'TestApp')
        self.assertEqual(header.date, '1 JAN 2024')
        self.assertEqual(header.charset, 'UTF-8')

    def test_gedcom_individual_creation(self):
        individual = GedcomIndividual(
            xref='@I1@',
            names=[GedcomName(full='John /Doe/', given='John', surname='Doe')],
            sex='M'
        )
        self.assertEqual(individual.xref, '@I1@')
        self.assertEqual(len(individual.names), 1)
        self.assertEqual(individual.names[0].full, 'John /Doe/')
        self.assertEqual(individual.names[0].given, 'John')
        self.assertEqual(individual.names[0].surname, 'Doe')
        self.assertEqual(individual.sex, 'M')

    def test_gedcom_name_creation(self):
        name = GedcomName(
            full='John /Doe/ Jr.',
            given='John',
            surname='Doe',
            suffix='Jr.'
        )
        self.assertEqual(name.full, 'John /Doe/ Jr.')
        self.assertEqual(name.given, 'John')
        self.assertEqual(name.surname, 'Doe')
        self.assertEqual(name.suffix, 'Jr.')

    def test_gedcom_date_creation(self):
        from lib.gedcom.parsers.utils import ParserUtils

        date = ParserUtils.parse_date('1 JAN 1900')
        self.assertEqual(date.raw, '1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.has_year)
        self.assertTrue(date.has_month)
        self.assertTrue(date.has_day)

    def test_gedcom_date_approximate(self):
        from lib.gedcom.parsers.utils import ParserUtils

        date = ParserUtils.parse_date('ABT 1 JAN 1900')
        self.assertTrue(date.is_approximate)
        self.assertEqual(date.year, 1900)

    def test_gedcom_date_range(self):
        date = GedcomDate(raw='BET 1 JAN 1900 AND 31 DEC 1900')
        date.is_range = True
        date.range_type = 'BET'
        self.assertTrue(date.is_range)
        self.assertEqual(date.range_type, 'BET')

    def test_gedcom_place_creation(self):
        place = GedcomPlace(
            name='New York, NY, USA',
            parts=['New York', 'NY', 'USA']
        )
        self.assertEqual(place.name, 'New York, NY, USA')
        self.assertEqual(place.parts, ['New York', 'NY', 'USA'])

    def test_gedcom_event_creation(self):
        from lib.gedcom.parsers.utils import ParserUtils

        event = GedcomEvent(
            tag='BIRT',
            date=ParserUtils.parse_date('1 JAN 1900'),
            place=GedcomPlace(name='New York, NY, USA')
        )
        self.assertIsNotNone(event.date)
        self.assertEqual(event.date.raw, '1 JAN 1900')
        self.assertIsNotNone(event.place)
        self.assertEqual(event.place.name, 'New York, NY, USA')

    def test_gedcom_family_creation(self):
        family = GedcomFamily(
            xref='@F1@',
            husband='I1',
            wife='I2'
        )
        self.assertEqual(family.xref, '@F1@')
        self.assertEqual(family.husband, 'I1')
        self.assertEqual(family.wife, 'I2')
        self.assertEqual(len(family.children), 0)

    def test_gedcom_family_with_marriage(self):
        from lib.gedcom.parsers.utils import ParserUtils

        family = GedcomFamily(
            xref='@F1@',
            husband='I1',
            wife='I2',
            marriage=GedcomEvent(
                tag='MARR',
                date=ParserUtils.parse_date('20 JUN 1925'),
                place=GedcomPlace(name='New York, NY, USA')
            )
        )
        self.assertIsNotNone(family.marriage)
        self.assertEqual(family.marriage.date.raw, '20 JUN 1925')
        self.assertEqual(family.marriage.place.name, 'New York, NY, USA')

    def test_gedcom_address_creation(self):
        address = GedcomAddress(
            value='123 Main St',
            city='New York',
            state='NY',
            postal_code='10001',
            country='USA'
        )
        self.assertEqual(address.value, '123 Main St')
        self.assertEqual(address.city, 'New York')
        self.assertEqual(address.state, 'NY')
        self.assertEqual(address.postal_code, '10001')
        self.assertEqual(address.country, 'USA')

    def test_gedcom_map_creation(self):
        map_obj = GedcomMap(
            latitude='N40.7128',
            longitude='W74.0060'
        )
        self.assertEqual(map_obj.latitude, 'N40.7128')
        self.assertEqual(map_obj.longitude, 'W74.0060')

    def test_gedcom_repository_creation(self):
        repository = GedcomRepository(
            xref='@R1@',
            name='Test Repository'
        )
        self.assertEqual(repository.xref, '@R1@')
        self.assertEqual(repository.name, 'Test Repository')

    def test_gedcom_multimedia_creation(self):
        multimedia = GedcomMultimedia(
            xref='@M1@',
            file_path='test.jpg',
            title='Test Image',
            format='jpeg'
        )
        self.assertEqual(multimedia.xref, '@M1@')
        self.assertEqual(multimedia.file_path, 'test.jpg')
        self.assertEqual(multimedia.title, 'Test Image')
        self.assertEqual(multimedia.format, 'jpeg')

    def test_gedcom_submitter_creation(self):
        submitter = GedcomSubmitter(
            xref='@SUBM@',
            name='Test Submitter'
        )
        self.assertEqual(submitter.xref, '@SUBM@')
        self.assertEqual(submitter.name, 'Test Submitter')

    def test_gedcom_note_creation(self):
        note = GedcomNote(
            xref='@N1@',
            text='This is a test note'
        )
        self.assertEqual(note.xref, '@N1@')
        self.assertEqual(note.text, 'This is a test note')

    def test_gedcom_source_creation(self):
        source = GedcomSource(
            xref='@S1@',
            title='Test Source',
            author='Test Author'
        )
        self.assertEqual(source.xref, '@S1@')
        self.assertEqual(source.title, 'Test Source')
        self.assertEqual(source.author, 'Test Author')

    def test_gedcom_source_citation_creation(self):
        citation = GedcomSourceCitation(
            xref='S1',
            sour_level=2
        )
        self.assertEqual(citation.xref, 'S1')
        self.assertEqual(citation.sour_level, 2)
        self.assertEqual(len(citation.sub_lines), 0)

    def test_gedcom_individual_with_events(self):
        from lib.gedcom.parsers.utils import ParserUtils

        individual = GedcomIndividual(
            xref='@I1@',
            names=[GedcomName(full='John /Doe/')],
            sex='M',
            birth=GedcomEvent(
                tag='BIRT',
                date=ParserUtils.parse_date('1 JAN 1900'),
                place=GedcomPlace(name='New York, NY, USA')
            ),
            death=GedcomEvent(
                tag='DEAT',
                date=ParserUtils.parse_date('31 DEC 1980'),
                place=GedcomPlace(name='New York, NY, USA')
            )
        )
        self.assertIsNotNone(individual.birth)
        self.assertEqual(individual.birth.date.raw, '1 JAN 1900')
        self.assertIsNotNone(individual.death)
        self.assertEqual(individual.death.date.raw, '31 DEC 1980')

    def test_gedcom_individual_with_attributes(self):
        individual = GedcomIndividual(
            xref='@I1@',
            names=[GedcomName(full='John /Doe/')],
            sex='M',
            occupations=['Engineer', 'Manager'],
            titles=['Dr.', 'Prof.'],
            reference_numbers=['12345', '67890'],
            multimedia=['M1', 'M2'],
            private_links=['https://example.com'],
            private_photos=['P1', 'P2']
        )
        self.assertEqual(len(individual.occupations), 2)
        self.assertEqual(individual.occupations[0], 'Engineer')
        self.assertEqual(len(individual.titles), 2)
        self.assertEqual(individual.titles[0], 'Dr.')
        self.assertEqual(len(individual.reference_numbers), 2)
        self.assertEqual(individual.reference_numbers[0], '12345')
        self.assertEqual(len(individual.multimedia), 2)
        self.assertEqual(individual.multimedia[0], 'M1')
        self.assertEqual(len(individual.private_links), 1)
        self.assertEqual(individual.private_links[0], 'https://example.com')
        self.assertEqual(len(individual.private_photos), 2)
        self.assertEqual(individual.private_photos[0], 'P1')

    def test_gedcom_family_with_children(self):
        family = GedcomFamily(
            xref='@F1@',
            husband='I1',
            wife='I2',
            children=['I3', 'I4', 'I5']
        )
        self.assertEqual(len(family.children), 3)
        self.assertEqual(family.children[0], 'I3')
        self.assertEqual(family.children[1], 'I4')
        self.assertEqual(family.children[2], 'I5')

    def test_gedcom_database_string_representations(self):
        individual = GedcomIndividual(
            xref='@I1@',
            names=[GedcomName(full='John /Doe/')],
            sex='M'
        )
        self.assertIn('John /Doe/', str(individual))

        family = GedcomFamily(
            xref='@F1@',
            husband='I1',
            wife='I2'
        )
        self.assertIn('@F1@', str(family))

        source = GedcomSource(
            xref='@S1@',
            title='Test Source'
        )
        self.assertEqual(str(source), 'Test Source')

if __name__ == '__main__':
    unittest.main()
