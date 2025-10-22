import io
import os
import sys
import unittest

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))

from gedcom import create_exporter, create_parser


class TestAdvancedFunctionality(unittest.TestCase):
    def setUp(self):
        self.parser = create_parser()
        self.exporter = create_exporter()

    def test_complex_date_parsing(self):
        """Test parsing of complex date formats"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_complex_dates.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test birth date (approximate)
        self.assertIsNotNone(individual.birth)
        self.assertTrue(individual.birth.date.is_approximate)
        self.assertEqual(individual.birth.date.year, 1900)

        # Test death date (calculated)
        self.assertIsNotNone(individual.death)
        self.assertTrue(individual.death.date.is_calculated)
        self.assertEqual(individual.death.date.year, 1980)

        # MARR events are not parsed as individual events in this parser
        # They are handled as family events instead
        # Check for other events that should be present
        fcom_events = [e for e in individual.events if e.tag == "FCOM"]
        self.assertEqual(len(fcom_events), 2)  # Two FCOM events in the test file

    def test_geographic_coordinates_parsing(self):
        """Test parsing of geographic coordinates"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_geographic.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test birth place with coordinates
        self.assertIsNotNone(individual.birth)
        self.assertIsNotNone(individual.birth.place)
        self.assertEqual(individual.birth.place.name, "New York, NY, USA")
        self.assertIsNotNone(individual.birth.place.map)
        self.assertEqual(individual.birth.place.map.latitude, "N40.7128")
        self.assertEqual(individual.birth.place.map.longitude, "W74.0060")

        # Test death place with coordinates
        self.assertIsNotNone(individual.death)
        self.assertIsNotNone(individual.death.place)
        self.assertEqual(individual.death.place.name, "Boston, MA, USA")
        self.assertIsNotNone(individual.death.place.map)
        self.assertEqual(individual.death.place.map.latitude, "N42.3601")
        self.assertEqual(individual.death.place.map.longitude, "W71.0589")

    def test_name_variants_parsing(self):
        """Test parsing of name variants"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_names.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test first name
        self.assertEqual(len(individual.names), 3)
        name1 = individual.names[0]
        self.assertEqual(name1.full, "John /Doe/")
        self.assertEqual(name1.given, "John")
        self.assertEqual(name1.surname, "Doe")
        self.assertEqual(name1.nickname, "Johnny")
        self.assertEqual(name1.prefix, "Dr.")
        self.assertEqual(name1.suffix, "Jr.")
        self.assertEqual(name1.romanized, "Giovanni /Rossi/")
        self.assertEqual(name1.phonetic, "JON DOE")
        self.assertEqual(name1.translation, "Jean /Dubois/")
        self.assertEqual(name1.name_type, "birth")

        # Test second name
        name2 = individual.names[1]
        self.assertEqual(name2.full, "Jean /Dubois/")
        self.assertEqual(name2.name_type, "married")
        self.assertEqual(name2.romanized, "Jean /Dubois/")

    def test_contact_information_parsing(self):
        """Test parsing of contact information"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_contact.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]
        submitter = database.submitters["@SUBM@"]

        # Test individual address
        self.assertIsNotNone(individual.address)
        self.assertEqual(individual.address.value, "123 Main St")
        self.assertEqual(individual.address.city, "New York")
        self.assertEqual(individual.address.state, "NY")
        self.assertEqual(individual.address.postal_code, "10001")
        self.assertEqual(individual.address.country, "USA")

        # Test individual contact info
        self.assertEqual(len(individual.address.phone), 1)
        self.assertEqual(individual.address.phone[0], "(555) 123-4567")
        self.assertEqual(len(individual.address.email), 1)
        self.assertEqual(individual.address.email[0], "john.doe@example.com")
        self.assertEqual(len(individual.address.fax), 1)
        self.assertEqual(individual.address.fax[0], "(555) 123-4568")
        self.assertEqual(len(individual.address.website), 1)
        self.assertEqual(individual.address.website[0], "https://johndoe.com")

        # Test submitter address
        self.assertIsNotNone(submitter.address)
        self.assertEqual(submitter.address.value, "789 Pine St")
        self.assertEqual(submitter.address.city, "Chicago")
        self.assertEqual(submitter.address.state, "IL")
        self.assertEqual(submitter.address.postal_code, "60601")
        self.assertEqual(submitter.address.country, "USA")

    def test_source_citations_parsing(self):
        """Test parsing of source citations with sub-tags"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_sources.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test source citations in events
        self.assertIsNotNone(individual.birth)
        self.assertEqual(len(individual.birth.sources), 1)
        self.assertEqual(individual.birth.sources[0], "@S1@")

        self.assertIsNotNone(individual.death)
        self.assertEqual(len(individual.death.sources), 1)
        self.assertEqual(individual.death.sources[0], "@S2@")

        # Test individual source citations
        # The parser includes multimedia object sources, so we have 3 sources total
        self.assertEqual(len(individual.sources), 3)
        self.assertEqual(individual.sources[0], "@S3@")  # Marriage source
        self.assertEqual(individual.sources[1], "@S4@")  # Individual source
        self.assertEqual(individual.sources[2], "@S5@")  # Multimedia source

        # Test source citations with sub-tags
        # The parser includes all source citations, including multimedia ones
        self.assertEqual(len(individual.source_citations), 3)

        # Find source citation
        source_citation = None
        for citation in individual.source_citations:
            if citation.xref == "S4":
                source_citation = citation
                break
        self.assertIsNotNone(source_citation)
        self.assertEqual(source_citation.sour_level, 1)
        self.assertTrue(
            any(
                line[1] == "PAGE" and line[2] == "Page 101112"
                for line in source_citation.sub_lines
            )
        )
        self.assertTrue(
            any(
                line[1] == "_LINK" and line[2] == "https://example.com/s4"
                for line in source_citation.sub_lines
            )
        )

    def test_multimedia_parsing(self):
        """Test parsing of multimedia objects"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_multimedia.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test multimedia references in individual
        self.assertEqual(len(individual.multimedia), 3)
        self.assertEqual(individual.multimedia[0], "M1")
        self.assertEqual(individual.multimedia[1], "M2")
        self.assertEqual(individual.multimedia[2], "M3")

        # Test multimedia objects
        self.assertEqual(len(database.multimedia), 12)

        multimedia1 = database.multimedia["@M1@"]
        self.assertEqual(multimedia1.file_path, "photo1.jpg")
        self.assertEqual(multimedia1.title, "John's Portrait")
        self.assertEqual(multimedia1.format, "jpeg")
        self.assertEqual(
            multimedia1.notes[0],
            "This is John's portrait\ntaken in 1920and shows him in his prime",
        )

        multimedia2 = database.multimedia["@M2@"]
        self.assertEqual(multimedia2.file_path, "document1.pdf")
        self.assertEqual(multimedia2.title, "Birth Certificate")
        self.assertEqual(multimedia2.format, "pdf")
        self.assertEqual(
            multimedia2.notes[0],
            "Official birth certificate\nfrom New York Stateshowing all details",
        )

    def test_notes_parsing(self):
        """Test parsing of notes with continuation and concatenation"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_simple.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        individual = database.individuals["@I1@"]

        # Test individual notes
        self.assertEqual(len(individual.notes), 1)
        self.assertEqual(
            individual.notes[0], "This is a test individual\nwith multiple lines"
        )

        # Test standalone note
        self.assertEqual(len(database.notes), 1)
        note = database.notes["@N1@"]
        self.assertEqual(note.text, "This is a standalone notewith concatenated text")

    def test_repository_parsing(self):
        """Test parsing of repository records"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_contact.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        database = self.parser.parse_content(content)
        repository = database.repositories["@R1@"]

        self.assertEqual(repository.name, "National Archives")
        self.assertIsNotNone(repository.address)
        self.assertEqual(repository.address.value, "700 Pennsylvania Ave NW")
        self.assertEqual(repository.address.city, "Washington")
        self.assertEqual(repository.address.state, "DC")
        self.assertEqual(repository.address.postal_code, "20408")
        self.assertEqual(repository.address.country, "USA")
        self.assertEqual(len(repository.address.phone), 1)
        self.assertEqual(repository.address.phone[0], "(202) 357-5000")
        self.assertEqual(len(repository.address.email), 1)
        self.assertEqual(repository.address.email[0], "info@archives.gov")
        self.assertEqual(len(repository.address.website), 1)
        self.assertEqual(repository.address.website[0], "https://www.archives.gov")
        self.assertEqual(repository.notes[0], "Official repository")

    def test_round_trip_complex_data(self):
        """Test round-trip with complex data structures"""
        # Read test file
        test_file = os.path.join(os.path.dirname(__file__), "test_simple.ged")
        with open(test_file, "r", encoding="utf-8") as f:
            content = f.read()

        # Parse
        database = self.parser.parse_content(content)

        # Export
        file = io.StringIO()
        self.exporter.export_content(file, database)
        exported_content = file.getvalue()

        # Round-trip
        database2 = self.parser.parse_content(exported_content)

        # Verify conservation
        self.assertEqual(len(database2.individuals), len(database.individuals))
        self.assertEqual(len(database2.families), len(database.families))
        self.assertEqual(len(database2.sources), len(database.sources))
        self.assertEqual(len(database2.notes), len(database.notes))
        self.assertEqual(len(database2.multimedia), len(database.multimedia))
        self.assertEqual(len(database2.repositories), len(database.repositories))
        self.assertEqual(len(database2.submitters), len(database.submitters))

        # Verify complex data is preserved
        ind1 = database2.individuals["@I1@"]
        self.assertEqual(len(ind1.names), 1)
        name = ind1.names[0]
        self.assertEqual(name.full, "John /Doe/")
        self.assertEqual(name.given, "John")
        self.assertEqual(name.surname, "Doe")

        # Verify birth place
        self.assertIsNotNone(ind1.birth)
        self.assertIsNotNone(ind1.birth.place)
        self.assertEqual(ind1.birth.place.name, "New York, NY, USA")

        # Verify other attributes
        self.assertEqual(len(ind1.occupations), 1)
        self.assertEqual(ind1.occupations[0], "Engineer")
        self.assertEqual(len(ind1.reference_numbers), 1)
        self.assertEqual(ind1.reference_numbers[0], "12345")
        self.assertEqual(len(ind1.multimedia), 1)
        self.assertEqual(ind1.multimedia[0], "M1")
        self.assertEqual(len(ind1.private_links), 1)
        self.assertEqual(ind1.private_links[0], "https://example.com")


if __name__ == "__main__":
    unittest.main()
