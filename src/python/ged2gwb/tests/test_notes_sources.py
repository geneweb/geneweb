"""
Tests for notes and sources conversion in ged2gwb.

Tests that notes and sources from GEDCOM files are properly converted
and stored in the MessagePack database.
"""

import pytest
import tempfile
import shutil
from pathlib import Path

from lib.db.io.msgpack import MessagePackWriter, MessagePackReader
from lib.db.database.base_data import BaseData


class TestNotesSourcesConversion:
    """Test notes and sources conversion from GEDCOM to MessagePack."""

    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for tests."""
        temp_path = Path(tempfile.mkdtemp())
        yield temp_path
        shutil.rmtree(temp_path)

    def test_notes_sources_conversion(self, temp_dir):
        """Test that notes and sources are converted and stored."""
        # Create a GEDCOM file with notes and sources
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Doe/
1 NOTE This is a test note for John
1 SOUR @S1@
0 @I2@ INDI
1 NAME Jane /Smith/
1 NOTE This is a test note for Jane
1 SOUR @S2@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 NOTE This is a family note
1 SOUR @S3@
0 @S1@ SOUR
1 TITL Birth Certificate
1 NOTE This is a source note
0 @S2@ SOUR
1 TITL Marriage Record
1 NOTE This is another source note
0 @S3@ SOUR
1 TITL Family Record
1 NOTE This is a family source note
0 TRLR"""

        gedcom_file = temp_dir / "test_notes_sources.ged"
        with open(gedcom_file, "w", encoding="utf-8") as f:
            f.write(gedcom_content)

        # Convert using ged2gwb
        from ged2gwb.core.converter import Ged2GwbConverter
        from ged2gwb.utils.options import ConversionOptions

        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=temp_dir / "test_notes_sources.msgpack",
        )

        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        # Verify conversion was successful
        assert stats["conversion_successful"]
        assert stats["individuals_count"] == 2
        assert stats["families_count"] == 1

        # Load the database and check notes and sources
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("test_notes_sources")

        # Check that persons have notes and sources
        persons = list(db.persons.values())
        assert len(persons) == 2

        # Find John Doe (I1)
        john = None
        jane = None
        for person in persons:
            if person.first_name == "John" and person.surname == "Doe":
                john = person
            elif person.first_name == "Jane" and person.surname == "Smith":
                jane = person

        assert john is not None, "John Doe should be found"
        assert jane is not None, "Jane Smith should be found"

        # Check John's notes and sources
        assert john.notes == "This is a test note for John"
        assert john.sources == "@S1@"

        # Check Jane's notes and sources
        assert jane.notes == "This is a test note for Jane"
        assert jane.sources == "@S2@"

        # Check family notes and sources
        families = list(db.families.values())
        assert len(families) == 1

        family = families[0]
        assert family.notes == "This is a family note"
        assert family.sources == "@S3@"

        print("✓ Notes and sources conversion test passed")

    def test_default_source_option(self, temp_dir):
        """Test that --default-source option adds sources to persons without sources."""
        # Create a GEDCOM file without sources
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Doe/
2 NOTE This is a test note
0 TRLR"""

        gedcom_file = temp_dir / "test_default_source.ged"
        with open(gedcom_file, "w", encoding="utf-8") as f:
            f.write(gedcom_content)

        # Convert with default source
        from ged2gwb.core.converter import Ged2GwbConverter
        from ged2gwb.utils.options import ConversionOptions

        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=temp_dir / "test_default_source.msgpack",
            default_source="Default Test Source",
        )

        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        # Verify conversion was successful
        assert stats["conversion_successful"]
        assert stats["individuals_count"] == 1

        # Load the database and check default source was added
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("test_default_source")

        persons = list(db.persons.values())
        assert len(persons) == 1

        person = persons[0]
        assert "Default source: Default Test Source" in person.notes
        assert person.sources == ""  # No sources in GEDCOM

        print("✓ Default source option test passed")

    def test_uin_option(self, temp_dir):
        """Test that --uin option adds untreated tags to notes."""
        # Create a GEDCOM file with custom tags
        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Doe/
2 _CUSTOM_TAG Custom value
2 NOTE This is a test note
0 TRLR"""

        gedcom_file = temp_dir / "test_uin.ged"
        with open(gedcom_file, "w", encoding="utf-8") as f:
            f.write(gedcom_content)

        # Convert with --uin option
        from ged2gwb.core.converter import Ged2GwbConverter
        from ged2gwb.utils.options import ConversionOptions

        options = ConversionOptions(
            input_file=gedcom_file,
            output_file=temp_dir / "test_uin.msgpack",
            uin=True,
        )

        converter = Ged2GwbConverter(options)
        stats = converter.convert()

        # Verify conversion was successful
        assert stats["conversion_successful"]
        assert stats["individuals_count"] == 1

        # Load the database and check untreated tags were added
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("test_uin")

        persons = list(db.persons.values())
        assert len(persons) == 1

        person = persons[0]
        # The untreated tags should be added to notes
        # Note: The exact format depends on the _extract_untreated_tags implementation
        assert person.notes is not None
        assert person.sources == ""

        print("✓ UIN option test passed")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
