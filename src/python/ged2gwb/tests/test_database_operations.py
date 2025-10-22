"""
Tests for database operations on MessagePack databases.

Tests read, write, search, and manipulation operations on MessagePack databases.
"""

import pytest
import tempfile
import shutil
import logging
from pathlib import Path
from typing import List

from lib.db.io.msgpack import MessagePackWriter, MessagePackReader
from lib.db.database.base_data import BaseData
from lib.db.database.func_creator import create_base_func
from lib.db.models.person import GenPerson
from lib.db.models.family import GenFamily
from lib.db.models.events import Date, Event, Title
from lib.db.core.types import Iper, Ifam, Istr
from lib.db.core.enums import Sex, RelationKind, DivorceStatus

# Configure logger
logger = logging.getLogger(__name__)


class TestDatabaseOperations:
    """Test database operations on MessagePack databases."""

    @pytest.fixture
    def temp_dir(self):
        """Create a temporary directory for tests."""
        temp_path = Path(tempfile.mkdtemp())
        yield temp_path
        shutil.rmtree(temp_path)

    @pytest.fixture
    def sample_database(self, temp_dir):
        """Create a sample database for testing."""
        # Create sample data
        db_data = BaseData()

        # Add persons
        person1 = GenPerson(
            first_name="John",
            surname="Doe",
            sex=Sex.MALE,
            key_index=Iper(1)
        )
        person2 = GenPerson(
            first_name="Jane",
            surname="Smith",
            sex=Sex.FEMALE,
            key_index=Iper(2)
        )
        person3 = GenPerson(
            first_name="Robert",
            surname="Johnson",
            sex=Sex.MALE,
            key_index=Iper(3)
        )

        db_data.persons[Iper(1)] = person1
        db_data.persons[Iper(2)] = person2
        db_data.persons[Iper(3)] = person3

        # Add families
        family1 = GenFamily(
            relation=RelationKind.MARRIED,
            fam_index=Ifam(1)
        )
        family2 = GenFamily(
            relation=RelationKind.MARRIED,
            fam_index=Ifam(2)
        )

        db_data.families[Ifam(1)] = family1
        db_data.families[Ifam(2)] = family2

        # Add strings
        db_data.strings[Istr(1)] = "New York"
        db_data.strings[Istr(2)] = "Boston"
        db_data.strings[Istr(3)] = "Engineer"

        # Build indexes
        db_data.build_indexes()

        return db_data

    def test_database_write_and_read(self, temp_dir, sample_database):
        """Test writing and reading a MessagePack database."""
        logger.info("=== Testing Database Write and Read ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        db_path = writer.write_database(sample_database, "test_db")

        assert Path(db_path).exists()
        assert Path(db_path).is_dir()
        logger.info(f"PASS: Database written to {db_path}")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        loaded_db = reader.load_database("test_db")

        assert loaded_db is not None
        assert loaded_db.persons_count == 3
        assert loaded_db.families_count == 2
        assert loaded_db.strings_count == 3
        logger.info("PASS: Database loaded successfully")

        # Verify data integrity
        assert Iper(1) in loaded_db.persons
        assert Iper(2) in loaded_db.persons
        assert Iper(3) in loaded_db.persons

        person1 = loaded_db.persons[Iper(1)]
        assert person1.first_name == "John"
        assert person1.surname == "Doe"
        assert person1.sex == Sex.MALE
        logger.info("PASS: Data integrity verified")

    def test_database_search_operations(self, temp_dir, sample_database):
        """Test search operations on the database."""
        logger.info("=== Testing Database Search Operations ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "search_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("search_test")

        # Test search by first name
        john_results = db.search_persons_by_first_name("John")
        assert len(john_results) == 1
        assert john_results[0] == Iper(1)
        logger.info("PASS: Search by first name works")

        # Test search by surname
        doe_results = db.search_persons_by_surname("Doe")
        assert len(doe_results) == 1
        assert doe_results[0] == Iper(1)
        logger.info("PASS: Search by surname works")

        # Test search by full name
        john_doe_results = db.search_persons_by_full_name("John Doe")
        assert len(john_doe_results) == 1
        assert john_doe_results[0] == Iper(1)
        logger.info("PASS: Search by full name works")

        # Test search by string content
        new_york_results = db.search_strings_by_content("New York")
        assert len(new_york_results) == 1
        assert str(new_york_results[0]) == "1"  # Compare as string
        logger.info("PASS: Search by string content works")

    def test_database_statistics(self, temp_dir, sample_database):
        """Test database statistics retrieval."""
        logger.info("=== Testing Database Statistics ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "stats_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("stats_test")

        # Test statistics
        stats = db.get_statistics()
        assert stats["persons"] == 3
        assert stats["families"] == 2
        assert stats["strings"] == 3
        logger.info("PASS: Database statistics correct")

        # Test metadata
        metadata = reader.get_database_metadata("stats_test")
        assert metadata is not None
        assert metadata["persons_count"] == 3
        assert metadata["families_count"] == 2
        assert metadata["strings_count"] == 3
        logger.info("PASS: Database metadata correct")

    def test_database_person_operations(self, temp_dir, sample_database):
        """Test person-specific operations."""
        logger.info("=== Testing Person Operations ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "person_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("person_test")

        # Test get person by ID
        person = reader.get_person_by_id("person_test", 1)
        assert person is not None
        assert person.first_name == "John"
        assert person.surname == "Doe"
        logger.info("PASS: Get person by ID works")

        # Test get all persons
        all_persons = reader.get_all_persons("person_test")
        assert len(all_persons) == 3
        logger.info("PASS: Get all persons works")

        # Test search persons by name
        john_persons = reader.search_persons_by_name("person_test", "John", "Doe")
        assert len(john_persons) == 1
        assert john_persons[0].first_name == "John"
        logger.info("PASS: Search persons by name works")

    def test_database_family_operations(self, temp_dir, sample_database):
        """Test family-specific operations."""
        logger.info("=== Testing Family Operations ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "family_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("family_test")

        # Test get family by ID
        family = reader.get_family_by_id("family_test", 1)
        assert family is not None
        assert family.fam_index == Ifam(1)
        logger.info("PASS: Get family by ID works")

        # Test get all families
        all_families = reader.get_all_families("family_test")
        assert len(all_families) == 2
        logger.info("PASS: Get all families works")

    def test_database_validation(self, temp_dir, sample_database):
        """Test database validation."""
        logger.info("=== Testing Database Validation ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "validation_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("validation_test")

        # Test validation
        validation_result = db.validate()
        assert validation_result == []  # Empty list means no validation errors
        logger.info("PASS: Database validation works")

        # Test copy
        db_copy = db.copy()
        assert db_copy.persons_count == db.persons_count
        assert db_copy.families_count == db.families_count
        assert db_copy.strings_count == db.strings_count
        logger.info("PASS: Database copy works")

    def test_database_index_operations(self, temp_dir, sample_database):
        """Test database index operations."""
        logger.info("=== Testing Database Index Operations ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "index_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("index_test")

        # Test index building
        db.build_indexes()
        assert len(db.first_name_index) > 0
        assert len(db.surname_index) > 0
        assert len(db.full_name_index) > 0
        logger.info("PASS: Index building works")

        # Test index clearing
        db.clear()
        assert db.persons_count == 0
        assert db.families_count == 0
        assert db.strings_count == 0
        logger.info("PASS: Database clearing works")

    def test_database_concurrent_operations(self, temp_dir):
        """Test concurrent database operations."""
        logger.info("=== Testing Concurrent Database Operations ===")

        # Create multiple databases
        for i in range(3):
            db_data = BaseData()
            person = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i}",
                sex=Sex.MALE,
                key_index=Iper(i)
            )
            db_data.persons[Iper(i)] = person
            db_data.build_indexes()

            writer = MessagePackWriter(str(temp_dir))
            writer.write_database(db_data, f"concurrent_test_{i}")

        # Read all databases concurrently
        reader = MessagePackReader(str(temp_dir))
        databases = []
        for i in range(3):
            db = reader.load_database(f"concurrent_test_{i}")
            assert db is not None
            assert db.persons_count == 1
            databases.append(db)

        assert len(databases) == 3
        logger.info("PASS: Concurrent database operations work")

    def test_database_error_handling(self, temp_dir):
        """Test database error handling."""
        logger.info("=== Testing Database Error Handling ===")

        reader = MessagePackReader(str(temp_dir))

        # Test loading non-existent database
        db = reader.load_database("non_existent")
        assert db is None
        logger.info("PASS: Non-existent database handling works")

        # Test invalid database path
        invalid_reader = MessagePackReader("/invalid/path")
        db = invalid_reader.load_database("test")
        assert db is None
        logger.info("PASS: Invalid path handling works")

    def test_database_performance(self, temp_dir):
        """Test database performance with larger datasets."""
        logger.info("=== Testing Database Performance ===")

        # Create larger dataset
        db_data = BaseData()

        # Add 100 persons
        for i in range(100):
            person = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i}",
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                key_index=Iper(i)
            )
            db_data.persons[Iper(i)] = person

        # Add 50 families
        for i in range(50):
            family = GenFamily(
                relation=RelationKind.MARRIED,
                fam_index=Ifam(i)
            )
            db_data.families[Ifam(i)] = family

        # Add 200 strings
        for i in range(200):
            db_data.strings[Istr(i)] = f"String{i}"

        # Build indexes
        db_data.build_indexes()

        # Write database
        import time
        start_time = time.time()
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(db_data, "performance_test")
        write_time = time.time() - start_time

        # Read database
        start_time = time.time()
        reader = MessagePackReader(str(temp_dir))
        loaded_db = reader.load_database("performance_test")
        read_time = time.time() - start_time

        assert loaded_db is not None
        assert loaded_db.persons_count == 100
        assert loaded_db.families_count == 50
        assert loaded_db.strings_count == 200

        logger.info(f"PASS: Performance test completed")
        logger.info(f"   Write time: {write_time:.3f}s")
        logger.info(f"   Read time: {read_time:.3f}s")
        logger.info(f"   Total persons: {loaded_db.persons_count}")
        logger.info(f"   Total families: {loaded_db.families_count}")
        logger.info(f"   Total strings: {loaded_db.strings_count}")

    def test_advanced_search_operations(self, temp_dir):
        """Test advanced search operations with complex data."""
        logger.info("=== Testing Advanced Search Operations ===")

        # Create complex database with multiple persons with similar names
        db_data = BaseData()

        # Add persons with similar names
        persons_data = [
            ("John", "Doe", Sex.MALE, 1),
            ("John", "Smith", Sex.MALE, 2),
            ("Jane", "Doe", Sex.FEMALE, 3),
            ("Johnny", "Doe", Sex.MALE, 4),
            ("Jean", "Dupont", Sex.MALE, 5),
            ("Jean-Pierre", "Martin", Sex.MALE, 6),
            ("Marie", "Dupont", Sex.FEMALE, 7),
            ("Marie-Claire", "Martin", Sex.FEMALE, 8),
        ]

        for first_name, surname, sex, key in persons_data:
            person = GenPerson(
                first_name=first_name,
                surname=surname,
                sex=sex,
                key_index=Iper(key)
            )
            db_data.persons[Iper(key)] = person

        # Add places and occupations
        places = ["Paris", "Lyon", "Marseille", "Toulouse", "Nice"]
        occupations = ["Engineer", "Doctor", "Teacher", "Lawyer", "Artist"]

        for i, place in enumerate(places):
            db_data.strings[Istr(i + 1)] = place
        for i, occupation in enumerate(occupations):
            db_data.strings[Istr(i + 10)] = occupation

        db_data.build_indexes()

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(db_data, "advanced_search_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("advanced_search_test")

        # Test partial name searches
        john_results = db.search_persons_by_first_name("John")
        assert len(john_results) == 3  # John Doe, John Smith, Johnny Doe
        logger.info("PASS: Partial first name search works")

        # Test case-insensitive search
        doe_results = db.search_persons_by_surname("doe")
        assert len(doe_results) == 3  # John Doe, Jane Doe, Johnny Doe
        logger.info("PASS: Case-insensitive surname search works")

        # Test exact full name search
        john_doe_results = db.search_persons_by_full_name("John Doe")
        assert len(john_doe_results) == 1
        assert john_doe_results[0] == Iper(1)
        logger.info("PASS: Exact full name search works")

        # Test string content search
        paris_results = db.search_strings_by_content("Paris")
        assert len(paris_results) == 1
        assert str(paris_results[0]) == "1"  # Compare as string
        logger.info("PASS: String content search works")

        # Test search with no results
        no_results = db.search_persons_by_first_name("NonExistent")
        assert len(no_results) == 0
        logger.info("PASS: No results search works")

    def test_search_by_place_and_occupation(self, temp_dir):
        """Test search by place and occupation strings."""
        logger.info("=== Testing Search by Place and Occupation ===")

        db_data = BaseData()

        # Add persons with events containing places and occupations
        person1 = GenPerson(
            first_name="Alice",
            surname="Johnson",
            sex=Sex.FEMALE,
            key_index=Iper(1)
        )

        # Add events with places and occupations
        birth_event = Event(
            name="BIRT",
            date=Date(year=1990, month=1, day=15),
            place="Paris",  # Direct string, not Istr
            note="Engineer"  # Direct string, not Istr
        )
        person1.events.append(birth_event)

        db_data.persons[Iper(1)] = person1

        # Add strings for places and occupations
        db_data.strings[Istr(1)] = "Paris, France"
        db_data.strings[Istr(2)] = "Lyon, France"
        db_data.strings[Istr(10)] = "Software Engineer"
        db_data.strings[Istr(11)] = "Doctor"

        db_data.build_indexes()

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(db_data, "place_occupation_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("place_occupation_test")

        # Test search by place
        paris_results = db.search_strings_by_content("Paris")
        assert len(paris_results) == 1
        logger.info("PASS: Place search works")

        # Test search by occupation
        engineer_results = db.search_strings_by_content("Engineer")
        assert len(engineer_results) == 1
        logger.info("PASS: Occupation search works")

    def test_family_search_operations(self, temp_dir):
        """Test family-specific search operations."""
        logger.info("=== Testing Family Search Operations ===")

        db_data = BaseData()

        # Add persons
        husband = GenPerson(
            first_name="John",
            surname="Doe",
            sex=Sex.MALE,
            key_index=Iper(1)
        )
        wife = GenPerson(
            first_name="Jane",
            surname="Smith",
            sex=Sex.FEMALE,
            key_index=Iper(2)
        )

        db_data.persons[Iper(1)] = husband
        db_data.persons[Iper(2)] = wife

        # Add family with marriage date
        marriage_date = Date(year=2020, month=6, day=15)
        family = GenFamily(
            relation=RelationKind.MARRIED,
            fam_index=Ifam(1)
        )
        family.events.append(Event(
            name="MARR",
            date=marriage_date,
            note="Wedding ceremony"  # Direct string, not Istr
        ))

        db_data.families[Ifam(1)] = family
        db_data.strings[Istr(1)] = "Wedding ceremony"

        db_data.build_indexes()

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(db_data, "family_search_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))
        db = reader.load_database("family_search_test")

        # Test search families by marriage date
        marriage_families = reader.search_families_by_marriage_date("family_search_test", 2020)
        assert len(marriage_families) == 1
        logger.info("PASS: Family search by marriage date works")

        # Test get all families
        all_families = reader.get_all_families("family_search_test")
        assert len(all_families) == 1
        logger.info("PASS: Get all families works")

    def test_database_metadata_operations(self, temp_dir, sample_database):
        """Test database metadata operations."""
        logger.info("=== Testing Database Metadata Operations ===")

        # Write database
        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(sample_database, "metadata_test")

        # Read database
        reader = MessagePackReader(str(temp_dir))

        # Test get database metadata
        metadata = reader.get_database_metadata("metadata_test")
        assert metadata is not None
        assert "persons_count" in metadata
        assert "families_count" in metadata
        assert "strings_count" in metadata
        assert metadata["persons_count"] == 3
        assert metadata["families_count"] == 2
        assert metadata["strings_count"] == 3
        logger.info("PASS: Database metadata retrieval works")

        # Test get database statistics
        db = reader.load_database("metadata_test")
        stats = db.get_statistics()
        assert stats["persons"] == 3
        assert stats["families"] == 2
        assert stats["strings"] == 3
        logger.info("PASS: Database statistics works")

    def test_database_validation_edge_cases(self, temp_dir):
        """Test database validation with edge cases."""
        logger.info("=== Testing Database Validation Edge Cases ===")

        # Test empty database
        empty_db = BaseData()
        empty_db.build_indexes()

        writer = MessagePackWriter(str(temp_dir))
        writer.write_database(empty_db, "empty_test")

        reader = MessagePackReader(str(temp_dir))
        loaded_empty_db = reader.load_database("empty_test")

        assert loaded_empty_db is not None
        assert loaded_empty_db.persons_count == 0
        assert loaded_empty_db.families_count == 0
        assert loaded_empty_db.strings_count == 0
        validation_result = loaded_empty_db.validate()
        assert validation_result == []  # Empty list means no validation errors
        logger.info("PASS: Empty database validation works")

        # Test database with only strings
        string_only_db = BaseData()
        string_only_db.strings[Istr(1)] = "Test string"
        string_only_db.build_indexes()

        writer.write_database(string_only_db, "strings_only_test")
        loaded_strings_db = reader.load_database("strings_only_test")

        assert loaded_strings_db is not None
        assert loaded_strings_db.strings_count == 1
        validation_result = loaded_strings_db.validate()
        assert validation_result == []  # Empty list means no validation errors
        logger.info("PASS: Strings-only database validation works")


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
