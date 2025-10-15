"""
Tests for db_pickle database operations.
"""

import pytest
from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.models.family import GenFamily
from lib.db_pickle.models.relations import GenCouple, GenDescend
from lib.db_pickle.core.enums import Sex, RelationKind
from lib.db_pickle.models.events import Date


class TestPickleBaseData:
    """Test PickleBaseData class."""

    def test_empty_database_creation(self):
        """Test creating an empty database."""
        db = PickleBaseData()

        assert len(db.persons) == 0
        assert len(db.families) == 0
        assert len(db.couples) == 0
        assert len(db.descends) == 0
        assert len(db.strings) == 0

    def test_add_person(self, empty_database):
        """Test adding a person to the database."""
        db = empty_database
        person = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=3, day=15)
        )

        db.persons[1] = person

        assert len(db.persons) == 1
        assert 1 in db.persons
        assert db.persons[1] == person

    def test_add_family(self, empty_database):
        """Test adding a family to the database."""
        db = empty_database
        family = GenFamily(relation=RelationKind.MARRIED)
        couple = GenCouple(father=1, mother=2)
        descend = GenDescend(children=[3, 4])

        db.families[1] = family
        db.couples[1] = couple
        db.descends[1] = descend

        assert len(db.families) == 1
        assert len(db.couples) == 1
        assert len(db.descends) == 1
        assert 1 in db.families
        assert 1 in db.couples
        assert 1 in db.descends

    def test_build_indexes(self, populated_database):
        """Test building search indexes."""
        db = populated_database

        # Indexes should be built in the fixture
        assert hasattr(db, 'first_name_index')
        assert hasattr(db, 'surname_index')
        assert hasattr(db, 'full_name_index')
        assert hasattr(db, 'string_content_index')

    def test_search_persons_by_first_name(self, populated_database):
        """Test searching persons by first name."""
        db = populated_database

        # Search for "John"
        results = db.search_persons_by_first_name("John")
        assert len(results) == 1
        assert results[0] == 1

        # Search for "Jane"
        results = db.search_persons_by_first_name("Jane")
        assert len(results) == 1
        assert results[0] == 2

        # Search for non-existent name
        results = db.search_persons_by_first_name("NonExistent")
        assert len(results) == 0

    def test_search_persons_by_surname(self, populated_database):
        """Test searching persons by surname."""
        db = populated_database

        # All persons have surname "Smith"
        results = db.search_persons_by_surname("Smith")
        assert len(results) == 4
        assert set(results) == {1, 2, 3, 4}

        # Search for non-existent surname
        results = db.search_persons_by_surname("NonExistent")
        assert len(results) == 0

    def test_search_persons_by_full_name(self, populated_database):
        """Test searching persons by full name."""
        db = populated_database

        # Search for "John Smith"
        results = db.search_persons_by_full_name("John Smith")
        assert len(results) == 1
        assert results[0] == 1

        # Search for "Jane Smith"
        results = db.search_persons_by_full_name("Jane Smith")
        assert len(results) == 1
        assert results[0] == 2

        # Search for non-existent full name
        results = db.search_persons_by_full_name("Non Existent")
        assert len(results) == 0

    def test_search_strings_by_content(self, populated_database):
        """Test searching strings by content."""
        db = populated_database

        # Add some strings
        db.strings[1] = "New York"
        db.strings[2] = "London"
        db.strings[3] = "New Orleans"

        # Rebuild indexes to include strings
        db.build_indexes()

        # Search for "New"
        results = db.search_strings_by_content("New")
        assert len(results) == 2
        assert set(results) == {1, 3}

        # Search for "London"
        results = db.search_strings_by_content("London")
        assert len(results) == 1
        assert results[0] == 2

    def test_case_insensitive_search(self, populated_database):
        """Test that search is case insensitive."""
        db = populated_database

        # Search with different cases
        results_lower = db.search_persons_by_first_name("john")
        results_upper = db.search_persons_by_first_name("JOHN")
        results_mixed = db.search_persons_by_first_name("John")

        assert results_lower == results_upper == results_mixed
        assert len(results_lower) == 1
        assert results_lower[0] == 1

    def test_partial_name_search(self, populated_database):
        """Test partial name searching."""
        db = populated_database

        # Add a person with compound first name
        db.persons[5] = GenPerson(
            first_name="John Michael",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1985, month=1, day=1)
        )
        db.build_indexes()

        # Search for "John" should find both "John" and "John Michael"
        results = db.search_persons_by_first_name("John")
        assert len(results) == 2
        assert set(results) == {1, 5}

        # Search for "Michael" should find "John Michael"
        results = db.search_persons_by_first_name("Michael")
        assert len(results) == 1
        assert results[0] == 5

    def test_database_validation(self, populated_database):
        """Test database validation."""
        db = populated_database

        # This should be valid
        errors = db.validate()
        assert len(errors) == 0

    def test_database_validation_with_errors(self, empty_database):
        """Test database validation with errors."""
        db = empty_database

        # Add invalid data (couple references non-existent persons)
        db.couples[1] = GenCouple(father=999, mother=998)  # Non-existent persons

        errors = db.validate()
        assert len(errors) > 0
        assert any("999" in error for error in errors)
        assert any("998" in error for error in errors)

    def test_database_statistics(self, populated_database):
        """Test database statistics."""
        db = populated_database

        stats = db.get_statistics()

        assert stats['persons'] == 4
        assert stats['families'] == 1
        assert stats['couples'] == 1
        assert stats['descends'] == 1
        assert stats['strings'] == 0

    def test_database_clear(self, populated_database):
        """Test clearing the database."""
        db = populated_database

        assert len(db.persons) == 4

        db.clear()

        assert len(db.persons) == 0
        assert len(db.families) == 0
        assert len(db.couples) == 0
        assert len(db.descends) == 0
        assert len(db.strings) == 0

    def test_database_copy(self, populated_database):
        """Test copying the database."""
        db = populated_database
        db_copy = db.copy()

        # Should have same data
        assert len(db_copy.persons) == len(db.persons)
        assert len(db_copy.families) == len(db.families)
        assert len(db_copy.couples) == len(db.couples)
        assert len(db_copy.descends) == len(db.descends)

        # Should be different objects
        assert db_copy is not db

        # Modifying copy shouldn't affect original
        db_copy.persons[5] = GenPerson(
            first_name="New",
            surname="Person",
            sex=Sex.MALE,
            birth=Date(year=2000, month=1, day=1)
        )

        assert len(db.persons) == 4
        assert len(db_copy.persons) == 5
