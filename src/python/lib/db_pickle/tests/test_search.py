"""
Tests for db_pickle search functionality.
"""

from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.core.enums import Sex
from lib.db_pickle.models.events import Date


class TestSearchFunctionality:
    """Test search functionality in PickleBaseData."""

    def test_search_persons_by_first_name_exact_match(self, populated_database):
        """Test exact first name search."""
        db = populated_database

        # Search for exact match
        results = db.search_persons_by_first_name("John")
        assert len(results) == 1
        assert results[0] == 1

        results = db.search_persons_by_first_name("Jane")
        assert len(results) == 1
        assert results[0] == 2

    def test_search_persons_by_first_name_partial_match(self, populated_database):
        """Test partial first name search."""
        db = populated_database

        # Add person with compound first name
        db.persons[5] = GenPerson(
            first_name="John Michael",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1985, month=1, day=1),
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

    def test_search_persons_by_first_name_case_insensitive(self, populated_database):
        """Test case insensitive first name search."""
        db = populated_database

        # Test different cases
        test_cases = ["john", "JOHN", "John", "JoHn"]

        for case in test_cases:
            results = db.search_persons_by_first_name(case)
            assert len(results) == 1
            assert results[0] == 1

    def test_search_persons_by_first_name_no_match(self, populated_database):
        """Test first name search with no matches."""
        db = populated_database

        results = db.search_persons_by_first_name("NonExistent")
        assert len(results) == 0

    def test_search_persons_by_surname_exact_match(self, populated_database):
        """Test exact surname search."""
        db = populated_database

        # All persons have surname "Smith"
        results = db.search_persons_by_surname("Smith")
        assert len(results) == 4
        assert set(results) == {1, 2, 3, 4}

    def test_search_persons_by_surname_partial_match(self, populated_database):
        """Test partial surname search."""
        db = populated_database

        # Add person with compound surname
        db.persons[5] = GenPerson(
            first_name="Alice",
            surname="Smith-Jones",
            sex=Sex.FEMALE,
            birth=Date(year=1990, month=1, day=1),
        )
        db.build_indexes()

        # Search for "Smith" should find all Smith and Smith-Jones
        results = db.search_persons_by_surname("Smith")
        assert len(results) == 5
        assert set(results) == {1, 2, 3, 4, 5}

        # Search for "Jones" should find Smith-Jones
        results = db.search_persons_by_surname("Jones")
        assert len(results) == 1
        assert results[0] == 5

    def test_search_persons_by_surname_case_insensitive(self, populated_database):
        """Test case insensitive surname search."""
        db = populated_database

        # Test different cases
        test_cases = ["smith", "SMITH", "Smith", "SmItH"]

        for case in test_cases:
            results = db.search_persons_by_surname(case)
            assert len(results) == 4
            assert set(results) == {1, 2, 3, 4}

    def test_search_persons_by_full_name_exact_match(self, populated_database):
        """Test exact full name search."""
        db = populated_database

        # Search for exact full names
        results = db.search_persons_by_full_name("John Smith")
        assert len(results) == 1
        assert results[0] == 1

        results = db.search_persons_by_full_name("Jane Smith")
        assert len(results) == 1
        assert results[0] == 2

    def test_search_persons_by_full_name_partial_match(self, populated_database):
        """Test partial full name search."""
        db = populated_database

        # Add person with compound names
        db.persons[5] = GenPerson(
            first_name="John Michael",
            surname="Smith-Jones",
            sex=Sex.MALE,
            birth=Date(year=1985, month=1, day=1),
        )
        db.build_indexes()

        # Search for "John" should find both "John Smith" and "John Michael Smith-Jones"
        results = db.search_persons_by_full_name("John")
        assert len(results) == 2
        assert set(results) == {1, 5}

        # Search for "Smith" should find all Smith surnames
        results = db.search_persons_by_full_name("Smith")
        assert len(results) == 5
        assert set(results) == {1, 2, 3, 4, 5}

    def test_search_persons_by_full_name_case_insensitive(self, populated_database):
        """Test case insensitive full name search."""
        db = populated_database

        # Test different cases
        test_cases = ["john smith", "JOHN SMITH", "John Smith", "JoHn SmItH"]

        for case in test_cases:
            results = db.search_persons_by_full_name(case)
            assert len(results) == 1
            assert results[0] == 1

    def test_search_strings_by_content(self, populated_database):
        """Test string content search."""
        db = populated_database

        # Add some strings
        db.strings[1] = "New York"
        db.strings[2] = "London"
        db.strings[3] = "New Orleans"
        db.strings[4] = "Paris"
        db.strings[5] = "Newark"

        db.build_indexes()

        # Search for "New"
        results = db.search_strings_by_content("New")
        assert len(results) == 3
        assert set(results) == {1, 3, 5}

        # Search for "London"
        results = db.search_strings_by_content("London")
        assert len(results) == 1
        assert results[0] == 2

        # Search for "Paris"
        results = db.search_strings_by_content("Paris")
        assert len(results) == 1
        assert results[0] == 4

    def test_search_strings_by_content_case_insensitive(self, populated_database):
        """Test case insensitive string content search."""
        db = populated_database

        # Add strings with different cases
        db.strings[1] = "New York"
        db.strings[2] = "london"
        db.strings[3] = "NEW ORLEANS"

        db.build_indexes()

        # Test different cases
        test_cases = ["new", "NEW", "New", "NeW"]

        for case in test_cases:
            results = db.search_strings_by_content(case)
            assert len(results) == 2
            assert set(results) == {1, 3}

    def test_search_strings_by_content_no_match(self, populated_database):
        """Test string content search with no matches."""
        db = populated_database

        # Add some strings
        db.strings[1] = "New York"
        db.strings[2] = "London"

        db.build_indexes()

        # Search for non-existent content
        results = db.search_strings_by_content("NonExistent")
        assert len(results) == 0

    def test_search_with_empty_database(self, empty_database):
        """Test search with empty database."""
        db = empty_database

        # All searches should return empty results
        assert db.search_persons_by_first_name("John") == []
        assert db.search_persons_by_surname("Smith") == []
        assert db.search_persons_by_full_name("John Smith") == []
        assert db.search_strings_by_content("New York") == []

    def test_search_with_special_characters(self, empty_database):
        """Test search with special characters."""
        db = empty_database

        # Add person with special characters in name
        db.persons[1] = GenPerson(
            first_name="José",
            surname="García-López",
            sex=Sex.MALE,
            birth=Date(year=1980, month=1, day=1),
        )
        db.build_indexes()

        # Search should work with special characters
        results = db.search_persons_by_first_name("José")
        assert len(results) == 1
        assert results[0] == 1

        results = db.search_persons_by_surname("García")
        assert len(results) == 1
        assert results[0] == 1

        results = db.search_persons_by_surname("López")
        assert len(results) == 1
        assert results[0] == 1

    def test_search_with_numbers(self, empty_database):
        """Test search with numbers in names."""
        db = empty_database

        # Add person with numbers in name
        db.persons[1] = GenPerson(
            first_name="John2",
            surname="Smith3",
            sex=Sex.MALE,
            birth=Date(year=1980, month=1, day=1),
        )
        db.build_indexes()

        # Search should work with numbers
        results = db.search_persons_by_first_name("John2")
        assert len(results) == 1
        assert results[0] == 1

        results = db.search_persons_by_surname("Smith3")
        assert len(results) == 1
        assert results[0] == 1

    def test_search_performance(self, large_database):
        """Test search performance with large dataset."""
        db = large_database

        # Search should be fast even with large dataset
        import time

        start_time = time.time()
        results = db.search_persons_by_surname("Surname0")
        end_time = time.time()

        # Should complete quickly (less than 1 second)
        assert end_time - start_time < 1.0
        assert len(results) == 10  # 10 persons with Surname0
