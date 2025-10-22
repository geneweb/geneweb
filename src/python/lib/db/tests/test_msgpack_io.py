"""
Tests for MessagePack I/O operations.
"""

import os

from lib.db.io.msgpack import MessagePackWriter, MessagePackReader


class TestMessagePackWriter:
    """Test MessagePackWriter class."""

    def test_write_database(self, populated_database, temp_file):
        """Test writing database to MessagePack format."""
        writer = MessagePackWriter("test_bases")
        db = populated_database

        db_path = writer.write_database(db, "test_db")

        # Check directory exists
        assert os.path.exists(db_path)
        assert db_path.endswith("test_db.msgpack")

        # Check main files exist
        assert os.path.exists(os.path.join(db_path, "base"))
        assert os.path.exists(os.path.join(db_path, "base.acc"))
        assert os.path.exists(os.path.join(db_path, "names.inx"))
        assert os.path.exists(os.path.join(db_path, "snames.inx"))
        assert os.path.exists(os.path.join(db_path, "fnames.inx"))

    def test_write_empty_database(self, empty_database):
        """Test writing empty database."""
        writer = MessagePackWriter("test_bases")
        db = empty_database

        db_path = writer.write_database(db, "empty_db")

        # Check directory exists
        assert os.path.exists(db_path)
        assert db_path.endswith("empty_db.msgpack")

    def test_write_database_with_validation(self, populated_database):
        """Test writing database with validation."""
        writer = MessagePackWriter("test_bases")
        db = populated_database

        # Validate database first
        errors = db.validate()
        assert len(errors) == 0, f"Database validation failed: {errors}"

        # Write database
        db_path = writer.write_database(db, "validated_db")

        assert os.path.exists(db_path)


class TestMessagePackReader:
    """Test MessagePackReader class."""

    def test_load_database(self, populated_database):
        """Test loading database from MessagePack format."""
        # First write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "test_load")

        # Then read it back
        reader = MessagePackReader("test_bases")
        loaded_db = reader.load_database("test_load")

        assert loaded_db is not None
        assert loaded_db.persons_count == populated_database.persons_count
        assert loaded_db.families_count == populated_database.families_count

    def test_load_nonexistent_database(self):
        """Test loading non-existent database."""
        reader = MessagePackReader("test_bases")
        loaded_db = reader.load_database("nonexistent")

        assert loaded_db is None

    def test_list_available_databases(self, populated_database):
        """Test listing available databases."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "list_test")

        # List databases
        reader = MessagePackReader("test_bases")
        databases = reader.list_available_databases()

        assert "list_test" in databases

    def test_search_persons_by_surname(self, populated_database):
        """Test searching persons by surname."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "search_test")

        # Search
        reader = MessagePackReader("test_bases")
        results = reader.search_persons_by_surname("search_test", "Smith")

        assert len(results) >= 1

    def test_search_persons_by_first_name(self, populated_database):
        """Test searching persons by first name."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "search_test")

        # Search
        reader = MessagePackReader("test_bases")
        results = reader.search_persons_by_first_name("search_test", "John")

        assert len(results) >= 1

    def test_search_persons_by_full_name(self, populated_database):
        """Test searching persons by full name."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "search_test")

        # Search
        reader = MessagePackReader("test_bases")
        results = reader.search_persons_by_full_name("search_test", "John Smith")

        assert len(results) >= 1

    def test_search_strings_by_content(self, populated_database):
        """Test searching strings by content."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "search_test")

        # Search
        reader = MessagePackReader("test_bases")
        results = reader.search_strings_by_content("search_test", "test")

        # Results should be a list of tuples (string_id, content)
        assert isinstance(results, list)

    def test_get_database_statistics(self, populated_database):
        """Test getting database statistics."""
        # Write a database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "stats_test")

        # Get statistics
        reader = MessagePackReader("test_bases")
        stats = reader.get_database_statistics("stats_test")

        assert stats is not None
        assert "total_persons" in stats
        assert "total_families" in stats
        assert stats["total_persons"] == populated_database.persons_count
        assert stats["total_families"] == populated_database.families_count


class TestMessagePackIntegration:
    """Integration tests for MessagePack I/O."""

    def test_write_read_roundtrip(self, populated_database):
        """Test complete write-read roundtrip."""
        # Write database
        writer = MessagePackWriter("test_bases")
        writer.write_database(populated_database, "roundtrip_test")

        # Read database back
        reader = MessagePackReader("test_bases")
        loaded_db = reader.load_database("roundtrip_test")

        # Verify data integrity
        assert loaded_db is not None
        assert loaded_db.persons_count == populated_database.persons_count
        assert loaded_db.families_count == populated_database.families_count
        assert loaded_db.strings_count == populated_database.strings_count

        # Verify search functionality
        original_results = populated_database.search_persons_by_surname("Smith")
        loaded_results = reader.search_persons_by_surname("roundtrip_test", "Smith")

        assert len(loaded_results) == len(original_results)

    def test_large_database_io(self, large_database):
        """Test I/O with large database."""
        # Write large database
        writer = MessagePackWriter("test_bases")
        writer.write_database(large_database, "large_test")

        # Read it back
        reader = MessagePackReader("test_bases")
        loaded_db = reader.load_database("large_test")

        # Verify
        assert loaded_db is not None
        assert loaded_db.persons_count == large_database.persons_count

        # Test search performance
        results = reader.search_persons_by_surname("large_test", "Surname1")
        assert len(results) >= 10  # Should find multiple persons with Surname1
