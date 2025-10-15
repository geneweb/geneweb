"""
Tests for db_pickle I/O operations.
"""

import pytest
import os
import gzip
import pickle
from pathlib import Path

from lib.db_pickle.io.writer import PickleWriter
from lib.db_pickle.io.reader import PickleReader
from lib.db_pickle.core.enums import Sex


class TestPickleWriter:
    """Test PickleWriter class."""

    def test_save_database_uncompressed(self, populated_database, temp_file):
        """Test saving database without compression."""
        writer = PickleWriter()
        db = populated_database

        writer.save_database(db, temp_file)

        # Check file exists
        assert os.path.exists(temp_file)

        # Check file is not compressed
        with open(temp_file, 'rb') as f:
            # Try to read as gzip, should fail
            try:
                gzip.decompress(f.read())
                assert False, "File should not be compressed"
            except (OSError, gzip.BadGzipFile):
                pass  # Expected

    def test_save_database_compressed(self, populated_database, temp_compressed_file):
        """Test saving database with compression."""
        writer = PickleWriter()
        db = populated_database

        writer.save_database(db, temp_compressed_file, compress=True)

        # Check file exists
        assert os.path.exists(temp_compressed_file)

        # Check file is compressed
        with open(temp_compressed_file, 'rb') as f:
            try:
                gzip.decompress(f.read())
                # If we get here, file is compressed
                pass
            except (OSError, gzip.BadGzipFile):
                assert False, "File should be compressed"

    def test_save_empty_database(self, empty_database, temp_file):
        """Test saving empty database."""
        writer = PickleWriter()
        db = empty_database

        writer.save_database(db, temp_file)

        assert os.path.exists(temp_file)

        # Load and verify it's empty
        reader = PickleReader()
        loaded_db = reader.load_database(temp_file)

        assert len(loaded_db.persons) == 0
        assert len(loaded_db.families) == 0
        assert len(loaded_db.couples) == 0
        assert len(loaded_db.descends) == 0

    def test_save_database_with_strings(self, empty_database, temp_file):
        """Test saving database with strings."""
        writer = PickleWriter()
        db = empty_database

        # Add some strings
        db.strings[1] = "New York"
        db.strings[2] = "London"
        db.strings[3] = "Paris"

        writer.save_database(db, temp_file)

        # Load and verify strings
        reader = PickleReader()
        loaded_db = reader.load_database(temp_file)

        assert len(loaded_db.strings) == 3
        assert loaded_db.strings[1] == "New York"
        assert loaded_db.strings[2] == "London"
        assert loaded_db.strings[3] == "Paris"

    def test_save_database_creates_directory(self, populated_database, temp_file):
        """Test that save_database creates parent directories."""
        writer = PickleWriter()
        db = populated_database

        # Create a path with non-existent directory
        temp_dir = Path(temp_file).parent / "test_subdir"
        temp_file_with_dir = temp_dir / "test.pkl"

        try:
            writer.save_database(db, str(temp_file_with_dir))

            # Check directory was created
            assert temp_dir.exists()
            assert temp_file_with_dir.exists()

        finally:
            # Cleanup
            if temp_file_with_dir.exists():
                temp_file_with_dir.unlink()
            if temp_dir.exists():
                for file in temp_dir.iterdir():
                    file.unlink()
                temp_dir.rmdir()


class TestPickleReader:
    """Test PickleReader class."""

    def test_load_database_uncompressed(self, populated_database, temp_file):
        """Test loading uncompressed database."""
        writer = PickleWriter()
        reader = PickleReader()
        db = populated_database

        # Save database
        writer.save_database(db, temp_file)

        # Load database
        loaded_db = reader.load_database(temp_file)

        # Verify data integrity
        assert len(loaded_db.persons) == len(db.persons)
        assert len(loaded_db.families) == len(db.families)
        assert len(loaded_db.couples) == len(db.couples)
        assert len(loaded_db.descends) == len(db.descends)

        # Verify specific data
        assert loaded_db.persons[1].first_name == "John"
        assert loaded_db.persons[1].surname == "Smith"
        assert loaded_db.persons[1].sex == Sex.MALE

    def test_load_database_compressed(self, populated_database, temp_compressed_file):
        """Test loading compressed database."""
        writer = PickleWriter()
        reader = PickleReader()
        db = populated_database

        # Save compressed database
        writer.save_database(db, temp_compressed_file, compress=True)

        # Load database
        loaded_db = reader.load_database(temp_compressed_file)

        # Verify data integrity
        assert len(loaded_db.persons) == len(db.persons)
        assert len(loaded_db.families) == len(db.families)
        assert len(loaded_db.couples) == len(db.couples)
        assert len(loaded_db.descends) == len(db.descends)

    def test_is_compressed(self, populated_database, temp_file, temp_compressed_file):
        """Test is_compressed method."""
        writer = PickleWriter()
        reader = PickleReader()
        db = populated_database

        # Save uncompressed
        writer.save_database(db, temp_file)
        assert not reader.is_compressed(temp_file)

        # Save compressed
        writer.save_database(db, temp_compressed_file, compress=True)
        assert reader.is_compressed(temp_compressed_file)

    def test_load_nonexistent_file(self):
        """Test loading non-existent file raises appropriate error."""
        reader = PickleReader()

        with pytest.raises(FileNotFoundError):
            reader.load_database("nonexistent_file.pkl")

    def test_load_corrupted_file(self, temp_file):
        """Test loading corrupted file."""
        reader = PickleReader()

        # Create a corrupted file
        with open(temp_file, 'w') as f:
            f.write("This is not a valid pickle file")

        with pytest.raises((pickle.UnpicklingError, EOFError)):
            reader.load_database(temp_file)

    def test_load_database_preserves_indexes(self, populated_database, temp_file):
        """Test that loaded database preserves search indexes."""
        writer = PickleWriter()
        reader = PickleReader()
        db = populated_database

        # Save database
        writer.save_database(db, temp_file)

        # Load database
        loaded_db = reader.load_database(temp_file)

        # Verify indexes are preserved
        assert hasattr(loaded_db, 'first_name_index')
        assert hasattr(loaded_db, 'surname_index')
        assert hasattr(loaded_db, 'full_name_index')
        assert hasattr(loaded_db, 'string_content_index')

        # Test that search still works
        results = loaded_db.search_persons_by_first_name("John")
        assert len(results) == 1
        assert results[0] == 1

    def test_load_database_with_large_dataset(self, large_database, temp_file):
        """Test loading database with large dataset."""
        writer = PickleWriter()
        reader = PickleReader()
        db = large_database

        # Save database
        writer.save_database(db, temp_file)

        # Load database
        loaded_db = reader.load_database(temp_file)

        # Verify data integrity
        assert len(loaded_db.persons) == 100
        assert len(loaded_db.families) == 0  # No families in large_database fixture

        # Test search functionality
        results = loaded_db.search_persons_by_surname("Surname0")
        assert len(results) == 10  # 10 persons with Surname0 (100/10 = 10)


class TestIOIntegration:
    """Integration tests for I/O operations."""

    def test_save_and_load_roundtrip(self, populated_database, temp_file):
        """Test complete save and load roundtrip."""
        writer = PickleWriter()
        reader = PickleReader()
        original_db = populated_database

        # Save database
        writer.save_database(original_db, temp_file)

        # Load database
        loaded_db = reader.load_database(temp_file)

        # Verify complete data integrity
        assert loaded_db.persons == original_db.persons
        assert loaded_db.families == original_db.families
        assert loaded_db.couples == original_db.couples
        assert loaded_db.descends == original_db.descends
        assert loaded_db.strings == original_db.strings

    def test_compressed_vs_uncompressed_size(self, populated_database, temp_file, temp_compressed_file):
        """Test that compressed files are smaller than uncompressed."""
        writer = PickleWriter()

        # Save uncompressed
        writer.save_database(populated_database, temp_file)
        uncompressed_size = os.path.getsize(temp_file)

        # Save compressed
        writer.save_database(populated_database, temp_compressed_file, compress=True)
        compressed_size = os.path.getsize(temp_compressed_file)

        # Compressed should be smaller (or equal for very small files)
        assert compressed_size <= uncompressed_size

    def test_multiple_save_load_cycles(self, populated_database, temp_file):
        """Test multiple save/load cycles don't corrupt data."""
        writer = PickleWriter()
        reader = PickleReader()
        db = populated_database

        # Perform multiple save/load cycles
        for i in range(3):
            writer.save_database(db, temp_file)
            db = reader.load_database(temp_file)

        # Verify data is still intact
        assert len(db.persons) == 4
        assert db.persons[1].first_name == "John"
        assert db.persons[1].surname == "Smith"
