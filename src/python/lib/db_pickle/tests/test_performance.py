"""
Performance tests for db_pickle module.
"""

import time
from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.core.enums import Sex
from lib.db_pickle.models.events import Date


class TestPerformance:
    """Performance tests for db_pickle operations."""

    def test_large_dataset_creation_performance(self):
        """Test performance of creating large dataset."""
        db = PickleBaseData()

        # Measure time to create 1000 persons
        start_time = time.time()

        for i in range(1000):
            db.persons[i] = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i % 50}",  # 50 different surnames
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=1900 + (i % 100), month=(i % 12) + 1, day=(i % 28) + 1),
            )

        end_time = time.time()
        creation_time = end_time - start_time

        # Should create 1000 persons in reasonable time (less than 5 seconds)
        assert creation_time < 5.0
        assert len(db.persons) == 1000

    def test_index_building_performance(self, large_database):
        """Test performance of building search indexes."""
        db = large_database

        # Clear indexes
        db.first_name_index.clear()
        db.surname_index.clear()
        db.full_name_index.clear()
        db.string_content_index.clear()

        # Measure time to build indexes
        start_time = time.time()
        db.build_indexes()
        end_time = time.time()

        index_building_time = end_time - start_time

        # Should build indexes quickly (less than 2 seconds for 100 persons)
        assert index_building_time < 2.0

    def test_search_performance_with_large_dataset(self):
        """Test search performance with large dataset."""
        db = PickleBaseData()

        # Create large dataset
        for i in range(1000):
            db.persons[i] = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i % 50}",
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=1900 + (i % 100), month=(i % 12) + 1, day=(i % 28) + 1),
            )

        db.build_indexes()

        # Test search performance
        start_time = time.time()

        # Perform multiple searches
        for _ in range(100):
            db.search_persons_by_surname("Surname0")
            db.search_persons_by_first_name("Person0")
            db.search_persons_by_full_name("Person0 Surname0")

        end_time = time.time()
        search_time = end_time - start_time

        # 300 searches should complete quickly (less than 1 second)
        assert search_time < 1.0

    def test_memory_usage_with_large_dataset(self):
        """Test memory usage with large dataset."""
        import sys

        # Measure memory before
        initial_memory = sys.getsizeof(PickleBaseData())

        db = PickleBaseData()

        # Create large dataset
        for i in range(1000):
            db.persons[i] = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i % 50}",
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=1900 + (i % 100), month=(i % 12) + 1, day=(i % 28) + 1),
            )

        db.build_indexes()

        # Measure memory after
        final_memory = sys.getsizeof(db)
        memory_increase = final_memory - initial_memory

        # Memory increase should be reasonable (less than 10MB for 1000 persons)
        # Note: This is a rough estimate and may vary
        assert memory_increase < 10 * 1024 * 1024  # 10MB

    def test_concurrent_search_performance(self, large_database):
        """Test performance of concurrent searches."""
        db = large_database

        # Measure time for multiple concurrent searches
        start_time = time.time()

        # Simulate concurrent searches
        search_operations = []
        for i in range(10):
            search_operations.extend(
                [
                    db.search_persons_by_surname(f"Surname{i}"),
                    db.search_persons_by_first_name(f"Person{i}"),
                    db.search_persons_by_full_name(f"Person{i} Surname{i}"),
                ]
            )

        end_time = time.time()
        total_time = end_time - start_time

        # 30 search operations should complete quickly (less than 0.5 seconds)
        assert total_time < 0.5
        assert len(search_operations) == 30

    def test_database_copy_performance(self, large_database):
        """Test performance of database copying."""
        db = large_database

        # Measure time to copy database
        start_time = time.time()
        db_copy = db.copy()
        end_time = time.time()

        copy_time = end_time - start_time

        # Copying should be fast (less than 1 second for 100 persons)
        assert copy_time < 1.0
        assert len(db_copy.persons) == len(db.persons)
        assert db_copy is not db

    def test_database_validation_performance(self, large_database):
        """Test performance of database validation."""
        db = large_database

        # Measure time to validate database
        start_time = time.time()
        errors = db.validate()
        end_time = time.time()

        validation_time = end_time - start_time

        # Validation should be fast (less than 1 second for 100 persons)
        assert validation_time < 1.0
        assert len(errors) == 0  # Should be valid

    def test_statistics_calculation_performance(self, large_database):
        """Test performance of statistics calculation."""
        db = large_database

        # Measure time to calculate statistics
        start_time = time.time()
        stats = db.get_statistics()
        end_time = time.time()

        stats_time = end_time - start_time

        # Statistics calculation should be fast (less than 0.1 seconds)
        assert stats_time < 0.1
        assert stats["persons"] == 100

    def test_string_operations_performance(self):
        """Test performance of string operations."""
        db = PickleBaseData()

        # Add many strings
        for i in range(1000):
            db.strings[i] = f"String{i} with some content {i % 100}"

        db.build_indexes()

        # Measure search performance
        start_time = time.time()

        # Perform multiple string searches
        for i in range(100):
            db.search_strings_by_content(f"String{i}")
            db.search_strings_by_content(f"content {i % 100}")

        end_time = time.time()
        search_time = end_time - start_time

        # 200 string searches should complete quickly (less than 0.5 seconds)
        assert search_time < 0.5

    def test_mixed_operations_performance(self, large_database):
        """Test performance of mixed operations."""
        db = large_database

        # Measure time for mixed operations
        start_time = time.time()

        # Mix of different operations
        for i in range(50):
            # Search operations
            db.search_persons_by_surname(f"Surname{i % 10}")
            db.search_persons_by_first_name(f"Person{i % 10}")

            # Add new person
            db.persons[100 + i] = GenPerson(
                first_name=f"NewPerson{i}",
                surname=f"NewSurname{i}",
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=2000 + i, month=1, day=1),
            )

            # Add string
            db.strings[i] = f"NewString{i}"

        # Rebuild indexes
        db.build_indexes()

        # More searches
        for i in range(25):
            db.search_persons_by_full_name(f"NewPerson{i}")
            db.search_strings_by_content(f"NewString{i}")

        end_time = time.time()
        total_time = end_time - start_time

        # Mixed operations should complete in reasonable time (less than 2 seconds)
        assert total_time < 2.0
        assert (
            len(db.persons) == 149
        )  # 100 + 49 new (starts from index 1, adds 100-149)
        assert len(db.strings) == 50
