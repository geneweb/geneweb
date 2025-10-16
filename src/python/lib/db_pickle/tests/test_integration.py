"""
Integration tests for db_pickle module.
"""

import tempfile
import os

from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.models.family import GenFamily
from lib.db_pickle.models.relations import GenCouple, GenDescend
from lib.db_pickle.core.enums import Sex, RelationKind
from lib.db_pickle.models.events import Date
from lib.db_pickle.io.writer import PickleWriter
from lib.db_pickle.io.reader import PickleReader


class TestDatabaseIntegration:
    """Integration tests for complete database workflows."""

    def test_complete_genealogical_workflow(self):
        """Test complete genealogical data workflow."""
        # Create database
        db = PickleBaseData()

        # Add grandparents
        db.persons[1] = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1920, month=1, day=1),
            death=Date(year=2000, month=12, day=31),
        )

        db.persons[2] = GenPerson(
            first_name="Mary",
            surname="Smith",
            sex=Sex.FEMALE,
            birth=Date(year=1925, month=3, day=15),
            death=Date(year=2005, month=6, day=20),
        )

        # Add parents
        db.persons[3] = GenPerson(
            first_name="Robert",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1950, month=7, day=10),
            death=Date(year=2020, month=3, day=5),
        )

        db.persons[4] = GenPerson(
            first_name="Sarah",
            surname="Johnson",
            sex=Sex.FEMALE,
            birth=Date(year=1955, month=9, day=25),
        )

        # Add children
        db.persons[5] = GenPerson(
            first_name="Michael",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=4, day=12),
        )

        db.persons[6] = GenPerson(
            first_name="Emily",
            surname="Smith",
            sex=Sex.FEMALE,
            birth=Date(year=1982, month=8, day=30),
        )

        # Add grandchildren
        db.persons[7] = GenPerson(
            first_name="James",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=2010, month=11, day=15),
        )

        db.persons[8] = GenPerson(
            first_name="Emma",
            surname="Smith",
            sex=Sex.FEMALE,
            birth=Date(year=2012, month=2, day=28),
        )

        # Create families
        # Grandparents' family
        db.families[1] = GenFamily(relation=RelationKind.MARRIED)
        db.couples[1] = GenCouple(father=1, mother=2)
        db.descends[1] = GenDescend(children=[3])

        # Parents' family
        db.families[2] = GenFamily(relation=RelationKind.MARRIED)
        db.couples[2] = GenCouple(father=3, mother=4)
        db.descends[2] = GenDescend(children=[5, 6])

        # Children's family
        db.families[3] = GenFamily(relation=RelationKind.MARRIED)
        db.couples[3] = GenCouple(father=5, mother=9)  # Mother not in database
        db.descends[3] = GenDescend(children=[7, 8])

        # Add some strings (places)
        db.strings[1] = "New York"
        db.strings[2] = "California"
        db.strings[3] = "Texas"

        # Build indexes
        db.build_indexes()

        # Test searches
        smiths = db.search_persons_by_surname("Smith")
        assert len(smiths) == 7  # All Smith family members

        johns = db.search_persons_by_first_name("John")
        assert len(johns) == 1
        assert johns[0] == 1

        # Test family relationships
        assert db.couples[1].father == 1
        assert db.couples[1].mother == 2
        assert db.descends[1].children == [3]

        # Test validation
        errors = db.validate()
        assert len(errors) > 0  # Should have errors due to missing mother in family 3

        # Test statistics
        stats = db.get_statistics()
        assert stats["persons"] == 8
        assert stats["families"] == 3
        assert stats["couples"] == 3
        assert stats["descends"] == 3
        assert stats["strings"] == 3

    def test_save_load_complete_workflow(self, populated_database):
        """Test complete save/load workflow."""
        with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl") as f:
            temp_file = f.name

        try:
            writer = PickleWriter()
            reader = PickleReader()

            # Save database
            writer.save_database(populated_database, temp_file)

            # Load database
            loaded_db = reader.load_database(temp_file)

            # Verify complete data integrity
            assert loaded_db.persons == populated_database.persons
            assert loaded_db.families == populated_database.families
            assert loaded_db.couples == populated_database.couples
            assert loaded_db.descends == populated_database.descends
            assert loaded_db.strings == populated_database.strings

            # Verify search functionality still works
            results = loaded_db.search_persons_by_first_name("John")
            assert len(results) == 1
            assert results[0] == 1

            # Verify indexes are preserved
            assert hasattr(loaded_db, "first_name_index")
            assert hasattr(loaded_db, "surname_index")
            assert hasattr(loaded_db, "full_name_index")
            assert hasattr(loaded_db, "string_content_index")

        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)

    def test_compressed_save_load_workflow(self, populated_database):
        """Test compressed save/load workflow."""
        with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl.gz") as f:
            temp_file = f.name

        try:
            writer = PickleWriter()
            reader = PickleReader()

            # Save compressed database
            writer.save_database(populated_database, temp_file, compress=True)

            # Verify file is compressed
            assert reader.is_compressed(temp_file)

            # Load database
            loaded_db = reader.load_database(temp_file)

            # Verify data integrity
            assert loaded_db.persons == populated_database.persons
            assert loaded_db.families == populated_database.families
            assert loaded_db.couples == populated_database.couples
            assert loaded_db.descends == populated_database.descends

        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)

    def test_large_dataset_workflow(self):
        """Test workflow with large dataset."""
        db = PickleBaseData()

        # Create large dataset
        for i in range(500):
            db.persons[i] = GenPerson(
                first_name=f"Person{i}",
                surname=f"Surname{i % 25}",  # 25 different surnames
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=1900 + (i % 100), month=(i % 12) + 1, day=(i % 28) + 1),
            )

        # Add some families
        for i in range(100):
            family_id = i + 1
            father = i * 2
            mother = i * 2 + 1

            if father < 500 and mother < 500:
                db.families[family_id] = GenFamily(relation=RelationKind.MARRIED)
                db.couples[family_id] = GenCouple(father=father, mother=mother)
                db.descends[family_id] = GenDescend(
                    children=[father + 100, mother + 100]
                )

        # Add strings
        for i in range(100):
            db.strings[i] = f"Place{i} in Country{i % 10}"

        # Build indexes
        db.build_indexes()

        # Test searches
        smiths = db.search_persons_by_surname("Surname0")
        assert len(smiths) == 20  # 500/25 = 20 persons per surname

        # Test save/load
        with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl") as f:
            temp_file = f.name

        try:
            writer = PickleWriter()
            reader = PickleReader()

            # Save
            writer.save_database(db, temp_file)

            # Load
            loaded_db = reader.load_database(temp_file)

            # Verify
            assert len(loaded_db.persons) == 500
            assert len(loaded_db.families) == 100
            assert len(loaded_db.strings) == 100

            # Test search still works
            smiths = loaded_db.search_persons_by_surname("Surname0")
            assert len(smiths) == 20

        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)

    def test_error_handling_workflow(self):
        """Test error handling in complete workflow."""
        db = PickleBaseData()

        # Add invalid data
        db.persons[1] = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=1, day=1),
        )

        # Add family with invalid references
        db.families[1] = GenFamily(relation=RelationKind.MARRIED)
        db.couples[1] = GenCouple(father=1, mother=999)  # Invalid mother
        db.descends[1] = GenDescend(children=[888, 777])  # Invalid children

        # Test validation catches errors
        errors = db.validate()
        assert len(errors) > 0
        assert any("999" in error for error in errors)
        assert any("888" in error for error in errors)
        assert any("777" in error for error in errors)

        # Test save/load still works despite errors
        with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl") as f:
            temp_file = f.name

        try:
            writer = PickleWriter()
            reader = PickleReader()

            # Save should work even with invalid data
            writer.save_database(db, temp_file)

            # Load should work
            loaded_db = reader.load_database(temp_file)

            # Validation errors should persist
            loaded_errors = loaded_db.validate()
            assert len(loaded_errors) > 0

        finally:
            if os.path.exists(temp_file):
                os.unlink(temp_file)

    def test_multiple_database_instances(self):
        """Test working with multiple database instances."""
        # Create two separate databases
        db1 = PickleBaseData()
        db2 = PickleBaseData()

        # Add different data to each
        db1.persons[1] = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=1, day=1),
        )

        db2.persons[1] = GenPerson(
            first_name="Jane",
            surname="Doe",
            sex=Sex.FEMALE,
            birth=Date(year=1985, month=6, day=15),
        )

        # Build indexes for both
        db1.build_indexes()
        db2.build_indexes()

        # Test searches work independently
        johns = db1.search_persons_by_first_name("John")
        janes = db2.search_persons_by_first_name("Jane")

        assert len(johns) == 1
        assert len(janes) == 1
        assert johns[0] == 1
        assert janes[0] == 1

        # Test that databases are independent
        assert len(db1.persons) == 1
        assert len(db2.persons) == 1
        assert db1.persons[1].first_name == "John"
        assert db2.persons[1].first_name == "Jane"

    def test_database_copy_and_modify_workflow(self, populated_database):
        """Test copying database and modifying copy."""
        # Copy database
        db_copy = populated_database.copy()

        # Verify initial state
        assert len(db_copy.persons) == 4
        assert len(populated_database.persons) == 4

        # Modify copy
        db_copy.persons[5] = GenPerson(
            first_name="New",
            surname="Person",
            sex=Sex.MALE,
            birth=Date(year=2000, month=1, day=1),
        )

        # Verify original is unchanged
        assert len(populated_database.persons) == 4
        assert 5 not in populated_database.persons

        # Verify copy is modified
        assert len(db_copy.persons) == 5
        assert 5 in db_copy.persons
        assert db_copy.persons[5].first_name == "New"

        # Rebuild indexes for copy
        db_copy.build_indexes()

        # Test search in copy
        new_persons = db_copy.search_persons_by_first_name("New")
        assert len(new_persons) == 1
        assert new_persons[0] == 5

        # Test search in original (should not find "New")
        new_persons_original = populated_database.search_persons_by_first_name("New")
        assert len(new_persons_original) == 0
