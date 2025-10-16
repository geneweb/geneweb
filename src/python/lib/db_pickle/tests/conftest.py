"""
Pytest configuration and fixtures for db_pickle tests.
"""

import pytest
import tempfile
import os

from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.models.family import GenFamily
from lib.db_pickle.models.relations import GenCouple, GenDescend
from lib.db_pickle.core.enums import Sex, RelationKind
from lib.db_pickle.models.events import Date


@pytest.fixture
def sample_person_data():
    """Sample person data for testing."""
    return {
        "first_name": "John",
        "surname": "Smith",
        "sex": Sex.MALE,
        "birth": Date(year=1980, month=3, day=15),
        "death": None,
        "burial": None,
    }


@pytest.fixture
def sample_family_data():
    """Sample family data for testing."""
    return {
        "relation": RelationKind.MARRIED,
        "father": 1,
        "mother": 2,
        "children": [3, 4],
    }


@pytest.fixture
def empty_database():
    """Create an empty database for testing."""
    return PickleBaseData()


@pytest.fixture
def populated_database(sample_person_data, sample_family_data):
    """Create a database with sample data."""
    db = PickleBaseData()

    # Add persons
    db.persons[1] = GenPerson(**sample_person_data)
    db.persons[2] = GenPerson(
        first_name="Jane",
        surname="Smith",
        sex=Sex.FEMALE,
        birth=Date(year=1982, month=5, day=20),
    )
    db.persons[3] = GenPerson(
        first_name="Alice",
        surname="Smith",
        sex=Sex.FEMALE,
        birth=Date(year=2010, month=8, day=10),
    )
    db.persons[4] = GenPerson(
        first_name="Bob",
        surname="Smith",
        sex=Sex.MALE,
        birth=Date(year=2012, month=12, day=25),
    )

    # Add family
    db.families[1] = GenFamily(relation=sample_family_data["relation"])
    db.couples[1] = GenCouple(
        father=sample_family_data["father"], mother=sample_family_data["mother"]
    )
    db.descends[1] = GenDescend(children=sample_family_data["children"])

    # Build indexes
    db.build_indexes()

    return db


@pytest.fixture
def temp_file():
    """Create a temporary file for testing I/O operations."""
    with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl") as f:
        temp_path = f.name
    yield temp_path
    # Cleanup
    if os.path.exists(temp_path):
        os.unlink(temp_path)


@pytest.fixture
def temp_compressed_file():
    """Create a temporary compressed file for testing I/O operations."""
    with tempfile.NamedTemporaryFile(delete=False, suffix=".pkl.gz") as f:
        temp_path = f.name
    yield temp_path
    # Cleanup
    if os.path.exists(temp_path):
        os.unlink(temp_path)


@pytest.fixture
def large_database():
    """Create a database with many persons for performance testing."""
    db = PickleBaseData()

    # Add 100 persons
    for i in range(1, 101):
        db.persons[i] = GenPerson(
            first_name=f"Person{i}",
            surname=f"Surname{i % 10}",  # 10 different surnames
            sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
            birth=Date(year=1900 + (i % 100), month=(i % 12) + 1, day=(i % 28) + 1),
        )

    # Build indexes
    db.build_indexes()

    return db
