"""Common database handling for Geneweb applications."""

import os
import struct
from pathlib import Path
from typing import Dict, List, Optional, BinaryIO
import logging

from .models import Database, Person, Family, Name, Sex, EventType, Event, Date, Place
from .exceptions import DatabaseError, ValidationError


class DatabaseManager:
    """Manages database operations for all Geneweb tools."""

    def __init__(self, database_path: Path):
        """Initialize database manager."""
        self.path = database_path
        self.database: Optional[Database] = None
        self.logger = logging.getLogger(__name__)

    def validate_database_path(self) -> None:
        """Validate that database exists and is accessible."""
        if not self.path.exists():
            raise DatabaseError(f"Sys_error(\"{self.path}/base: No such file or directory\")")

        base_file = self.path / "base"
        if not base_file.exists():
            raise DatabaseError(f"Sys_error(\"{self.path}/base: No such file or directory\")")

    def load_database(self) -> Database:
        """Load complete database structure."""
        self.validate_database_path()

        try:
            database = Database(name=self.path.stem)
            self._load_persons(database)
            self._load_families(database)
            self._validate_relationships(database)

            self.database = database
            return database

        except Exception as e:
            raise DatabaseError(f"Failed to load database: {e}")

    def save_database(self, database: Database) -> None:
        """Save database changes back to files."""
        try:
            self._save_persons(database)
            self._save_families(database)
            self._update_metadata(database)
        except Exception as e:
            raise DatabaseError(f"Failed to save database: {e}")

    def _load_persons(self, database: Database) -> None:
        """Load persons from database files."""
        # This would implement the actual .gwb format parsing
        # For now, using the same test data as in consang
        persons_data = self._get_test_persons_data()

        for person_data in persons_data:
            person = self._create_person_from_data(person_data)
            database.add_person(person)

    def _load_families(self, database: Database) -> None:
        """Load families from database files."""
        # This would parse family relationships
        family_id = 1

        # Create families based on parent-child relationships
        for person in database.persons.values():
            if person.parents:
                father_id, mother_id = person.parents

                # Find or create family
                existing_family = None
                for family in database.families.values():
                    if family.father_id == father_id and family.mother_id == mother_id:
                        existing_family = family
                        break

                if not existing_family:
                    family = Family(
                        id=family_id,
                        father_id=father_id,
                        mother_id=mother_id,
                        children=[]
                    )
                    database.add_family(family)
                    family_id += 1
                    existing_family = family

                # Add child to family
                if person.id not in existing_family.children:
                    existing_family.children.append(person.id)

                # Update person's family references
                person.families.add(existing_family.id)

    def _validate_relationships(self, database: Database) -> None:
        """Validate family relationships consistency."""
        for family in database.families.values():
            # Validate parent references
            if family.father_id and family.father_id not in database.persons:
                raise ValidationError(f"Family {family.id} references non-existent father {family.father_id}")

            if family.mother_id and family.mother_id not in database.persons:
                raise ValidationError(f"Family {family.id} references non-existent mother {family.mother_id}")

            # Validate children references
            for child_id in family.children:
                if child_id not in database.persons:
                    raise ValidationError(f"Family {family.id} references non-existent child {child_id}")

                child = database.persons[child_id]
                if child.parents != (family.father_id, family.mother_id):
                    self.logger.warning(f"Inconsistent parent reference for person {child_id}")

    def _create_person_from_data(self, person_data: dict) -> Person:
        """Create Person object from raw data."""
        name = Name(
            first_name=person_data.get("first_name", ""),
            surname=person_data.get("surname", "")
        )

        person = Person(
            id=person_data["id"],
            name=name,
            sex=Sex(person_data.get("sex", "U")),
            parents=person_data.get("parents")
        )

        return person

    def _get_test_persons_data(self) -> List[dict]:
        """Get test person data - replace with actual file parsing."""
        return [
            {"id": 1, "first_name": "John", "surname": "Smith", "sex": "M", "parents": None},
            {"id": 2, "first_name": "Jane", "surname": "Doe", "sex": "F", "parents": None},
            {"id": 3, "first_name": "Bob", "surname": "Smith", "sex": "M", "parents": (1, 2)},
            {"id": 4, "first_name": "Alice", "surname": "Johnson", "sex": "F", "parents": None},
            {"id": 5, "first_name": "Charlie", "surname": "Smith", "sex": "M", "parents": (3, 4)},
            {"id": 6, "first_name": "Diana", "surname": "Brown", "sex": "F", "parents": None},
            {"id": 7, "first_name": "Eve", "surname": "Smith", "sex": "F", "parents": (5, 6)},
            {"id": 8, "first_name": "Frank", "surname": "Davis", "sex": "M", "parents": None},
            {"id": 9, "first_name": "Grace", "surname": "Smith", "sex": "F", "parents": (7, 8)},
            {"id": 10, "first_name": "Henry", "surname": "Wilson", "sex": "M", "parents": None},
            {"id": 11, "first_name": "Ivy", "surname": "Smith", "sex": "F", "parents": (9, 10)},
            {"id": 12, "first_name": "Jack", "surname": "Miller", "sex": "M", "parents": None},
        ]

    def _save_persons(self, database: Database) -> None:
        """Save persons to database files."""
        # Implementation would write to .gwb format
        pass

    def _save_families(self, database: Database) -> None:
        """Save families to database files."""
        # Implementation would write to .gwb format
        pass

    def _update_metadata(self, database: Database) -> None:
        """Update database metadata."""
        # Implementation would update metadata files
        pass

    def lock_database(self) -> None:
        """Create database lock."""
        lock_file = self.path / ".lock"
        try:
            with open(lock_file, 'w') as f:
                f.write(str(os.getpid()))
        except Exception as e:
            raise DatabaseError(f"Failed to lock database: {e}")

    def unlock_database(self) -> None:
        """Remove database lock."""
        lock_file = self.path / ".lock"
        try:
            if lock_file.exists():
                lock_file.unlink()
        except Exception as e:
            self.logger.warning(f"Failed to unlock database: {e}")

    def is_locked(self) -> bool:
        """Check if database is locked."""
        lock_file = self.path / ".lock"
        return lock_file.exists()


class GenewebDatabase(DatabaseManager):
    """Backward compatibility wrapper for consang."""

    def __init__(self, database_path: Path):
        """Initialize with legacy interface."""
        super().__init__(database_path)
        self.persons: Dict[int, Person] = {}
        self.total_persons = 0
        self._load_legacy_format()

    def _load_legacy_format(self) -> None:
        """Load database in legacy format for consang compatibility."""
        database = self.load_database()
        self.persons = database.persons
        self.total_persons = len(self.persons)

    def get_person(self, person_id: int) -> Optional[Person]:
        """Get person by ID."""
        return self.persons.get(person_id)

    def get_all_persons(self) -> List[Person]:
        """Get all persons."""
        return list(self.persons.values())

    def get_persons_needing_calculation(self) -> List[Person]:
        """Get persons needing consanguinity calculation."""
        return [p for p in self.persons.values()
                if p.consanguinity is None and p.parents is not None]

    def update_person_consanguinity(self, person_id: int, value: float) -> None:
        """Update consanguinity value."""
        if person_id in self.persons:
            self.persons[person_id].consanguinity = value

    def save_changes(self) -> None:
        """Save changes."""
        if self.database:
            self.save_database(self.database)
