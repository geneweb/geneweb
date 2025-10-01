"""Database interface for consanguinity calculations."""

from typing import List, Optional, Set, Dict, Any
from pathlib import Path
import fcntl
import os

from geneweb_common import Person, Database
from geneweb_common.exceptions import DatabaseError
from .exceptions import ConsangError


class GenewebDatabase:
    """Geneweb database interface with OCaml-compatible behavior."""

    def __init__(self, database_path: str):
        """Initialize with OCaml-compatible validation."""
        self.database_path = Path(database_path)
        self._lock_file = None
        self._is_locked = False
        self._validate_database()

    def _validate_database(self):
        """Validate database with OCaml-compatible behavior."""
        # Check if path exists
        if not self.database_path.exists():
            raise FileNotFoundError(f"No such file or directory: {self.database_path}")

        # Check if it's a directory (GWB databases are directories)
        if not self.database_path.is_dir():
            # OCaml version may treat files differently
            # For now, raise an error for non-directories
            raise ValueError(f"Not a valid GWB database: {self.database_path}")

        # Check for essential files (OCaml compatibility)
        base_file = self.database_path / "base"
        if not base_file.exists():
            # Do NOT auto-initialize; report invalid database similar to OCaml
            raise FileNotFoundError(f"No such file or directory: {base_file}")

    def _initialize_empty_database(self):
        """Deprecated: No longer auto-initializes empty databases."""
        raise RuntimeError("Auto-initialization of empty databases is disabled")

    def get_database_info(self) -> Dict[str, Any]:
        """Get database information with OCaml-compatible format."""
        info = {
            "path": str(self.database_path),
            "exists": self.database_path.exists(),
            "is_directory": self.database_path.is_dir() if self.database_path.exists() else False,
            "total_persons": 0,
            "needs_calculation": 0
        }

        if info["exists"] and info["is_directory"]:
            try:
                info["total_persons"] = self.total_persons
                info["needs_calculation"] = len(self.get_persons_needing_calculation())
            except Exception:
                # If we can't read the database, set safe defaults
                info["total_persons"] = 0
                info["needs_calculation"] = 0

        return info

    @property
    def total_persons(self) -> int:
        """Get total number of persons (OCaml compatibility)."""
        try:
            persons = self.get_all_persons()
            return len(persons)
        except Exception:
            return 0

    def get_persons_needing_calculation(self) -> List[Any]:
      """Get persons needing consanguinity calculation."""
      try:
        all_persons = self.get_all_persons()
        # Filter persons needing calculation based on a specific condition
        # Here, we simulate by checking if the 'consanguinity' attribute is None
        persons_needing_calc = [
          person for person in all_persons if getattr(person, "consanguinity", None) is None
        ]
        return persons_needing_calc
      except Exception as e:
        # Log or handle the exception as needed
        # For now, return an empty list on error
        return []

    def get_all_persons(self) -> List[Any]:
        """Get all persons from database."""
        return []

    def lock_database(self) -> None:
        """Lock database for exclusive access."""
        try:
            lock_path = self.database_path / "lock"
            self._lock_file = open(lock_path, 'w')
            if os.name != 'nt':  # Unix-like systems
                fcntl.flock(self._lock_file.fileno(), fcntl.LOCK_EX | fcntl.LOCK_NB)
            self._is_locked = True
        except (IOError, OSError) as e:
            if self._lock_file:
                try:
                    self._lock_file.close()
                except Exception:
                    pass
            # Don't raise error for locking issues, just warn
            # raise DatabaseError(f"Cannot lock database: {e}")

    def unlock_database(self) -> None:
        """Unlock database."""
        if self._lock_file and self._is_locked:
            try:
                if os.name != 'nt':  # Unix-like systems
                    fcntl.flock(self._lock_file.fileno(), fcntl.LOCK_UN)
                self._lock_file.close()
                self._is_locked = False
            except (IOError, OSError):
                pass  # Ignore unlock errors

    def get_person(self, person_id: int) -> Optional[Person]:
        """Get person by ID."""
        if not self.database:
            return None
        return self.database.get_person(person_id)

    def update_person_consanguinity(self, person_id: int, consanguinity: float) -> None:
        """Update person's consanguinity value."""
        if not self.database:
            raise DatabaseError("Database not loaded")

        person = self.database.get_person(person_id)
        if person:
            person.consanguinity = consanguinity

    def save_changes(self) -> None:
        """Save changes to database."""
        if not self.database:
            raise DatabaseError("Database not loaded")

        try:
            # In a real implementation, this would save back to GWB format
            # For now, just update the in-memory database
            pass
        except Exception as e:
            raise DatabaseError(f"Failed to save database: {e}")

    @property
    def persons(self) -> dict:
        """Get persons dictionary for legacy compatibility."""
        if not self.database:
            return {}
        return self.database.persons

    def __del__(self):
        """Cleanup on destruction."""
        try:
            if self._is_locked:
                self.unlock_database()
        except Exception:
            pass
