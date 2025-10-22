"""
Base database interface for MessagePack format.

Provides high-level interface for MessagePack-based genealogical data access.
"""

from typing import Any, Dict, Optional, List

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from .base_data import BaseData


class Base:
    """
    Base database class for MessagePack operations.

    Provides interface for MessagePack-based genealogical database.
    """

    def __init__(self, data: BaseData, db_name: str = "", data_dir: str = "bases"):
        """
        Initialize MessagePack database.

        Args:
            data: Database data container
            db_name: Name of the database
            data_dir: Directory containing databases
        """
        self.data = data
        self.db_name = db_name
        self.data_dir = data_dir

        from ..io.msgpack import MessagePackReader, MessagePackWriter

        self.reader = MessagePackReader(data_dir)
        self.writer = MessagePackWriter(data_dir)

    # ========================================================================
    # Basic Accessors
    # ========================================================================

    def sou(self, istr: Istr) -> str:
        """Get string from string ID."""
        return self.data.strings.get(istr, "")

    def bname(self) -> str:
        """Get base name."""
        return self.db_name

    def nb_of_persons(self) -> int:
        """Get number of persons."""
        return self.data.persons_count

    def nb_of_families(self) -> int:
        """Get number of families."""
        return self.data.families_count

    def nb_of_strings(self) -> int:
        """Get number of strings."""
        return self.data.strings_count

    # ========================================================================
    # Person Access
    # ========================================================================

    def person(self, iper: Iper) -> Optional[GenPerson]:
        """Get person by ID."""
        return self.data.persons.get(iper)

    def persons(self) -> Dict[Iper, GenPerson]:
        """Get all persons."""
        return self.data.persons

    # ========================================================================
    # Family Access
    # ========================================================================

    def family(self, ifam: Ifam) -> Optional[GenFamily]:
        """Get family by ID."""
        return self.data.families.get(ifam)

    def families(self) -> Dict[Ifam, GenFamily]:
        """Get all families."""
        return self.data.families

    # ========================================================================
    # Relation Access
    # ========================================================================

    def ascend(self, iper: Iper) -> Optional[Any]:
        """Get person's ascendants."""
        return self.data.ascends.get(iper)

    def union(self, iper: Iper) -> Optional[Any]:
        """Get person's unions."""
        return self.data.unions.get(iper)

    def couple(self, ifam: Ifam) -> Optional[Any]:
        """Get family's couple."""
        return self.data.couples.get(ifam)

    def descend(self, ifam: Ifam) -> Optional[Any]:
        """Get family's descendants."""
        return self.data.descends.get(ifam)

    # ========================================================================
    # String Management
    # ========================================================================

    def insert_string(self, s: str) -> Istr:
        """Insert string and return its ID."""
        for istr, existing_str in self.data.strings.items():
            if existing_str == s:
                return istr

        new_istr = Istr(len(self.data.strings) + 1)
        self.data.strings[new_istr] = s
        return new_istr

    def get_string(self, istr: Istr) -> str:
        """Get string by ID."""
        return self.sou(istr)

    # ========================================================================
    # Search Functions
    # ========================================================================

    def search_persons_by_name(
        self, first_name: str = "", surname: str = ""
    ) -> List[GenPerson]:
        """Search persons by first name and/or surname."""
        return self.reader.search_persons_by_name(self.db_name, first_name, surname)

    def search_persons_by_surname(self, surname: str) -> List[GenPerson]:
        """Search persons by surname."""
        return self.reader.search_persons_by_surname(self.db_name, surname)

    def search_persons_by_first_name(self, first_name: str) -> List[GenPerson]:
        """Search persons by first name."""
        return self.reader.search_persons_by_first_name(self.db_name, first_name)

    def search_persons_by_place(self, place: str) -> List[GenPerson]:
        """Search persons by place."""
        return self.reader.search_persons_by_place(self.db_name, place)

    def search_persons_by_full_name(self, full_name: str) -> List[GenPerson]:
        """Search persons by full name (case-insensitive, partial match)."""
        return self.reader.search_persons_by_full_name(self.db_name, full_name)

    def search_strings_by_content(self, content: str) -> List[tuple]:
        """Search strings by content (case-insensitive, partial match)."""
        return self.reader.search_strings_by_content(self.db_name, content)

    def search_families_by_marriage_date(self, year: int) -> List[GenFamily]:
        """Search families by marriage year."""
        return self.reader.search_families_by_marriage_date(self.db_name, year)

    # ========================================================================
    # Database Operations
    # ========================================================================

    def save(self) -> bool:
        """Save database to MessagePack format."""
        try:
            self.writer.write_database(self.data, self.db_name)
            return True
        except Exception as e:
            print(f"Error saving database: {e}")
            return False

    def load(self) -> bool:
        """Load database from MessagePack format."""
        try:
            loaded_data = self.reader.load_database(self.db_name)
            if loaded_data:
                self.data = loaded_data
                return True
            return False
        except Exception as e:
            print(f"Error loading database: {e}")
            return False

    # ========================================================================
    # Statistics
    # ========================================================================

    def get_statistics(self) -> Dict[str, Any]:
        """Get database statistics."""
        return {
            "persons_count": self.nb_of_persons(),
            "families_count": self.nb_of_families(),
            "strings_count": self.nb_of_strings(),
            "particles_count": len(self.data.particles),
            "bdir": self.data.bdir,
        }

    def build_indexes(self, verbose: bool = True) -> None:
        """Build all search indexes for fast lookups."""
        self.data.build_indexes(verbose)

    def clear(self) -> None:
        """Clear all data from the database."""
        self.data.clear()

    def copy(self) -> "Base":
        """Create a deep copy of the database."""
        import copy

        copied_data = copy.deepcopy(self.data)
        return Base(copied_data, self.db_name, self.data_dir)

    def get_database_info(self) -> Optional[Dict[str, Any]]:
        """Get detailed database information."""
        return self.reader.get_database_statistics(self.db_name)

    # ========================================================================
    # Validation
    # ========================================================================

    def validate(self) -> List[str]:
        """Validate database integrity and return list of errors."""
        errors = []

        # Check for orphaned references in couples
        for family_id, couple in self.data.couples.items():
            if couple.father not in self.data.persons:
                errors.append(
                    f"Family {family_id} references non-existent father {couple.father}"
                )
            if couple.mother not in self.data.persons:
                errors.append(
                    f"Family {family_id} references non-existent mother {couple.mother}"
                )

        # Check for orphaned references in descends
        for family_id, descend in self.data.descends.items():
            for child_id in descend.children:
                if child_id not in self.data.persons:
                    errors.append(
                        f"Family {family_id} references non-existent child {child_id}"
                    )

        return errors
