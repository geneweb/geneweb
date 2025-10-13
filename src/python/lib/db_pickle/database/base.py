"""
Main database class for pickle database.

Provides high-level interface for pickle-based genealogical data access.
"""

from pathlib import Path
from typing import Any, Dict, Optional

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from .base_data import PickleBaseData


class PickleBase:
    """
    Main database class for pickle operations.

    Provides interface for pickle-based genealogical database.
    """

    def __init__(self, data: PickleBaseData):
        """
        Initialize pickle database.

        Args:
            data: Pickle database data container
        """
        self.data = data

    # ========================================================================
    # Basic Accessors
    # ========================================================================

    def sou(self, istr: Istr) -> str:
        """Get string from string ID."""
        return self.data.strings.get(istr, "")

    def bname(self) -> str:
        """Get base name."""
        return Path(self.data.bdir).stem if self.data.bdir else ""

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
    # Statistics
    # ========================================================================

    def get_statistics(self) -> Dict[str, Any]:
        """Get database statistics."""
        return {
            "persons_count": self.nb_of_persons(),
            "families_count": self.nb_of_families(),
            "strings_count": self.nb_of_strings(),
            "particles_count": len(self.data.particles),
        }
