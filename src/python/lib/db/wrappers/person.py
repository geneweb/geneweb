"""
Person wrapper with lazy loading.

The Person class provides a convenient interface to person data
with automatic caching and lazy loading from the database.
"""

from typing import TYPE_CHECKING, List, Optional

from ..core.enums import Access, DeathType, Sex
from ..models.events import Date
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenUnion

if TYPE_CHECKING:
    from ..database import Base


class Person:
    """
    Person wrapper with lazy loading and caching.
    Data is loaded from database only when accessed.
    """

    def __init__(self, base: "Base", iper: int):
        """
        Create person reference.

        Args:
            base: Database reference
            iper: Person ID
        """
        self.base = base
        self.iper = iper
        self._person: Optional[GenPerson] = None
        self._ascend: Optional["GenAscend"] = None
        self._union: Optional["GenUnion"] = None

    def _ensure_person(self) -> GenPerson:
        """Load person data if not cached."""
        if self._person is None:
            self._person = self.base.data.persons.get(self.iper)
        return self._person

    def _ensure_ascend(self) -> "GenAscend":
        """Load ascend data if not cached."""
        if self._ascend is None:
            self._ascend = self.base.data.ascends.get(self.iper)
        return self._ascend

    def _ensure_union(self) -> "GenUnion":
        """Load union data if not cached."""
        if self._union is None:
            self._union = self.base.data.unions.get(self.iper)
        return self._union

    # Cached property accessors
    def get_first_name(self) -> int:
        """Get first name string ID."""
        return self._ensure_person().first_name

    def get_surname(self) -> int:
        """Get surname string ID."""
        return self._ensure_person().surname

    def get_occ(self) -> int:
        """Get occurrence number."""
        return self._ensure_person().occ

    def get_sex(self) -> Sex:
        """Get person's sex."""
        return self._ensure_person().sex

    def get_birth(self) -> Date:
        """Get birth date."""
        return self._ensure_person().birth

    def get_death(self) -> DeathType:
        """Get death status."""
        return self._ensure_person().death

    def get_parents(self) -> Optional[int]:
        """Get family ID where person is a child."""
        return self._ensure_ascend().parents

    def get_family(self) -> List[int]:
        """Get list of family IDs where person is a parent."""
        return self._ensure_union().family

    def get_access(self) -> Access:
        """Get privacy access level."""
        return self._ensure_person().access

    def get_consang(self) -> float:
        """Get consanguinity coefficient."""
        return self._ensure_ascend().consang

    def gen_person(self) -> GenPerson:
        """Get underlying GenPerson structure."""
        return self._ensure_person()

    def gen_ascend(self) -> "GenAscend":
        """Get underlying GenAscend structure."""
        return self._ensure_ascend()

    def gen_union(self) -> "GenUnion":
        """Get underlying GenUnion structure."""
        return self._ensure_union()
