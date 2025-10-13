"""
Family wrapper with lazy loading and caching.

Data is loaded from database only when accessed.
"""

from typing import TYPE_CHECKING, List, Optional

from ..core.enums import DivorceStatus, RelationKind
from ..models.events import Date
from ..models.family import GenFamily
from ..models.relations import GenCouple, GenDescend

if TYPE_CHECKING:
    from ..database import Base


class Family:
    """
    Family wrapper with lazy loading and caching.
    Data is loaded from database only when accessed.
    """

    def __init__(self, base: "Base", ifam: int):
        """
        Create family reference.

        Args:
            base: Database reference
            ifam: Family ID
        """
        self.base = base
        self.ifam = ifam
        self._family: Optional[GenFamily] = None
        self._couple: Optional[GenCouple] = None
        self._descend: Optional[GenDescend] = None

    def _ensure_family(self) -> GenFamily:
        """Load family data if not cached."""
        if self._family is None:
            self._family = self.base.data.families.get(self.ifam)
        return self._family

    def _ensure_couple(self) -> GenCouple:
        """Load couple data if not cached."""
        if self._couple is None:
            self._couple = self.base.data.couples.get(self.ifam)
        return self._couple

    def _ensure_descend(self) -> GenDescend:
        """Load descend data if not cached."""
        if self._descend is None:
            self._descend = self.base.data.descends.get(self.ifam)
        return self._descend

    # Cached property accessors
    def get_father(self) -> int:
        """Get father's person ID."""
        return self._ensure_couple().father

    def get_mother(self) -> int:
        """Get mother's person ID."""
        return self._ensure_couple().mother

    def get_children(self) -> List[int]:
        """Get list of children's person IDs."""
        return self._ensure_descend().children

    def get_marriage(self) -> Date:
        """Get marriage date."""
        return self._ensure_family().marriage

    def get_relation(self) -> RelationKind:
        """Get relationship type."""
        return self._ensure_family().relation

    def get_divorce(self) -> DivorceStatus:
        """Get divorce status (extracted from events)."""
        fevents = self._ensure_family().fevents
        for event in fevents:
            if event.name == "divorce":
                return DivorceStatus.DIVORCED
            elif event.name == "separated":
                return DivorceStatus.SEPARATED
        return DivorceStatus.NOT_DIVORCED

    def get_witnesses(self) -> List[int]:
        """Get list of witness person IDs."""
        return self._ensure_family().witnesses

    def gen_family(self) -> GenFamily:
        """Get underlying GenFamily structure."""
        return self._ensure_family()

    def gen_couple(self) -> GenCouple:
        """Get underlying GenCouple structure."""
        return self._ensure_couple()

    def gen_descend(self) -> GenDescend:
        """Get underlying GenDescend structure."""
        return self._ensure_descend()
