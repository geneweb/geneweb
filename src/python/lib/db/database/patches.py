"""
Patches for MessagePack database modifications.

Handles all modification operations on the MessagePack database.
"""

from typing import Dict, List, Any

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion
from .base_data import BaseData


class Patches:
    """Handles all modification operations on MessagePack database."""

    def __init__(self, data: BaseData):
        """
        Initialize patches handler.

        Args:
            data: MessagePack database data container
        """
        self.data = data
        self.pending_patches: List[Dict[str, Any]] = []

    def patch_person(self, iper: Iper, person: GenPerson) -> None:
        """Add or modify a person."""
        self.data.persons[iper] = person
        self.pending_patches.append({"type": "person", "id": iper, "data": person})

    def patch_ascend(self, iper: Iper, ascend: GenAscend) -> None:
        """Add or modify person's ascendants."""
        self.data.ascends[iper] = ascend
        self.pending_patches.append({"type": "ascend", "id": iper, "data": ascend})

    def patch_union(self, iper: Iper, union: GenUnion) -> None:
        """Add or modify person's unions."""
        self.data.unions[iper] = union
        self.pending_patches.append({"type": "union", "id": iper, "data": union})

    def patch_family(self, ifam: Ifam, family: GenFamily) -> None:
        """Add or modify a family."""
        self.data.families[ifam] = family
        self.pending_patches.append({"type": "family", "id": ifam, "data": family})

    def patch_couple(self, ifam: Ifam, couple: GenCouple) -> None:
        """Add or modify family's couple."""
        self.data.couples[ifam] = couple
        self.pending_patches.append({"type": "couple", "id": ifam, "data": couple})

    def patch_descend(self, ifam: Ifam, descend: GenDescend) -> None:
        """Add or modify family's descendants."""
        self.data.descends[ifam] = descend
        self.pending_patches.append({"type": "descend", "id": ifam, "data": descend})

    def insert_string(self, s: str) -> Istr:
        """Insert string and return its ID."""
        for istr, existing_str in self.data.strings.items():
            if existing_str == s:
                return istr

        new_istr = Istr(len(self.data.strings) + 1)
        self.data.strings[new_istr] = s
        self.pending_patches.append({"type": "string", "id": new_istr, "data": s})
        return new_istr

    def get_string(self, istr: Istr) -> str:
        """Get string by ID."""
        return self.data.strings.get(istr, "")

    def commit_patches(self) -> None:
        """Commit all pending patches."""
        # In MessagePack, patches are applied immediately
        # This method is kept for compatibility
        self.pending_patches.clear()

    def get_pending_patches(self) -> List[Dict[str, Any]]:
        """Get list of pending patches."""
        return self.pending_patches.copy()

    def clear_patches(self) -> None:
        """Clear all pending patches."""
        self.pending_patches.clear()

    def iper_exists(self, iper: Iper) -> bool:
        """Check if person exists."""
        return iper in self.data.persons

    def ifam_exists(self, ifam: Ifam) -> bool:
        """Check if family exists."""
        return ifam in self.data.families

    def istr_exists(self, istr: Istr) -> bool:
        """Check if string exists."""
        return istr in self.data.strings

    def delete_person(self, iper: Iper) -> bool:
        """Delete a person."""
        if iper in self.data.persons:
            del self.data.persons[iper]
            self.pending_patches.append({"type": "delete_person", "id": iper})
            return True
        return False

    def delete_family(self, ifam: Ifam) -> bool:
        """Delete a family."""
        if ifam in self.data.families:
            del self.data.families[ifam]
            if ifam in self.data.couples:
                del self.data.couples[ifam]
            if ifam in self.data.descends:
                del self.data.descends[ifam]
            self.pending_patches.append({"type": "delete_family", "id": ifam})
            return True
        return False

    def delete_string(self, istr: Istr) -> bool:
        """Delete a string."""
        if istr in self.data.strings:
            del self.data.strings[istr]
            self.pending_patches.append({"type": "delete_string", "id": istr})
            return True
        return False
