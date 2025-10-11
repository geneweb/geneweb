"""
Patches for pickle database.

Handles all modification operations on the pickle database.
"""

from ..core.types import Iper, Ifam, Istr
from ..models.person import GenPerson
from ..models.family import GenFamily
from ..models.relations import GenAscend, GenUnion, GenCouple, GenDescend
from .base_data import PickleBaseData

class PicklePatches:
    """
    Handles all database modification operations for pickle database.
    """

    def __init__(self, data: PickleBaseData):
        """
        Initialize patches handler.

        Args:
            data: Pickle database data container
        """
        self.data = data
        self.pending_changes = []

    # ========================================================================
    # Person Operations
    # ========================================================================

    def patch_person(self, iper: Iper, person: GenPerson) -> None:
        """
        Modify or add person.

        Args:
            iper: Person ID
            person: New person data
        """
        self.data.persons[iper] = person
        self.pending_changes.append(('person', iper, person))

    def patch_ascend(self, iper: Iper, ascend: GenAscend) -> None:
        """
        Modify person's ascendants.

        Args:
            iper: Person ID
            ascend: Ascendants data
        """
        self.data.ascends[iper] = ascend
        self.pending_changes.append(('ascend', iper, ascend))

    def patch_union(self, iper: Iper, union: GenUnion) -> None:
        """
        Modify person's unions.

        Args:
            iper: Person ID
            union: Union data
        """
        self.data.unions[iper] = union
        self.pending_changes.append(('union', iper, union))

    # ========================================================================
    # Family Operations
    # ========================================================================

    def patch_family(self, ifam: Ifam, family: GenFamily) -> None:
        """
        Modify family.

        Args:
            ifam: Family ID
            family: Family data
        """
        self.data.families[ifam] = family
        self.pending_changes.append(('family', ifam, family))

    def patch_couple(self, ifam: Ifam, couple: GenCouple) -> None:
        """
        Modify family's couple.

        Args:
            ifam: Family ID
            couple: Couple data
        """
        self.data.couples[ifam] = couple
        self.pending_changes.append(('couple', ifam, couple))

    def patch_descend(self, ifam: Ifam, descend: GenDescend) -> None:
        """
        Modify family's descendants.

        Args:
            ifam: Family ID
            descend: Descendants data
        """
        self.data.descends[ifam] = descend
        self.pending_changes.append(('descend', ifam, descend))

    # ========================================================================
    # String Operations
    # ========================================================================

    def insert_string(self, s: str) -> Istr:
        """
        Insert or get string ID.

        Args:
            s: String to insert

        Returns:
            String ID (new or existing)
        """
        for istr, existing_str in self.data.strings.items():
            if existing_str == s:
                return istr

        new_istr = Istr(len(self.data.strings) + 1)
        self.data.strings[new_istr] = s
        self.pending_changes.append(('string', new_istr, s))
        return new_istr

    # ========================================================================
    # Commit Operations
    # ========================================================================

    def commit_patches(self) -> None:
        """
        Commit all pending modifications.

        In pickle database, changes are immediately applied,
        so this is mainly for logging/debugging.
        """
        if self.pending_changes:
            print(f"Committed {len(self.pending_changes)} changes to pickle database")
            self.pending_changes.clear()

    # ========================================================================
    # Deletion Operations
    # ========================================================================

    def delete_person(self, iper: Iper) -> None:
        """
        Delete person (replace with empty entry).

        Args:
            iper: Person ID to delete
        """
        empty = GenPerson(
            first_name="?",
            surname="?",
            occ=0,
            key_index=iper
        )
        self.patch_person(iper, empty)

    def delete_family(self, ifam: Ifam) -> None:
        """
        Delete family (replace with dummy entry).

        Args:
            ifam: Family ID to delete
        """
        empty = GenFamily(fam_index=ifam)
        self.patch_family(ifam, empty)

    def clear_ascend(self, iper: Iper) -> None:
        """Clear person's ascendants."""
        self.patch_ascend(iper, GenAscend())

    def clear_union(self, iper: Iper) -> None:
        """Clear person's unions."""
        self.patch_union(iper, GenUnion())

    def clear_couple(self, ifam: Ifam) -> None:
        """Clear family's couple."""
        self.patch_couple(ifam, GenCouple())

    def clear_descend(self, ifam: Ifam) -> None:
        """Clear family's descendants."""
        self.patch_descend(ifam, GenDescend())
