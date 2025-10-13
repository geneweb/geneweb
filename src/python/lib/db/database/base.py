"""
Main database class.

The Base class is the primary interface for interacting with
a genealogical database. Matches OCaml driver.ml structure.
"""

from __future__ import annotations

from dataclasses import replace
from pathlib import Path
from typing import Callable, List, Optional, TypeVar

from ..collections import Collection, Marker
from ..core.types import Ifam, Iper
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion
from ..wrappers.family import Family
from ..wrappers.person import Person
from .base_data import BaseData
from .base_func import BaseFunc
from .patches import DatabasePatches

T = TypeVar("T")


class Base:
    """
    Main database class.
    Provides high-level interface for genealogical data access.
    """

    def __init__(self, data: BaseData, func: BaseFunc):
        """
        Initialize database.

        Args:
            data: Database data arrays
            func: Database operations
        """
        self.data = data
        self.func = func
        self.patches = DatabasePatches(self)

    # ========================================================================
    # Basic Accessors (matches OCaml accessors)
    # ========================================================================

    def sou(self, istr: int) -> str:
        """Get string from string ID. Matches OCaml sou."""
        return self.data.strings.get(istr)

    def bname(self) -> str:
        """Get base name. Matches OCaml bname."""
        return Path(self.data.bdir).stem

    def nb_of_persons(self) -> int:
        """Get number of person slots. Matches OCaml nb_of_persons."""
        return self.data.persons.len

    def nb_of_families(self) -> int:
        """Get number of family slots. Matches OCaml nb_of_families."""
        return self.data.families.len

    def nb_of_real_persons(self) -> int:
        """
        Get number of actual persons (excluding "? ?").
        """
        return self.func.nb_of_real_persons()

    # ========================================================================
    # Person and Family Construction (matches OCaml poi/foi)
    # ========================================================================

    def poi(self, iper: int) -> Person:
        """
        Get person by ID. Matches OCaml poi.

        Args:
            iper: Person ID

        Returns:
            Person wrapper with lazy loading
        """
        if Iper.is_dummy(iper):
            return self.empty_person(iper)
        return Person(self, iper)

    def foi(self, ifam: int) -> Family:
        """
        Get family by ID. Matches OCaml foi.

        Args:
            ifam: Family ID

        Returns:
            Family wrapper with lazy loading
        """
        if Ifam.is_dummy(ifam):
            return self.empty_family(ifam)
        return Family(self, ifam)

    def empty_person(self, iper: int) -> Person:
        """Create empty person. Matches OCaml empty_person."""
        return Person(self, iper)

    def empty_family(self, ifam: int) -> Family:
        """Create empty family. Matches OCaml empty_family."""
        return Family(self, ifam)

    def iper_exists(self, iper: int) -> bool:
        """Check if person exists. Matches OCaml iper_exists."""
        return self.func.iper_exists(iper)

    def ifam_exists(self, ifam: int) -> bool:
        """Check if family exists. Matches OCaml ifam_exists."""
        return self.func.ifam_exists(ifam)

    # ========================================================================
    # Searching (matches OCaml search functions)
    # ========================================================================

    def person_of_key(self, first_name: str, surname: str, occ: int) -> Optional[int]:
        """Find person by name key. Matches OCaml person_of_key."""
        return self.func.person_of_key(first_name, surname, occ)

    def persons_of_name(self, name: str) -> List[int]:
        """Find persons by name. Matches OCaml persons_of_name."""
        return self.func.persons_of_name(name)

    def persons_of_first_name(self):
        """Get first name index. Matches OCaml persons_of_first_name."""
        return self.func.persons_of_first_name()

    def persons_of_surname(self):
        """Get surname index. Matches OCaml persons_of_surname."""
        return self.func.persons_of_surname()

    # ========================================================================
    # Modification Operations (delegates to patches module)
    # ========================================================================

    def insert_string(self, s: str) -> int:
        """Insert or get string ID. Matches OCaml insert_string."""
        return self.patches.insert_string(s)

    def patch_person(self, iper: int, person: GenPerson) -> None:
        """Modify person. Matches OCaml patch_person."""
        self.patches.patch_person(iper, person)

    def patch_ascend(self, iper: int, ascend: GenAscend) -> None:
        """Modify ascendants. Matches OCaml patch_ascend."""
        self.patches.patch_ascend(iper, ascend)

    def patch_union(self, iper: int, union: GenUnion) -> None:
        """Modify unions. Matches OCaml patch_union."""
        self.patches.patch_union(iper, union)

    def patch_family(self, ifam: int, family: GenFamily) -> None:
        """Modify family. Matches OCaml patch_family."""
        self.patches.patch_family(ifam, family)

    def patch_couple(self, ifam: int, couple: GenCouple) -> None:
        """Modify couple. Matches OCaml patch_couple."""
        self.patches.patch_couple(ifam, couple)

    def patch_descend(self, ifam: int, descend: GenDescend) -> None:
        """Modify descendants. Matches OCaml patch_descend."""
        self.patches.patch_descend(ifam, descend)

    def commit_patches(self) -> None:
        """Commit modifications. Matches OCaml commit_patches."""
        self.patches.commit_patches()

    def new_iper(self) -> int:
        """Allocate new person ID. Matches OCaml new_iper."""
        return self.data.persons.len

    def new_ifam(self) -> int:
        """Allocate new family ID. Matches OCaml new_ifam."""
        return self.data.families.len

    # ========================================================================
    # Deletion Operations (delegates to patches module)
    # ========================================================================

    def delete_person(self, iper: int) -> None:
        """Delete person. Matches OCaml delete_person."""
        self.patches.delete_person(iper)

    def delete_ascend(self, iper: int) -> None:
        """Delete ascendants. Matches OCaml delete_ascend."""
        self.patches.delete_ascend(iper)

    def delete_union(self, iper: int) -> None:
        """Delete unions. Matches OCaml delete_union."""
        self.patches.delete_union(iper)

    def delete_family(self, ifam: int) -> None:
        """Delete family. Matches OCaml delete_family."""
        self.patches.delete_family(ifam)

    def delete_couple(self, ifam: int) -> None:
        """Delete couple. Matches OCaml delete_couple."""
        self.patches.delete_couple(ifam)

    def delete_descend(self, ifam: int) -> None:
        """Delete descendants. Matches OCaml delete_descend."""
        self.patches.delete_descend(ifam)

    def delete_person_rec(self, iper: int) -> None:
        """Recursively delete person. Matches OCaml delete_person_rec."""
        self.patches.delete_person_rec(iper)

    def delete_family_rec(self, ifam: int) -> None:
        """Recursively delete family. Matches OCaml delete_family_rec."""
        self.patches.delete_family_rec(ifam)

    # ========================================================================
    # Insertion with Auto-allocation (matches OCaml insert_*_with_*)
    # ========================================================================

    def insert_person_with_union_and_ascendants(
        self, person: GenPerson, ascend: GenAscend, union: GenUnion
    ) -> int:
        """
        Insert new person with all data.
        """
        iper = self.new_iper()
        person = replace(person, key_index=iper)

        self.patch_ascend(iper, ascend)
        self.patch_union(iper, union)
        self.patch_person(iper, person)

        return iper

    def insert_family_with_couple_and_descendants(
        self, family: GenFamily, couple: GenCouple, descend: GenDescend
    ) -> int:
        """
        Insert new family with all data.
        """
        ifam = self.new_ifam()
        family = replace(family, fam_index=ifam)

        self.patch_family(ifam, family)
        self.patch_couple(ifam, couple)
        self.patch_descend(ifam, descend)

        return ifam

    # ========================================================================
    # Collections (matches OCaml collection functions)
    # ========================================================================

    def persons(self) -> Collection[Person]:
        """Get person collection. Matches OCaml persons."""
        return Collection(
            self.nb_of_persons(),
            lambda i: self.poi(i) if 0 <= i < self.nb_of_persons() else None,
        )

    def ipers(self) -> Collection[int]:
        """Get person ID collection. Matches OCaml ipers."""
        return Collection(
            self.nb_of_persons(), lambda i: i if 0 <= i < self.nb_of_persons() else None
        )

    def families(
        self, select: Optional[Callable[[Family], bool]] = None
    ) -> Collection[Family]:
        """Get family collection. Matches OCaml families."""

        def get_family(i: int) -> Optional[Family]:
            if i < 0 or i >= self.nb_of_families():
                return None
            fam = self.foi(i)
            if Ifam.is_dummy(fam.ifam):
                return None
            if select is not None and not select(fam):
                return None
            return fam

        return Collection(self.nb_of_families(), get_family)

    def ifams(self, select: Optional[Callable[[int], bool]] = None) -> Collection[int]:
        """Get family ID collection. Matches OCaml ifams."""

        def get_ifam(i: int) -> Optional[int]:
            if i < 0 or i >= self.nb_of_families():
                return None
            if select is not None and not select(i):
                return None
            return i

        return Collection(self.nb_of_families(), get_ifam)

    # ========================================================================
    # Markers (matches OCaml marker functions)
    # ========================================================================

    @staticmethod
    def iper_marker(collection: Collection[int], initial: T) -> Marker[int, T]:
        """Create person ID marker. Matches OCaml iper_marker."""
        return Marker(lambda i: i, collection, initial)

    @staticmethod
    def ifam_marker(collection: Collection[int], initial: T) -> Marker[int, T]:
        """Create family ID marker. Matches OCaml ifam_marker."""
        return Marker(lambda i: i, collection, initial)

    # ========================================================================
    # Utility Functions (matches OCaml utility functions)
    # ========================================================================

    def p_first_name(self, person: Person) -> str:
        """Get person's first name. Matches OCaml p_first_name."""
        return self.sou(person.get_first_name())

    def p_surname(self, person: Person) -> str:
        """Get person's surname. Matches OCaml p_surname."""
        return self.sou(person.get_surname())

    def children_of_person(self, person: Person) -> List[int]:
        """Get all children of person. Matches OCaml children_of_p."""
        children = []
        for ifam in person.get_family():
            family = self.foi(ifam)
            children.extend(family.get_children())
        return children
