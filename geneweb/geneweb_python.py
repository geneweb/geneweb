"""
GeneWeb Database Driver Implementation in Python
Complete implementation including person and family management
"""

from typing import (
    TypeVar, Generic, Callable, Optional, Iterator, Any, Dict, List,
    Tuple, Set as TSet, Protocol
)
from dataclasses import dataclass, field, replace
from abc import ABC, abstractmethod
from pathlib import Path
from enum import Enum
import pickle
import os
import hashlib
import re
from datetime import datetime


T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')


# ============================================================================
# Core Type System - Indexed Types
# ============================================================================

class Indexed(Protocol):
    """Protocol for indexed database types (person ID, family ID, string ID)."""

    @staticmethod
    def dummy() -> int:
        """Return a dummy/invalid ID value."""
        ...

    @staticmethod
    def is_dummy(value: int) -> bool:
        """Check if value is a dummy ID."""
        ...

    @staticmethod
    def hash(value: int) -> int:
        """Compute hash (identity for integer IDs)."""
        ...

    @staticmethod
    def equal(a: int, b: int) -> bool:
        """Check equality."""
        ...

    @staticmethod
    def compare(a: int, b: int) -> int:
        """Compare two values (-1, 0, 1)."""
        ...

    @staticmethod
    def to_string(value: int) -> str:
        """Convert to string."""
        ...

    @staticmethod
    def of_string(s: str) -> int:
        """Parse from string."""
        ...


class Iper:
    """Person ID implementation."""

    DUMMY = -1

    @staticmethod
    def dummy() -> int:
        return Iper.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Iper.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)


class Ifam:
    """Family ID implementation."""

    DUMMY = -1

    @staticmethod
    def dummy() -> int:
        return Ifam.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Ifam.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)


class Istr:
    """String ID implementation with special values for empty and quest."""

    EMPTY = 0  # ID for empty string ""
    QUEST = 1  # ID for question mark "?"
    DUMMY = -1

    @staticmethod
    def empty() -> int:
        return Istr.EMPTY

    @staticmethod
    def quest() -> int:
        return Istr.QUEST

    @staticmethod
    def is_empty(value: int) -> bool:
        return value == Istr.EMPTY

    @staticmethod
    def is_quest(value: int) -> bool:
        return value == Istr.QUEST

    @staticmethod
    def dummy() -> int:
        return Istr.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Istr.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)


# ============================================================================
# Collection Module
# ============================================================================

class Collection(Generic[T]):
    """
    A collection is a set of elements you want to traverse.
    Provides functional operations like map, fold, filter.
    """

    def __init__(self, length: int, get_func: Callable[[int], Optional[T]]):
        """
        Create a collection with accessor function.

        Args:
            length: Number of elements in collection
            get_func: Function to get element at index (returns None if invalid)
        """
        self._length = length
        self._get_func = get_func

    @classmethod
    def empty(cls) -> 'Collection[T]':
        """Create an empty collection."""
        return cls(-1, lambda _: None)

    @property
    def length(self) -> int:
        """Return the number of elements in collection."""
        return self._length

    def get(self, index: int) -> Optional[T]:
        """Get element at index."""
        return self._get_func(index)

    def map(self, fn: Callable[[T], Any]) -> 'Collection':
        """
        Apply transformation to each element.

        Args:
            fn: Transformation function

        Returns:
            New collection with transformed elements
        """
        def mapped_get(i: int) -> Optional[Any]:
            value = self._get_func(i)
            return fn(value) if value is not None else None

        return Collection(self._length, mapped_get)

    def iter(self, fn: Callable[[T], None]) -> None:
        """Apply side-effect function to each element."""
        for i in range(self._length):
            value = self.get(i)
            if value is not None:
                fn(value)

    def iteri(self, fn: Callable[[int, T], None]) -> None:
        """Apply side-effect function to each element with its index."""
        for i in range(self._length):
            value = self.get(i)
            if value is not None:
                fn(i, value)

    def fold(
        self,
        fn: Callable[[Any, T], Any],
        acc: Any,
        from_idx: Optional[int] = None,
        until_idx: Optional[int] = None
    ) -> Any:
        """
        Fold collection into single value.

        Args:
            fn: Reduction function (accumulator, element) -> new_accumulator
            acc: Initial accumulator value
            from_idx: Start index (inclusive)
            until_idx: End index (inclusive)

        Returns:
            Final accumulated value
        """
        start = from_idx if from_idx is not None else 0
        end = (until_idx + 1) if until_idx is not None else self._length

        result = acc
        for i in range(start, end):
            value = self.get(i)
            if value is not None:
                result = fn(result, value)

        return result

    def fold_until(
        self,
        continue_fn: Callable[[Any], bool],
        fn: Callable[[Any, T], Any],
        acc: Any
    ) -> Any:
        """
        Fold until predicate fails.

        Args:
            continue_fn: Predicate to check if folding should continue
            fn: Reduction function
            acc: Initial value

        Returns:
            Final accumulated value
        """
        result = acc
        for i in range(self._length):
            if not continue_fn(result):
                break
            value = self.get(i)
            if value is not None:
                result = fn(result, value)

        return result

    def iterator(self) -> Callable[[], Optional[T]]:
        """
        Create iterator function.

        Returns:
            Function that returns next element or None when exhausted
        """
        cursor = 0

        def next_func() -> Optional[T]:
            nonlocal cursor
            while cursor < self._length:
                value = self.get(cursor)
                cursor += 1
                if value is not None:
                    return value
            return None

        return next_func


class Marker(Generic[K, V]):
    """
    Markers annotate collection elements with extra information.
    Used for marking visited nodes, storing temporary data, etc.
    """

    def __init__(self, key_to_int: Callable[[K], int], collection: Collection, initial: V):
        """
        Create marker for collection.

        Args:
            key_to_int: Function to convert key to array index
            collection: Collection to mark
            initial: Initial value for all markers
        """
        self._key_to_int = key_to_int
        self._storage: List[V] = [initial] * collection.length

    @classmethod
    def dummy(cls, initial_value: V) -> 'Marker[Any, V]':
        """Create dummy marker for placeholders."""
        marker = cls.__new__(cls)
        marker._key_to_int = lambda _: 0
        marker._storage = []
        marker._dummy_value = initial_value
        return marker

    def get(self, key: K) -> V:
        """Get annotation for key."""
        if not self._storage:
            return self._dummy_value
        idx = self._key_to_int(key)
        return self._storage[idx]

    def set(self, key: K, value: V) -> None:
        """Set annotation for key."""
        if self._storage:
            idx = self._key_to_int(key)
            self._storage[idx] = value


# ============================================================================
# Database Enums and Basic Types
# ============================================================================

class Sex(Enum):
    """Person's biological sex."""
    MALE = "M"
    FEMALE = "F"
    NEUTER = "U"  # Unknown/unspecified


class Access(Enum):
    """Privacy access level for person data."""
    PUBLIC = "public"
    PRIVATE = "private"
    FRIEND = "friend"


class DeathType(Enum):
    """Death status of a person."""
    NOT_DEAD = "not_dead"
    DEAD = "dead"
    DEAD_YOUNG = "dead_young"
    DEAD_DONT_KNOW_WHEN = "dead_dont_know_when"
    DONT_KNOW_IF_DEAD = "dont_know_if_dead"
    OF_COURSE_DEAD = "of_course_dead"


class BurialType(Enum):
    """Type of burial."""
    UNKNOWN = "unknown"
    BURIED = "buried"
    CREMATED = "cremated"


class RelationKind(Enum):
    """Type of relationship between couple."""
    MARRIED = "married"
    NOT_MARRIED = "not_married"
    ENGAGED = "engaged"
    NO_SEXES_CHECK_NOT_MARRIED = "no_sexes_check_not_married"
    NO_MENTION = "no_mention"
    NO_SEXES_CHECK_MARRIED = "no_sexes_check_married"


class DivorceStatus(Enum):
    """Divorce status of a couple."""
    NOT_DIVORCED = "not_divorced"
    DIVORCED = "divorced"
    SEPARATED = "separated"


# ============================================================================
# Core Data Structures
# ============================================================================

@dataclass
class Date:
    """Represents a calendar date with precision."""
    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None
    precision: str = "exact"  # exact, about, maybe, before, after

    @classmethod
    def none(cls) -> 'Date':
        """Create empty/unknown date."""
        return cls()


@dataclass
class Title:
    """Nobility title of a person."""
    name: int  # String ID
    title: str
    place: int  # String ID
    date_start: Date = field(default_factory=Date.none)
    date_end: Date = field(default_factory=Date.none)
    nth: int = 0


@dataclass
class Event:
    """Generic event (birth, death, marriage, etc.)."""
    name: str
    date: Date = field(default_factory=Date.none)
    place: int = Istr.empty()  # String ID
    reason: int = Istr.empty()  # String ID
    note: int = Istr.empty()  # String ID
    source: int = Istr.empty()  # String ID
    witnesses: List[Tuple[int, str]] = field(default_factory=list)  # (iper, witness_kind)


@dataclass
class Relation:
    """Relationship to non-biological parents."""
    father: Optional[int] = None  # iper
    mother: Optional[int] = None  # iper
    source: int = Istr.empty()
    relation_type: str = "rparent_adoption"


@dataclass
class GenPerson:
    """
    Complete person record.
    This is the gen_person structure from OCaml.
    """
    first_name: int  # istr
    surname: int  # istr
    occ: int  # Occurrence number for disambiguation
    image: int = Istr.empty()
    public_name: int = Istr.empty()
    qualifiers: List[int] = field(default_factory=list)
    aliases: List[int] = field(default_factory=list)
    first_names_aliases: List[int] = field(default_factory=list)
    surnames_aliases: List[int] = field(default_factory=list)
    titles: List[Title] = field(default_factory=list)
    related: List[int] = field(default_factory=list)  # List of iper
    rparents: List[Relation] = field(default_factory=list)
    occupation: int = Istr.empty()
    sex: Sex = Sex.NEUTER
    access: Access = Access.PRIVATE
    birth: Date = field(default_factory=Date.none)
    birth_place: int = Istr.empty()
    birth_note: int = Istr.empty()
    birth_src: int = Istr.empty()
    baptism: Date = field(default_factory=Date.none)
    baptism_place: int = Istr.empty()
    baptism_note: int = Istr.empty()
    baptism_src: int = Istr.empty()
    death: DeathType = DeathType.DONT_KNOW_IF_DEAD
    death_place: int = Istr.empty()
    death_note: int = Istr.empty()
    death_src: int = Istr.empty()
    burial: BurialType = BurialType.UNKNOWN
    burial_place: int = Istr.empty()
    burial_note: int = Istr.empty()
    burial_src: int = Istr.empty()
    pevents: List[Event] = field(default_factory=list)
    notes: int = Istr.empty()
    psources: int = Istr.empty()
    key_index: int = Iper.dummy()  # iper - person's unique ID


@dataclass
class GenAscend:
    """Ascendant information (parents)."""
    parents: Optional[int] = None  # ifam - family where this person is a child
    consang: float = 0.0  # Consanguinity coefficient


@dataclass
class GenUnion:
    """Union information (families where person is a parent)."""
    family: List[int] = field(default_factory=list)  # List of ifam


@dataclass
class GenFamily:
    """
    Complete family record.
    This is the gen_family structure from OCaml.
    """
    marriage: Date = field(default_factory=Date.none)
    marriage_place: int = Istr.empty()
    marriage_note: int = Istr.empty()
    marriage_src: int = Istr.empty()
    witnesses: List[int] = field(default_factory=list)  # List of iper
    relation: RelationKind = RelationKind.MARRIED
    divorce: DivorceStatus = DivorceStatus.NOT_DIVORCED
    fevents: List[Event] = field(default_factory=list)
    comment: int = Istr.empty()
    origin_file: int = Istr.empty()
    fsources: int = Istr.empty()
    fam_index: int = Ifam.dummy()  # ifam - family's unique ID


@dataclass
class GenCouple:
    """Couple in a family (father and mother)."""
    father: int  # iper
    mother: int  # iper


@dataclass
class GenDescend:
    """Descendant information (children)."""
    children: List[int] = field(default_factory=list)  # List of iper


# ============================================================================
# Cache-enabled Person and Family Classes
# ============================================================================

class Person:
    """
    Person wrapper with lazy loading and caching.
    Data is loaded from database only when accessed.
    """

    def __init__(self, base: 'Base', iper: int):
        """
        Create person reference.

        Args:
            base: Database reference
            iper: Person ID
        """
        self.base = base
        self.iper = iper
        self._person: Optional[GenPerson] = None
        self._ascend: Optional[GenAscend] = None
        self._union: Optional[GenUnion] = None

    def _ensure_person(self) -> GenPerson:
        """Load person data if not cached."""
        if self._person is None:
            self._person = self.base.data.persons.get(self.iper)
        return self._person

    def _ensure_ascend(self) -> GenAscend:
        """Load ascend data if not cached."""
        if self._ascend is None:
            self._ascend = self.base.data.ascends.get(self.iper)
        return self._ascend

    def _ensure_union(self) -> GenUnion:
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

    def gen_ascend(self) -> GenAscend:
        """Get underlying GenAscend structure."""
        return self._ensure_ascend()

    def gen_union(self) -> GenUnion:
        """Get underlying GenUnion structure."""
        return self._ensure_union()


class Family:
    """
    Family wrapper with lazy loading and caching.
    Data is loaded from database only when accessed.
    """

    def __init__(self, base: 'Base', ifam: int):
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


# ============================================================================
# Record Access Layer
# ============================================================================

@dataclass
class RecordAccess(Generic[T]):
    """
    Access layer for database arrays with patching support.
    Handles both committed and pending modifications.
    """
    load_array: Callable[[], None]
    get: Callable[[int], T]
    get_nopending: Callable[[int], T]  # Get without pending patches
    output_array: Callable[[Any], None]
    length: int
    clear_array: Callable[[], None]


# ============================================================================
# Database Implementation
# ============================================================================

@dataclass
class BaseData:
    """Container for all database data arrays."""
    persons: RecordAccess[GenPerson]
    ascends: RecordAccess[GenAscend]
    unions: RecordAccess[GenUnion]
    families: RecordAccess[GenFamily]
    couples: RecordAccess[GenCouple]
    descends: RecordAccess[GenDescend]
    strings: RecordAccess[str]
    bdir: str  # Base directory
    particles: List[str] = field(default_factory=list)


@dataclass
class SearchIndex:
    """Index for efficient name searching."""
    find: Callable[[int], List[int]]  # Find persons by string ID
    cursor: Callable[[str], int]  # Find first string ID >= key
    next: Callable[[int], int]  # Get next string ID


@dataclass
class BaseFunc:
    """Container for database operations."""
    person_of_key: Callable[[str, str, int], Optional[int]]
    persons_of_name: Callable[[str], List[int]]
    strings_of_fname: Callable[[str], List[int]]
    strings_of_sname: Callable[[str], List[int]]
    persons_of_surname: SearchIndex
    persons_of_first_name: SearchIndex
    patch_person: Callable[[int, GenPerson], None]
    patch_ascend: Callable[[int, GenAscend], None]
    patch_union: Callable[[int, GenUnion], None]
    patch_family: Callable[[int, GenFamily], None]
    patch_couple: Callable[[int, GenCouple], None]
    patch_descend: Callable[[int, GenDescend], None]
    insert_string: Callable[[str], int]
    commit_patches: Callable[[], None]
    nb_of_real_persons: Callable[[], int]
    iper_exists: Callable[[int], bool]
    ifam_exists: Callable[[int], bool]


class Base:
    """
    Main database class.
    Provides high-level interface for genealogical data access and modification.
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

    # ========================================================================
    # Basic Accessors
    # ========================================================================

    def sou(self, istr: int) -> str:
        """
        Get string from string ID.

        Args:
            istr: String ID

        Returns:
            String value
        """
        return self.data.strings.get(istr)

    def bname(self) -> str:
        """Get base name (directory name without extension)."""
        return Path(self.data.bdir).stem

    def nb_of_persons(self) -> int:
        """Get total number of person slots (including empty)."""
        return self.data.persons.length

    def nb_of_families(self) -> int:
        """Get total number of family slots (including empty)."""
        return self.data.families.length

    def nb_of_real_persons(self) -> int:
        """Get number of actual persons (excluding empty '? ?' entries)."""
        return self.func.nb_of_real_persons()

    # ========================================================================
    # Person and Family Construction
    # ========================================================================

    def poi(self, iper: int) -> Person:
        """
        Get person by ID.

        Args:
            iper: Person ID

        Returns:
            Person wrapper with lazy loading
        """
        return Person(self, iper)

    def foi(self, ifam: int) -> Family:
        """
        Get family by ID.

        Args:
            ifam: Family ID

        Returns:
            Family wrapper with lazy loading
        """
        return Family(self, ifam)

    def empty_person(self, iper: int) -> Person:
        """Create empty person with given ID."""
        return Person(self, iper)

    def empty_family(self, ifam: int) -> Family:
        """Create empty family with given ID."""
        return Family(self, ifam)

    def iper_exists(self, iper: int) -> bool:
        """Check if person ID exists in database."""
        return self.func.iper_exists(iper)

    def ifam_exists(self, ifam: int) -> bool:
        """Check if family ID exists in database."""
        return self.func.ifam_exists(ifam)

    # ========================================================================
    # Searching
    # ========================================================================

    def person_of_key(self, first_name: str, surname: str, occ: int) -> Optional[int]:
        """
        Find person by name key.

        Args:
            first_name: Person's first name
            surname: Person's surname
            occ: Occurrence number for disambiguation

        Returns:
            Person ID if found, None otherwise
        """
        return self.func.person_of_key(first_name, surname, occ)

    def persons_of_name(self, name: str) -> List[int]:
        """
        Find persons by full name.

        Args:
            name: Full name to search for

        Returns:
            List of matching person IDs
        """
        return self.func.persons_of_name(name)

    # ========================================================================
    # Modification Operations
    # ========================================================================

    def insert_string(self, s: str) -> int:
        """
        Insert or get string ID.

        Args:
            s: String to insert

        Returns:
            String ID (new or existing)
        """
        return self.func.insert_string(s)

    def patch_person(self, iper: int, person: GenPerson) -> None:
        """
        Modify or add person (pending until commit).

        Args:
            iper: Person ID
            person: New person data
        """
        self.func.patch_person(iper, person)

    def patch_family(self, ifam: int, family: GenFamily) -> None:
        """
        Modify or add family (pending until commit).

        Args:
            ifam: Family ID
            family: New family data
        """
        self.func.patch_family(ifam, family)

    def commit_patches(self) -> None:
        """Commit all pending modifications to disk."""
        self.func.commit_patches()

    def new_iper(self) -> int:
        """Allocate new person ID."""
        return self.data.persons.length

    def new_ifam(self) -> int:
        """Allocate new family ID."""
        return self.data.families.length

    # ========================================================================
    # Deletion Operations
    # ========================================================================

    def delete_person(self, iper: int) -> None:
        """
        Delete person (replace with empty '? ?' entry).

        Args:
            iper: Person ID to delete
        """
        empty = GenPerson(
            first_name=Istr.quest(),
            surname=Istr.quest(),
            occ=0,
            key_index=iper
        )
        self.patch_person(iper, empty)

    def delete_family(self, ifam: int) -> None:
        """
        Delete family (replace with dummy entry).

        Args:
            ifam: Family ID to delete
        """
        empty = GenFamily(fam_index=ifam)
        self.patch_family(ifam, empty)

    def delete_person_rec(self, iper: int) -> None:
        """
        Recursively delete person and related empty data.

        This function:
        - Deletes the person
        - Removes them from parent family
        - Deletes empty parent families
        - Deletes empty spouse families

        Args:
            iper: Person ID to delete recursively
        """
        self._delete_person_aux(set(), set(), iper)

    def _delete_person_aux(
        self,
        excluded_ipers: TSet[int],
        excluded_ifams: TSet[int],
        iper: int
    ) -> Tuple[TSet[int], TSet[int]]:
        """
        Internal recursive person deletion.

        Args:
            excluded_ipers: Already processed person IDs
            excluded_ifams: Already processed family IDs
            iper: Person ID to delete

        Returns:
            Updated exclusion sets
        """
        if Iper.is_dummy(iper) or iper in excluded_ipers:
            raise ValueError(f"Cannot delete person {iper}: already in exclusion list")

        person = self.poi(iper)
        ascend = person.gen_ascend()

        # Handle parent family
        ipers_to_delete = []
        ifams_to_delete = []

        if ascend.parents is not None:
            ifam = ascend.parents
            family = self.foi(ifam)

            # Remove person from children
            children = [c for c in family.get_children() if c != iper]
            self.func.patch_descend(ifam, GenDescend(children=children))

            # Check if parents are empty
            if len(children) == 1:
                father = family.get_father()
                mother = family.get_mother()
                if self._is_empty_person(father, ifam) and self._is_empty_person(mother, ifam):
                    ipers_to_delete.extend([father, mother])
                    ifams_to_delete.append(ifam)

        # Handle spouse families
        union = person.gen_union()
        should_delete = True

        for ifam in union.family:
            family = self.foi(ifam)
            children = family.get_children()

            if len(children) > 1:
                should_delete = False
            else:
                father = family.get_father()
                mother = family.get_mother()
                spouse = mother if father == iper else father

                if spouse in excluded_ipers:
                    continue
                elif self._is_empty_person(spouse, ifam):
                    ipers_to_delete.append(spouse)
                    ifams_to_delete.append(ifam)
                else:
                    should_delete = False

        # Delete or mark as empty
        if should_delete:
            self.delete_person(iper)
        else:
            # Mark as "? ?" but keep structure
            empty = GenPerson(
                first_name=Istr.quest(),
                surname=Istr.quest(),
                occ=0,
                key_index=iper
            )
            self.patch_person(iper, empty)

        if should_delete:
            excluded_ipers.add(iper)

        # Recursively delete related persons and families
        for ip in ipers_to_delete:
            excluded_ipers, excluded_ifams = self._delete_person_aux(
                excluded_ipers, excluded_ifams, ip
            )

        for ifam in ifams_to_delete:
            excluded_ipers, excluded_ifams = self._delete_family_aux(
                excluded_ipers, excluded_ifams, ifam
            )

        return excluded_ipers, excluded_ifams

    def _delete_family_aux(
        self,
        excluded_ipers: TSet[int],
        excluded_ifams: TSet[int],
        ifam: int
    ) -> Tuple[TSet[int], TSet[int]]:
        """
        Internal recursive family deletion.

        Args:
            excluded_ipers: Already processed person IDs
            excluded_ifams: Already processed family IDs
            ifam: Family ID to delete

        Returns:
            Updated exclusion sets
        """
        if Ifam.is_dummy(ifam) or ifam in excluded_ifams:
            raise ValueError(f"Cannot delete family {ifam}: already in exclusion list")

        family = self.foi(ifam)
        father = family.get_father()
        mother = family.get_mother()
        children = family.get_children()

        # Remove family from parents' unions
        self._remove_union(father, ifam)
        self._remove_union(mother, ifam)

        # Clear children's ascendants
        for child_iper in children:
            self.func.patch_ascend(child_iper, GenAscend())

        # Delete family structures
        self.delete_family(ifam)
        self.func.patch_couple(ifam, GenCouple(Iper.dummy(), Iper.dummy()))
        self.func.patch_descend(ifam, GenDescend())

        excluded_ifams.add(ifam)

        # Recursively delete empty persons
        for person_id in [father, mother] + children:
            if person_id not in excluded_ipers and self._is_empty_person(person_id):
                excluded_ipers, excluded_ifams = self._delete_person_aux(
                    excluded_ipers, excluded_ifams, person_id
                )

        return excluded_ipers, excluded_ifams

    def _is_empty_person(self, iper: int, exclude_ifam: Optional[int] = None) -> bool:
        """
        Check if person is effectively empty.

        Args:
            iper: Person ID
            exclude_ifam: Family ID to exclude from union check

        Returns:
            True if person has no real data
        """
        person = self.poi(iper)
        ascend = person.gen_ascend()
        union = person.gen_union()
        gen_person = person.gen_person()

        # Check ascendants
        if ascend.parents is not None:
            return False

        # Check unions
        expected_union = [exclude_ifam] if exclude_ifam is not None else []
        if union.family != expected_union:
            return False

        # Check if person data is empty
        return (gen_person.first_name == Istr.quest() and
                gen_person.surname == Istr.quest())

    def _remove_union(self, iper: int, ifam: int) -> None:
        """
        Remove family from person's union list.

        Args:
            iper: Person ID
            ifam: Family ID to remove
        """
        person = self.poi(iper)
        union = person.gen_union()
        new_families = [f for f in union.family if f != ifam]
        self.func.patch_union(iper, GenUnion(family=new_families))

    # ========================================================================
    # Insertion with Auto-allocation
    # ========================================================================

    def insert_person_with_union_and_ascendants(
        self,
        person: GenPerson,
        ascend: GenAscend,
        union: GenUnion
    ) -> int:
        """
        Insert new person with all related data.

        Args:
            person: Person data (key_index should be dummy)
            ascend: Ascendant data
            union: Union data

        Returns:
            Allocated person ID
        """
        iper = self.new_iper()
        person = replace(person, key_index=iper)

        self.func.patch_ascend(iper, ascend)
        self.func.patch_union(iper, union)
        self.patch_person(iper, person)

        return iper

    def insert_family_with_couple_and_descendants(
        self,
        family: GenFamily,
        couple: GenCouple,
        descend: GenDescend
    ) -> int:
        """
        Insert new family with all related data.

        Args:
            family: Family data (fam_index should be dummy)
            couple: Couple data
            descend: Descendant data

        Returns:
            Allocated family ID
        """
        ifam = self.new_ifam()
        family = replace(family, fam_index=ifam)

        self.patch_family(ifam, family)
        self.func.patch_couple(ifam, couple)
        self.func.patch_descend(ifam, descend)

        return ifam

    # ========================================================================
    # Collections
    # ========================================================================

    def persons(self) -> Collection[Person]:
        """
        Get collection of all persons.

        Returns:
            Collection that lazily loads persons
        """
        return Collection(
            self.nb_of_persons(),
            lambda i: self.poi(i) if 0 <= i < self.nb_of_persons() else None
        )

    def ipers(self) -> Collection[int]:
        """
        Get collection of all person IDs.

        Returns:
            Collection of integer person IDs
        """
        return Collection(
            self.nb_of_persons(),
            lambda i: i if 0 <= i < self.nb_of_persons() else None
        )

    def families(self, select: Optional[Callable[[Family], bool]] = None) -> Collection[Family]:
        """
        Get collection of families.

        Args:
            select: Optional filter function

        Returns:
            Filtered collection of families
        """
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
        """
        Get collection of family IDs.

        Args:
            select: Optional filter function

        Returns:
            Filtered collection of family IDs
        """
        def get_ifam(i: int) -> Optional[int]:
            if i < 0 or i >= self.nb_of_families():
                return None
            fam = self.foi(i)
            if Ifam.is_dummy(fam.ifam):
                return None
            if select is not None and not select(i):
                return None
            return i

        return Collection(self.nb_of_families(), get_ifam)

    # ========================================================================
    # Markers
    # ========================================================================

    @staticmethod
    def iper_marker(collection: Collection[int], initial: T) -> Marker[int, T]:
        """
        Create marker for person IDs.

        Args:
            collection: Collection of person IDs
            initial: Initial marker value

        Returns:
            Marker for annotating persons
        """
        return Marker(lambda i: i, collection, initial)

    @staticmethod
    def ifam_marker(collection: Collection[int], initial: T) -> Marker[int, T]:
        """
        Create marker for family IDs.

        Args:
            collection: Collection of family IDs
            initial: Initial marker value

        Returns:
            Marker for annotating families
        """
        return Marker(lambda i: i, collection, initial)

    # ========================================================================
    # Utility Functions
    # ========================================================================

    def p_first_name(self, person: Person) -> str:
        """
        Get person's first name as string (nominative form).

        Args:
            person: Person object

        Returns:
            First name string
        """
        istr = person.get_first_name()
        return self.sou(istr)

    def p_surname(self, person: Person) -> str:
        """
        Get person's surname as string (nominative form).

        Args:
            person: Person object

        Returns:
            Surname string
        """
        istr = person.get_surname()
        return self.sou(istr)

    def children_of_person(self, person: Person) -> List[int]:
        """
        Get all children of a person across all families.

        Args:
            person: Person object

        Returns:
            List of children person IDs
        """
        children = []
        for ifam in person.get_family():
            family = self.foi(ifam)
            children.extend(family.get_children())
        return children


# ============================================================================
# Utility Functions
# ============================================================================

class NameUtils:
    """Utilities for name processing and comparison."""

    @staticmethod
    def normalize(name: str) -> str:
        """
        Normalize name for comparison.

        Args:
            name: Input name

        Returns:
            Normalized lowercase name
        """
        return name.lower().strip()

    @staticmethod
    def crush_lower(name: str) -> str:
        """
        Apply "crushing" normalization (remove accents, special chars).

        Args:
            name: Input name

        Returns:
            Crushed lowercase name
        """
        # Remove common accents and normalize
        normalized = name.lower()
        # Remove non-alphanumeric except spaces
        crushed = re.sub(r'[^a-z0-9\s]', '', normalized)
        return crushed.strip()

    @staticmethod
    def split_fname(name: str) -> List[str]:
        """
        Split first name into components.

        Args:
            name: First name

        Returns:
            List of name parts
        """
        return [part.strip() for part in name.split() if part.strip()]

    @staticmethod
    def split_sname(name: str) -> List[str]:
        """
        Split surname into components.

        Args:
            name: Surname

        Returns:
            List of surname parts
        """
        return [part.strip() for part in name.split() if part.strip()]

    @staticmethod
    def name_index(name: str, table_size: int = 16384) -> int:
        """
        Calculate hash index for name.

        Args:
            name: Name to hash
            table_size: Size of hash table

        Returns:
            Hash index in range [0, table_size)
        """
        h = hashlib.md5(NameUtils.crush_lower(name).encode()).digest()
        return int.from_bytes(h[:4], 'little') % table_size


# ============================================================================
# Example Usage and Tests
# ============================================================================

def example_usage():
    """Demonstrate usage of the genealogical database system."""

    print("=== GeneWeb Python Implementation Demo ===\n")

    # Example 1: Working with IDs
    print("1. Working with indexed IDs:")
    iper1 = 42
    print(f"  Person ID: {Iper.to_string(iper1)}")
    print(f"  Is dummy? {Iper.is_dummy(iper1)}")
    print(f"  Dummy ID: {Iper.dummy()}")

    istr1 = Istr.empty()
    print(f"  Empty string ID: {istr1}")
    print(f"  Is empty? {Istr.is_empty(istr1)}")

    # Example 2: Creating person data
    print("\n2. Creating person data:")
    family =  GenFamily(
        relation=RelationKind.MARRIED,
        marriage=Date(year=2005, month=6, day=20),
        fam_index=50
    )
    couple = GenCouple(father=100, mother=101)
    descend = GenDescend(children=[102, 103])
    print(f"  Family: {family.fam_index}, relation={family.relation.value}")
    print(f"  Couple: father={couple.father}, mother={couple.mother}")
    print(f"  Children: {descend.children}")

    person = GenPerson(
        first_name=10,  # String ID for "John"
        surname=20,     # String ID for "Smith"
        occ=0,
        sex=Sex.MALE,
        birth=Date(year=1980, month=5, day=15),
        key_index=100
    )
    print(f"  Person: {person.first_name=}, {person.surname=}, {person.occ=}")
    print(f"  Birth: {person.birth.year}-{person.birth.month}-{person.birth.day}")

    # Example 3: Creating family data
    print("\n3. Creating family data:")
    family = GenFamily(
        relation=RelationKind.MARRIED,
        marriage=Date(year=2005, month=6, day=20),
        fam_index=50
    )
    couple = GenCouple(father=100, mother=101)
    descend = GenDescend(children=[102, 103])
    print(f"  Family: {family.fam_index}, relation={family.relation.value}")
    print(f"  Couple: father={couple.father}, mother={couple.mother}")
    print(f"  Children: {descend.children}")

    # Example 4: Collections
    print("\n4. Working with collections:")
    data = [1, 2, 3, 4, 5]
    collection = Collection(len(data), lambda i: data[i] if 0 <= i < len(data) else None)

    # Map operation
    squared = collection.map(lambda x: x ** 2)
    print("  Squared values:", end=" ")
    squared.iter(lambda x: print(x, end=" "))
    print()

    # Fold operation
    total = collection.fold(lambda acc, x: acc + x, 0)
    print(f"  Sum: {total}")

    # Example 5: Markers
    print("\n5. Using markers:")
    marker = Marker(lambda x: x, collection, False)
    marker.set(0, True)
    marker.set(2, True)
    print(f"  Marker[0]: {marker.get(0)}")
    print(f"  Marker[1]: {marker.get(1)}")
    print(f"  Marker[2]: {marker.get(2)}")

    # Example 6: Name utilities
    print("\n6. Name utilities:")
    name = "Jean-François"
    print(f"  Original: {name}")
    print(f"  Normalized: {NameUtils.normalize(name)}")
    print(f"  Crushed: {NameUtils.crush_lower(name)}")
    print(f"  Split: {NameUtils.split_fname(name)}")
    print(f"  Hash index: {NameUtils.name_index(name)}")


if __name__ == "__main__":
    example_usage()
