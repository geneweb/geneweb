import dataclasses
from enum import Enum
import enum
import functools
from http import HTTPStatus
import logging
import os
from pathlib import Path
import re
from lib.db.unmarshall.v2.intern_rec import (
    read_bin_caml_input_rec,
    unmarshall_ocaml_data,
)
from lib.db.unmarshall.v2.ocaml_input import OCamlInput
from lib.db.v2 import mutil as Mutil
from typing import Any, Callable, Generic, List, Optional, Tuple, TypeVar

from lib.db.v2.defs import (
    Ascend,
    BaseNotes,
    Couple,
    Descend,
    DskUnion,
    Family,
    Person,
    HttpError,
)

T = TypeVar("T")


@dataclasses.dataclass
class RecordAccess(Generic[T]):
    """Record access with patches and pending changes support"""

    len: int

    @staticmethod
    def create(
        load_array: Callable[[], None],
        get: Callable[[int], T],
        get_nopending: Callable[[int], T],
        len: int,
        output_array: Callable[[Any], None],  # file object -> None
        clear_array: Callable[[], None],
    ) -> "RecordAccess":
        ra = RecordAccess(len)
        ra.load_array = load_array
        ra.get = get
        ra.get_nopending = get_nopending
        ra.output_array = output_array
        ra.clear_array = clear_array
        return ra

    def load_array(self) -> None:
        """Load the array from the database."""
        raise NotImplementedError

    def get(self, i: int, *, safe: bool = False) -> T:
        """Get the record at index i, applying any pending patches."""
        raise NotImplementedError

    def get_nopending(self, i: int) -> T:
        """Get the record at index i without applying pending patches."""
        raise NotImplementedError

    def output_array(self, f: Any) -> None:
        """Output the array to the given file object."""
        raise NotImplementedError

    def clear_array(self) -> None:
        """Clear the array and any pending patches."""
        raise NotImplementedError


class Permission(Enum):
    """Database access permissions"""

    RDONLY = True
    RDRW = False


class VisibleState(enum.Enum):
    VsNone = 0
    VsTrue = 1
    VsFalse = 2


@dataclasses.dataclass
class VisibleRecordAccess:
    """Access interface for visibility records"""

    read_only: bool
    bdir: str
    persons: RecordAccess[Person]
    visible: Optional[List[VisibleState]] = dataclasses.field(init=False)
    fname: str = dataclasses.field(init=False)

    def __post_init__(self):
        self.visible = None
        self.fname = os.path.join(self.bdir, "restrict")
        self.logger = Mutil.get_logger("VisibleRecordAccess")

    def write(self):
        if not self.visible:
            return
        if self.read_only:
            raise HttpError(HTTPStatus.FORBIDDEN)
        with open(self.fname, "w") as f:
            self.logger.debug("*** write restrict file")
            # f.write("".join(str(v.value) + "\n" for v in self.visible))
            self.logger.warning("writing to db is not implemented yet")

    def get(self, check_func: Callable[[Person], bool], iper: int) -> bool:
        """
        Get visibility status for person.

        Args:
            check_func: Function to check visibility
            iper: Person ID to check

        Returns:
            True if person is visible, False otherwise
        """
        if self.visible and self.visible[iper] != VisibleState.VsNone:
            return self.visible
        else:  # read or create visible
            try:
                with open(self.fname, "r") as f:
                    self.logger.debug("*** read restrict file")
                    oi = OCamlInput(f)
                    self.visible = unmarshall_ocaml_data(
                        oi, magic=None, structure=VisibleState
                    )
            except FileNotFoundError:
                self.logger.debug("*** restrict file not found")
                self.visible = [VisibleState.VsNone] * (self.persons.len + 1)

        if len(self.visible) >= iper:
            return check_func(self.persons.get(iper))
        match self.visible[iper]:
            case VisibleState.VsNone:
                status = check_func(self.persons.get(iper))
                self.visible[iper] = (
                    VisibleState.VsTrue if status else VisibleState.VsFalse
                )
                return status
            case VisibleState.VsTrue:
                return True
            case VisibleState.VsFalse:
                return False


@dataclasses.dataclass
class BaseData:
    """Database core data structure"""

    persons: RecordAccess[Person]
    ascends: RecordAccess[Ascend]
    unions: RecordAccess[DskUnion]
    visible: VisibleRecordAccess
    families: RecordAccess[Family]
    couples: RecordAccess[Couple]
    descends: RecordAccess[Descend]
    strings: RecordAccess[str]
    particles_txt: List[str]
    particles: re.Pattern
    bnotes: BaseNotes
    bdir: str
    perm: Permission


@dataclasses.dataclass
class BaseFunc:
    """Database operations interface"""

    persons_of_surname: "StringPersonIndex"
    persons_of_first_name: "StringPersonIndex"

    @staticmethod
    def build(
        person_of_key: Callable[[str, str, int], Optional[int]],
        persons_of_name: Callable[[str], List[int]],
        strings_of_sname: Callable[[str], List[int]],
        strings_of_fname: Callable[[str], List[int]],
        persons_of_surname: "StringPersonIndex",
        persons_of_first_name: "StringPersonIndex",
        patch_person: Callable[[int, Person], None],
        patch_ascend: Callable[[int, Ascend], None],
        patch_union: Callable[[int, DskUnion], None],
        patch_family: Callable[[int, Family], None],
        patch_couple: Callable[[int, Couple], None],
        patch_descend: Callable[[int, Descend], None],
        patch_name: Callable[[str, int], None],
        insert_string: Callable[[str], int],
        commit_patches: Callable[[], None],
        commit_notes: Callable[[], None],
        commit_wiznotes: Callable[[], None],
        nb_of_real_persons: Callable[[], int],
        iper_exists: Callable[[int], bool],
        ifam_exists: Callable[[int], bool],
    ) -> "BaseFunc":
        bf = BaseFunc(persons_of_surname, persons_of_first_name)
        bf.person_of_key = person_of_key
        bf.persons_of_name = persons_of_name
        bf.strings_of_sname = strings_of_sname
        bf.strings_of_fname = strings_of_fname
        bf.patch_person = patch_person
        bf.patch_ascend = patch_ascend
        bf.patch_union = patch_union
        bf.patch_family = patch_family
        bf.patch_couple = patch_couple
        bf.patch_descend = patch_descend
        bf.patch_name = patch_name
        bf.insert_string = insert_string
        bf.commit_patches = commit_patches
        bf.commit_notes = commit_notes
        bf.commit_wiznotes = commit_wiznotes
        bf.nb_of_real_persons = nb_of_real_persons
        bf.iper_exists = iper_exists
        bf.ifam_exists = ifam_exists
        return bf

    def person_of_key(self, first_name: str, surname: str, occ: int) -> Optional[int]:
        """Find person ID by first name, surname, and occurrence."""
        ...

    # person_of_key: Callable[[str, str, int], Optional[int]]
    def persons_of_name(self, name: str) -> List[int]:
        """Find person IDs by full name."""
        ...

    # persons_of_name: Callable[[str], List[int]]
    def strings_of_sname(self, surname: str) -> List[int]:
        """Find person IDs by surname."""
        ...

    # strings_of_sname: Callable[[str], List[int]]
    def strings_of_fname(self, first_name: str) -> List[int]:
        """Find person IDs by first name."""
        ...

    # persons_of_first_name: StringPersonIndex
    def patch_person(self, iper: int, p: Person) -> None:
        """Patch person record."""
        ...

    # patch_person: Callable[[int, Person], None]
    def patch_ascend(self, iasc: int, a: Ascend) -> None:
        """Patch ascend record."""
        ...

    # patch_ascend: Callable[[int, Ascend], None]
    def patch_union(self, iunion: int, u: DskUnion) -> None:
        """Patch union record."""
        ...

    # patch_union: Callable[[int, DskUnion], None]
    def patch_family(self, ifam: int, f: Family) -> None:
        """Patch family record."""
        ...

    # patch_family: Callable[[int, Family], None]
    def patch_couple(self, icouple: int, c: Couple) -> None:
        """Patch couple record."""
        ...

    # patch_couple: Callable[[int, Couple], None]
    def patch_descend(self, idescend: int, d: Descend) -> None:
        """Patch descend record."""
        ...

    # patch_descend: Callable[[int, Descend], None]
    def patch_name(self, name: str, iper: int) -> None:
        """Patch name record."""
        ...

    def insert_string(self, s: str) -> int:
        """Insert string into database."""
        ...

    def commit_patches(self) -> None:
        """Commit all patches."""
        ...

    def commit_notes(self, title: str, content: str) -> None:
        """Commit a note."""
        ...

    def commit_wiznotes(self, title: str, content: str) -> None:
        """Commit a wizard note."""
        ...

    def nb_of_real_persons(self) -> int:
        """Get number of real persons."""
        ...

    def iper_exists(self, iper: int) -> bool:
        """Check if person ID exists."""
        ...

    # iper_exists: Callable[[int], bool]
    def ifam_exists(self, ifam: int) -> bool:
        """Check if family ID exists."""
        ...

    # ifam_exists: Callable[[int], bool]


class BaseVersion(enum.Enum):
    GnWb0020 = 0
    GnWb0021 = 1
    GnWb0022 = 2
    GnWb0023 = 3
    GnWb0024 = 4


@dataclasses.dataclass
class DskBase:
    """Main database container"""

    data: BaseData
    func: BaseFunc
    version: BaseVersion


@dataclasses.dataclass
class StringPersonIndex:
    """Index for person lookup by string"""

    _cmp_str: Callable[[BaseData, str, str], int]
    _cmp_istr: Callable[[BaseData, int, int], int]
    _base_data: BaseData
    _proj: Callable[[Any], int]
    _person_patches: dict[int, Any]
    _names_inx: str
    _names_dat: str
    _bpath: Path
    _fname_data: str = dataclasses.field(init=False)
    # _bt: List[Tuple[int, int]] = dataclasses.field(init=False, default_factory=list)
    # _patched_cache: List[Tuple[int, List[Any]]] = dataclasses.field(
    #     init=False, default_factory=list
    # )
    logger: logging.Logger = dataclasses.field(kw_only=True, default=None)

    @functools.cached_property
    def _bt(self) -> List[Tuple[int, int]]:
        """B-tree index, loading from file if necessary."""
        with open(self._bpath / self._names_inx, "rb") as f:
            oi = OCamlInput(f)
            r = read_bin_caml_input_rec(oi, structure=List[Tuple[int, int]])
            self.logger.debug(f"Loaded B-tree index with {len(r)} entries")
            return r

    @functools.cached_property
    def _patched(self):
        """
        Lazy computation of patched entries for cursor/next operations.

        Returns:
            Sorted array of (string_id, person_list) tuples
        """
        # if self._patched_cache is not None:
        #     return self._patched_cache

        # Create hash table to collect unique string IDs
        string_ids: dict[int, list[Person]] = {}

        for person_id, person in self._person_patches.items():
            k = self._proj(person)  # Extract string ID (surname/firstname)
            if k not in string_ids:
                string_ids[k] = []

        # Convert to array format: (string_id, empty_list)
        # Note: The list is empty because patched is only used by cursor/next,
        # not by find, so we don't need to track person IDs here
        result = [(k, v) for k, v in string_ids.items()]
        # Sort by string comparison
        result.sort(
            key=functools.cmp_to_key(
                lambda a, b: self._cmp_istr(self._base_data, a[0], b[0])
            ),
        )
        self.logger.debug(f"Patched index entries: {result}")
        # self._patched_cache = result
        return result

    def __post_init__(self):
        if not self.logger:
            self.logger = Mutil.get_logger("StringPersonIndex")
        else:
            self.logger = Mutil.get_child_logger(self.logger, "StringPersonIndex")
        self._fname_data = self._bpath / self._names_dat

    def find(self, idx: int) -> List[int]:
        """Find person IDs by index."""
        ipera = []

        try:
            bt = self._bt
            s = self._base_data.strings.get(idx)

            def cmp(item: tuple[int, Any]) -> int:
                k, _ = item
                if k == idx:
                    return 0
                return self._cmp_str(self._base_data, s, self._base_data.strings.get(k))

            # Binary search for the string in the index
            pos = bt[self._binary_search(bt, cmp)][1]

            if pos is not None:
                # Read person IDs from data file
                with open(self._fname_data, "rb") as ic_dat:
                    ic_dat.seek(pos)
                    oi = OCamlInput(ic_dat)
                    length = oi.read_int()
                    ipera = []
                    for _ in range(length):
                        iper = oi.read_int()
                        ipera.append(iper)

        except (FileNotFoundError, IndexError):
            # If not found or error, start with empty list
            self.logger.warning("Index or data file not found or error reading")
            ipera = []

        # Get list of patched person IDs to exclude from main results
        patched_ids = list(self._person_patches.keys())

        # Filter out patched persons from main results
        ipera = [i for i in ipera if i not in patched_ids]

        # Add matching persons from patches
        for person_id, person in self._person_patches.items():
            istr1 = self._proj(person)
            if istr1 == idx:
                if person_id not in ipera:
                    ipera.append(person_id)

        return ipera

    def cursor(self, s: str) -> int:
        """Get cursor position for string."""
        bt = self._bt
        patched = self._patched

        def cmp(x: Tuple[int, Any]) -> int:
            k, _ = x
            return self._cmp_str(
                self._base_data,
                s,
                self._base_data.strings.get(k),
            )

        try:  # Binary search in main index
            istr1 = bt[self._binary_search_key_after(bt, cmp)][0]
        except KeyError:
            self.logger.debug("Cursor: not found in main index")
            istr1 = -1
        try:  # Binary search in patched index
            istr2 = patched[self._binary_search_key_after(patched, cmp)][0]
        except KeyError:
            self.logger.debug("Cursor: not found in patched index")
            istr2 = -1
        # Return the lexicographically smallest
        if istr2 == -1:
            if istr1 == -1:
                raise KeyError("Not found")
            return istr1
        if istr1 == -1:
            return istr2
        if istr1 == istr2:
            return istr1

        # Compare strings to find smaller one
        str1 = self._base_data.strings.get(istr1)
        str2 = self._base_data.strings.get(istr2)
        if self._cmp_str(self._base_data, str1, str2) < 0:
            return istr1
        return istr2

    def next(self, idx: int) -> int:
        """Get next index position."""
        bt = self._bt
        patched = self._patched
        s = self._base_data.strings.get(idx)

        def cmp(x: Tuple[int, Any]) -> int:
            k, _ = x
            if k == idx:
                return 0
            return self._cmp_str(
                self._base_data,
                s,
                self._base_data.strings.get(k),
            )

        try:  # Binary search in main index
            istr1 = bt[self._binary_search_next(bt, cmp)][0]
        except KeyError:
            istr1 = -1
        try:  # Binary search in patched index
            istr2 = patched[self._binary_search_next(patched, cmp)][0]
        except KeyError:
            istr2 = -1
        # Return the lexicographically smallest
        if istr2 == -1:
            if istr1 == -1:
                raise KeyError("Not found")
            return istr1
        if istr1 == -1:
            return istr2
        if istr1 == istr2:
            return istr1

        # Compare strings to find smaller one
        str1 = self._base_data.strings.get(istr1)
        str2 = self._base_data.strings.get(istr2)
        if self._cmp_str(self._base_data, str1, str2) < 0:
            return istr1
        return istr2

    class NotFound(KeyError):
        """Exception raised when binary search fails"""

        pass

    def _binary_search_key_after(self, arr: List[T], cmp: Callable[[T], int]) -> int:
        """
        Binary search for first element where cmp(element) <= 0.

        Args:
            arr: Sorted array to search
            cmp: Comparison function returning <0, 0, or >0

        Returns:
            Index of first element where cmp <= 0

        Raises:
            NotFound: If no such element exists
        """
        if not arr:
            raise StringPersonIndex.NotFound("Array is empty")

        logger = Mutil.get_child_logger(self.logger, "binary_search")
        logger.debug(f"Starting binary search in array of length {len(arr)} {arr}")

        def search(acc: Optional[int], low: int, high: int) -> int:
            if high <= low:
                logger.debug(f"Binary search range {low} to {high}, acc={acc}")
                logger.debug(f"Comparing with element {arr[low]}")
                if cmp(arr[low]) <= 0:
                    return low
                elif acc is None:
                    raise StringPersonIndex.NotFound("No element found")
                return acc
            else:
                mid = (low + high) // 2
                logger.debug(
                    f"Binary search range {low} to {high}, mid={mid}, acc={acc}"
                )
                logger.debug(f"Comparing with element {arr[mid]}")
                c = cmp(arr[mid])
                if c < 0:
                    return search(mid, low, mid - 1)
                elif c > 0:
                    return search(acc, mid + 1, high)
                else:
                    return mid

        return search(None, 0, len(arr) - 1)

    def _binary_search_next(self, arr: List[T], cmp: Callable[[T], int]) -> int:
        """
        Binary search for first element where cmp(element) < 0.

        Args:
            arr: Sorted array to search
            cmp: Comparison function returning <0, 0, or >0

        Returns:
            Index of first element where cmp < 0

        Raises:
            NotFound: If no such element exists
        """
        if not arr:
            raise StringPersonIndex.NotFound("Array is empty")

        def search(acc: Optional[int], low: int, high: int) -> int:
            if high <= low:
                if cmp(arr[low]) < 0:
                    return low
                elif acc is None:
                    raise StringPersonIndex.NotFound("No element found")
                return acc
            else:
                mid = (low + high) // 2
                c = cmp(arr[mid])
                if c < 0:
                    return search(mid, low, mid - 1)
                else:
                    return search(acc, mid + 1, high)

        return search(None, 0, len(arr) - 1)

    def _binary_search(self, arr: List[T], cmp: Callable[[T], int]) -> int:
        """
        Standard binary search for exact match.

        Args:
            arr: Sorted array to search
            cmp: Comparison function returning <0, 0, or >0

        Returns:
            Index of element where cmp == 0

        Raises:
            NotFound: If no exact match found
        """
        if not arr:
            raise StringPersonIndex.NotFound("Array is empty")

        def search(low: int, high: int) -> int:
            if high <= low:
                if cmp(arr[low]) == 0:
                    return low
                else:
                    raise StringPersonIndex.NotFound("Element not found")
            else:
                mid = (low + high) // 2
                c = cmp(arr[mid])
                if c < 0:
                    return search(low, mid - 1)
                elif c > 0:
                    return search(mid + 1, high)
                else:
                    return mid

        return search(0, len(arr) - 1)
