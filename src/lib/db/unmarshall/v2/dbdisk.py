import dataclasses
from enum import Enum
import enum
from http import HTTPStatus
import os
import re
from urllib.error import HTTPError
from lib.db.unmarshall.v2.intern_rec import unmarshall_ocaml_data
from lib.db.unmarshall.v2.ocaml_input import OCamlInput
from lib.db.v2 import mutil as Mutil
from typing import Any, Callable, Generic, List, Optional, TypeVar

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


@dataclasses.dataclass
class StringPersonIndex:
    """Index for person lookup by string"""

    find: Callable[[int], List[int]]
    cursor: Callable[[str], int]
    next: Callable[[int], int]


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


# @dataclasses.dataclass
class BaseFunc:
    """Database operations interface"""

    @staticmethod
    def build(
        person_of_key: Callable[[str, str, int], Optional[int]],
        persons_of_name: Callable[[str], List[int]],
        strings_of_sname: Callable[[str], List[int]],
        strings_of_fname: Callable[[str], List[int]],
        persons_of_surname: StringPersonIndex,
        persons_of_first_name: StringPersonIndex,
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
        bf = BaseFunc()
        bf.person_of_key = person_of_key
        bf.persons_of_name = persons_of_name
        bf.strings_of_sname = strings_of_sname
        bf.strings_of_fname = strings_of_fname
        bf.persons_of_surname = persons_of_surname
        bf.persons_of_first_name = persons_of_first_name
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

    # strings_of_fname: Callable[[str], List[int]]
    def persons_of_surname(self, surname: str) -> List[int]:
        """Find person IDs by surname."""
        ...

    # persons_of_surname: StringPersonIndex
    def persons_of_first_name(self, first_name: str) -> List[int]:
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
