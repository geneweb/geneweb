import functools
from dataclasses import dataclass
from pathlib import Path
from typing import Any, Callable, Dict, List, Tuple

from lib.old_db.unmarshall.v2 import iovalue as Iovalue
from lib.old_db.unmarshall.v2.dbdisk import RecordAccess
from lib.old_db.unmarshall.v2.intern_rec import read_bin_caml_input_rec
from lib.old_db.unmarshall.v2.ocaml_input import OCamlInput
from lib.old_db.unmarshall.v2.stdlib import Ref
from lib.old_db.v2 import dutil as Dutil
from lib.old_db.v2.defs import Person


@dataclass
class BaseByName:
    """Manages person lookup by name"""

    bname: Path
    strings: RecordAccess[str]
    h_persons: Tuple[Ref[int], Dict[int, Person]]

    def __call__(self, surname: str) -> List[int]:
        """
        Find person IDs by surname.

        Args:
            surname: Surname to search

        Returns:
            List of string
        """
        ...

    def _new_strings_of_fsname_aux(
        self,
        offset_acc: int,
        offset_inx: int,
        split: Callable[[str], List[str]],
        get: Callable[[Person], int],
        baname: str,
        strings: RecordAccess[str],
        person_patches: Tuple[Ref[int], Dict[int, Person]],
    ) -> Callable[[str], List[int]]:
        """Create a function to find person IDs by surname"""
        t: List[List[int]] = None

        def strings_of_fsname(surname: str) -> List[int]:
            i = Dutil.name_index(surname)
            r: List[int] = None
            with open(self.bname / "names.inx", "rb") as f_inx:
                oi_inx = OCamlInput(f_inx)
                fname_inx_acc = self.bname / "names.acc"
                if fname_inx_acc.exists():
                    with open(fname_inx_acc, "rb") as f_acc:
                        oi_acc = OCamlInput(f_acc)
                        f_acc.seek(
                            Iovalue.WORD_SIZE * (i + offset_acc * Dutil.TABLE_SIZE)
                        )
                        pos = oi_acc.read_int()
                        f_inx.seek(pos + offset_inx)
                    r = read_bin_caml_input_rec(OCamlInput(f_inx))
                else:
                    if t:
                        return t[i]
                    f_inx.seek(offset_inx)
                    pos = oi_inx.read_int()
                    f_inx.seek(pos)
                    a: List[List[int]] = read_bin_caml_input_rec(oi_inx)
                    t = a
                    r = a[i]

            def extract_relevant_surnames(acc: List[int], p: Any) -> List[int]:
                """Extracts relevant surname indices from a person's patch data and accumulates them.
                This function checks if the surname index (`istr`) obtained from the person patch `p`
                is not already present in the accumulator `acc`. It then splits the corresponding
                surname string and matches it against a given index `i` (assumed to be in scope).
                If a match is found, the surname index is prepended to the accumulator.

                Args:
                    acc (list): Accumulator list of surname indices.
                    p (Any): Person patch data from which to extract the surname.
                    list: Updated accumulator with relevant surname indices prepended if matched.

                Returns:
                    list: Updated accumulator with relevant surname indices prepended if matched.
                """
                istr = get(p)
                str_ = strings.get(istr)
                if istr not in acc:
                    match split(str_):
                        case [s]:
                            if i == Dutil.name_index(s):
                                return [istr] + acc
                        case list_:
                            if any(i == Dutil.name_index(s) for s in ([str_] + list_)):
                                return [istr] + acc
                return acc

            return functools.reduce(
                extract_relevant_surnames, person_patches, r if r else []
            )

        return strings_of_fsname


def setup_decorator(
    offset_acc: int,
    offset_inx: int,
    split: Callable[[str], List[str]],
    get: Callable[[Person], int],
):
    def wrapper(func):
        @functools.wraps(func)
        def decorated(self: "BaseByName", surname: str) -> List[int]:
            return self._new_strings_of_fsname_aux(
                offset_acc,
                offset_inx,
                split,
                get,
                baname=self.bname,
                strings=self.strings,
                person_patches=self.h_persons,
            )(surname)

        return decorated

    return wrapper
