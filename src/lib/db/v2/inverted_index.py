import dataclasses
import logging
import pathlib
from typing import Protocol, Dict, List, Optional

from lib.db.unmarshall.v2.dbdisk import BaseVersion
from lib.db.unmarshall.v2.ocaml_input import OCamlInput
from lib.db.v2 import mutil


class StringIndex(Protocol):
    """Protocol defining string indexing requirements"""

    def hash(self, s: str) -> int: ...
    def equal(self, s1: str, s2: str) -> bool: ...
    def string_of_id(self, id: int) -> str: ...


class InvertedIndex:
    class ConflictError(RuntimeError):
        """Exception raised when a conflict between indexes is detected."""

        def __init__(self, s: str, i: int, j: int):
            super().__init__(f"Conflict for string '{s}' with IDs {i} and {j}")

    @dataclasses.dataclass
    class INX:
        fl: str
        start: int
        size: int

    @property
    def inx(self):
        return self._inx

    @property
    def tbl(self):
        return self._tbl

    """Inverted index for string lookups"""

    def __init__(
        self, string_accessor: StringIndex, *, logger: Optional[logging.Logger] = None
    ):
        self._inx: Optional[InvertedIndex.INX] = None
        self._tbl: Dict[int, str] = {}
        self.string_accessor = string_accessor
        self.int_size = 4
        self.logger = (
            logger.getChild("InvertedIndex") if logger else mutil.get_logger(__name__)
        )

    def _load_inx(self, version: BaseVersion, inx: pathlib.Path):
        with open(inx, "rb") as ic:
            start = 0
            if version in (
                BaseVersion.GnWb0024,
                BaseVersion.GnWb0023,
                BaseVersion.GnWb0022,
            ):
                start = self.int_size
            elif version in (BaseVersion.GnWb0021, BaseVersion.GnWb0020):
                start = 3 * self.int_size
            oi = OCamlInput(ic)
            size = oi.read_int()
            oi.read_int()
            oi.read_int()
            return InvertedIndex.INX(str(inx), start, size)

    def load(
        self, version: BaseVersion, inx: pathlib.Path, indexes: List[Dict[int, str]]
    ) -> "InvertedIndex":
        """Load index from file or create new"""
        if inx.exists():
            self._inx = self._load_inx(version, inx)
        else:
            # Build from indexes
            self.logger.warning(f"Cannot load the inverted index file {inx}")
        self._tbl = {}
        for idx in indexes:
            for index, string in idx.items():
                self.insert(string, index)
        self.logger.debug(
            f"Loaded inverted index from {self._inx} with {len(self._tbl)} entries"
        )
        self.logger.debug(f"Index entries: {self._tbl}")
        return self

    def find(self, s: str) -> int:
        """Find ID for string"""
        try:
            return self.index[s]
        except KeyError as e:
            if not self.inx:
                raise e
            with open(self.inx.fl, "rb") as ic:
                oi = OCamlInput(ic)
                h = self.string_accessor.hash(s) % self.inx.size
                ic.seek(self.inx.start + h * self.int_size)

                def loop(i: int) -> int:
                    if i == -1:
                        raise KeyError(s)
                    if self.string_accessor.equal(
                        self.string_accessor.string_of_id(i), s
                    ):
                        return i
                    ic.seek(self.inx.start + ((self.inx.size + i) * self.int_size))
                    return loop(oi.read_int())

                return loop(oi.read_int())

    def insert(self, s: str, i: int) -> None:
        """Insert new string"""
        try:
            self._tbl[s]
            raise InvertedIndex.ConflictError(s, self._tbl[s], i)
        except KeyError:
            self._tbl[s] = i
