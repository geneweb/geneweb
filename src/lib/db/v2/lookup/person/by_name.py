from dataclasses import dataclass
from typing import List, Dict, Optional
from pathlib import Path

from lib.db.unmarshall.v2.intern_rec import convert_structure, read_bin_caml_input_rec
from lib.db.unmarshall.v2.ocaml_input import OCamlInput
from lib.db.v2 import dutil as Dutil, mutil as Mutil
from lib.db.unmarshall.v2 import iovalue as Iovalue


@dataclass
class ByName:
    """Manages person lookup by name"""

    bname: Path
    patches: Dict[int, List[int]]
    index_cache: Optional[List[List[int]]] = None  # Cache for name index data

    def __call__(self, name: str) -> List[int]:
        """
        Find person IDs by name.

        Args:
            name: Full name to search

        Returns:
            List of person IDs matching name
        """
        # Calculate name index
        idx = Dutil.name_index(name)

        # Get array of IDs from index file
        person_ids = self._get_person_ids(idx)

        # Check patches
        try:
            patch_ids = self.patches[idx]
            # Add patched IDs that aren't already in array
            return [pid for pid in patch_ids if pid not in person_ids] + person_ids
        except KeyError:
            return person_ids

    def _get_person_ids(self, idx: int) -> List[int]:
        """Get person IDs from index files"""
        # Try using direct access file first
        acc_path = self.bname / "names.acc"
        idx_path = self.bname / "names.inx"
        if acc_path.exists():
            with open(acc_path, "rb") as acc_file:
                oi = OCamlInput(acc_file)
                # Seek to position and read array position
                acc_file.seek(Iovalue.WORD_SIZE * idx)
                pos = oi.read_int()

                # Read array from index file at position
                with open(idx_path, "rb") as idx_file:
                    idx_file.seek(pos)
                    return read_bin_caml_input_rec(OCamlInput(idx_file))

        # Fall back to cached full index
        with open(idx_path, "rb") as f:
            if not self.index_cache:
                f.seek(Iovalue.WORD_SIZE)  # Skip header
                tmp = read_bin_caml_input_rec(OCamlInput(f))
                tmp = convert_structure(
                    tmp, List[List[int]], logger=Mutil.get_logger("lookup.ByName")
                )
                self.index_cache = tmp
            return self.index_cache[idx]
