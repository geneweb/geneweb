"""
File reading utilities for GeneWeb database arrays.
"""

import os
import pickle
import struct
from pathlib import Path
from typing import Any, Callable, List, Tuple


class FileReader:
    """
    Handles reading database arrays from disk.
    """

    @staticmethod
    def input_binary_int(f) -> int:
        """
        Read 4-byte big-endian integer.
        """
        data = f.read(4)
        if len(data) != 4:
            raise EOFError("Cannot read binary int")
        return struct.unpack(">I", data)[0]

    @staticmethod
    def input_value(f) -> Any:
        """
        Read value from file (matches OCaml Marshal.from_channel).
        Uses pickle for Python compatibility.
        """
        return pickle.load(f)

    @staticmethod
    def load_array(
        filepath: str, default_factory: Callable[[], List[Any]]
    ) -> List[Any]:
        """
        Load array from file with fallback to default.

        Args:
            filepath: Path to array file
            default_factory: Function to create default array

        Returns:
            Loaded or default array
        """
        if os.path.exists(filepath):
            try:
                with open(filepath, "rb") as f:
                    return pickle.load(f)
            except (IOError, pickle.PickleError, EOFError):
                return default_factory()
        return default_factory()

    @staticmethod
    def load_strings(base_dir: str) -> List[str]:
        """
        Load string array with default entries.
        """
        filepath = Path(base_dir) / "strings.dat"
        strings = FileReader.load_array(str(filepath), list)

        # Ensure default strings exist (matches OCaml [""; "?"])
        if len(strings) < 2:
            strings = ["", "?"]  # Empty string and question mark
        elif len(strings) == 1:
            strings.append("?")

        return strings

    @staticmethod
    def load_particles(base_dir: str) -> List[str]:
        """
        Load particle list for name processing.
        """
        filepath = Path(base_dir) / "particles.dat"

        # Default particles (matches OCaml common particles)
        default_particles = [
            "de",
            "du",
            "des",
            "le",
            "la",
            "les",
            "d'",
            "von",
            "van",
            "der",
        ]

        return FileReader.load_array(str(filepath), lambda: default_particles)

    @staticmethod
    def read_strings_hash(filepath: str) -> Tuple[List[int], List[int]]:
        """
        Read string hash table.

        Returns:
            Tuple of (hash_array, link_array)
        """
        with open(filepath, "rb") as f:
            # Read table size
            table_size = FileReader.input_binary_int(f)

            # Read hash array
            hash_array = []
            for _ in range(table_size):
                val = FileReader.input_binary_int(f)
                # Convert to signed int if needed
                if val >= 0x80000000:
                    val = val - 0x100000000
                hash_array.append(val)

            # Read link array (if present)
            link_array = []
            try:
                while True:
                    val = FileReader.input_binary_int(f)
                    if val >= 0x80000000:
                        val = val - 0x100000000
                    link_array.append(val)
            except EOFError:
                pass

            return hash_array, link_array

    @staticmethod
    def read_name_index(index_path: str, data_path: str) -> List[Tuple[int, List[int]]]:
        """
        Read name index files.
        Args:
            index_path: Path to index file
            data_path: Path to data file
        Returns:
            List of (name_id, [person_ids])
        """
        # Read index file (offsets)
        with open(index_path, "rb") as f_idx:
            offsets = FileReader.input_value(f_idx)

        # Read data file (person lists)
        results = []
        with open(data_path, "rb") as f_dat:
            for name_id, offset in offsets:
                f_dat.seek(offset)
                count = FileReader.input_binary_int(f_dat)
                person_ids = []
                for _ in range(count):
                    iper = FileReader.input_binary_int(f_dat)
                    person_ids.append(iper)
                results.append((name_id, person_ids))

        return results
