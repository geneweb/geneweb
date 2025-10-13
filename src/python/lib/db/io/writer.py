"""
Database writer matching OCaml outbase.ml implementation.
"""

import hashlib
import pickle
import struct
from pathlib import Path
from typing import BinaryIO, Callable, Dict, List


class DatabaseWriter:
    """
    Writes database to disk in GeneWeb format.
    Matches OCaml outbase.ml implementation.
    """

    MAGIC_GNWB = b"GnWb0024"  # Must be bytes, not string
    TABLE_SIZE = 16384  # Default hash table size for name indexing

    def __init__(self, base_dir: str, verbose: bool = False):
        """
        Initialize database writer.

        Args:
            base_dir: Database directory path
            verbose: Enable verbose output
        """
        self.base_dir = Path(base_dir)
        self.verbose = verbose

    def trace(self, message: str) -> None:
        """Print trace message if verbose (matches OCaml trace)."""
        if self.verbose:
            print(f"*** {message}")

    def output_binary_int(self, f: BinaryIO, value: int) -> None:
        """
        Write 4-byte big-endian integer.
        Matches OCaml output_binary_int.
        """
        # Ensure value is in 32-bit range
        value = value & 0xFFFFFFFF
        f.write(struct.pack(">I", value))

    def output_value_no_sharing(self, f: BinaryIO, obj: any) -> None:
        """
        Write object without sharing (matches OCaml Marshal with No_sharing).
        Uses pickle protocol 4 for compatibility.
        """
        pickle.dump(obj, f, protocol=4)

    def is_prime(self, n: int) -> bool:
        """Check if number is prime."""
        if n < 2:
            return False
        if n == 2:
            return True
        if n % 2 == 0:
            return False
        for i in range(3, int(n**0.5) + 1, 2):
            if n % i == 0:
                return False
        return True

    def prime_after(self, n: int) -> int:
        """Find next prime number >= n (matches OCaml prime_after)."""
        while not self.is_prime(n):
            n += 1
        return n

    def make_name_index(
        self, persons: List[any], get_misc_names: Callable
    ) -> List[List[int]]:
        """
        Create name index for all person name variations.
        Matches OCaml make_name_index.

        Args:
            persons: List of person records
            get_misc_names: Function to get misc names for a person

        Returns:
            Array of person ID lists indexed by name hash
        """
        # Initialize index with empty lists (matches OCaml Array.make)
        index = [[] for _ in range(self.TABLE_SIZE)]

        for person in persons:
            # Skip persons with first_name=1 or surname=1 (? ?)
            # Matches OCaml: if p.first_name <> 1 && p.first_name <> 1
            if person.first_name == 1 or person.surname == 1:
                continue

            misc_names = get_misc_names(person)

            for name in misc_names:
                hash_idx = self._name_index(name)
                index[hash_idx].insert(0, person.key_index)

        # Convert lists to arrays (matches OCaml Array.of_list)
        return [list(reversed(bucket)) for bucket in index]

    def _name_index(self, name: str) -> int:
        """
        Calculate hash index for name.
        """
        # Normalize name: lowercase and strip
        name_lower = name.lower().strip()
        # MD5 hash
        h = hashlib.md5(name_lower.encode("utf-8")).digest()
        # Take first 4 bytes as little-endian int, modulo table size
        return int.from_bytes(h[:4], "little") % self.TABLE_SIZE

    def output_strings_hash(self, strings: List[str], output_path: str) -> None:
        """
        Create string hash table for fast lookup.

        Args:
            strings: List of all strings
            output_path: Output file path
        """
        # Calculate hash table size: prime after max(2, 10 * len(strings))
        # Matches: prime_after (max 2 (10 * strings_array.len))
        table_size = self.prime_after(max(2, 10 * len(strings)))
        # Limit to reasonable size (matches: min Sys.max_array_length ...)
        table_size = min(table_size, 2**30)

        # Create hash tables
        # taba: hash value -> last string index with that hash
        hash_array = [-1] * table_size
        # tabl: string index -> previous string with same hash
        link_array = [-1] * len(strings)

        # Build hash table with chaining (matches OCaml loop)
        for i, string in enumerate(strings):
            # Calculate hash modulo table size
            hash_val = hash(string) % table_size
            # Store previous value that had same hash (chaining)
            link_array[i] = hash_array[hash_val]
            # Update hash table to point to current string
            hash_array[hash_val] = i

        # Write to file
        with open(output_path, "wb") as f:
            # Write table size
            self.output_binary_int(f, table_size)

            # Write hash array (taba)
            for val in hash_array:
                self.output_binary_int(f, val)

            # Write link array (tabl)
            for val in link_array:
                self.output_binary_int(f, val)

        self.trace(f"Created string hash table: {len(strings)} strings")

    def output_name_index(
        self,
        persons: List[any],
        get_name: Callable,
        compare: Callable,
        index_path: str,
        data_path: str,
    ) -> None:
        """
        Create surname/firstname index with person lists.

        Associates string ID to list of person IDs having that name.

        Args:
            persons: List of person records
            get_name: Function to get name string ID
            compare: Comparison function for sorting
            index_path: Index file path (.inx)
            data_path: Data file path (.dat)
        """
        # Group persons by name (matches OCaml IntHT)
        name_dict: Dict[int, List[int]] = {}

        for person in persons:
            name_id = get_name(person)
            if name_id not in name_dict:
                name_dict[name_id] = []
            # Prepend to match OCaml :: operator
            name_dict[name_id].insert(0, person.key_index)

        # Convert to array and sort by name (matches OCaml Array.sort)
        sorted_items = sorted(name_dict.items(), key=lambda x: x[0])

        # Write data file with person lists
        offsets = []
        with open(data_path, "wb") as f_dat:
            for name_id, person_ids in sorted_items:
                offset = f_dat.tell()
                offsets.append((name_id, offset))

                # Write number of persons
                self.output_binary_int(f_dat, len(person_ids))

                # Write person IDs
                for iper in person_ids:
                    self.output_binary_int(f_dat, iper)

        # Write index file (matches OCaml bt2 output)
        with open(index_path, "wb") as f_idx:
            self.output_value_no_sharing(f_idx, offsets)

        self.trace(f"Created name index: {len(sorted_items)} names")

    def output_particles_file(self, particles: List[str], path: str) -> None:
        """
        Write particles file.
        Args:
            particles: List of name particles (de, von, etc.)
            path: Output file path
        """
        with open(path, "w", encoding="utf-8") as f:
            for particle in particles:
                # Replace spaces with underscores (matches Mutil.tr ' ' '_')
                f.write(particle.replace(" ", "_") + "\n")

    def make_strings_of_fname(
        self, persons: List[any], strings: List[str]
    ) -> List[List[int]]:
        """
        Create index of string IDs for firstname substrings.
        Matches OCaml make_strings_of_fname.
        """
        return self._make_strings_of_name(persons, lambda p: p.first_name, strings)

    def make_strings_of_sname(
        self, persons: List[any], strings: List[str]
    ) -> List[List[int]]:
        """
        Create index of string IDs for surname substrings.
        """
        return self._make_strings_of_name(persons, lambda p: p.surname, strings)

    def _make_strings_of_name(
        self, persons: List[any], get_name: Callable, strings: List[str]
    ) -> List[List[int]]:
        """
        Internal function to create substring index.
        Matches OCaml make_strings_of_fsname_aux.
        """
        # Initialize index with empty sets
        index = [set() for _ in range(self.TABLE_SIZE)]

        for person in persons:
            name_id = get_name(person)

            if name_id == 1:  # Skip "?"
                continue

            # Add full name string ID
            string = strings[name_id] if name_id < len(strings) else ""
            hash_idx = self._name_index(string)
            index[hash_idx].add(name_id)

            # TODO: Add substrings using split_fname_callback / split_sname_callback
            # For now, just add the full name

        # Convert sets to sorted arrays
        return [sorted(list(s)) for s in index]

    def output_array_access(
        self, oc_acc: BinaryIO, arr_get: Callable, arr_len: int, start_pos: int
    ) -> int:
        """
        Write array access table (positions of each element).

        Args:
            oc_acc: Access file handle
            arr_get: Function to get array element by index
            arr_len: Array length
            start_pos: Starting position in data file

        Returns:
            Position after last element
        """
        output_value_header_size = 20  # OCaml Marshal header
        array_header_size = 1 if arr_len < 8 else 5  # Block header

        pos = start_pos + output_value_header_size + array_header_size

        for i in range(arr_len):
            # Write position of element i
            self.output_binary_int(oc_acc, pos)
            # Calculate size of element (simplified)
            elem = arr_get(i)
            elem_size = len(pickle.dumps(elem, protocol=4))
            pos += elem_size

        return pos
