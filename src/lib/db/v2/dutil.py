from lib.db.unmarshall.v2.dbdisk import BaseData
from lib.db.v2 import mutil
import lib.db.v2.caml_hash as caml_hash


magic_GnWb0020 = b"GnWb0020"
magic_GnWb0021 = b"GnWb0021"
magic_GnWb0022 = b"GnWb0022"
magic_GnWb0023 = b"GnWb0023"
magic_GnWb0024 = b"GnWb0024"

TABLE_SIZE = 0x3FFF  # Same as OCaml's table_size


def name_index(s: str) -> int:
    """
    Generate index for name using hash function.

    Args:
        s: Name to hash

    Returns:
        Integer hash value modulo table size
    """
    # Convert string to lowercase and normalize
    s_lower = s.lower()
    # Note: Python's hash function may differ from OCaml's; for consistent results,

    # Hash string and take modulo to match OCaml's behavior
    return caml_hash.hash(s_lower) % TABLE_SIZE


def compare_snames(base_data: BaseData, a: str, b: str) -> int:
    return mutil.compare_after_particle(base_data.particles, a, b)


def compare_snames_id(base_data: BaseData, a: int, b: int) -> int:
    if a == b:
        return 0
    return compare_snames(base_data, base_data.strings.get(a), base_data.strings.get(b))


def compare_fnames(a: str, b: str) -> int:
    return (a > b) - (a < b)


def compare_fnames_id(base_data: BaseData, a: int, b: int) -> int:
    if a == b:
        return 0
    return compare_fnames(base_data.strings.get(a), base_data.strings.get(b))
