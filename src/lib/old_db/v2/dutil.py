from lib.old_db.unmarshall.v2.dbdisk import BaseData
from lib.old_db.v2 import mutil
import lib.old_db.unmarshall.v2.hash as caml_hash
from lib.util import name


TABLE_SIZE = 0x3FFF  # Same as OCaml's table_size


def name_index(s: str) -> int:
    """
    Generate index for name using hash function.

    Args:
        s: Name to hash

    Returns:
        Integer hash value modulo table size
    """
    # Hash string and take modulo to match OCaml's behavior
    return caml_hash.hash(name.crush_lower(s)) % TABLE_SIZE


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
