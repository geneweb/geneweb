"""
BaseFunc container for database operations.

Holds all operation functions (searching, patching, etc.)
that work on the database.
"""

from dataclasses import dataclass
from typing import Callable, List, Optional

from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion
from .search_index import SearchIndex


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
