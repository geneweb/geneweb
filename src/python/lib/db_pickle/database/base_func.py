"""
BaseFunc container for pickle database operations.

Holds all operation functions (searching, patching, etc.)
that work on the pickle database.
"""

from dataclasses import dataclass
from typing import Callable, List, Optional

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion


@dataclass
class PickleBaseFunc:
    """Container for pickle database operations."""

    # Search functions
    person_of_key: Callable[[str, str, int], Optional[Iper]]
    persons_of_name: Callable[[str], List[Iper]]
    strings_of_fname: Callable[[str], List[Istr]]
    strings_of_sname: Callable[[str], List[Istr]]
    persons_of_surname: Callable[[str], List[Iper]]
    persons_of_first_name: Callable[[str], List[Iper]]

    # Patch functions
    patch_person: Callable[[Iper, GenPerson], None]
    patch_ascend: Callable[[Iper, GenAscend], None]
    patch_union: Callable[[Iper, GenUnion], None]
    patch_family: Callable[[Ifam, GenFamily], None]
    patch_couple: Callable[[Ifam, GenCouple], None]
    patch_descend: Callable[[Ifam, GenDescend], None]

    # String functions
    insert_string: Callable[[str], Istr]
    get_string: Callable[[Istr], str]

    # Commit function
    commit_patches: Callable[[], None]

    # Existence checks
    iper_exists: Callable[[Iper], bool]
    ifam_exists: Callable[[Ifam], bool]

    # Statistics
    nb_of_real_persons: Callable[[], int]
    nb_of_persons: Callable[[], int]
    nb_of_families: Callable[[], int]
    nb_of_strings: Callable[[], int]
