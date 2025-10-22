"""
Function factory for MessagePack database.

Creates all the functions needed for BaseFunc.
"""

from typing import Callable, List, Optional

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion
from .base_data import BaseData
from .patches import Patches
from .search_index import SearchIndex


class FuncFactory:
    """Factory for creating MessagePack database functions."""

    def __init__(self, data: BaseData):
        """
        Initialize function factory.

        Args:
            data: MessagePack database data container
        """
        self.data = data
        self.search_index = SearchIndex()
        self.patches = Patches(data)
        self._build_search_index()

    def _build_search_index(self) -> None:
        """Build search index from existing data."""
        for iper, person in self.data.persons.items():
            full_name = f"{person.first_name} {person.surname}".strip()
            if full_name and full_name != " ":
                self.search_index.add_person_name(full_name, iper)
                self.search_index.add_first_name(person.first_name, iper)
                self.search_index.add_surname(person.surname, iper)

        for istr, string in self.data.strings.items():
            self.search_index.add_string(string, istr)

    def create_person_of_key(self) -> Callable[[str, str, int], Optional[Iper]]:
        """Create person_of_key function."""

        def person_of_key(first_name: str, surname: str, occ: int) -> Optional[Iper]:
            for iper, person in self.data.persons.items():
                if (
                    person.first_name == first_name
                    and person.surname == surname
                    and person.occ == occ
                ):
                    return iper
            return None

        return person_of_key

    def create_persons_of_name(self) -> Callable[[str], List[Iper]]:
        """Create persons_of_name function."""

        def persons_of_name(name: str) -> List[Iper]:
            return self.search_index.search_persons_by_name(name)

        return persons_of_name

    def create_strings_of_fname(self) -> Callable[[str], List[Istr]]:
        """Create strings_of_fname function."""

        def strings_of_fname(first_name: str) -> List[Istr]:
            # Search for strings containing the first name
            results = []
            first_name_lower = first_name.lower()
            for istr, string in self.data.strings.items():
                if first_name_lower in string.lower():
                    results.append(istr)
            return results

        return strings_of_fname

    def create_strings_of_sname(self) -> Callable[[str], List[Istr]]:
        """Create strings_of_sname function."""

        def strings_of_sname(surname: str) -> List[Istr]:
            # Search for strings containing the surname
            results = []
            surname_lower = surname.lower()
            for istr, string in self.data.strings.items():
                if surname_lower in string.lower():
                    results.append(istr)
            return results

        return strings_of_sname

    def create_persons_of_surname(self) -> Callable[[str], List[Iper]]:
        """Create persons_of_surname function."""

        def persons_of_surname(surname: str) -> List[Iper]:
            return self.search_index.search_persons_by_surname(surname)

        return persons_of_surname

    def create_persons_of_first_name(self) -> Callable[[str], List[Iper]]:
        """Create persons_of_first_name function."""

        def persons_of_first_name(first_name: str) -> List[Iper]:
            return self.search_index.search_persons_by_first_name(first_name)

        return persons_of_first_name

    def create_patch_person(self) -> Callable[[Iper, GenPerson], None]:
        """Create patch_person function."""

        def patch_person(iper: Iper, person: GenPerson) -> None:
            self.patches.patch_person(iper, person)

        return patch_person

    def create_patch_ascend(self) -> Callable[[Iper, GenAscend], None]:
        """Create patch_ascend function."""

        def patch_ascend(iper: Iper, ascend: GenAscend) -> None:
            self.patches.patch_ascend(iper, ascend)

        return patch_ascend

    def create_patch_union(self) -> Callable[[Iper, GenUnion], None]:
        """Create patch_union function."""

        def patch_union(iper: Iper, union: GenUnion) -> None:
            self.patches.patch_union(iper, union)

        return patch_union

    def create_patch_family(self) -> Callable[[Ifam, GenFamily], None]:
        """Create patch_family function."""

        def patch_family(ifam: Ifam, family: GenFamily) -> None:
            self.patches.patch_family(ifam, family)

        return patch_family

    def create_patch_couple(self) -> Callable[[Ifam, GenCouple], None]:
        """Create patch_couple function."""

        def patch_couple(ifam: Ifam, couple: GenCouple) -> None:
            self.patches.patch_couple(ifam, couple)

        return patch_couple

    def create_patch_descend(self) -> Callable[[Ifam, GenDescend], None]:
        """Create patch_descend function."""

        def patch_descend(ifam: Ifam, descend: GenDescend) -> None:
            self.patches.patch_descend(ifam, descend)

        return patch_descend

    def create_insert_string(self) -> Callable[[str], Istr]:
        """Create insert_string function."""

        def insert_string(s: str) -> Istr:
            return self.patches.insert_string(s)

        return insert_string

    def create_get_string(self) -> Callable[[Istr], str]:
        """Create get_string function."""

        def get_string(istr: Istr) -> str:
            return self.patches.get_string(istr)

        return get_string

    def create_commit_patches(self) -> Callable[[], None]:
        """Create commit_patches function."""

        def commit_patches() -> None:
            self.patches.commit_patches()

        return commit_patches

    def create_iper_exists(self) -> Callable[[Iper], bool]:
        """Create iper_exists function."""

        def iper_exists(iper: Iper) -> bool:
            return self.patches.iper_exists(iper)

        return iper_exists

    def create_ifam_exists(self) -> Callable[[Ifam], bool]:
        """Create ifam_exists function."""

        def ifam_exists(ifam: Ifam) -> bool:
            return self.patches.ifam_exists(ifam)

        return ifam_exists

    def create_nb_of_real_persons(self) -> Callable[[], int]:
        """Create nb_of_real_persons function."""

        def nb_of_real_persons() -> int:
            return len(self.data.persons)

        return nb_of_real_persons

    def create_nb_of_persons(self) -> Callable[[], int]:
        """Create nb_of_persons function."""

        def nb_of_persons() -> int:
            return len(self.data.persons)

        return nb_of_persons

    def create_nb_of_families(self) -> Callable[[], int]:
        """Create nb_of_families function."""

        def nb_of_families() -> int:
            return len(self.data.families)

        return nb_of_families

    def create_nb_of_strings(self) -> Callable[[], int]:
        """Create nb_of_strings function."""

        def nb_of_strings() -> int:
            return len(self.data.strings)

        return nb_of_strings

    def create_families_of_marriage_date(self) -> Callable[[int], List[Ifam]]:
        """Create families_of_marriage_date function."""

        def families_of_marriage_date(year: int) -> List[Ifam]:
            return self.data.search_families_by_marriage_date(year)

        return families_of_marriage_date
