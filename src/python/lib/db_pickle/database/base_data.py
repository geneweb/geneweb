"""
Base data structure for pickle database.

Defines the data container for pickle-based genealogical database.
"""

from dataclasses import dataclass, field
from typing import Dict, List

from ..core.types import Ifam, Iper, Istr
from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion


@dataclass
class PickleBaseData:
    """Base data container for pickle database."""

    persons: Dict[Iper, GenPerson] = field(default_factory=dict)
    families: Dict[Ifam, GenFamily] = field(default_factory=dict)
    ascends: Dict[Iper, GenAscend] = field(default_factory=dict)
    unions: Dict[Iper, GenUnion] = field(default_factory=dict)
    couples: Dict[Ifam, GenCouple] = field(default_factory=dict)
    descends: Dict[Ifam, GenDescend] = field(default_factory=dict)

    strings: Dict[Istr, str] = field(default_factory=dict)

    # Search indexes for fast lookups
    first_name_index: Dict[str, List[Iper]] = field(default_factory=dict)
    surname_index: Dict[str, List[Iper]] = field(default_factory=dict)
    full_name_index: Dict[str, List[Iper]] = field(default_factory=dict)
    string_content_index: Dict[str, List[Istr]] = field(default_factory=dict)

    bdir: str = ""  # Base directory
    particles: List[str] = field(default_factory=list)

    @property
    def persons_count(self) -> int:
        """Get number of persons."""
        return len(self.persons)

    @property
    def families_count(self) -> int:
        """Get number of families."""
        return len(self.families)

    @property
    def strings_count(self) -> int:
        """Get number of strings."""
        return len(self.strings)

    def build_indexes(self, verbose: bool = True) -> None:
        """Build all search indexes for fast lookups."""
        if verbose:
            print("*** create name index")
            print("*** create strings of sname")
            print("*** create strings of fname")
            print("*** create string index")
            print("*** create surname index")
            print("*** create first name index")

        # Clear existing indexes
        self.first_name_index.clear()
        self.surname_index.clear()
        self.full_name_index.clear()
        self.string_content_index.clear()

        # Build person name indexes
        for iper, person in self.persons.items():
            # First name index
            if person.first_name:
                first_name = person.first_name.lower().strip()
                if first_name not in self.first_name_index:
                    self.first_name_index[first_name] = []
                self.first_name_index[first_name].append(iper)

            # Surname index
            if person.surname:
                surname = person.surname.lower().strip()
                if surname not in self.surname_index:
                    self.surname_index[surname] = []
                self.surname_index[surname].append(iper)

            # Full name index
            full_name = f"{person.first_name} {person.surname}".lower().strip()
            if full_name not in self.full_name_index:
                self.full_name_index[full_name] = []
            self.full_name_index[full_name].append(iper)

        # Build string content index
        for istr, content in self.strings.items():
            content_lower = content.lower().strip()
            if content_lower not in self.string_content_index:
                self.string_content_index[content_lower] = []
            self.string_content_index[content_lower].append(istr)

    def search_persons_by_first_name(self, first_name: str) -> List[Iper]:
        """Search persons by first name (case-insensitive, partial match)."""
        search_name = first_name.lower().strip()
        results = []

        if search_name in self.first_name_index:
            results.extend(self.first_name_index[search_name])

        for stored_name, person_ids in self.first_name_index.items():
            if search_name in stored_name and search_name != stored_name:
                results.extend(person_ids)

        # Remove duplicates while preserving order
        seen = set()
        unique_results = []
        for person_id in results:
            if person_id not in seen:
                seen.add(person_id)
                unique_results.append(person_id)

        return unique_results

    def search_persons_by_surname(self, surname: str) -> List[Iper]:
        """Search persons by surname (case-insensitive, partial match)."""
        search_surname = surname.lower().strip()
        results = []

        # Exact match
        if search_surname in self.surname_index:
            results.extend(self.surname_index[search_surname])

        # Partial match
        for stored_surname, person_ids in self.surname_index.items():
            if search_surname in stored_surname and search_surname != stored_surname:
                results.extend(person_ids)

        # Remove duplicates while preserving order
        seen = set()
        unique_results = []
        for person_id in results:
            if person_id not in seen:
                seen.add(person_id)
                unique_results.append(person_id)

        return unique_results

    def search_persons_by_full_name(self, full_name: str) -> List[Iper]:
        """Search persons by full name (case-insensitive, partial match)."""
        search_full_name = full_name.lower().strip()
        results = []

        # Exact match
        if search_full_name in self.full_name_index:
            results.extend(self.full_name_index[search_full_name])

        # Partial match
        for stored_full_name, person_ids in self.full_name_index.items():
            if (
                search_full_name in stored_full_name
                and search_full_name != stored_full_name
            ):
                results.extend(person_ids)

        # Remove duplicates while preserving order
        seen = set()
        unique_results = []
        for person_id in results:
            if person_id not in seen:
                seen.add(person_id)
                unique_results.append(person_id)

        return unique_results

    def search_strings_by_content(self, content: str) -> List[Istr]:
        """Search strings by content (case-insensitive, partial match)."""
        search_content = content.lower().strip()
        results = []

        # Exact match
        if search_content in self.string_content_index:
            results.extend(self.string_content_index[search_content])

        # Partial match
        for stored_content, string_ids in self.string_content_index.items():
            if search_content in stored_content and search_content != stored_content:
                results.extend(string_ids)

        # Remove duplicates while preserving order
        seen = set()
        unique_results = []
        for string_id in results:
            if string_id not in seen:
                seen.add(string_id)
                unique_results.append(string_id)

        return unique_results

    def validate(self) -> List[str]:
        """Validate database integrity and return list of errors."""
        errors = []

        # Check for orphaned references in couples
        for family_id, couple in self.couples.items():
            if couple.father not in self.persons:
                errors.append(
                    f"Family {family_id} references non-existent father {couple.father}"
                )
            if couple.mother not in self.persons:
                errors.append(
                    f"Family {family_id} references non-existent mother {couple.mother}"
                )

        # Check for orphaned references in descends
        for family_id, descend in self.descends.items():
            for child_id in descend.children:
                if child_id not in self.persons:
                    errors.append(
                        f"Family {family_id} references non-existent child {child_id}"
                    )

        return errors

    def get_statistics(self) -> Dict[str, int]:
        """Get database statistics."""
        return {
            "persons": len(self.persons),
            "families": len(self.families),
            "couples": len(self.couples),
            "descends": len(self.descends),
            "strings": len(self.strings),
            "ascends": len(self.ascends),
            "unions": len(self.unions),
        }

    def clear(self) -> None:
        """Clear all data from the database."""
        self.persons.clear()
        self.families.clear()
        self.ascends.clear()
        self.unions.clear()
        self.couples.clear()
        self.descends.clear()
        self.strings.clear()
        self.first_name_index.clear()
        self.surname_index.clear()
        self.full_name_index.clear()
        self.string_content_index.clear()

    def copy(self) -> "PickleBaseData":
        """Create a deep copy of the database."""
        import copy

        return copy.deepcopy(self)
