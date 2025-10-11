"""
Base data structure for pickle database.

Defines the data container for pickle-based genealogical database.
"""

from dataclasses import dataclass, field
from typing import Dict, List
from ..core.types import Iper, Ifam, Istr
from ..models.person import GenPerson
from ..models.family import GenFamily
from ..models.relations import GenAscend, GenUnion, GenCouple, GenDescend

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
            if search_name in stored_name:
                results.extend(person_ids)

        return results

    def search_persons_by_surname(self, surname: str) -> List[Iper]:
        """Search persons by surname (case-insensitive)."""
        return self.surname_index.get(surname.lower().strip(), [])

    def search_persons_by_full_name(self, full_name: str) -> List[Iper]:
        """Search persons by full name (case-insensitive)."""
        return self.full_name_index.get(full_name.lower().strip(), [])

    def search_strings_by_content(self, content: str) -> List[Istr]:
        """Search strings by content (case-insensitive)."""
        return self.string_content_index.get(content.lower().strip(), [])
