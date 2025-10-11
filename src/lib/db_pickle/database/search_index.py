"""
Search index for pickle database.

Provides search functionality for names and strings.
"""

from typing import List, Dict, Set
from ..core.types import Iper, Istr

class PickleSearchIndex:
    """Search index for pickle database."""

    def __init__(self):
        """Initialize search index."""
        self.name_to_persons: Dict[str, List[Iper]] = {}
        self.string_to_ids: Dict[str, List[Istr]] = {}

    def add_person_name(self, name: str, person_id: Iper) -> None:
        """Add person to name index."""
        if name not in self.name_to_persons:
            self.name_to_persons[name] = []
        if person_id not in self.name_to_persons[name]:
            self.name_to_persons[name].append(person_id)

    def add_string(self, string: str, string_id: Istr) -> None:
        """Add string to index."""
        if string not in self.string_to_ids:
            self.string_to_ids[string] = []
        if string_id not in self.string_to_ids[string]:
            self.string_to_ids[string].append(string_id)

    def search_persons_by_name(self, name: str) -> List[Iper]:
        """Search persons by name."""
        return self.name_to_persons.get(name, [])

    def search_strings(self, string: str) -> List[Istr]:
        """Search strings by content."""
        return self.string_to_ids.get(string, [])

    def search_persons_by_surname(self, surname: str) -> List[Iper]:
        """Search persons by surname."""
        return self.search_persons_by_name(surname)

    def search_persons_by_first_name(self, first_name: str) -> List[Iper]:
        """Search persons by first name."""
        return self.search_persons_by_name(first_name)

    def get_all_names(self) -> List[str]:
        """Get all indexed names."""
        return list(self.name_to_persons.keys())

    def get_all_strings(self) -> List[str]:
        """Get all indexed strings."""
        return list(self.string_to_ids.keys())
