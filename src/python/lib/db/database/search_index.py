"""
Search index for MessagePack database.

Provides search functionality for names and strings.
"""

from typing import Dict, List

from ..core.types import Iper, Istr


class SearchIndex:
    """Search index for MessagePack database."""

    def __init__(self):
        """Initialize search index."""
        self.person_name_to_ids: Dict[str, List[Iper]] = {}
        self.string_to_ids: Dict[str, List[Istr]] = {}
        self.first_name_to_ids: Dict[str, List[Iper]] = {}
        self.surname_to_ids: Dict[str, List[Iper]] = {}

    def add_person_name(self, name: str, iper: Iper) -> None:
        """Add person name to index."""
        name_lower = name.lower().strip()
        if name_lower and name_lower != " ":
            if name_lower not in self.person_name_to_ids:
                self.person_name_to_ids[name_lower] = []
            if iper not in self.person_name_to_ids[name_lower]:
                self.person_name_to_ids[name_lower].append(iper)

    def add_string(self, content: str, istr: Istr) -> None:
        """Add string content to index."""
        content_lower = content.lower().strip()
        if content_lower and content_lower != " ":
            if content_lower not in self.string_to_ids:
                self.string_to_ids[content_lower] = []
            if istr not in self.string_to_ids[content_lower]:
                self.string_to_ids[content_lower].append(istr)

    def add_first_name(self, first_name: str, iper: Iper) -> None:
        """Add first name to index."""
        first_name_lower = first_name.lower().strip()
        if first_name_lower and first_name_lower != " ":
            if first_name_lower not in self.first_name_to_ids:
                self.first_name_to_ids[first_name_lower] = []
            if iper not in self.first_name_to_ids[first_name_lower]:
                self.first_name_to_ids[first_name_lower].append(iper)

    def add_surname(self, surname: str, iper: Iper) -> None:
        """Add surname to index."""
        surname_lower = surname.lower().strip()
        if surname_lower and surname_lower != " ":
            if surname_lower not in self.surname_to_ids:
                self.surname_to_ids[surname_lower] = []
            if iper not in self.surname_to_ids[surname_lower]:
                self.surname_to_ids[surname_lower].append(iper)

    def search_persons_by_name(self, name: str) -> List[Iper]:
        """Search persons by name (case-insensitive, partial match)."""
        search_name = name.lower().strip()
        results = []

        # Exact match
        if search_name in self.person_name_to_ids:
            results.extend(self.person_name_to_ids[search_name])

        # Partial match
        for stored_name, person_ids in self.person_name_to_ids.items():
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

    def search_persons_by_first_name(self, first_name: str) -> List[Iper]:
        """Search persons by first name (case-insensitive, partial match)."""
        search_name = first_name.lower().strip()
        results = []

        # Exact match
        if search_name in self.first_name_to_ids:
            results.extend(self.first_name_to_ids[search_name])

        # Partial match
        for stored_name, person_ids in self.first_name_to_ids.items():
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
        if search_surname in self.surname_to_ids:
            results.extend(self.surname_to_ids[search_surname])

        # Partial match
        for stored_surname, person_ids in self.surname_to_ids.items():
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

    def search_strings_by_content(self, content: str) -> List[Istr]:
        """Search strings by content (case-insensitive, partial match)."""
        search_content = content.lower().strip()
        results = []

        # Exact match
        if search_content in self.string_to_ids:
            results.extend(self.string_to_ids[search_content])

        # Partial match
        for stored_content, string_ids in self.string_to_ids.items():
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

    def get_all_person_names(self) -> List[str]:
        """Get all person names in index."""
        return list(self.person_name_to_ids.keys())

    def get_all_strings(self) -> List[str]:
        """Get all strings in index."""
        return list(self.string_to_ids.keys())

    def clear(self) -> None:
        """Clear all indexes."""
        self.person_name_to_ids.clear()
        self.string_to_ids.clear()
        self.first_name_to_ids.clear()
        self.surname_to_ids.clear()

    def get_statistics(self) -> Dict[str, int]:
        """Get index statistics."""
        return {
            "person_names": len(self.person_name_to_ids),
            "strings": len(self.string_to_ids),
            "first_names": len(self.first_name_to_ids),
            "surnames": len(self.surname_to_ids),
        }
