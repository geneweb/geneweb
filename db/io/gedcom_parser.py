"""Basic GEDCOM file parsing."""

import re
from typing import Dict, List, Optional, Tuple
from ..models.person import GenPerson
from ..models.family import GenFamily
from ..models.events import Date
from ..core.enums import Sex
from ..core.types import Istr


class GedcomParser:
    """Basic GEDCOM file parser for genealogical data."""

    def __init__(self):
        self.persons: Dict[str, GenPerson] = {}
        self.families: Dict[str, GenFamily] = {}
        self.string_map: Dict[str, int] = {"": 0, "?": 1}
        self.string_reverse_map: Dict[int, str] = {0: "", 1: "?"}
        self.next_string_id = 2

    def get_string_id(self, text: str) -> int:
        """Get or create string ID."""
        text = text.strip()
        if text in self.string_map:
            return self.string_map[text]

        string_id = self.next_string_id
        self.string_map[text] = string_id
        self.string_reverse_map[string_id] = text
        self.next_string_id += 1
        return string_id

    def get_string(self, string_id: int) -> str:
        """Get string from ID."""
        return self.string_reverse_map.get(string_id, "")

    def parse_date(self, date_str: str) -> Date:
        """Parse GEDCOM date string."""
        if not date_str:
            return Date.none()

        # Simple date parsing - extend as needed
        match = re.search(r'(\d{1,2})\s+(\w+)\s+(\d{4})', date_str)
        if match:
            day, month_str, year = match.groups()
            month_map = {
                'JAN': 1, 'FEB': 2, 'MAR': 3, 'APR': 4,
                'MAY': 5, 'JUN': 6, 'JUL': 7, 'AUG': 8,
                'SEP': 9, 'OCT': 10, 'NOV': 11, 'DEC': 12
            }
            month = month_map.get(month_str.upper(), 1)
            return Date(year=int(year), month=month, day=int(day))

        # Year only
        match = re.search(r'(\d{4})', date_str)
        if match:
            return Date(year=int(match.group(1)))

        return Date.none()

    def parse_file(self, filepath: str) -> Tuple[List[GenPerson], List[GenFamily]]:
        """
        Parse GEDCOM file.

        Args:
            filepath: Path to GEDCOM file

        Returns:
            Tuple of (persons, families)
        """
        with open(filepath, 'r', encoding='utf-8') as f:
            lines = f.readlines()

        current_record = None
        current_type = None

        for line in lines:
            line = line.strip()
            if not line:
                continue

            parts = line.split(' ', 2)
            level = int(parts[0])

            if level == 0 and len(parts) >= 3:
                # New record
                record_id = parts[1].strip('@')
                record_type = parts[2]

                if record_type == 'INDI':
                    current_record = GenPerson(
                        first_name=Istr.quest(),
                        surname=Istr.quest(),
                        occ=0
                    )
                    current_type = 'INDI'
                    self.persons[record_id] = current_record
                elif record_type == 'FAM':
                    current_record = GenFamily()
                    current_type = 'FAM'
                    self.families[record_id] = current_record

            elif level == 1 and current_record:
                tag = parts[1]
                value = parts[2] if len(parts) > 2 else ""

                if current_type == 'INDI':
                    self._parse_individual_tag(current_record, tag, value)
                elif current_type == 'FAM':
                    self._parse_family_tag(current_record, tag, value)

        return list(self.persons.values()), list(self.families.values())

    def _parse_individual_tag(self, person: GenPerson, tag: str, value: str):
        """Parse individual record tags."""
        if tag == 'NAME':
            # Parse "First /Last/"
            match = re.match(r'(.+?)\s*/(.+?)/', value)
            if match:
                first_name, surname = match.groups()
                person.first_name = self.get_string_id(first_name.strip())
                person.surname = self.get_string_id(surname.strip())
        elif tag == 'SEX':
            if value == 'M':
                person.sex = Sex.MALE
            elif value == 'F':
                person.sex = Sex.FEMALE
        elif tag == 'BIRT':
            # Birth event follows
            pass
        elif tag == 'DEAT':
            # Death event follows
            pass

    def _parse_family_tag(self, family: GenFamily, tag: str, value: str):
        """Parse family record tags."""
        if tag == 'HUSB':
            # Husband reference
            pass
        elif tag == 'WIFE':
            # Wife reference
            pass
        elif tag == 'CHIL':
            # Child reference
            pass
        elif tag == 'MARR':
            # Marriage event follows
            pass
