from dataclasses import dataclass
from typing import Optional

from lib.db.unmarshall.v2.dbdisk import RecordAccess
from lib.db.v2 import mutil as Mutil

from . import ByName
from lib.db.v2.defs import Person


@dataclass
class ByKey:
    """Manages person lookup by key"""

    persons: RecordAccess[Person]
    strings: RecordAccess[str]
    search_by_name: ByName

    def __call__(self, first_name: str, surname: str, occ: int) -> Optional[int]:
        """
        Find person IDs by first name, surname, and occurrence.

        Args:
            first_name: First name to search
            surname: Surname to search
            occ: Occurrence number (1-based)

        Returns:
            Person ID if found, else None
        """
        first_name = Mutil.nominative(first_name)
        surname = Mutil.nominative(surname)
        matching_person_ids = self.search_by_name(f"{first_name} {surname}")
        first_name = first_name.lower()
        surname = surname.lower()
        for pid in matching_person_ids:
            person = self.persons.get(pid)
            if (
                occ == person.occ
                and person.first_name.lower() == first_name
                and person.surname.lower() == surname
            ):
                return pid
        return None
