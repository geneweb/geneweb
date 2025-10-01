"""Core data models for genealogical data."""

from typing import Dict, List, Optional, Set, Tuple, Any
from dataclasses import dataclass, field
from datetime import date
from pathlib import Path
from .exceptions import ValidationError


@dataclass
class Date:
    """Represents a genealogical date with precision handling."""
    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None
    precision: str = "exact"  # exact, about, before, after
    calendar: str = "gregorian"

    def __str__(self) -> str:
        """String representation of date."""
        if not self.year:
            return ""

        parts = []
        if self.precision != "exact":
            parts.append(self.precision)

        if self.day and self.month:
            parts.append(f"{self.day:02d}/{self.month:02d}/{self.year}")
        elif self.month:
            parts.append(f"{self.month:02d}/{self.year}")
        else:
            parts.append(str(self.year))

        return " ".join(parts)


@dataclass
class Name:
    """Represents a person's name components."""
    first_name: str = ""
    surname: str = ""
    public_name: str = ""
    aliases: List[str] = field(default_factory=list)

    def get_full_name(self) -> str:
        """Get formatted full name."""
        if self.public_name:
            return self.public_name
        return f"{self.first_name} {self.surname}".strip()

    def __str__(self) -> str:
        return self.get_full_name()


@dataclass
class Event:
    """Represents a genealogical event."""
    event_type: str  # birth, death, marriage, etc.
    date: Optional[Date] = None
    place: Optional[str] = None
    note: Optional[str] = None
    sources: List[str] = field(default_factory=list)

    def __str__(self) -> str:
        parts = [self.event_type]
        if self.date:
            parts.append(str(self.date))
        if self.place:
            parts.append(self.place)
        return " ".join(parts)


@dataclass
class Person:
    """Represents a person in the genealogical database."""
    id: int
    name: Name
    sex: str = "U"  # M, F, U (unknown)
    birth: Optional[Event] = None
    death: Optional[Event] = None
    parents: Optional[Tuple[int, int]] = None  # (father_id, mother_id)
    families: List[int] = field(default_factory=list)
    consanguinity: Optional[float] = None
    events: List[Event] = field(default_factory=list)
    notes: str = ""

    def get_full_name(self) -> str:
        """Get person's full formatted name."""
        return self.name.get_full_name()

    def is_alive(self) -> bool:
        """Check if person is considered alive (no death event)."""
        return self.death is None

    def get_birth_year(self) -> Optional[int]:
        """Get birth year if available."""
        return self.birth.date.year if self.birth and self.birth.date else None

    def get_death_year(self) -> Optional[int]:
        """Get death year if available."""
        return self.death.date.year if self.death and self.death.date else None

    def __str__(self) -> str:
        return f"Person({self.id}: {self.get_full_name()})"


@dataclass
class Family:
    """Represents a family unit (marriage/partnership)."""
    id: int
    father_id: Optional[int] = None
    mother_id: Optional[int] = None
    children: List[int] = field(default_factory=list)
    marriage: Optional[Event] = None
    divorce: Optional[Event] = None
    events: List[Event] = field(default_factory=list)
    notes: str = ""

    def get_parents(self) -> Tuple[Optional[int], Optional[int]]:
        """Get parent IDs as tuple."""
        return (self.father_id, self.mother_id)

    def add_child(self, child_id: int) -> None:
        """Add child to family."""
        if child_id not in self.children:
            self.children.append(child_id)

    def __str__(self) -> str:
        return f"Family({self.id}: {self.father_id} + {self.mother_id})"


@dataclass
class Database:
    """Represents a genealogical database."""
    persons: Dict[int, Person] = field(default_factory=dict)
    families: Dict[int, Family] = field(default_factory=dict)
    metadata: Dict[str, Any] = field(default_factory=dict)

    def add_person(self, person: Person) -> None:
        """Add person to database."""
        self.persons[person.id] = person

    def add_family(self, family: Family) -> None:
        """Add family to database."""
        self.families[family.id] = family

    def get_person(self, person_id: int) -> Optional[Person]:
        """Get person by ID."""
        return self.persons.get(person_id)

    def get_family(self, family_id: int) -> Optional[Family]:
        """Get family by ID."""
        return self.families.get(family_id)

    def get_all_persons(self) -> List[Person]:
        """Get all persons as list."""
        return list(self.persons.values())

    def get_all_families(self) -> List[Family]:
        """Get all families as list."""
        return list(self.families.values())

    def get_ancestors(self, person_id: int) -> Set[int]:
        """Get all ancestor IDs for a person."""
        ancestors = set()
        self._collect_ancestors(person_id, ancestors)
        return ancestors

    def _collect_ancestors(self, person_id: int, ancestors: Set[int]) -> None:
        """Recursively collect ancestors."""
        person = self.get_person(person_id)
        if not person or not person.parents:
            return

        father_id, mother_id = person.parents
        if father_id and father_id not in ancestors:
            ancestors.add(father_id)
            self._collect_ancestors(father_id, ancestors)

        if mother_id and mother_id not in ancestors:
            ancestors.add(mother_id)
            self._collect_ancestors(mother_id, ancestors)

    def get_descendants(self, person_id: int) -> Set[int]:
        """Get all descendant IDs for a person."""
        descendants = set()
        self._collect_descendants(person_id, descendants)
        return descendants

    def _collect_descendants(self, person_id: int, descendants: Set[int]) -> None:
        """Recursively collect descendants."""
        person = self.get_person(person_id)
        if not person:
            return

        # Find families where this person is a parent
        for family in self.families.values():
            if person_id in (family.father_id, family.mother_id):
                for child_id in family.children:
                    if child_id not in descendants:
                        descendants.add(child_id)
                        self._collect_descendants(child_id, descendants)

    @classmethod
    def load_from_gwb(cls, database_path: Path) -> 'Database':
        """Load database from GWB format."""
        # Placeholder implementation - would need actual GWB parser
        database = cls()
        # TODO: Implement GWB loading
        return database

    def save_to_gwb(self, database_path: Path) -> None:
        """Save database to GWB format."""
        # Placeholder implementation - would need actual GWB writer
        # TODO: Implement GWB saving
        pass

    def __len__(self) -> int:
        """Return number of persons in database."""
        return len(self.persons)

    def __str__(self) -> str:
        return f"Database({len(self.persons)} persons, {len(self.families)} families)"
