"""
Event and date models.

Contains classes for:
- Date: Calendar date with precision
- Event: Life events (birth, marriage, etc.)
- Title: Nobility titles
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple

from ..core.types import Istr


@dataclass
class Date:
    """Represents a calendar date with precision."""

    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None
    precision: str = "exact"

    @classmethod
    def none(cls) -> "Date":
        """Create empty/unknown date."""
        return cls()


@dataclass
class Title:
    """Nobility title of a person."""

    name: int
    title: str
    place: int
    date_start: Date = field(default_factory=Date.none)
    date_end: Date = field(default_factory=Date.none)
    nth: int = 0


@dataclass
class Event:
    """Generic event (birth, death, marriage, etc.)."""

    name: str
    date: Date = field(default_factory=Date.none)
    place: int = Istr.empty()
    reason: int = Istr.empty()  # String ID
    note: int = Istr.empty()  # String ID
    source: int = Istr.empty()  # String ID
    witnesses: List[Tuple[int, str]] = field(
        default_factory=list
    )  # (iper, witness_kind)
