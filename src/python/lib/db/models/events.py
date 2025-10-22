"""
Event models for pickle database.

Defines date, event, and title models.
"""

from dataclasses import dataclass
from typing import Optional


@dataclass
class Date:
    """Date model matching OCaml implementation."""

    year: int = 0
    month: int = 0
    day: int = 0
    prec: int = 0  # Precision
    delta: int = 0  # Delta for approximate dates

    @classmethod
    def none(cls) -> "Date":
        """Create empty date."""
        return cls()

    def is_empty(self) -> bool:
        """Check if date is empty."""
        return self.year == 0 and self.month == 0 and self.day == 0


@dataclass
class Event:
    """Event model for person/family events."""

    name: str = ""
    date: Optional[Date] = None
    place: str = ""
    note: str = ""
    src: str = ""
    witness: str = ""


@dataclass
class Title:
    """Title model for person titles."""

    name: str = ""
    title: str = ""
    date_begin: Optional[Date] = None
    date_end: Optional[Date] = None
    nth: int = 0
