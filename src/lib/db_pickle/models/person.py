"""
Person model for pickle database.

Defines the GenPerson dataclass matching OCaml implementation.
"""

from dataclasses import dataclass
from typing import Optional, List
from ..core.types import Istr, Iper
from ..core.enums import Sex
from .events import Date, Event, Title

@dataclass
class GenPerson:
    """Person model matching OCaml implementation."""
    first_name: str = ""
    surname: str = ""
    occ: int = 0
    sex: Sex = Sex.NEUTER
    birth: Optional[Date] = None
    death: Optional[Date] = None
    baptism: Optional[Date] = None
    burial: Optional[Date] = None
    titles: List[Title] = None
    events: List[Event] = None
    notes: str = ""
    sources: str = ""
    key_index: Iper = None

    def __post_init__(self):
        """Initialize default values after dataclass creation."""
        if self.titles is None:
            self.titles = []
        if self.events is None:
            self.events = []
        if self.key_index is None:
            self.key_index = Iper(0)
