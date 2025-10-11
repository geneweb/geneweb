"""
Family model for pickle database.

Defines the GenFamily dataclass matching OCaml implementation.
"""

from dataclasses import dataclass
from typing import Optional, List
from ..core.types import Ifam
from ..core.enums import RelationKind, DivorceStatus
from .events import Date, Event

@dataclass
class GenFamily:
    """Family model matching OCaml implementation."""
    marriage: Optional[Date] = None
    marriage_place: str = ""
    marriage_src: str = ""
    marriage_note: str = ""
    relation: RelationKind = RelationKind.MARRIED
    divorce: DivorceStatus = DivorceStatus.NOT_DIVORCED
    divorce_date: Optional[Date] = None
    divorce_place: str = ""
    divorce_src: str = ""
    divorce_note: str = ""
    events: List[Event] = None
    notes: str = ""
    sources: str = ""
    fam_index: Ifam = None

    def __post_init__(self):
        """Initialize default values after dataclass creation."""
        if self.events is None:
            self.events = []
        if self.fam_index is None:
            self.fam_index = Ifam(0)
