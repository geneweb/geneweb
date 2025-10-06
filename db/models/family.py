"""
Family data model.

Contains the GenFamily dataclass representing complete family information.
"""

from dataclasses import dataclass, field
from typing import List
from ..core.enums import RelationKind, DivorceStatus
from ..core.types import Ifam, Istr
from .events import Date, Event


@dataclass
class GenFamily:
    """
    Complete family record.
    This is the gen_family structure from OCaml.
    """
    marriage: Date = field(default_factory=Date.none)
    marriage_place: int = Istr.empty()
    marriage_note: int = Istr.empty()
    marriage_src: int = Istr.empty()
    witnesses: List[int] = field(default_factory=list)  # List of iper
    relation: RelationKind = RelationKind.MARRIED
    divorce: DivorceStatus = DivorceStatus.NOT_DIVORCED
    fevents: List[Event] = field(default_factory=list)
    comment: int = Istr.empty()
    origin_file: int = Istr.empty()
    fsources: int = Istr.empty()
    fam_index: int = Ifam.dummy()  # ifam - family's unique ID
