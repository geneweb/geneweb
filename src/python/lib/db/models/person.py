"""
Person data model.

Contains the GenPerson dataclass representing complete person information.
"""

from dataclasses import dataclass, field
from typing import List

from ..core.enums import Access, BurialType, DeathType, Sex
from ..core.types import Iper, Istr
from .events import Date, Event, Title
from .relations import Relation


@dataclass
class GenPerson:
    """
    Complete person record.
    This is the gen_person structure from OCaml.
    """

    first_name: int  # istr
    surname: int  # istr
    occ: int  # Occurrence number for disambiguation
    image: int = Istr.empty()
    public_name: int = Istr.empty()
    qualifiers: List[int] = field(default_factory=list)
    aliases: List[int] = field(default_factory=list)
    first_names_aliases: List[int] = field(default_factory=list)
    surnames_aliases: List[int] = field(default_factory=list)
    titles: List[Title] = field(default_factory=list)
    related: List[int] = field(default_factory=list)  # List of iper
    rparents: List[Relation] = field(default_factory=list)
    occupation: int = Istr.empty()
    sex: Sex = Sex.NEUTER
    access: Access = Access.PRIVATE
    birth: Date = field(default_factory=Date.none)
    birth_place: int = Istr.empty()
    birth_note: int = Istr.empty()
    birth_src: int = Istr.empty()
    baptism: Date = field(default_factory=Date.none)
    baptism_place: int = Istr.empty()
    baptism_note: int = Istr.empty()
    baptism_src: int = Istr.empty()
    death: DeathType = DeathType.DONT_KNOW_IF_DEAD
    death_place: int = Istr.empty()
    death_note: int = Istr.empty()
    death_src: int = Istr.empty()
    burial: BurialType = BurialType.UNKNOWN
    burial_place: int = Istr.empty()
    burial_note: int = Istr.empty()
    burial_src: int = Istr.empty()
    pevents: List[Event] = field(default_factory=list)
    notes: int = Istr.empty()
    psources: int = Istr.empty()
    key_index: int = Iper.dummy()  # iper - person's unique ID
