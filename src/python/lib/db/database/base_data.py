"""
BaseData container for database arrays.

Holds all the data arrays (persons, families, strings, etc.)
with their access methods.
"""

from dataclasses import dataclass, field
from typing import List

from ..models.family import GenFamily
from ..models.person import GenPerson
from ..models.relations import GenAscend, GenCouple, GenDescend, GenUnion
from .record_access import RecordAccess


@dataclass
class BaseData:
    """Container for all database data arrays."""

    persons: RecordAccess[GenPerson]
    ascends: RecordAccess[GenAscend]
    unions: RecordAccess[GenUnion]
    families: RecordAccess[GenFamily]
    couples: RecordAccess[GenCouple]
    descends: RecordAccess[GenDescend]
    strings: RecordAccess[str]
    bdir: str  # Base directory
    particles: List[str] = field(default_factory=list)
