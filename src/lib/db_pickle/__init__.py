"""
Pickle Database - GeneWeb Database System in Python

Complete implementation of pickle-based genealogical database driver.
"""

from .core import Iper, Ifam, Istr, Sex, Access, DeathType, BurialType, RelationKind, DivorceStatus
from .models import Date, Event, Title, GenAscend, GenUnion, GenFamily, GenCouple, GenDescend, GenPerson
from .database import (
    PickleBase, PickleBaseData, PickleBaseFunc,
    PickleSearchIndex, PicklePatches, PickleFuncFactory,
    create_pickle_base_func
)
from .io import PickleWriter, PickleReader

__version__ = "1.0.0"

__all__ = [
    # Core types
    "Iper", "Ifam", "Istr",
    # Enums
    "Sex", "Access", "DeathType", "BurialType", "RelationKind", "DivorceStatus",
    # Models
    "Date", "Event", "Title",
    "GenPerson", "GenAscend", "GenUnion",
    "GenFamily", "GenCouple", "GenDescend",
    # Database
    "PickleBase", "PickleBaseData", "PickleBaseFunc",
    "PickleSearchIndex", "PicklePatches", "PickleFuncFactory",
    "create_pickle_base_func",
    # IO
    "PickleWriter", "PickleReader"
]
