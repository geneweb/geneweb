"""
Pickle Database - GeneWeb Database System in Python

Complete implementation of pickle-based genealogical database driver.
"""

from .core import (
    Access,
    BurialType,
    DeathType,
    DivorceStatus,
    Ifam,
    Iper,
    Istr,
    RelationKind,
    Sex,
)
from .database import (
    PickleBase,
    PickleBaseData,
    PickleBaseFunc,
    PickleFuncFactory,
    PicklePatches,
    PickleSearchIndex,
    create_pickle_base_func,
)
from .io import PickleReader, PickleWriter
from .models import (
    Date,
    Event,
    GenAscend,
    GenCouple,
    GenDescend,
    GenFamily,
    GenPerson,
    GenUnion,
    Title,
)

__version__ = "1.0.0"

__all__ = [
    # Core types
    "Iper",
    "Ifam",
    "Istr",
    # Enums
    "Sex",
    "Access",
    "DeathType",
    "BurialType",
    "RelationKind",
    "DivorceStatus",
    # Models
    "Date",
    "Event",
    "Title",
    "GenPerson",
    "GenAscend",
    "GenUnion",
    "GenFamily",
    "GenCouple",
    "GenDescend",
    # Database
    "PickleBase",
    "PickleBaseData",
    "PickleBaseFunc",
    "PickleSearchIndex",
    "PicklePatches",
    "PickleFuncFactory",
    "create_pickle_base_func",
    # IO
    "PickleWriter",
    "PickleReader",
]
