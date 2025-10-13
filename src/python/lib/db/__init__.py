"""
GeneWeb - Genealogical Database System in Python
Complete implementation of GeneWeb database driver.
"""

from .collections import Collection, Marker
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
from .database import Base, BaseData, BaseFunc, RecordAccess, SearchIndex
from .io import DatabaseWriter
from .models import (
    Date,
    Event,
    GenAscend,
    GenCouple,
    GenDescend,
    GenFamily,
    GenPerson,
    GenUnion,
    Relation,
    Title,
)
from .utils import NameUtils
from .wrappers.family import Family
from .wrappers.person import Person

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
    "Title",
    "Event",
    "Relation",
    "GenPerson",
    "GenAscend",
    "GenUnion",
    "GenFamily",
    "GenCouple",
    "GenDescend",
    # Collections
    "Collection",
    "Marker",
    # Database
    "RecordAccess",
    "BaseData",
    "SearchIndex",
    "BaseFunc",
    "Base",
    "Person",
    "Family",
    # Utils
    "NameUtils",
    "BinarySearch"
    # IO
    "FileReader",
    "DatabaseWriter",
    "GedcomParser",
]
