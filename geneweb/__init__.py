"""
GeneWeb - Genealogical Database System in Python
Complete implementation of GeneWeb database driver.
"""

from .core import Iper, Ifam, Istr, Sex, Access, DeathType, BurialType, RelationKind, DivorceStatus
from .models import Date, Title, Event, GenAscend, GenUnion, GenFamily, GenCouple, GenDescend, GenPerson, Relation
from .collections import Collection, Marker
from .database import RecordAccess, BaseData, SearchIndex, BaseFunc, Base
from .wrappers.person import Person
from .wrappers.family import Family
from .utils import NameUtils, BinarySearch
from .io import FileReader, DatabaseWriter, GedcomParser

__version__ = "1.0.0"

__all__ = [
    # Core types
    "Iper", "Ifam", "Istr",
    # Enums
    "Sex", "Access", "DeathType", "BurialType", "RelationKind", "DivorceStatus",
    # Models
    "Date", "Title", "Event", "Relation",
    "GenPerson", "GenAscend", "GenUnion",
    "GenFamily", "GenCouple", "GenDescend",
    # Collections
    "Collection", "Marker",
    # Database
    "RecordAccess", "BaseData", "SearchIndex", "BaseFunc", "Base",
    "Person", "Family",
    # Utils
    "NameUtils", "BinarySearch"
    # IO
    "FileReader", "DatabaseWriter", "GedcomParser"
]
