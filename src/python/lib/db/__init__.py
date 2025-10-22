"""
GeneWeb database library.
MessagePack format for genealogical data.
"""

# Import MessagePack components
from .io.msgpack import MessagePackWriter, MessagePackReader
from .database.base import Base as Database
from .database.base import Base
from .database.base_data import BaseData
from .database.base_func import BaseFunc
from .database.search_index import SearchIndex
from .database.patches import Patches
from .database.func_factory import FuncFactory
from .database.func_creator import create_base_func

# Import shared models
from .models.person import GenPerson
from .models.family import GenFamily
from .models.events import Date, Event, Title
from .core.types import Iper, Ifam, Istr
from .core.enums import Sex, RelationKind, DivorceStatus

__all__ = [
    # MessagePack classes
    "MessagePackWriter",
    "MessagePackReader",
    "Database",
    "Base",
    "BaseData",
    "BaseFunc",
    "SearchIndex",
    "Patches",
    "FuncFactory",
    "create_base_func",
    # Shared model classes
    "GenPerson",
    "GenFamily",
    "Date",
    "Event",
    "Title",
    "Iper",
    "Ifam",
    "Istr",
    "Sex",
    "RelationKind",
    "DivorceStatus",
]
