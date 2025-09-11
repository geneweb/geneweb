from dataclasses import dataclass, field
from enum import Enum, auto
from typing import Any, Dict, List, Optional, Tuple
import io

# Basic enums
class MonthNumberDates(Enum):
    MonthDayDates = auto()
    DayMonthDates = auto()
    NoMonthNumberDates = auto()
    MonthNumberHappened = auto()

class Charset(Enum):
    Ansel = auto()
    Ansi = auto()
    Ascii = auto()
    Msdos = auto()
    MacIntosh = auto()
    Utf8 = auto()

class Case(Enum):
    NoCase = auto()
    LowerCase = auto()
    UpperCase = auto()

# Date-related enums & types
class Calendar(Enum):
    GREGORIAN = auto()
    JULIAN = auto()
    FRENCH = auto()
    HEBREW = auto()

class Precision(Enum):
    SURE = auto()
    ABOUT = auto()
    MAYBE = auto()
    BEFORE = auto()
    AFTER = auto()
    OR = auto()
    BETWEEN = auto()

class DateFormat(Enum):
    Day = auto()
    Month = auto()
    Year = auto()
    DayMonth = auto()
    MonthYear = auto()
    DayMonthYear = auto()
    Text = auto()

# Event types
class PersonEventType(Enum):
    Birth = auto()
    Baptism = auto()
    Death = auto()
    Burial = auto()
    # ...etc, same as in events.py

class FamilyEventType(Enum):
    Marriage = auto()
    NoMarriage = auto()
    # ...etc, same as in events.py

class WitnessType(Enum):
    Witness = auto()
    GodParent = auto()
    # ...etc, same as in events.py

@dataclass
class Event:
    type: PersonEventType | FamilyEventType
    date: Optional[str] = None
    place: str = ""
    note: str = ""
    source: str = ""
    witnesses: List[Tuple[int, WitnessType]] = field(default_factory=list)

@dataclass
class DateValue:
    format: DateFormat
    calendar: Calendar
    precision: Precision
    day: Optional[int] = None
    month: Optional[int] = None
    year: Optional[int] = None
    text: Optional[str] = None

@dataclass
class Tab:
    arr: List[Any] = field(default_factory=list)
    tlen: int = 0

@dataclass
class Gen:
    g_per: Tab = field(default_factory=Tab)
    g_fam: Tab = field(default_factory=Tab)
    g_str: Tab = field(default_factory=Tab)
    g_bnot: str = ""
    g_ic: Optional[io.BufferedReader] = None
    g_not: Dict[str,int] = field(default_factory=dict)
    g_src: Dict[str,int] = field(default_factory=dict)
    g_hper: Dict[str,int] = field(default_factory=dict)
    g_hfam: Dict[str,int] = field(default_factory=dict)
    g_hstr: Dict[str,int] = field(default_factory=dict)
    g_hnam: Dict[str,int] = field(default_factory=dict)
    g_adop: Dict[str, Tuple[int,str]] = field(default_factory=dict)
    g_godp: List[Tuple[int,int]] = field(default_factory=list)
    g_prelated: List[Tuple[int,int]] = field(default_factory=list)
    g_frelated: List[Tuple[int,int]] = field(default_factory=list)
    g_witn: List[Tuple[int,int]] = field(default_factory=list)

@dataclass
class Record:
    rlab: str
    rval: str
    rcont: str = ""
    rsons: List["Record"] = field(default_factory=list)
    rpos: int = 0
    rused: bool = False
