from dataclasses import dataclass, field
from enum import Enum
import enum
import sys
from typing import (
    Callable,
    Generator,
    Iterable,
    Literal,
    TextIO,
    List,
    Optional,
    Dict,
    Any,
    Type,
    TypeVar,
    Union,
)
import re

T = TypeVar("T")
B = TypeVar("B", bound="Block")
B2 = TypeVar("B2", bound="Block")

class GedcomTag(Enum):
    INDIVIDUAL = "INDI"
    FAMILY = "FAM"
    NAME = "NAME"
    SEX = "SEX"
    BIRTH = "BIRT"
    DEATH = "DEAT"
    MARRIAGE = "MARR"
    DATE = "DATE"
    PLACE = "PLAC"
    NOTE = "NOTE"
    UNKNOWN = "UNKNOWN"


@dataclass
class GedcomNode:
    level: int
    tag: GedcomTag
    value: Optional[str] = None
    children: List["GedcomNode"] = field(default_factory=list)


@dataclass
class GedcomIndividual:
    id: str
    name: Optional[str] = None
    sex: Optional[str] = None
    birth: Optional[str] = None
    death: Optional[str] = None
    notes: List[str] = field(default_factory=list)


@dataclass
class GedcomFamily:
    id: str
    husband: Optional[str] = None
    wife: Optional[str] = None
    children: List[str] = field(default_factory=list)
    marriage: Optional[str] = None
    notes: List[str] = field(default_factory=list)


class HeaderTag(Enum):
    SOURCE = "SOUR"
    VERSION = "VERS"
    NAME = "NAME"
    CORPORATION = "CORP"
    ADDRESS = "ADDR"
    CONTACT = "CONT"
    PHONE = "PHON"
    DESTINATION = "DEST"
    DATE = "DATE"
    CHARSET = "CHAR"
    FILE = "FILE"
    SUBM = "SUBM"


@dataclass
class GEDCOM:
    version: Optional[str] = None
    format: Optional[str] = None


@dataclass
class Header(Dict[HeaderTag, Optional[str]]):
    # source: Optional[str] = None
    # version: Optional[str] = None
    # name: Optional[str] = None
    # corporation: Optional[str] = None
    # address: Optional[str] = None
    # contact: Optional[str] = None
    # phone: Optional[str] = None
    # file: Optional[str] = None
    # encoding: Optional[str] = None
    # submitter: Optional[str] = None

    # def __setattr__(self, name: str, value: Any) -> None:
    #     return self.__setitem__(HeaderTag[name.upper()], value)

    def __setitem__(self, key: Union[HeaderTag, str], value: str | None) -> None:
        try:
            key = HeaderTag(key)
            super().__setitem__(key, value)
            return
        except ValueError:
            super().__setitem__(key, value)


# usage:
# @dataclass
# @add_structure("PLAC", GEDPlace, "place")
# class GEDPlaceStructure(Block): ...
def add_structure(
    tag: str,
    cls: Type[T],
    property_name: str,
    *,
    preproc=Callable[[Type[T], list[B2]], Any],
) -> Callable[[Type[B]], Type[B]]:
    """Class decorator to add a property to Block that returns an instance of cls

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Actual structure
        property_name (str) : Name of the new property
        preproc (Callable[[Type[B], list["Block"], Any]]) :
            preprocessor function applied before return

    Returns:
        Type[B2]: The class
    """

    def decorator(base_cls: Type[B2]) -> Type[B2]:

        def getter(self: Block) -> Optional[B]:
            try:
                return preproc(cls, self[tag])
            except KeyError:
                return None

        def setter(self: Block, val: Block):
            if tag in self:
                self[tag][0] = val

        setattr(base_cls, property_name, property(getter, setter))
        return base_cls

    return decorator

    # def decorator_list(base_cls: Type[Block]) -> list[Type[B]]:
    #     def getter(self: Block) -> list[B]:
    #         try:
    #             return [cls.from_block(b) for b in self[tag]]
    #         except KeyError:
    #             return []

    #     def setter(self: Block, val: list[Block]):
    #         if tag in self:
    #             self[tag] = val

    #     setattr(base_cls, property_name, property(getter))
    #     return base_cls

    # if is_unique_item:
    #     return decorator_unique
    # return decorator_list


def add_single_structure(tag: str, cls: Type[B], property_name: str):
    """Class decorator to add a property that returns the block of type `cls` found in its
    children that matches the `tag`.

    ### Property Returns:
        Type[B]: An instance of `cls` with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[B], block_list: list[Block]):
        return cls.from_block(block_list[0])

    return add_structure(tag, cls, property_name, preproc=pre)


def add_structure_list(tag: str, cls: Type[B], property_name: str):
    """Class decorator to add a property that returns a list of block of type `cls` found
    in its children that matches the `tag`.

    ### Property Returns:
        list[Type[B]]: A list of instance of `cls` with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[B], block_list: list[Block]):
        return [cls.from_block(b) for b in block_list]

    return add_structure(tag, cls, property_name, preproc=pre)


def add_single_value(tag: str, cls: Type[B], property_name: str):
    """Class decorator to add a property that returns the value of the block of type `cls`
    found in its children that matches the `tag`.

    ### Property Returns:
        Type[B]: Value of an instance of `cls` loaded with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[B], block_list: list[Block]):
        return cls.from_block(block_list[0]).value

    return add_structure(tag, cls, property_name, preproc=pre)


def add_value_list(tag: str, cls: Type[B], property_name: str):
    """Class decorator to add a property that returns the list of values of the blocks of
    type `cls` found in its children that matches the `tag`.

    ### Property Returns:
        Type[B]: A list of value of instances of `cls` loaded with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[B], block_list: list[Block]):
        return [cls.from_block(b).value for b in block_list]

    return add_structure(tag, cls, property_name, preproc=pre)


def add_enum_value(tag: str, cls: Type[Enum], property_name: str):
    """Class decorator to add a property that returns the value of the block of type `cls`
    found in its children that matches the `tag`.

    ### Property Returns:
        Type[B]: Value of an instance of `cls` loaded with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[Enum], block_list: list[Block]):
        return cls(block_list[0].value).value

    return add_structure(tag, cls, property_name, preproc=pre)


def add_object_value(tag: str, cls: Type[T], property_name: str):
    """Class decorator to add a property that returns the value of the block of type `cls`
    found in its children that matches the `tag`.

    ### Property Returns:
        Type[B]: Value of an instance of `cls` loaded with the correct data

    Args:
        tag (str) : Tag that contains the structure
        cls (Type[B]) : Class of the structure
        property_name (str) : Name of the new property

    Returns:
        Type[B2]: The class
    """

    def pre(cls: Type[T], block_list: list[Block]):
        return cls(block_list[0].value).value

    return add_structure(tag, cls, property_name, preproc=pre)


@dataclass
class Block:
    level: int
    tag: str
    pointer: Optional[str] = None
    value: Optional[str] = None
    children_b: List["Block"] = field(default_factory=list)
    _children_idx: dict[str, int] = field(default_factory=dict)
    _children_tag_idx: dict[str, List[int]] = field(default_factory=dict)

    @property
    def phrase(self) -> Optional[str]:
        """Return phrase if present"""
        try:
            return str(StrBlock.from_block(self["PHRASE"][0]))
        except KeyError:
            return None

    @property
    def description(self) -> Optional[str]:
        """Alias for phrase"""
        return self.phrase

    def add_child(self, child: "Block"):
        self.children_b.append(child)
        self._children_idx[child.pointer or child.tag] = len(self.children_b) - 1
        if child.tag not in self._children_tag_idx:
            self._children_tag_idx[child.tag] = []
        self._children_tag_idx[child.tag].append(len(self.children_b) - 1)
        # self.children[child.pointer or child.tag] = child

    def get(self, id: str, default: T = None) -> Union[list["Block"], T]:
        """Return list of blocks with given id/tag or default if not found"""
        if id in self:
            return self[id]
        return default

    def get_value(self, id: str, default: T = None) -> list[Union[str, T]]:
        """
        Return list of values of blocks with given id/tag or empty list if not found.
        If a block has no value, the default is used instead.
        """
        if id in self:
            return [b.value if b.value is not None else default for b in self[id]]
        return []

    def first(self, id: str, default: T = None) -> Union["Block", T]:
        """Return first block with given id/tag or default if not found"""
        if id not in self:
            return default
        try:
            return self[id][0]
        except IndexError:
            return default

    def first_value(self, id: str, default: T = None) -> Union[Any, T]:
        """Return value of first block with given id/tag or default if not found"""
        block = self.first(id)
        if block is None:
            return default
        return block.value if block.value is not None else default

    def __iter__(self):
        return iter(self.children_b)

    def __getitem__(self, key: str) -> List["Block"]:
        if key in self._children_idx:
            return [self.children_b[self._children_idx[key]]]
        if key in self._children_tag_idx:
            idxs = self._children_tag_idx[key]
            return [self.children_b[i] for i in idxs]
        raise KeyError(f"No child with key {key}")

    def __setitem__(self, key: str, val: Union["Block", list["Block"]]):
        if key in self._children_idx and isinstance(val, Block):
            self.children_b[self._children_idx[key]] = val
        if key in self._children_tag_idx and isinstance(val, (list, tuple)):
            idxs = self._children_tag_idx[key]
            if len(idxs) != len(val):
                raise IndexError(
                    f"Too few values to unpack. Expected {len(idxs)}, got {len(val)}"
                )
            for i, idx in enumerate(idxs):
                self.children_b[idx] = val[i]

    def __contains__(self, key: str) -> bool:
        return key in self._children_idx or key in self._children_tag_idx

    @classmethod
    def from_block(cls: Type[T], block: "Block") -> T:
        if not issubclass(cls, Block):
            raise TypeError("Expected block")
        new_block = cls(block.level, block.tag, block.pointer, block.value)
        for child in block.children_b:
            new_block.add_child(child)
        return new_block


@dataclass
class StrBlock(Block):
    def __str__(self):
        val = self.value or ""
        for child in self.children_b:
            if child.tag == "CONT":
                val += "\n" + (child.value or "")
            if child.tag == "CONC":
                val += child.value or ""
        return val


@dataclass
class GEDNotes(StrBlock):
    # We could implement more of it but StrBlock already implements
    # the most important things
    pass


@dataclass
class GEDMultimediaLink(Block):

    @property
    def crop(self) -> Optional[tuple[int, int, int, int]]:
        """Return (top, left, height, width) or None if not present"""
        try:
            crop = self["CROP"][0]
            return (
                crop.first_value("TOP", 0),
                crop.first_value("LEFT", 0),
                crop.first_value("HEIGHT", 0),
                crop.first_value("WIDTH", 0),
            )
        except KeyError:
            return None

    @property
    def title(self) -> Optional[str]:
        try:
            return str(StrBlock.from_block(self["TITL"][0]))
        except KeyError:
            return None


@dataclass
class GEDPlace(StrBlock):
    @property
    def latitude(self) -> Optional[str]:
        try:
            place = self["PLAC"][0]
            return str(StrBlock.from_block(place["LATI"][0]))
        except KeyError:
            return None

    @property
    def longitude(self) -> Optional[str]:
        try:
            place = self["PLAC"][0]
            return str(StrBlock.from_block(place["LONG"][0]))
        except KeyError:
            return None


@dataclass
class GEDDateValue(StrBlock):
    @property
    def time(self) -> Optional[str]:
        try:
            return self["TIME"][0].value
        except KeyError:
            return None


class GEDQuality(Enum):
    UNRELIABLE = "0"
    """Unreliable evidence or estimated data"""
    QUESTIONABLE = "1"
    """
    Questionable reliability of evidence (interviews, census, oral
    genealogies, or potential for bias, such as an autobiography)
    """
    SECONDARY = "2"
    """Secondary evidence, data officially recorded sometime after the event"""
    DIRECT = "3"
    """Direct and primary evidence used, or by dominance of the evidence"""


class GEDSourceCitation(Block):

    @property
    def page(self) -> Optional[str]:
        try:
            return str(StrBlock.from_block(self["PAGE"][0]))
        except KeyError:
            return None

    @property
    def text(self) -> Optional[str]:
        try:
            data = self["DATA"][0]
            return str(StrBlock.from_block(data["TEXT"][0]))
        except KeyError:
            return None

    @property
    def date(self) -> Optional[str]:
        try:
            data = self["DATA"][0]
            return str(GEDDateValue.from_block(data["DATE"][0]))
        except KeyError:
            return None

    @property
    def quality(self) -> Optional[GEDQuality]:
        try:
            return GEDQuality(self["QUAY"][0].value)
        except (KeyError, ValueError):
            return None

    @property
    def multimedia_links(self) -> list[GEDMultimediaLink]:
        try:
            return [GEDMultimediaLink.from_block(link) for link in self["OBJE"]]
        except KeyError:
            return []

    @property
    def notes(self) -> list[str]:
        try:
            return [str(GEDNotes.from_block(note)) for note in self["NOTE"]]
        except KeyError:
            return []


@dataclass
class GEDAddress(Block):
    @property
    def address1(self) -> Optional[str]:
        try:
            return self["ADR1"][0].value
        except KeyError:
            return None

    @property
    def address2(self) -> Optional[str]:
        try:
            return self["ADR2"][0].value
        except KeyError:
            return None

    @property
    def address3(self) -> Optional[str]:
        try:
            return self["ADR3"][0].value
        except KeyError:
            return None

    @property
    def city(self) -> Optional[str]:
        try:
            return self["CITY"][0].value
        except KeyError:
            return None

    @property
    def state(self) -> Optional[str]:
        try:
            return self["STAE"][0].value
        except KeyError:
            return None

    @property
    def post_code(self) -> Optional[str]:
        try:
            return self["POST"][0].value
        except KeyError:
            return None

    @property
    def country(self) -> Optional[str]:
        try:
            return self["CTRY"][0].value
        except KeyError:
            return None


class GEDDate(StrBlock):
    VALID_MONTHS = (
        "JAN",
        "FEB",
        "MAR",
        "APR",
        "MAY",
        "JUN",
        "JUL",
        "AUG",
        "SEP",
        "OCT",
        "NOV",
        "DEC",
    )

    @property
    def year(self):
        if self.value is None:
            return float("nan")
        s = self.value.split()
        return int(s[2])

    @property
    def month(self):
        if self.value is None:
            return -1
        s = self.value.split()
        return int(GEDDate.VALID_MONTHS.index(s[1]))

    @property
    def day(self):
        if self.value is None:
            return float("nan")
        s = self.value.split()
        return int(s[0])

    def __str__(self):
        return f"{self.value}"


@add_single_structure("DATE", GEDDate, "date")
@add_single_structure("PLAC", GEDPlace, "place")
@add_single_structure("ADDR", GEDAddress, "address")
@add_structure_list("NOTE", GEDNotes, "notes")
@add_structure_list("SOUR", GEDSourceCitation, "sources")
@add_structure_list("OBJE", GEDMultimediaLink, "multimedia_links")
@add_single_value("OBJE", StrBlock, "multimedia_links")
class GEDEventDetail(Block):
    date: Optional[GEDDate]
    place: Optional[str]
    address: Optional["GEDAddress"]
    notes: List[GEDNotes]
    sources: List[GEDSourceCitation]
    multimedia_links: list[GEDMultimediaLink]
    cause: Optional[str]


@dataclass
class GEDIndiEventDetail(GEDEventDetail):
    @property
    def age(self) -> Optional[str]:
        try:
            return self["AGE"][0].value
        except KeyError:
            return None

    @property
    def type(self) -> Optional[str]:
        try:
            return self["TYPE"][0].value
        except KeyError:
            return None


@add_single_value("HUSB", Block, "husband_age")
@add_single_value("WIFE", Block, "wife_age")
class GEDFamEventDetail(GEDEventDetail):
    husband_age: Optional[Block]
    wife_age: Optional[Block]


@dataclass
class GEDHeaderSource(StrBlock):
    @property
    def version(self) -> Optional[str]:
        try:
            return self["VERS"][0].value
        except KeyError:
            return None

    @property
    def name(self) -> Optional[str]:
        try:
            return self["NAME"][0].value
        except KeyError:
            return None

    @property
    def corporation(self) -> Optional[str]:
        try:
            return self["CORP"][0].value
        except KeyError:
            return None

    @property
    def address(self) -> Optional[str]:
        try:
            corp = self["CORP"][0]
            return str(StrBlock.from_block(corp["ADDR"][0]))
        except KeyError:
            return None

    @property
    def phone(self) -> Optional[str]:
        try:
            corp = self["CORP"][0]
            return corp["PHON"][0].value
        except KeyError:
            return None


@dataclass
class GEDCOMVersion(Block):

    @property
    def version(self) -> Optional[str]:
        try:
            return self["VERS"][0].value
        except KeyError:
            return None


# @dataclass
@add_single_structure("SOUR", GEDHeaderSource, "source")
class GEDHeader(Block):
    source: Optional[GEDHeaderSource]

    @property
    def destination(self) -> Optional[str]:
        try:
            return self["DEST"][0].value
        except KeyError:
            return None

    @property
    def date(self) -> Optional[str]:
        try:
            return self["DATE"][0].value
        except KeyError:
            return None

    @property
    def encoding(self) -> Optional[str]:
        try:
            return self["CHAR"][0].value
        except KeyError:
            return None

    @property
    def file(self) -> Optional[str]:
        try:
            return self["FILE"][0].value
        except KeyError:
            return None

    @property
    def submitter(self) -> Optional[str]:
        try:
            return self["SUBM"][0].value
        except KeyError:
            return None

    @property
    def gedcom(self) -> Optional[GEDCOMVersion]:
        try:
            return GEDCOMVersion.from_block(self["GEDC"][0])
        except KeyError:
            return None

    def __str__(self):
        source = self.source
        gedcom = self.gedcom
        res = "Header"
        if gedcom and gedcom.version:
            res += f" GEDCOM_{gedcom.version}"
        if source:
            res += f"@{source.value or '<unknown>'} "
            if source.name:
                res += f"({source.name})"
            if source.version:
                res += f" v{source.version}"
        return res


@dataclass
class GEDSubmitter(Block):
    @property
    def name(self) -> str:
        return self["NAME"][0].value

    @property
    def address(self) -> Optional[GEDAddress]:
        try:
            return GEDAddress.from_block(self["ADDR"][0])
        except KeyError:
            return None

    @property
    def email(self) -> Optional[str]:
        try:
            return self["EMAIL"][0].value
        except KeyError:
            return None

    @property
    def notes(self) -> Optional[str]:
        try:
            return str(GEDNotes.from_block(self["NOTE"][0]))
        except KeyError:
            return None


class GEDPedigree(Enum):
    ADOPTED = enum.auto()
    """Adoptive parents"""
    BIRTH = enum.auto()
    """Family structure at time of birth"""
    FOSTER = enum.auto()
    """The child was included in a foster or guardian family"""
    SEALING = enum.auto()
    """The child was sealed to parents other than birth parents"""
    OTHER = enum.auto()
    """A value not listed here; should have a PHRASE substructure"""


class GEDFamilyChild(Block):
    class Status(Enum):
        CHALLENGED = enum.auto()
        """
        Linking this child to this family is suspect, but the linkage has
        been neither proven nor disproven.
        """
        DISPROVEN = enum.auto()
        """
        There has been a claim by some that this child belongs to this
        family, but the linkage has been disproven.
        """
        PROVEN = enum.auto()
        """Linking this child to this family has been proven"""

    @property
    def pedigree(self) -> Optional[tuple[GEDPedigree, Optional[str]]]:
        try:
            phrase = (
                str(StrBlock.from_block(self["PHRASE"][0]))
                if "PHRASE" in self
                else None
            )
            return (
                GEDPedigree[self["PEDI"][0].value or ""],
                phrase,
            )
        except (KeyError, ValueError):
            return None

    @property
    def status(self) -> Optional[tuple[Status, Optional[str]]]:
        try:
            return (
                GEDFamilyChild.Status[self["STAT"][0].value or ""],
                str(StrBlock.from_block(self["PHRASE"][0])),
            )
        except (KeyError, ValueError):
            return None

    @property
    def notes(self) -> Optional[str]:
        try:
            return str(StrBlock.from_block(self["NOTE"][0]))
        except KeyError:
            return None


class GEDNameRecord(Block):
    def __str__(self) -> str:
        return self.value or "<None>"


class GEDSex(Enum):
    MALE = "M"
    FEMALE = "F"
    INTERSEX = "X"
    UNKNWON = "U"
    NOT_RECORDED = "N"


@add_value_list("NAME", GEDNameRecord, "name")
@add_enum_value("SEX", GEDSex, "sex")
@add_single_structure("BIRT", GEDIndiEventDetail, "birth")
@add_single_structure("DEAT", GEDIndiEventDetail, "death")
@add_single_structure("EVEN", GEDIndiEventDetail, "events")
@add_structure_list("FAMC", GEDFamilyChild, "children")
@add_structure_list("FAMS", Block, "spouse")
@add_structure_list("NOTE", GEDNotes, "notes")
@add_structure_list("SOUR", GEDSourceCitation, "sources")
@add_structure_list("OBJE", GEDMultimediaLink, "multimedia_links")
class GEDIndividual(Block):
    name: list[str]
    sex: Optional[Literal["M", "F", "U", "X", "N"]]
    birth: Optional[GEDIndiEventDetail]
    death: Optional[GEDIndiEventDetail]
    events: Optional[GEDIndiEventDetail]
    children: List[GEDFamilyChild]
    spouses: List[GEDFamilyChild]
    notes: list[GEDNotes]
    sources: list[GEDSourceCitation]
    multimedia_links: list[GEDMultimediaLink]

    # TODO: Events/Marriage
    # TODO: Events/Divorce
    # TODO: Events/Burial
    # TODO: Events/Even (Other events)

    # TODO: association
    # TODO: aliases

    # TODO: sources

    def __str__(self):
        return f"Individual {self.name}"


@add_single_structure("HUSB", Block, "husband")
@add_single_structure("WIFE", Block, "wife")
@add_structure_list("CHIL", Block, "children")
@add_structure_list("NOTE", GEDNotes, "notes")
@add_structure_list("SOUR", GEDSourceCitation, "sources")
@add_structure_list("OBJE", GEDMultimediaLink, "multimedia_links")
@add_single_structure("MARR", GEDFamEventDetail, "marriage")
class GEDFamily(Block):
    husband: Optional[Block]
    wife: Optional[Block]
    children: list[Block]
    notes: list[GEDNotes]
    sources: list[GEDSourceCitation]
    multimedia_links: list[GEDMultimediaLink]
    marriage: Optional[GEDFamEventDetail]


@dataclass
class GEDSharedNotes(StrBlock):
    pass


@dataclass
class GEDSource(Block):
    pass


@dataclass
class GEDRepository(Block):
    pass


@dataclass
class GEDMultimedia(Block):
    pass


@dataclass
class GedcomData:
    header: GEDHeader = field(default_factory=lambda: GEDHeader(0, "HEAD"))
    submitter: GEDSubmitter = field(default_factory=lambda: GEDSubmitter(0, "SUBM"))
    individuals: Dict[str, GEDIndividual] = field(default_factory=dict)  # indi
    families: Dict[str, GEDFamily] = field(default_factory=dict)  # fam
    notes: Dict[str, GEDNotes] = field(default_factory=dict)  # note
    sources: Dict[str, GEDSource] = field(default_factory=dict)  # sour
    repository: Optional[GEDRepository] = None  # repo
    multimedia: Dict[str, GEDMultimediaLink] = field(default_factory=dict)  # obje


class InvalidDepth(RuntimeError): ...


class GedcomParser:
    def __init__(self):
        self.individuals: Dict[str, GedcomIndividual] = {}
        self.families: Dict[str, GedcomFamily] = {}
        self.header: Header = Header()
        self.nodes: List[GedcomNode] = []

    @staticmethod
    def read_line(
        io: TextIO,
    ) -> Optional[tuple[int, str, Optional[str], Optional[str]]]:
        line = io.readline()
        if not line:
            return None
        match = re.match(r"^(\d+)\s+(@[^@]+@)?\s*(\w+)(?:\s+(.*))?", line)
        if not match:
            return None
        level = int(match.group(1))
        pointer = match.group(2)
        tag = match.group(3)
        value = match.group(4) if match.group(4) else None
        return level, pointer, tag, value

    def parsed(self, io: TextIO):
        res = GedcomData()
        while block := self.parse_block(io, -1):
            # res[block.pointer or block.tag] = block
            match block.tag:
                case "HEAD":
                    res.header = GedcomParser.specialize_block(block)
                case "SUBM":
                    res.submitter = GedcomParser.specialize_block(block)
                case "INDI":
                    if block.pointer:
                        res.individuals[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "FAM":
                    if block.pointer:
                        res.families[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "NOTE":
                    if block.pointer:
                        res.notes[block.pointer] = GEDSharedNotes.from_block(block)
                case "SOUR":
                    if block.pointer:
                        res.sources[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "REPO":
                    res.repository = GedcomParser.specialize_block(block)
                case "OBJE":
                    print(f"Multimedia: {block.pointer}", file=sys.stderr)
                    if block.pointer:
                        res.multimedia[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "FAMS" | "FAMC" | "_PHOTO":
                    # ignore these, they are inside individuals
                    pass
                case "TRLR":
                    break
                case _:
                    print(f"Unknown block: {block.tag}", file=sys.stderr)
        return res

    def parse_block(
        self,
        io: TextIO,
        root_level: int,
    ):
        level = ""
        i = 0
        while c := io.read(1):
            if c == " ":
                break
            level += c
            i += 1
        io.seek(io.tell() - i - 1)
        if not level.isdigit():
            return None
        level = int(level)
        if level <= root_level:
            # print("no go back")
            return None
        level, pointer, tag, value = GedcomParser.read_line(io) or (
            None,
            None,
            None,
            None,
        )
        if level is None or tag is None:
            return None
        block = GedcomParser.specialize_block(Block(level, tag, pointer, value))
        while child := self.parse_block(io, level):
            block.add_child(child)
        return block

    @staticmethod
    def specialize_block(block: Block) -> Block:
        match block.tag:
            case "HEAD":
                return GEDHeader.from_block(block)
            case "SUBM":
                return GEDSubmitter.from_block(block)
            case "NOTE":
                return GEDNotes.from_block(block)
            case "INDI":
                return GEDIndividual.from_block(block)
            case "FAM":
                return GEDFamily.from_block(block)
            case "SOUR":
                return GEDSource.from_block(block)
            case "REPO":
                return GEDRepository.from_block(block)
            case "OBJE":
                return GEDMultimedia.from_block(block)
            case _:
                return block

    @staticmethod
    def from_file(filepath: str) -> GedcomData:
        with open(filepath, "r", encoding="utf-8-sig") as f:
            parser = GedcomParser()
            return parser.parsed(f)


class GedcomExporter:
    @staticmethod
    def export_summary(data: GedcomData) -> Generator[str, None, None]:
        # head
        yield from GedcomExporter._export_block(data.header)
        # subm
        yield from GedcomExporter._export_block(data.submitter)
        # indi
        for indi in data.individuals.values():
            yield from GedcomExporter._export_block(indi)
        # fam
        for fam in data.families.values():
            yield from GedcomExporter._export_block(fam)
        # note
        for note in data.notes.values():
            yield from GedcomExporter._export_block(note)
        # sour
        for sour in data.sources.values():
            yield from GedcomExporter._export_block(sour)
        # repo
        if data.repository:
            yield from GedcomExporter._export_block(data.repository)
        # obje
        for obje in data.multimedia.values():
            yield from GedcomExporter._export_block(obje)
        # trlr
        yield "0 TRLR"

    @staticmethod
    def _export_block(block: Block) -> Generator[str, None, None]:
        line = f"{block.level} "
        if block.pointer:
            line += f"{block.pointer} "
        line += block.tag
        if block.value:
            line += f" {block.value}"
        yield line
        for child in block:
            yield from GedcomExporter._export_block(child)


if __name__ == "__main__":
    path = "/home/nico/Dev/Legacy/geneweb/examples/uk.ged"
    import pickle

    r = GedcomParser.from_file(path)
    print(r.individuals["@I399@"])
    # print(r.submitter["FAM"])
    # for l in GedcomExporter.export_summary(r):
    #     print(l)

    # print(r.header["SOUR"]["VERS"])
    # p = pickle.dumps(r, protocol=4)
    # print(p)
    # dp = pickle.loads(p)
    # print(dp)
    # print(dp.header["SOUR"]["VERS"])
    # print(list((b.tag, b.value) for b in r["@SUBM@"]))
    # print(parser.export())
    # for i in range(max(0, len(parser.families) - 1)):
    #     print(list(parser.families.items())[i])
