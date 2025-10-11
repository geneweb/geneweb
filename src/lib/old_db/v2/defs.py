import dataclasses
import enum
from http import HTTPStatus
import time
from typing import (
    Any,
    Callable,
    Dict,
    Generic,
    List,
    Literal,
    Optional,
    Tuple,
    TypeVar,
    Union,
)

from lib.old_db.unmarshall.v2.stdlib import Ref, StringRef, TypeEnum


T = TypeVar("T")

TN = TypeVar("TN", bound=str)


@dataclasses.dataclass
class TypeRef(Generic[TN, T]):
    type_name: str
    type: T


@dataclasses.dataclass
class CDate:
    """Compressed date."""

    compressed: int = dataclasses.field(default=None)
    year: Optional[int] = dataclasses.field(init=False, default=None)
    month: Optional[int] = dataclasses.field(init=False, default=None)
    day: Optional[int] = dataclasses.field(init=False, default=None)
    prec: Optional["CDate.Precision"] = dataclasses.field(init=False, default=None)

    class Precision(enum.Enum):
        Sure = 0
        About = 1
        Maybe = 2
        Before = 3
        After = 4

    def __post_init__(self):
        x = self.compressed
        self.year, x = x % 2500, x // 2500
        self.month, x = x % 13, x // 13
        self.day, x = x % 32, x // 32
        self.prec: CDate.Precision
        match x:
            case 1:
                self.prec = CDate.Precision.About
            case 2:
                self.prec = CDate.Precision.Maybe
            case 3:
                self.prec = CDate.Precision.Before
            case 4:
                self.prec = CDate.Precision.After
            case _:
                self.prec = CDate.Precision.Sure

    # calendar is only used if date is not None
    def __str__(self):
        if self.compressed is None:
            return "Cnone"
        return f"{self.day:02}/{self.month:02}/{self.year} {self.prec.name}"


@dataclasses.dataclass
class Title:
    t_name: Union[Literal["Tmain"], StringRef, None]
    t_ident: StringRef
    t_place: StringRef
    t_date_start: CDate
    t_date_end: CDate
    t_nth: int


RelationType = Union[
    Literal["Adoption"],
    Literal["Recognition"],
    Literal["CandidateParent"],
    Literal["GodParent"],
    Literal["FosterParent"],
]


@dataclasses.dataclass
class Relation:
    r_type: RelationType
    r_fath: Optional[Ref["Person"]]
    r_moth: Optional[Ref["Person"]]
    r_sources: str


DeathReason = Union[
    # this is weird but it is easier to add those here than to implement the logic
    # Literal["NotDead"],
    # Literal["DeadYoung"],
    # Literal["DeadDontKnowWhen"],
    # Literal["DontKnowIfDead"],
    # Literal["OfCourseDead"],
    # actual death reasons
    Literal["Killed"],
    Literal["Murdered"],
    Literal["Executed"],
    Literal["Disappeared"],
    Literal["Unspecified"],
]


class Death(TypeEnum):
    NOT_DEAD = Literal["NotDead"]
    DEATH = Tuple[DeathReason, CDate]  # Death
    DEAD_YOUNG = (Literal["DeadYoung"],)
    DEAD_DONT_KNOW_WHEN = (Literal["DeadDontKnowWhen"],)
    DONT_KNOW_IF_DEAD = (Literal["DontKnowIfDead"],)
    OF_COURSE_DEAD = (Literal["OfCourseDead"],)


class Burial(TypeEnum):
    UnknownBurial = Literal["UnknownBurial"]
    Burried = Tuple[CDate]
    Cremated = Tuple[CDate]


PersEventName = Union[
    Literal["Epers_Birth"],
    Literal["Epers_Baptism"],
    Literal["Epers_Death"],
    Literal["Epers_Burial"],
    Literal["Epers_Cremation"],
    Literal["Epers_Accomplishment"],
    Literal["Epers_Acquisition"],
    Literal["Epers_Adhesion"],
    Literal["Epers_BaptismLDS"],
    Literal["Epers_BarMitzvah"],
    Literal["Epers_BatMitzvah"],
    Literal["Epers_Benediction"],
    Literal["Epers_ChangeName"],
    Literal["Epers_Circumcision"],
    Literal["Epers_Confirmation"],
    Literal["Epers_ConfirmationLDS"],
    Literal["Epers_Decoration"],
    Literal["Epers_DemobilisationMilitaire"],
    Literal["Epers_Diploma"],
    Literal["Epers_Distinction"],
    Literal["Epers_Dotation"],
    Literal["Epers_DotationLDS"],
    Literal["Epers_Education"],
    Literal["Epers_Election"],
    Literal["Epers_Emigration"],
    Literal["Epers_Excommunication"],
    Literal["Epers_FamilyLinkLDS"],
    Literal["Epers_FirstCommunion"],
    Literal["Epers_Funeral"],
    Literal["Epers_Graduate"],
    Literal["Epers_Hospitalisation"],
    Literal["Epers_Illness"],
    Literal["Epers_Immigration"],
    Literal["Epers_ListePassenger"],
    Literal["Epers_MilitaryDistinction"],
    Literal["Epers_MilitaryPromotion"],
    Literal["Epers_MilitaryService"],
    Literal["Epers_MobilisationMilitaire"],
    Literal["Epers_Naturalisation"],
    Literal["Epers_Occupation"],
    Literal["Epers_Ordination"],
    Literal["Epers_Property"],
    Literal["Epers_Recensement"],
    Literal["Epers_Residence"],
    Literal["Epers_Retired"],
    Literal["Epers_ScellentChildLDS"],
    Literal["Epers_ScellentParentLDS"],
    Literal["Epers_ScellentSpouseLDS"],
    Literal["Epers_VenteBien"],
    Literal["Epers_Will"],
    str,  # Epers_Name
]

WitnessKind = Union[
    Literal["Witness"],
    Literal["Witness_GodParent"],
    Literal["Witness_CivilOfficer"],
    Literal["Witness_ReligiousOfficer"],
    Literal["Witness_Informant"],
    Literal["Witness_Attending"],
    Literal["Witness_Mentioned"],
    Literal["Witness_Other"],
]


@dataclasses.dataclass
class PersEvent:
    epers_name: PersEventName
    epers_date: CDate
    epers_place: StringRef
    epers_reason: StringRef
    epers_note: StringRef
    epers_src: StringRef
    epers_witnesses: List[Tuple[Ref["Person"], WitnessKind]]


class Sex(enum.Enum):
    Male = "Male"
    Female = "Female"
    Neuter = "Neuter"


class PersonAccess(enum.Enum):
    IfTitles = "IfTitles"
    Public = "Public"
    SemiPublic = "SemiPublic"
    Private = "Private"


@dataclasses.dataclass
class Person:
    """Represents a person record in the database."""

    first_name: StringRef
    surname: StringRef
    occ: int
    image: StringRef
    public_name: StringRef
    qualifiers: List[StringRef]
    aliases: List[StringRef]
    first_names_aliases: List[StringRef]
    surnames_aliases: List[StringRef]
    titles: List[Title]
    # (* relations with not native parents *)
    rparents: List[Relation]
    # (* related persons like (father of witnessed family,
    #     concerned person of witnessed event, adopted child, etc.) *)
    related: List[Ref["Person"]]
    occupation: StringRef
    sex: Sex
    access: PersonAccess
    birth: CDate
    birth_place: StringRef
    birth_note: StringRef
    birth_src: StringRef
    baptism: CDate
    baptism_place: StringRef
    baptism_note: StringRef
    baptism_src: StringRef
    death: Death
    death_place: StringRef
    death_note: StringRef
    death_src: StringRef
    burial: Burial
    burial_place: StringRef
    burial_note: StringRef
    burial_src: StringRef
    pevents: List[PersEvent]
    notes: StringRef
    psources: StringRef
    key_index: Ref[int]


class RelationKind(enum.Enum):
    Married = "Married"
    NotMarried = "NotMarried"
    Engaged = "Engaged"
    NoSexesCheckNotMarried = "NoSexesCheckNotMarried"
    NoMention = "NoMention"
    NoSexesCheckMarried = "NoSexesCheckMarried"
    MarriageBann = "MarriageBann"
    MarriageContract = "MarriageContract"
    MarriageLicense = "MarriageLicense"
    Pacs = "Pacs"
    Residence = "Residence"


class Divorce(TypeEnum):
    NotDivorced = Literal["NotDivorced"]
    Divorced = CDate
    Separated_old = Literal["Separated_old"]
    NotSeparated = Literal["NotSeparated"]
    Separated = CDate


FamEventName = Union[
    Literal["Efam_Marriage"],
    Literal["Efam_NoMarriage"],
    Literal["Efam_NoMention"],
    Literal["Efam_Engage"],
    Literal["Efam_Divorce"],
    Literal["Efam_Separated"],
    Literal["Efam_Annulation"],
    Literal["Efam_MarriageBann"],
    Literal["Efam_MarriageContract"],
    Literal["Efam_MarriageLicense"],
    Literal["Efam_PACS"],
    Literal["Efam_Residence"],
]


@dataclasses.dataclass
class FamEvent:
    efam_name: FamEventName
    efam_date: CDate
    efam_place: StringRef
    efam_reason: StringRef
    efam_note: StringRef
    efam_src: StringRef
    efam_witnesses: List[Tuple[Ref[Person], WitnessKind]]


@dataclasses.dataclass
class Family:
    """Represents a family record in the database."""

    marriage: CDate
    marriage_place: StringRef
    marriage_note: StringRef
    marriage_src: StringRef
    witnesses: List[Ref[Person]]
    relation: RelationKind
    divorce: Divorce
    fevents: List[FamEvent]
    comment: StringRef
    origin_file: StringRef
    # (* .gw filename where family is defined *)
    fsources: StringRef
    fam_index: Ref[int]


@dataclasses.dataclass
class Ascend:
    parents: Optional[Ref[Family]]
    consang: int


@dataclasses.dataclass
class DskUnion:
    family: List[Ref[Family]]


@dataclasses.dataclass
class Couple:
    father: Ref[Person]
    mother: Ref[Person]


@dataclasses.dataclass
class Descend:
    children: List[Ref[Person]]


# TODO: check if we can use that
PatchEntry = Tuple[Ref[int], Dict[int, T]]


# Equivalent of OCaml patches_ht
@dataclasses.dataclass
class PatchesHT:
    h_person: Tuple[Ref[int], Dict[int, Person]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_ascend: Tuple[Ref[int], Dict[int, Ascend]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_union: Tuple[Ref[int], Dict[int, DskUnion]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_family: Tuple[Ref[int], Dict[int, Family]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_couple: Tuple[Ref[int], Dict[int, Couple]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_descend: Tuple[Ref[int], Dict[int, Descend]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_string: Tuple[Ref[int], Dict[int, str]] = dataclasses.field(
        default_factory=lambda: (Ref(0), {})
    )
    h_name: Dict[int, List[int]] = dataclasses.field(default_factory=dict)


# Equivalent of OCaml Old.patches
class OldPatches:
    def __init__(self):
        self.p_person: List[Tuple[int, Person]] = []
        self.p_ascend: List[Tuple[int, Ascend]] = []
        self.p_union: List[Tuple[int, DskUnion]] = []
        self.p_family: List[Tuple[int, Family]] = []
        self.p_couple: List[Tuple[int, Couple]] = []
        self.p_descend: List[Tuple[int, Descend]] = []
        self.p_string: List[Tuple[int, str]] = []
        self.p_name: List[Tuple[int, List[int]]] = []


@dataclasses.dataclass
class SynchroPatch:
    """
    Represents synchronization patches for GeneWeb database.
    Each patch contains:
    - timestamp string
    - list of modified person IDs
    - list of modified family IDs
    """

    synch_list: List[Tuple[str, List[int], List[int]]] = dataclasses.field(
        default_factory=list
    )

    def insert_patch(self, persons: List[int], families: List[int]):
        timestamp = str(int(time.time()))
        new_entry = (timestamp, persons, families)
        self.synch_list.insert(0, new_entry)


@enum.unique
class NoteReadMode(enum.Enum):
    """Database note/page reading mode"""

    ALL = 0  #      Read all content
    ONE_LINE = 1  # Read first line
    DEG = 2  #      If file isn't empty returns a space


@dataclasses.dataclass
class BaseNotes:
    """
    Database note/page explorer structure

    Attributes:
        nread: Read content of the page with given mode.
               Page "" represents database note
        norigin_file: Origin .gw filename
        efiles: Returns list of extended pages
    """

    norigin_file: str

    def nread(self, title: str, mode: NoteReadMode) -> str:
        """Read content of the page with given mode.
        Page "" represents database note
        """
        raise NotImplementedError

    def efiles(self) -> List[str]:
        """Returns list of extended pages"""
        raise NotImplementedError


class HttpError(RuntimeError):
    def __init__(self, status: HTTPStatus):
        super().__init__(status.phrase)
