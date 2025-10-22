"""
GEDCOM Data Models - Clean Code Architecture

This module contains all data models for GEDCOM 5.5.1 following clean code principles:
- Immutable where possible
- Clear data structures
- Type safety
- Comprehensive documentation
"""

from dataclasses import dataclass, field
from enum import Enum
from typing import Any, Dict, List, Optional, Tuple


class GedcomSex(Enum):
    """GEDCOM sex values."""

    MALE = "M"
    FEMALE = "F"
    UNKNOWN = "U"


class GedcomDateType(Enum):
    """GEDCOM date qualifier types."""

    ABOUT = "ABT"
    CALCULATED = "CAL"
    ESTIMATED = "EST"
    BEFORE = "BEF"
    AFTER = "AFT"
    BETWEEN = "BET"
    FROM = "FROM"
    TO = "TO"


@dataclass
class GedcomDateRange:
    """GEDCOM date range representation."""

    start_date: Optional["GedcomDate"] = None
    end_date: Optional["GedcomDate"] = None
    range_type: Optional[str] = None


@dataclass
class GedcomDate:
    """GEDCOM date representation with full 5.5.1 support."""

    raw: str
    year: Optional[int] = None
    month: Optional[int] = None
    day: Optional[int] = None

    # Date qualifiers
    is_approximate: bool = False  # ABT
    is_calculated: bool = False  # CAL
    is_estimated: bool = False  # EST
    is_before: bool = False  # BEF
    is_after: bool = False  # AFT

    # Date ranges
    is_range: bool = False
    range_type: Optional[str] = None  # BET, FROM...TO, etc.
    start_date: Optional["GedcomDate"] = None
    end_date: Optional["GedcomDate"] = None

    # Calendar support
    calendar: Optional[str] = None  # GREG, JUL, HEB, FRENCH_R, etc.

    # Partial dates
    has_year: bool = False
    has_month: bool = False
    has_day: bool = False

    def __str__(self) -> str:
        """String representation of the date."""
        return self.raw

    @property
    def is_valid(self) -> bool:
        """Check if date has valid components."""
        return self.year is not None or self.is_range

    @property
    def sort_key(self) -> int:
        """Get sort key for date comparison."""
        if self.is_range and self.start_date:
            return self.start_date.sort_key
        if self.year is None:
            return 0
        return (self.year * 10000) + ((self.month or 1) * 100) + (self.day or 1)

    @property
    def is_partial(self) -> bool:
        """Check if this is a partial date (missing year, month, or day)."""
        return not self.has_year or not self.has_month or not self.has_day


@dataclass
class GedcomMap:
    """GEDCOM map structure for geographic coordinates."""

    latitude: Optional[str] = None  # LATI
    longitude: Optional[str] = None  # LONG


@dataclass
class GedcomPlace:
    """GEDCOM place representation."""

    name: str
    parts: List[str] = field(default_factory=list)
    latitude: Optional[str] = None  # LATI
    longitude: Optional[str] = None  # LONG
    map: Optional[GedcomMap] = None  # MAP

    def __str__(self) -> str:
        """String representation of the place."""
        return self.name

    @property
    def city(self) -> Optional[str]:
        """Get city from place parts."""
        return self.parts[0] if self.parts else None

    @property
    def country(self) -> Optional[str]:
        """Get country from place parts (usually last)."""
        return self.parts[-1] if self.parts else None


@dataclass
class GedcomEvent:
    """GEDCOM event with details."""

    tag: str
    date: Optional[GedcomDate] = None
    place: Optional[GedcomPlace] = None
    age: Optional[str] = None
    cause: Optional[str] = None
    note: Optional[str] = None
    sources: List[str] = field(default_factory=list)
    attributes: Dict[str, str] = field(default_factory=dict)

    def __str__(self) -> str:
        """String representation of the event."""
        parts = [self.tag]
        if self.date:
            parts.append(str(self.date))
        if self.place:
            parts.append(str(self.place))
        return " ".join(parts)

    @property
    def is_birth(self) -> bool:
        """Check if this is a birth event."""
        return self.tag in ["BIRT", "CHR", "BAPM"]

    @property
    def is_death(self) -> bool:
        """Check if this is a death event."""
        return self.tag in ["DEAT", "BURI", "CREM"]


@dataclass
class GedcomName:
    """Person's name with components."""

    full: str
    given: Optional[str] = None
    surname: Optional[str] = None
    prefix: Optional[str] = None
    suffix: Optional[str] = None
    nickname: Optional[str] = None

    # Name variants
    romanized: Optional[str] = None  # ROMN
    phonetic: Optional[str] = None  # FONE
    translation: Optional[str] = None  # TRAN
    name_type: Optional[str] = None  # TYPE

    def __str__(self) -> str:
        """String representation of the name."""
        return self.full

    @property
    def display_name(self) -> str:
        """Get display-friendly name."""
        if self.given and self.surname:
            return f"{self.given} {self.surname}"
        return self.full

    @property
    def sort_name(self) -> str:
        """Get name for sorting (surname, given)."""
        if self.surname and self.given:
            return f"{self.surname}, {self.given}"
        return self.full


@dataclass
class GedcomIndividual:
    """GEDCOM individual (person) record."""

    xref: str
    names: List[GedcomName] = field(default_factory=list)
    sex: Optional[str] = None

    # Life events
    birth: Optional[GedcomEvent] = None
    death: Optional[GedcomEvent] = None
    baptism: Optional[GedcomEvent] = None
    burial: Optional[GedcomEvent] = None

    # Religious events
    confirmation: Optional[GedcomEvent] = None  # CONF
    adult_christening: Optional[GedcomEvent] = None  # CHRA
    bar_mitzvah: Optional[GedcomEvent] = None  # BARM
    bas_mitzvah: Optional[GedcomEvent] = None  # BASM
    blessing: Optional[GedcomEvent] = None  # BLES
    ordination: Optional[GedcomEvent] = None  # ORDN

    # Legal events
    adoption: Optional[GedcomEvent] = None  # ADOP
    naturalization: Optional[GedcomEvent] = None  # NATU
    probate: Optional[GedcomEvent] = None  # PROB
    will: Optional[GedcomEvent] = None  # WILL

    # Migration events
    emigration: Optional[GedcomEvent] = None  # EMIG
    immigration: Optional[GedcomEvent] = None  # IMMI

    # Census events
    census: List[GedcomEvent] = field(default_factory=list)  # CENS

    # Other events
    retirement: Optional[GedcomEvent] = None  # RETI
    events: List[GedcomEvent] = field(default_factory=list)  # EVEN and other events

    # Attributes
    occupations: List[str] = field(default_factory=list)
    titles: List[str] = field(default_factory=list)

    # Physical attributes
    caste: Optional[str] = None  # CAST
    physical_description: Optional[str] = None  # DSCR

    # Education and identification
    education: List[str] = field(default_factory=list)  # EDUC
    identification_numbers: List[Dict[str, str]] = field(
        default_factory=list
    )  # IDNO with TYPE
    nationality: List[str] = field(default_factory=list)  # NATI

    # Family statistics
    number_of_children: Optional[str] = None  # NCHI
    number_of_marriages: Optional[str] = None  # NMR

    # Other attributes
    properties: List[str] = field(default_factory=list)  # PROP
    religion: List[str] = field(default_factory=list)  # RELI
    residence: List[GedcomEvent] = field(default_factory=list)  # RESI
    social_security_number: Optional[str] = None  # SSN

    # Address and contact
    address: Optional["GedcomAddress"] = None  # ADDR

    # Family relationships
    famc: List[str] = field(default_factory=list)  # Family as child
    fams: List[str] = field(default_factory=list)  # Family as spouse

    # Other
    notes: List[str] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)
    source_citations: List["GedcomSourceCitation"] = field(default_factory=list)
    attributes: Dict[str, Any] = field(default_factory=dict)

    reference_numbers: List[str] = field(default_factory=list)  # REFN
    multimedia: List[str] = field(default_factory=list)  # OBJE
    private_links: List[str] = field(default_factory=list)  # _LINK
    private_photos: List[str] = field(default_factory=list)  # _PHOTO
    private_dates: List[str] = field(default_factory=list)  # _DATE
    private_texts: List[str] = field(default_factory=list)  # _TEXT
    family_relationships: List[str] = field(default_factory=list)  # _FREL
    marriage_relationships: List[str] = field(default_factory=list)  # _MREL
    source_pages: List[str] = field(default_factory=list)
    source_data: List[str] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the individual."""
        name = self.primary_name.display_name if self.primary_name else "Unknown"
        return f"{name} ({self.xref})"

    @property
    def primary_name(self) -> Optional[GedcomName]:
        """Get primary name."""
        return self.names[0] if self.names else None

    @property
    def birth_year(self) -> Optional[int]:
        """Get birth year."""
        return self.birth.date.year if self.birth and self.birth.date else None

    @property
    def death_year(self) -> Optional[int]:
        """Get death year."""
        return self.death.date.year if self.death and self.death.date else None

    @property
    def is_alive(self) -> bool:
        """Check if person is likely alive (no death event)."""
        return self.death is None

    @property
    def lifespan(self) -> str:
        """Get lifespan string."""
        birth = str(self.birth_year) if self.birth_year else "?"
        death = (
            str(self.death_year) if self.death_year else ("" if self.is_alive else "?")
        )
        return f"{birth}-{death}"


@dataclass
class GedcomFamily:
    """GEDCOM family record."""

    xref: str
    husband: Optional[str] = None
    wife: Optional[str] = None
    children: List[str] = field(default_factory=list)

    # Marriage events
    marriage: Optional[GedcomEvent] = None
    engagement: Optional[GedcomEvent] = None
    marriage_banns: Optional[GedcomEvent] = None  # MARB
    marriage_contract: Optional[GedcomEvent] = None  # MARC
    marriage_license: Optional[GedcomEvent] = None  # MARL
    marriage_settlement: Optional[GedcomEvent] = None  # MARS

    # Divorce events
    divorce: Optional[GedcomEvent] = None
    annulment: Optional[GedcomEvent] = None  # ANUL

    # Census events
    census: List[GedcomEvent] = field(default_factory=list)  # CENS

    # Other events
    events: List[GedcomEvent] = field(default_factory=list)  # EVEN and other events

    # Other
    notes: List[str] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the family."""
        return f"Family {self.xref}"

    @property
    def parents(self) -> List[str]:
        """Get list of parent XREFs."""
        parents = []
        if self.husband:
            parents.append(self.husband)
        if self.wife:
            parents.append(self.wife)
        return parents

    @property
    def marriage_year(self) -> Optional[int]:
        """Get marriage year."""
        return self.marriage.date.year if self.marriage and self.marriage.date else None


@dataclass
class GedcomHeader:
    """GEDCOM header information."""

    raw_lines: List[Tuple[int, str, str]] = field(default_factory=list)
    source: Optional[str] = None
    destination: Optional[str] = None
    date: Optional[str] = None
    charset: str = "UTF-8"
    version: str = "5.5.1"
    filename: Optional[str] = None
    submitter: Optional[str] = None

    def __str__(self) -> str:
        """String representation of the header."""
        return f"GEDCOM {self.version} ({self.charset})"


@dataclass
class GedcomStructuredText:
    """Structured text with CONC/CONT preservation."""

    main_value: str
    segments: List[Tuple[str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """Get full text content."""
        result = self.main_value
        for tag, value in self.segments:
            if tag == "CONC":
                result += value
            elif tag == "CONT":
                result += "\n" + value
        return result


@dataclass
class GedcomAddress:
    """GEDCOM address structure."""

    value: str = ""
    address_line2: Optional[str] = None  # ADR2
    city: Optional[str] = None
    state: Optional[str] = None
    postal_code: Optional[str] = None
    country: Optional[str] = None

    # Contact information
    phone: List[str] = field(default_factory=list)  # PHON
    email: List[str] = field(default_factory=list)  # EMAIL
    fax: List[str] = field(default_factory=list)  # FAX
    website: List[str] = field(default_factory=list)  # WWW

    def __str__(self) -> str:
        """String representation of the address."""
        return self.value


@dataclass
class GedcomSubmitter:
    """GEDCOM submitter information."""

    xref: str
    name: Optional[str] = None
    address: Optional[GedcomAddress] = None
    email: List[str] = field(default_factory=list)  # EMAIL
    phone: List[str] = field(default_factory=list)  # PHON
    fax: List[str] = field(default_factory=list)  # FAX
    website: List[str] = field(default_factory=list)  # WWW
    notes: List[str] = field(default_factory=list)  # NOTE
    sources: List[str] = field(default_factory=list)  # SOUR
    note: Optional[GedcomStructuredText] = None
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the submitter."""
        return self.name or "Unknown Submitter"


@dataclass
class GedcomMultimedia:
    """GEDCOM multimedia object."""

    xref: str
    file_path: Optional[str] = None
    format: Optional[str] = None
    title: Optional[str] = None
    notes: List[str] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the multimedia object."""
        return self.title or self.file_path or f"Media {self.xref}"


@dataclass
class GedcomRepository:
    """GEDCOM repository."""

    xref: str
    name: Optional[str] = None
    address: Optional[GedcomAddress] = None
    notes: List[str] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the repository."""
        return self.name or f"Repository {self.xref}"


@dataclass
class GedcomNote:
    """GEDCOM note record."""

    xref: str
    text: str = ""
    sources: List[str] = field(default_factory=list)
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the note."""
        return self.text[:50] + "..." if len(self.text) > 50 else self.text


@dataclass
class GedcomNote:
    """GEDCOM note record."""

    xref: str
    text: str = ""
    sources: List[str] = field(default_factory=list)
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the note."""
        return self.text[:50] + "..." if len(self.text) > 50 else self.text


@dataclass
class GedcomSource:
    """GEDCOM source record."""

    xref: str
    title: Optional[str] = None
    author: Optional[str] = None
    publication: Optional[str] = None
    repository: Optional[str] = None
    text: str = ""
    notes: List[str] = field(default_factory=list)
    sources: List[str] = field(default_factory=list)
    pages: List[str] = field(default_factory=list)
    data_records: List[Dict[str, str]] = field(default_factory=list)
    private_links: List[str] = field(default_factory=list)
    raw_structure: List[Tuple[int, str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the source."""
        return self.title or "Unknown Source"


@dataclass
class GedcomSourceCitation:
    """Generic captured citation of a source within a record, with full raw subtree."""

    xref: str
    sour_level: int = 2
    sub_lines: List[Tuple[int, str, str]] = field(default_factory=list)


@dataclass
class GedcomDatabase:
    """Complete GEDCOM database with all records."""

    header: GedcomHeader
    individuals: Dict[str, GedcomIndividual] = field(default_factory=dict)
    families: Dict[str, GedcomFamily] = field(default_factory=dict)
    notes: Dict[str, GedcomNote] = field(default_factory=dict)  # Changed to GedcomNote
    sources: Dict[str, GedcomSource] = field(
        default_factory=dict
    )  # Changed to GedcomSource
    submitters: Dict[str, GedcomSubmitter] = field(default_factory=dict)
    multimedia: Dict[str, GedcomMultimedia] = field(default_factory=dict)
    repositories: Dict[str, GedcomRepository] = field(default_factory=dict)
    record_order: List[Tuple[str, str]] = field(default_factory=list)

    def __str__(self) -> str:
        """String representation of the database."""
        return f"GEDCOM Database: {len(self.individuals)} individuals, {len(self.families)} families"

    def get_individual(self, xref: str) -> Optional[GedcomIndividual]:
        """Get individual by XREF."""
        clean_xref = xref.strip("@")
        return self.individuals.get(f"@{clean_xref}@") or self.individuals.get(
            clean_xref
        )

    def get_family(self, xref: str) -> Optional[GedcomFamily]:
        """Get family by XREF."""
        clean_xref = xref.strip("@")
        return self.families.get(f"@{clean_xref}@") or self.families.get(clean_xref)

    def get_children(self, family_xref: str) -> List[GedcomIndividual]:
        """Get children of a family."""
        family = self.get_family(family_xref)
        if not family:
            return []

        children = []
        for child_xref in family.children:
            child = self.get_individual(child_xref)
            if child:
                children.append(child)
        return children

    def get_parents(self, individual_xref: str) -> List[GedcomIndividual]:
        """Get parents of an individual."""
        individual = self.get_individual(individual_xref)
        if not individual or not individual.famc:
            return []

        parents = []
        for family_xref in individual.famc:
            family = self.get_family(family_xref)
            if family:
                if family.husband:
                    father = self.get_individual(family.husband)
                    if father:
                        parents.append(father)
                if family.wife:
                    mother = self.get_individual(family.wife)
                    if mother:
                        parents.append(mother)
        return parents

    def get_spouses(self, individual_xref: str) -> List[GedcomIndividual]:
        """Get spouses of an individual."""
        individual = self.get_individual(individual_xref)
        if not individual or not individual.fams:
            return []

        # Normalize the individual XREF for comparison
        clean_individual_xref = individual_xref.strip("@")

        spouses = []
        for family_xref in individual.fams:
            family = self.get_family(family_xref)
            if family:
                if family.husband and family.husband != clean_individual_xref:
                    spouse = self.get_individual(family.husband)
                    if spouse:
                        spouses.append(spouse)
                if family.wife and family.wife != clean_individual_xref:
                    spouse = self.get_individual(family.wife)
                    if spouse:
                        spouses.append(spouse)
        return spouses

    @property
    def statistics(self) -> Dict[str, int]:
        """Get database statistics."""
        return {
            "individuals": len(self.individuals),
            "families": len(self.families),
            "notes": len(self.notes),
            "sources": len(self.sources),
            "submitters": len(self.submitters),
            "multimedia": len(self.multimedia),
            "repositories": len(self.repositories),
        }
