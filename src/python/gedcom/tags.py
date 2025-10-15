"""
GEDCOM Tags Constants

This module contains only the GEDCOM tags actually used in the parser.
Using Enums provides type safety and better IDE support.
"""

from enum import Enum
from typing import List, Set


class GedcomTags(Enum):
    """GEDCOM tags as enum for type safety and IDE support."""

    # Record-level tags (level 0)
    HEAD = "HEAD"
    TRLR = "TRLR"
    INDI = "INDI"
    FAM = "FAM"
    SOUR = "SOUR"
    REPO = "REPO"
    OBJE = "OBJE"
    NOTE = "NOTE"
    SUBM = "SUBM"

    # Individual record tags
    NAME = "NAME"
    SEX = "SEX"
    OCCU = "OCCU"
    TITL = "TITL"
    ADDR = "ADDR"
    FAMC = "FAMC"
    FAMS = "FAMS"
    REFN = "REFN"

    # Family record tags
    HUSB = "HUSB"
    WIFE = "WIFE"
    CHIL = "CHIL"

    # Event tags (only those actually used)
    BIRT = "BIRT"
    CHR = "CHR"
    DEAT = "DEAT"
    BURI = "BURI"
    CREM = "CREM"
    ADOP = "ADOP"
    BAPM = "BAPM"
    BARM = "BARM"
    BASM = "BASM"
    BLES = "BLES"
    CHRA = "CHRA"
    CONF = "CONF"
    FCOM = "FCOM"
    ORDN = "ORDN"
    NATU = "NATU"
    EMIG = "EMIG"
    IMMI = "IMMI"
    CENS = "CENS"
    PROB = "PROB"
    WILL = "WILL"
    GRAD = "GRAD"
    RETI = "RETI"
    EVEN = "EVEN"

    # Marriage event tags
    MARR = "MARR"
    DIV = "DIV"
    DIVF = "DIVF"
    ENGA = "ENGA"
    MARC = "MARC"
    MARB = "MARB"
    MARS = "MARS"
    MARL = "MARL"
    ANUL = "ANUL"

    # Attribute tags (only those actually used)
    CAST = "CAST"
    DSCR = "DSCR"
    EDUC = "EDUC"
    IDNO = "IDNO"
    NATI = "NATI"
    NCHI = "NCHI"
    NMR = "NMR"
    PROP = "PROP"
    RELI = "RELI"
    RESI = "RESI"
    SSN = "SSN"

    # Address sub-tags
    ADR1 = "ADR1"
    ADR2 = "ADR2"
    CITY = "CITY"
    STAE = "STAE"
    POST = "POST"
    CTRY = "CTRY"

    # Contact sub-tags
    PHON = "PHON"
    EMAIL = "EMAIL"
    FAX = "FAX"
    WWW = "WWW"

    # Name sub-tags
    GIVN = "GIVN"
    SURN = "SURN"
    NPFX = "NPFX"
    NSFX = "NSFX"
    NICK = "NICK"
    ROMN = "ROMN"
    FONE = "FONE"
    TRAN = "TRAN"

    # Event sub-tags
    DATE = "DATE"
    PLAC = "PLAC"
    LATI = "LATI"
    LONG = "LONG"
    MAP = "MAP"
    AGE = "AGE"
    CAUS = "CAUS"
    TYPE = "TYPE"

    # Source tags
    AUTH = "AUTH"
    PUBL = "PUBL"
    TEXT = "TEXT"
    CONC = "CONC"
    CONT = "CONT"
    PAGE = "PAGE"
    DATA = "DATA"

    # Multimedia tags
    FILE = "FILE"
    FORM = "FORM"

    # Header tags
    DEST = "DEST"
    CHAR = "CHAR"
    VERS = "VERS"

    # Custom/extension tags (only those actually used)
    LINK = "_LINK"
    PHOTO = "_PHOTO"
    DATE_CUSTOM = "_DATE"
    TEXT_CUSTOM = "_TEXT"
    FREL = "_FREL"
    MREL = "_MREL"

    def __str__(self) -> str:
        """Return the string value for compatibility."""
        return str(self.value)

    def __eq__(self, other) -> bool:
        """Allow comparison with strings."""
        if isinstance(other, str):
            return str(self.value) == other
        return super().__eq__(other)

    def __hash__(self):
        """Make enum hashable."""
        return hash(self.value)


class GedcomTagGroups:
    """Helper class for tag group operations using enum values."""

    # Tag groups as sets of enum values (only actually used tags)
    INDIVIDUAL_EVENTS: Set[GedcomTags] = {
        GedcomTags.BIRT,
        GedcomTags.CHR,
        GedcomTags.DEAT,
        GedcomTags.BURI,
        GedcomTags.CREM,
        GedcomTags.ADOP,
        GedcomTags.BAPM,
        GedcomTags.BARM,
        GedcomTags.BASM,
        GedcomTags.BLES,
        GedcomTags.CHRA,
        GedcomTags.CONF,
        GedcomTags.FCOM,
        GedcomTags.ORDN,
        GedcomTags.NATU,
        GedcomTags.EMIG,
        GedcomTags.IMMI,
        GedcomTags.CENS,
        GedcomTags.PROB,
        GedcomTags.WILL,
        GedcomTags.GRAD,
        GedcomTags.RETI,
        GedcomTags.EVEN,
    }

    INDIVIDUAL_ATTRIBUTES: Set[GedcomTags] = {
        GedcomTags.CAST,
        GedcomTags.DSCR,
        GedcomTags.EDUC,
        GedcomTags.IDNO,
        GedcomTags.NATI,
        GedcomTags.NCHI,
        GedcomTags.NMR,
        GedcomTags.PROP,
        GedcomTags.RELI,
        GedcomTags.RESI,
        GedcomTags.SSN,
    }

    MARRIAGE_EVENTS: Set[GedcomTags] = {
        GedcomTags.MARR,
        GedcomTags.DIV,
        GedcomTags.DIVF,
        GedcomTags.ENGA,
        GedcomTags.MARC,
        GedcomTags.MARB,
        GedcomTags.MARS,
    }

    FAMILY_EVENTS: Set[GedcomTags] = {
        GedcomTags.MARR,
        GedcomTags.ENGA,
        GedcomTags.MARB,
        GedcomTags.MARC,
        GedcomTags.MARL,
        GedcomTags.MARS,
        GedcomTags.DIV,
        GedcomTags.ANUL,
        GedcomTags.CENS,
        GedcomTags.EVEN,
    }

    ADDRESS_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.ADR1,
        GedcomTags.ADR2,
        GedcomTags.CITY,
        GedcomTags.STAE,
        GedcomTags.POST,
        GedcomTags.CTRY,
    }

    CONTACT_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.PHON,
        GedcomTags.EMAIL,
        GedcomTags.FAX,
        GedcomTags.WWW,
    }

    NAME_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.GIVN,
        GedcomTags.SURN,
        GedcomTags.NPFX,
        GedcomTags.NSFX,
        GedcomTags.NICK,
        GedcomTags.ROMN,
        GedcomTags.FONE,
        GedcomTags.TRAN,
    }

    EVENT_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.DATE,
        GedcomTags.PLAC,
        GedcomTags.LATI,
        GedcomTags.LONG,
        GedcomTags.MAP,
        GedcomTags.AGE,
        GedcomTags.CAUS,
        GedcomTags.TYPE,
    }

    SOURCE_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.TITL,
        GedcomTags.AUTH,
        GedcomTags.PUBL,
        GedcomTags.REPO,
        GedcomTags.TEXT,
        GedcomTags.CONC,
        GedcomTags.CONT,
        GedcomTags.PAGE,
        GedcomTags.DATA,
    }

    MULTIMEDIA_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.FILE,
        GedcomTags.FORM,
        GedcomTags.TITL,
        GedcomTags.NOTE,
        GedcomTags.SOUR,
        GedcomTags.CONC,
        GedcomTags.CONT,
    }

    HEADER_SUBTAGS: Set[GedcomTags] = {
        GedcomTags.SOUR,
        GedcomTags.DEST,
        GedcomTags.DATE,
        GedcomTags.FILE,
        GedcomTags.CHAR,
        GedcomTags.SUBM,
        GedcomTags.VERS,
    }

    CUSTOM_TAGS: Set[GedcomTags] = {
        GedcomTags.LINK,
        GedcomTags.PHOTO,
        GedcomTags.DATE_CUSTOM,
        GedcomTags.TEXT_CUSTOM,
        GedcomTags.FREL,
        GedcomTags.MREL,
    }

    @staticmethod
    def is_individual_event(tag) -> bool:
        """Check if tag is an individual event."""
        return tag in GedcomTagGroups.INDIVIDUAL_EVENTS

    @staticmethod
    def is_individual_attribute(tag) -> bool:
        """Check if tag is an individual attribute."""
        return tag in GedcomTagGroups.INDIVIDUAL_ATTRIBUTES

    @staticmethod
    def is_marriage_event(tag) -> bool:
        """Check if tag is a marriage event."""
        return tag in GedcomTagGroups.MARRIAGE_EVENTS

    @staticmethod
    def is_address_subtag(tag) -> bool:
        """Check if tag is an address subtag."""
        return tag in GedcomTagGroups.ADDRESS_SUBTAGS

    @staticmethod
    def is_contact_subtag(tag) -> bool:
        """Check if tag is a contact subtag."""
        return tag in GedcomTagGroups.CONTACT_SUBTAGS

    @staticmethod
    def is_name_subtag(tag) -> bool:
        """Check if tag is a name subtag."""
        return tag in GedcomTagGroups.NAME_SUBTAGS

    @staticmethod
    def is_event_subtag(tag) -> bool:
        """Check if tag is an event subtag."""
        return tag in GedcomTagGroups.EVENT_SUBTAGS

    @staticmethod
    def is_source_subtag(tag) -> bool:
        """Check if tag is a source subtag."""
        return tag in GedcomTagGroups.SOURCE_SUBTAGS

    @staticmethod
    def is_multimedia_subtag(tag) -> bool:
        """Check if tag is a multimedia subtag."""
        return tag in GedcomTagGroups.MULTIMEDIA_SUBTAGS

    @staticmethod
    def is_header_subtag(tag) -> bool:
        """Check if tag is a header subtag."""
        return tag in GedcomTagGroups.HEADER_SUBTAGS

    @staticmethod
    def is_custom_tag(tag) -> bool:
        """Check if tag is a custom/extension tag."""
        return tag in GedcomTagGroups.CUSTOM_TAGS

    @staticmethod
    def is_death_event(tag) -> bool:
        """Check if tag is a death-related event."""
        return tag in {GedcomTags.DEAT, GedcomTags.BURI, GedcomTags.CREM}

    @staticmethod
    def is_note_continuation(tag) -> bool:
        """Check if tag is a note continuation tag."""
        return tag in {GedcomTags.CONT, GedcomTags.CONC}

    @staticmethod
    def is_coordinate_tag(tag) -> bool:
        """Check if tag is a coordinate tag."""
        return tag in {GedcomTags.LATI, GedcomTags.LONG}

    @staticmethod
    def is_contact_tag(tag) -> bool:
        """Check if tag is a contact tag."""
        return tag in {
            GedcomTags.PHON,
            GedcomTags.EMAIL,
            GedcomTags.FAX,
            GedcomTags.WWW,
        }

    @staticmethod
    def is_baptism_tag(tag) -> bool:
        """Check if tag is a baptism-related tag."""
        return tag in {GedcomTags.BAPM, GedcomTags.CHR}

    @staticmethod
    def is_burial_tag(tag) -> bool:
        """Check if tag is a burial-related tag."""
        return tag in {GedcomTags.BURI, GedcomTags.CREM}

    @staticmethod
    def is_marriage_related_tag(tag) -> bool:
        """Check if tag is a marriage-related tag."""
        return tag in {
            GedcomTags.MARR,
            GedcomTags.DIV,
            GedcomTags.ANUL,
            GedcomTags.ENGA,
        }

    @staticmethod
    def is_family_event(tag) -> bool:
        """Check if tag is a family event."""
        return tag in GedcomTagGroups.FAMILY_EVENTS

    @staticmethod
    def get_family_event_tags() -> List[str]:
        """Get list of all family event tags as strings."""
        return [tag.value for tag in GedcomTagGroups.FAMILY_EVENTS]

    @staticmethod
    def get_individual_event_tags() -> List[str]:
        """Get list of all individual event tags as strings."""
        return [tag.value for tag in GedcomTagGroups.INDIVIDUAL_EVENTS]

    @staticmethod
    def get_individual_attribute_tags() -> List[str]:
        """Get list of all individual attribute tags as strings."""
        return [tag.value for tag in GedcomTagGroups.INDIVIDUAL_ATTRIBUTES]

    @staticmethod
    def get_tag_category(tag) -> str:
        """Get the category of a tag."""
        if tag in GedcomTagGroups.INDIVIDUAL_EVENTS:
            return "individual_event"
        elif tag in GedcomTagGroups.INDIVIDUAL_ATTRIBUTES:
            return "individual_attribute"
        elif tag in GedcomTagGroups.MARRIAGE_EVENTS:
            return "marriage_event"
        elif tag in GedcomTagGroups.ADDRESS_SUBTAGS:
            return "address_subtag"
        elif tag in GedcomTagGroups.CONTACT_SUBTAGS:
            return "contact_subtag"
        elif tag in GedcomTagGroups.NAME_SUBTAGS:
            return "name_subtag"
        elif tag in GedcomTagGroups.EVENT_SUBTAGS:
            return "event_subtag"
        elif tag in GedcomTagGroups.SOURCE_SUBTAGS:
            return "source_subtag"
        elif tag in GedcomTagGroups.MULTIMEDIA_SUBTAGS:
            return "multimedia_subtag"
        elif tag in GedcomTagGroups.HEADER_SUBTAGS:
            return "header_subtag"
        elif tag in GedcomTagGroups.CUSTOM_TAGS:
            return "custom_tag"
        else:
            return "unknown"


# Convenience imports for easier access
TAGS = GedcomTags
GROUPS = GedcomTagGroups
