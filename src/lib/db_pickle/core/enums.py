"""
Enumerations for pickle database.

Defines all enums used in the genealogical database.
"""

from enum import Enum

class Sex(Enum):
    """Person sex enumeration."""
    MALE = "M"
    FEMALE = "F"
    NEUTER = "U"

class Access(Enum):
    """Access level enumeration."""
    IF_TITLES = "if_titles"
    PRIVATE = "private"
    PUBLIC = "public"

class DeathType(Enum):
    """Death type enumeration."""
    NOT_DEAD = "not_dead"
    DEAD = "dead"
    DEAD_YOUNG = "dead_young"
    DEAD_DONT_KNOW_WHEN = "dead_dont_know_when"
    DEAD_DONT_KNOW_WHERE = "dead_dont_know_where"
    DEAD_DONT_KNOW_WHEN_WHERE = "dead_dont_know_when_where"

class BurialType(Enum):
    """Burial type enumeration."""
    UNKNOWN = "unknown"
    BURIAL = "burial"
    CREMATION = "cremation"
    FUNERAL = "funeral"

class RelationKind(Enum):
    """Family relation kind enumeration."""
    MARRIED = "married"
    NOT_MARRIED = "not_married"
    ENGAGED = "engaged"
    SEPARATED = "separated"
    DIVORCED = "divorced"
    WIDOWED = "widowed"
    NO_SEXES_CHECK_NOT_MARRIED = "no_sexes_check_not_married"
    NO_SEXES_CHECK_MARRIED = "no_sexes_check_married"
    NO_MENTION = "no_mention"
    NO_MENTION_IF_NO_DATE = "no_mention_if_no_date"
    MARRIAGE_BANN = "marriage_bann"
    MARRIAGE_CONTRACT = "marriage_contract"
    MARRIAGE_LICENSE = "marriage_license"
    PACS = "pacs"
    RESIDENCE = "residence"

class DivorceStatus(Enum):
    """Divorce status enumeration."""
    NOT_DIVORCED = "not_divorced"
    DIVORCED = "divorced"
    SEPARATED = "separated"
