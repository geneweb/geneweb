"""
Enumerations used throughout the database.

Contains all enum types: Sex, Access, DeathType, BurialType,
RelationKind, DivorceStatus, etc.
"""

from enum import Enum


class Sex(Enum):
    """Person's biological sex."""
    MALE = "M"
    FEMALE = "F"
    NEUTER = "U"  # Unknown/unspecified


class Access(Enum):
    """Privacy access level for person data."""
    PUBLIC = "public"
    PRIVATE = "private"
    FRIEND = "friend"


class DeathType(Enum):
    """Death status of a person."""
    NOT_DEAD = "not_dead"
    DEAD = "dead"
    DEAD_YOUNG = "dead_young"
    DEAD_DONT_KNOW_WHEN = "dead_dont_know_when"
    DONT_KNOW_IF_DEAD = "dont_know_if_dead"
    OF_COURSE_DEAD = "of_course_dead"


class BurialType(Enum):
    """Type of burial."""
    UNKNOWN = "unknown"
    BURIED = "buried"
    CREMATED = "cremated"


class RelationKind(Enum):
    """Type of relationship between couple."""
    MARRIED = "married"
    NOT_MARRIED = "not_married"
    ENGAGED = "engaged"
    NO_SEXES_CHECK_NOT_MARRIED = "no_sexes_check_not_married"
    NO_MENTION = "no_mention"
    NO_SEXES_CHECK_MARRIED = "no_sexes_check_married"


class DivorceStatus(Enum):
    """Divorce status of a couple."""
    NOT_DIVORCED = "not_divorced"
    DIVORCED = "divorced"
    SEPARATED = "separated"
