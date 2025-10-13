"""
Relationship data models.

Contains dataclasses for family relationships:
- GenAscend: Parent information
- GenUnion: Family memberships as parent
- GenCouple: Father and mother pair
- GenDescend: Children information
- Relation: Non-biological parent relationships
"""

from dataclasses import dataclass, field
from typing import List, Optional

from ..core.types import Istr


@dataclass
class Relation:
    """Relationship to non-biological parents."""

    father: Optional[int] = None  # iper
    mother: Optional[int] = None  # iper
    source: int = Istr.empty()
    relation_type: str = "rparent_adoption"


@dataclass
class GenAscend:
    """Ascendant information (parents)."""

    parents: Optional[int] = None  # ifam - family where this person is a child
    consang: float = 0.0  # Consanguinity coefficient


@dataclass
class GenUnion:
    """Union information (families where person is a parent)."""

    family: List[int] = field(default_factory=list)  # List of ifam


@dataclass
class GenCouple:
    """Couple in a family (father and mother)."""

    father: int  # iper
    mother: int  # iper


@dataclass
class GenDescend:
    """Descendant information (children)."""

    children: List[int] = field(default_factory=list)  # List of iper
