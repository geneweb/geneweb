"""
Relation models for pickle database.

Defines relationship models for families and persons.
"""

from dataclasses import dataclass
from typing import List, Optional

from ..core.types import Ifam, Iper


@dataclass
class GenAscend:
    """Ascendants model for person parents."""

    parents: Optional[List[Iper]] = None
    consang: float = 0.0


@dataclass
class GenUnion:
    """Union model for person families."""

    family: List[Ifam] = None

    def __post_init__(self):
        """Initialize default values after dataclass creation."""
        if self.family is None:
            self.family = []


@dataclass
class GenCouple:
    """Couple model for family parents."""

    father: Iper = Iper(0)
    mother: Iper = Iper(0)


@dataclass
class GenDescend:
    """Descendants model for family children."""

    children: List[Iper] = None

    def __post_init__(self):
        """Initialize default values after dataclass creation."""
        if self.children is None:
            self.children = []
