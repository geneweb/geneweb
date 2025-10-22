"""
Core types for pickle database.

Defines indexed ID types that match the OCaml GeneWeb implementation.
"""

from typing import NewType

Iper = NewType("Iper", int)  # Person ID
Ifam = NewType("Ifam", int)  # Family ID
Istr = NewType("Istr", int)  # String ID


# Helper functions for creating dummy values
def dummy_iper() -> Iper:
    """Create dummy person ID."""
    return Iper(0)


def dummy_ifam() -> Ifam:
    """Create dummy family ID."""
    return Ifam(0)


def dummy_istr() -> Istr:
    """Create dummy string ID."""
    return Istr(0)


def quest_istr() -> Istr:
    """Create question mark string ID (for unknown values)."""
    return Istr(1)
