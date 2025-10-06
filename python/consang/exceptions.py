"""Exceptions for consang module."""

from common.exceptions import GenewebError, ArgumentError, DatabaseError, ValidationError
import sys
from pathlib import Path

# Ajouter le chemin geneweb_common si nécessaire
parent_dir = Path(__file__).parent.parent
if str(parent_dir) not in sys.path:
    sys.path.insert(0, str(parent_dir))


class ConsangError(GenewebError):
    """Base exception for consang operations."""

    pass


class CalculationError(ConsangError):
    """Exception raised during consanguinity calculation."""

    pass


class DatabaseLockError(ConsangError):
    """Exception raised when database locking fails."""

    pass


# Re-export common exceptions for convenience
__all__ = [
    "ConsangError",
    "CalculationError",
    "DatabaseLockError",
    "ArgumentError",
    "DatabaseError",
    "ValidationError",
]
