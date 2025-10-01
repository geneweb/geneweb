"""
Geneweb Common - Shared utilities and models for Geneweb Python implementations.

This package provides common functionality used across all Geneweb Python binaries:
- Data models (Person, Family, Database, etc.)
- Utility functions (validation, name handling, date parsing, etc.)
- Exception hierarchy
- Base interfaces

Version: 1.0.0
"""

__version__ = "1.0.0"
__author__ = "Geneweb Team"

# Core models
from .models import (
    Date, Name, Event, Person, Family, Database
)

# Utility classes
from .utils import (
    ValidationUtils, NameUtils, DateUtils,
    RelationshipUtils, DatabaseUtils
)

# Exception hierarchy
from .exceptions import (
    GenewebError, ValidationError, DatabaseError,
    ArgumentError, CalculationError, ParsingError,
    FileFormatError, NetworkError
)

# Enums et constantes
class Sex:
    MALE = "M"
    FEMALE = "F"
    UNKNOWN = "U"

# Define public API
__all__ = [
    # Models
    "Date", "Name", "Event", "Person", "Family", "Database",

    # Enums
    "Sex",

    # Utils
    "ValidationUtils", "NameUtils", "DateUtils",
    "RelationshipUtils", "DatabaseUtils",

    # Exceptions
    "GenewebError", "ValidationError", "DatabaseError",
    "ArgumentError", "CalculationError", "ParsingError",
    "FileFormatError", "NetworkError"
]
