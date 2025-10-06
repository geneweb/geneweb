"""
Consang - Python implementation of genealogical consanguinity calculator.

This package provides functionality to calculate consanguinity coefficients
for genealogical databases, equivalent to the original OCaml implementation.
"""

__version__ = "1.0.0"
__author__ = "Geneweb Team"

from .calculator import ConsanguinityCalculator, CalculationResult
from .database import GenewebDatabase
from .cli import ConsangCLI
from .exceptions import ConsangError, CalculationError, DatabaseLockError
from common.exceptions import ArgumentError, DatabaseError

__all__ = [
    "ConsanguinityCalculator",
    "CalculationResult",
    "GenewebDatabase",
    "ConsangCLI",
    "ConsangError",
    "CalculationError",
    "DatabaseLockError",
    "ArgumentError",
    "DatabaseError",
]
