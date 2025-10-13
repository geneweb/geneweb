"""
Base Parser Classes

This module contains the base classes and protocols for GEDCOM parsing.
"""

from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Protocol

from ..tokenizer import GedcomLine


class FileReader(Protocol):
    """Protocol for file reading operations."""

    def read_file(self, filepath: Path, encoding: str = "utf-8") -> str:
        """Read file content."""
        ...


class DefaultFileReader:
    """Default file reader implementation."""

    def read_file(self, filepath: Path, encoding: str = "utf-8") -> str:
        """Read file content with encoding detection."""
        try:
            with open(filepath, "r", encoding=encoding, errors="replace") as f:
                return f.read()
        except UnicodeDecodeError:
            # Fallback to latin-1 for legacy files
            with open(filepath, "r", encoding="latin-1", errors="replace") as f:
                return f.read()


class RecordParser(ABC):
    """Abstract base class for parsing specific GEDCOM record types."""

    @abstractmethod
    def can_parse(self, tag: str) -> bool:
        """Check if this parser can handle the given tag."""
        pass

    @abstractmethod
    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse record from lines starting at start_index.

        Returns:
            tuple: (parsed_object, next_index)
        """
        pass
