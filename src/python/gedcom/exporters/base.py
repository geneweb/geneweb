"""
Base Exporter Classes

This module contains the base classes for GEDCOM exporting.
"""

from abc import ABC, abstractmethod
from typing import TextIO


class RecordExporter(ABC):
    """Abstract base class for exporting specific GEDCOM record types."""

    @abstractmethod
    def can_export(self, record_type: str) -> bool:
        """Check if this exporter can handle the given record type."""
        pass

    @abstractmethod
    def export(self, file: TextIO, xref: str, record: object) -> None:
        """Export record to file.

        Args:
            file: Output file object
            xref: Record XREF identifier
            record: Record object to export
        """
        pass
