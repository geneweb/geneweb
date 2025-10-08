"""
GEDCOM Exporter

This module contains the main GEDCOM exporter class.
"""

from typing import TextIO, List, Optional
from pathlib import Path
import logging

from ..models import GedcomDatabase, GedcomHeader
from ..exceptions import GedcomExportError
from .base import RecordExporter
from .header_exporter import HeaderExporter
from .individual_exporter import IndividualExporter
from .family_exporter import FamilyExporter
from .submitter_exporter import SubmitterExporter
from .multimedia_exporter import MultimediaExporter
from .repository_exporter import RepositoryExporter
from .note_exporter import NoteExporter
from .source_exporter import SourceExporter


class GedcomExporter:
    """Main GEDCOM exporter following clean code principles."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

        # Register record exporters
        self.record_exporters: List[RecordExporter] = [
            HeaderExporter(),
            IndividualExporter(),
            FamilyExporter(),
            SubmitterExporter(),
            MultimediaExporter(),
            RepositoryExporter(),
            NoteExporter(),
            SourceExporter(),
        ]

    def export_file(self, filepath: Path, database: GedcomDatabase, encoding: str = 'utf-8') -> None:
        """
        Export database to GEDCOM file.

        Args:
            filepath: Output file path
            database: Database to export
            encoding: File encoding

        Raises:
            GedcomExportError: If export fails
        """
        self.logger.info(f"Exporting GEDCOM to: {filepath}")

        try:
            with open(filepath, 'w', encoding=encoding) as f:
                self.export_content(f, database)

            self.logger.info(f"Successfully exported GEDCOM to {filepath}")

        except Exception as e:
            error_msg = f"Failed to export GEDCOM to {filepath}: {e}"
            self.logger.error(error_msg)
            raise GedcomExportError(error_msg) from e

    def export_content(self, file: TextIO, database: GedcomDatabase) -> None:
        """
        Export database to GEDCOM format.

        Args:
            file: Output file object
            database: Database to export

        Raises:
            GedcomExportError: If export fails
        """
        try:
            # Export header
            self._export_header(file, database.header)

            # Export records in original order
            self._export_records(file, database)

            # Export trailer
            file.write("0 TRLR\n")

        except Exception as e:
            error_msg = f"Failed to export GEDCOM content: {e}"
            self.logger.error(error_msg)
            raise GedcomExportError(error_msg) from e

    def _export_header(self, file: TextIO, header: GedcomHeader) -> None:
        """Export header record."""
        exporter = self._find_exporter('HEAD')
        if exporter:
            exporter.export(file, '', header)
        else:
            raise GedcomExportError("No exporter found for header")

    def _export_records(self, file: TextIO, database: GedcomDatabase) -> None:
        """Export all records in original order."""
        for record_type, xref in database.record_order:
            try:
                record = self._get_record(database, record_type, xref)
                if record:
                    exporter = self._find_exporter(record_type)
                    if exporter:
                        # XREF already contains @ symbols, don't add more
                        clean_xref = xref.strip('@')  # Remove @ for export
                        exporter.export(file, clean_xref, record)
                    else:
                        self.logger.warning(f"No exporter found for record type: {record_type}")
            except Exception as e:
                self.logger.warning(f"Failed to export {record_type} {xref}: {e}")
                # Continue with other records

    def _find_exporter(self, record_type: str) -> Optional[RecordExporter]:
        """Find appropriate exporter for the given record type."""
        for exporter in self.record_exporters:
            if exporter.can_export(record_type):
                return exporter
        return None

    def _get_record(self, database: GedcomDatabase, record_type: str, xref: str) -> Optional[object]:
        """Get record from database by type and XREF."""
        if record_type == 'INDI':
            return database.individuals.get(xref)
        elif record_type == 'FAM':
            return database.families.get(xref)
        elif record_type == 'NOTE':
            return database.notes.get(xref)
        elif record_type == 'SOUR':
            return database.sources.get(xref)
        elif record_type == 'SUBM':
            return database.submitters.get(xref)
        elif record_type == 'OBJE':
            return database.multimedia.get(xref)
        elif record_type == 'REPO':
            return database.repositories.get(xref)
        else:
            return None


def create_exporter() -> GedcomExporter:
    """Create a GEDCOM exporter.

    Returns:
        Configured GedcomExporter instance
    """
    return GedcomExporter()
