from typing import TextIO

from ..models import GedcomHeader
from .base import RecordExporter


class HeaderExporter(RecordExporter):
    """Exporter for GEDCOM header records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "HEAD"

    def export(self, file: TextIO, xref: str, header: GedcomHeader) -> None:
        """Export header to GEDCOM format."""
        file.write("0 HEAD\n")

        # Use raw structure if available for exact preservation
        if header.raw_lines:
            for level, tag, value in header.raw_lines:
                if value:
                    file.write(f"{level} {tag} {value}\n")
                else:
                    file.write(f"{level} {tag}\n")
        else:
            # Fallback to default structure
            if header.source:
                file.write(f"1 SOUR {header.source}\n")
                if header.version:
                    file.write(f"2 VERS {header.version}\n")

            file.write(f"1 CHAR {header.charset}\n")

            if header.destination:
                file.write(f"1 DEST {header.destination}\n")

            if header.date:
                file.write(f"1 DATE {header.date}\n")

            if header.filename:
                file.write(f"1 FILE {header.filename}\n")

            if header.submitter:
                file.write(f"1 SUBM {header.submitter}\n")
