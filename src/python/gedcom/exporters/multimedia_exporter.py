from typing import TextIO

from ..models import GedcomMultimedia
from .base import RecordExporter


class MultimediaExporter(RecordExporter):
    """Exporter for GEDCOM multimedia records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "OBJE"

    def export(self, file: TextIO, xref: str, multimedia: GedcomMultimedia) -> None:
        """Export multimedia record."""
        # Ensure XREF has @ symbols
        if not xref.startswith("@"):
            xref = f"@{xref}@"
        file.write(f"0 {xref} OBJE\n")

        if multimedia.raw_structure:
            for level, tag, value in multimedia.raw_structure:
                if value:
                    file.write(f"{level} {tag} {value}\n")
                else:
                    file.write(f"{level} {tag}\n")
        else:
            if multimedia.file_path:
                file.write(f"1 FILE {multimedia.file_path}\n")
            if multimedia.title:
                file.write(f"1 TITL {multimedia.title}\n")
            if multimedia.format:
                file.write(f"2 FORM {multimedia.format}\n")

            # Export notes
            for note in multimedia.notes:
                file.write(f"1 NOTE {note}\n")

            # Export sources
            for source in multimedia.sources:
                file.write(f"1 SOUR {source}\n")
