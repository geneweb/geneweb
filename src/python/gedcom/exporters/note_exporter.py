from typing import TextIO

from ..models import GedcomNote
from .base import RecordExporter


class NoteExporter(RecordExporter):
    """Exporter for GEDCOM note records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "NOTE"

    def export(self, file: TextIO, xref: str, note: GedcomNote) -> None:
        """Export note record."""
        file.write(f"0 @{xref}@ NOTE\n")
        if note.raw_structure:
            for level, tag, value in note.raw_structure:
                if value:
                    file.write(f"{level} {tag} {value}\n")
                else:
                    file.write(f"{level} {tag}\n")
        else:
            if note.text:
                self._write_multiline(file, "1", note.text)

            for source in note.sources:
                file.write(f"1 SOUR @{source}@\n")

    def _write_multiline(self, file: TextIO, tag: str, text: str) -> None:
        """Write multiline text using CONC and CONT tags."""
        if not text:
            return

        lines = text.split("\n")
        level = tag.split()[0]

        # First line
        if lines:
            first_line = lines[0]
            self._write_line_with_conc(file, tag, first_line)
            lines = lines[1:]

        # Additional lines
        for line in lines:
            if not line:
                file.write(f"{level} CONT\n")
            else:
                self._write_line_with_conc(file, f"{level} CONT", line)

    def _write_line_with_conc(self, file: TextIO, tag: str, line: str) -> None:
        """Write a line with CONC continuation if needed."""
        if len(line) <= 248:  # GEDCOM line limit
            file.write(f"{tag} {line}\n")
        else:
            # Split long line
            file.write(f"{tag} {line[:248]}\n")
            remaining = line[248:]
            while remaining:
                chunk = remaining[:248]
                remaining = remaining[248:]
                file.write(f"2 CONC {chunk}\n")
