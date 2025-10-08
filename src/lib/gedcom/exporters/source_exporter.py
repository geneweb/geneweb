from typing import TextIO
from ..models import (
    GedcomSource
)
from .base import RecordExporter


class SourceExporter(RecordExporter):
    """Exporter for GEDCOM source records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == 'SOUR'

    def export(self, file: TextIO, xref: str, source: GedcomSource) -> None:
        """Export source record."""
        file.write(f"0 @{xref}@ SOUR\n")

        # Use raw structure if available for exact preservation
        if source.raw_structure:
            for level, tag, value in source.raw_structure:
                if value:
                    file.write(f"{level} {tag} {value}\n")
                else:
                    file.write(f"{level} {tag}\n")
        else:
            # Fallback to structured export
            if source.title:
                file.write(f"1 TITL {source.title}\n")
            if source.author:
                file.write(f"1 AUTH {source.author}\n")
            if source.publication:
                file.write(f"1 PUBL {source.publication}\n")
            if source.repository:
                file.write(f"1 REPO @{source.repository}@\n")

            if source.text:
                self._write_multiline(file, "1 TEXT", source.text)

            # Export notes
            for note in source.notes:
                file.write(f"1 NOTE @{note}@\n")

            # Export sources
            for source_ref in source.sources:
                file.write(f"1 SOUR @{source_ref}@\n")

            for page in source.pages:
                file.write(f"1 PAGE {page}\n")
            for data_record in source.data_records:
                file.write(f"1 DATA {data_record.get('value', '')}\n")
                if 'text' in data_record:
                    self._write_multiline(file, "2 TEXT", data_record['text'])
            for link in source.private_links:
                file.write(f"1 _LINK {link}\n")

    def _write_multiline(self, file: TextIO, tag: str, text: str) -> None:
        """Write multiline text using CONC and CONT tags."""
        if not text:
            return

        lines = text.split('\n')
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

