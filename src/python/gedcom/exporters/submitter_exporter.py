from typing import TextIO

from ..models import GedcomSubmitter
from .base import RecordExporter


class SubmitterExporter(RecordExporter):
    """Exporter for GEDCOM submitter records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "SUBM"

    def export(self, file: TextIO, xref: str, submitter: GedcomSubmitter) -> None:
        """Export a submitter record."""
        file.write(f"0 @{xref}@ SUBM\n")

        if submitter.name:
            file.write(f"1 NAME {submitter.name}\n")

        if submitter.address:
            file.write(f"1 ADDR {submitter.address.value}\n")
            if submitter.address.city:
                file.write(f"2 CITY {submitter.address.city}\n")
            if submitter.address.state:
                file.write(f"2 STAE {submitter.address.state}\n")
            if submitter.address.postal_code:
                file.write(f"2 POST {submitter.address.postal_code}\n")
            if submitter.address.country:
                file.write(f"2 CTRY {submitter.address.country}\n")

            # Contact information in address
            for phone in submitter.address.phone:
                file.write(f"2 PHON {phone}\n")
            for email in submitter.address.email:
                file.write(f"2 EMAIL {email}\n")
            for fax in submitter.address.fax:
                file.write(f"2 FAX {fax}\n")
            for website in submitter.address.website:
                file.write(f"2 WWW {website}\n")

        # Direct contact information
        for phone in submitter.phone:
            file.write(f"1 PHON {phone}\n")
        for email in submitter.email:
            file.write(f"1 EMAIL {email}\n")
        for fax in submitter.fax:
            file.write(f"1 FAX {fax}\n")
        for website in submitter.website:
            file.write(f"1 WWW {website}\n")

        for note in submitter.notes:
            self._write_multiline(file, "1 NOTE", note)

        for source in submitter.sources:
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
