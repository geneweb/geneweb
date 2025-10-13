from typing import TextIO

from ..models import GedcomEvent, GedcomFamily
from .base import RecordExporter
from .individual_exporter import IndividualExporter


class FamilyExporter(RecordExporter):
    """Exporter for GEDCOM family records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "FAM"

    def export(self, file: TextIO, xref: str, family: GedcomFamily) -> None:
        """Export family to GEDCOM format."""
        # Ensure XREF has @ symbols
        if not xref.startswith("@"):
            xref = f"@{xref}@"
        file.write(f"0 {xref} FAM\n")

        if family.husband:
            file.write(f"1 HUSB @{family.husband}@\n")

        if family.wife:
            file.write(f"1 WIFE @{family.wife}@\n")

        for child in family.children:
            file.write(f"1 CHIL @{child}@\n")

        # Marriage events
        if family.marriage:
            self._write_event(file, "MARR", family.marriage)

        if family.engagement:
            self._write_event(file, "ENGA", family.engagement)

        if family.marriage_banns:
            self._write_event(file, "MARB", family.marriage_banns)

        if family.marriage_contract:
            self._write_event(file, "MARC", family.marriage_contract)

        if family.marriage_license:
            self._write_event(file, "MARL", family.marriage_license)

        if family.marriage_settlement:
            self._write_event(file, "MARS", family.marriage_settlement)

        # Divorce events
        if family.divorce:
            self._write_event(file, "DIV", family.divorce)

        if family.annulment:
            self._write_event(file, "ANUL", family.annulment)

        # Census events
        for census_event in family.census:
            self._write_event(file, "CENS", census_event)

        # Generic events
        for event in family.events:
            self._write_event(file, event.tag, event)

        for note in family.notes:
            self._write_multiline(file, "1 NOTE", note)

        for source in family.sources:
            file.write(f"1 SOUR @{source}@\n")

    def _write_event(self, file: TextIO, tag: str, event: GedcomEvent) -> None:
        """Write an event record (reuse from IndividualExporter)."""
        individual_exporter = IndividualExporter()
        individual_exporter._write_event(file, tag, event)

    def _write_multiline(self, file: TextIO, tag: str, text: str) -> None:
        """Write multiline text (reuse from IndividualExporter)."""
        individual_exporter = IndividualExporter()
        individual_exporter._write_multiline(file, tag, text)
