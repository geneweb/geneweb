from typing import TextIO

from ..models import GedcomRepository
from .base import RecordExporter


class RepositoryExporter(RecordExporter):
    """Exporter for GEDCOM repository records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == "REPO"

    def export(self, file: TextIO, xref: str, repository: GedcomRepository) -> None:
        """Export repository record."""
        file.write(f"0 @{xref}@ REPO\n")

        if repository.name:
            file.write(f"1 NAME {repository.name}\n")

        if repository.address:
            file.write(f"1 ADDR {repository.address.value}\n")
            if repository.address.city:
                file.write(f"2 CITY {repository.address.city}\n")
            if repository.address.state:
                file.write(f"2 STAE {repository.address.state}\n")
            if repository.address.postal_code:
                file.write(f"2 POST {repository.address.postal_code}\n")
            if repository.address.country:
                file.write(f"2 CTRY {repository.address.country}\n")

        # Export notes
        for note in repository.notes:
            file.write(f"1 NOTE {note}\n")

        # Export sources
        for source in repository.sources:
            file.write(f"1 SOUR @{source}@\n")
