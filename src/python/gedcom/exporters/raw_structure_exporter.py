from typing import List, TextIO

from .base import RecordExporter


class RawStructureExporter(RecordExporter):
    """Exporter for records with raw structure preservation."""

    def __init__(self, record_types: List[str]):
        self.record_types = set(record_types)

    def can_export(self, record_type: str) -> bool:
        return record_type in self.record_types

    def export(self, file: TextIO, xref: str, record: object) -> None:
        """Export record using raw structure."""
        file.write(f"0 {xref} {self._get_record_tag(record)}\n")

        # Use raw structure if available
        if hasattr(record, "raw_structure"):
            for level, tag, value in record.raw_structure:
                if value:
                    file.write(f"{level} {tag} {value}\n")
                else:
                    file.write(f"{level} {tag}\n")

    def _get_record_tag(self, record: object) -> str:
        """Get record tag from record type."""
