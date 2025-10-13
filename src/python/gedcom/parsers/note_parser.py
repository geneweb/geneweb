from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomNote
from ..tokenizer import GedcomLine
from .base import RecordParser


class NoteParser(RecordParser):
    """Parser for GEDCOM note records."""

    def can_parse(self, tag: str) -> bool:
        return tag == "NOTE"

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse note record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        note = GedcomNote(xref=lines[start_index].xref_id)
        current_index = start_index + 1
        base_level = lines[start_index].level

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            note.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == "CONC":
                # Continue previous line
                note.text += line.value
            elif line.tag == "CONT":
                # New line
                note.text += "\n" + line.value
            elif line.tag == "SOUR":
                note.sources.append(line.value.strip("@"))

            current_index += 1

        return note, current_index
