from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomNote
from ..tokenizer import GedcomLine
from .base import RecordParser
from ..tags import TAGS


class NoteParser(RecordParser):
    """Parser for GEDCOM note records."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.NOTE

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse note record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        note = GedcomNote(xref=lines[start_index].xref_id or "")
        current_index = start_index + 1
        base_level = lines[start_index].level

        if lines[start_index].value:
            note.text = lines[start_index].value

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            note.raw_structure.append((line.level, line.tag, line.value))

            if line.level == base_level + 1:
                if line.tag == TAGS.NOTE:
                    note.text = line.value
                elif line.tag == TAGS.CONT:
                    note.text = line.value
                elif line.tag == TAGS.SOUR:
                    note.sources.append(line.value.strip("@"))
            elif line.level == base_level + 2:
                if line.tag == TAGS.CONC:
                    note.text += line.value
                elif line.tag == TAGS.CONT:
                    note.text += "\n" + line.value

            current_index += 1

        return note, current_index
