from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomMultimedia
from ..tokenizer import GedcomLine
from .base import RecordParser
from ..tags import TAGS


class MultimediaParser(RecordParser):
    """Parser for GEDCOM multimedia."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.OBJE

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse multimedia record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        multimedia = GedcomMultimedia(xref=lines[start_index].xref_id or "")
        current_index = start_index + 1
        base_level = lines[start_index].level
        current_text_field = None

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break
            multimedia.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == TAGS.FILE:
                multimedia.file_path = line.value
                current_text_field = "file_path"
            elif line.tag == TAGS.FORM:
                multimedia.format = line.value
            elif line.tag == TAGS.TITL:
                multimedia.title = line.value
                current_text_field = "title"
            elif line.tag == TAGS.NOTE:
                multimedia.notes.append(line.value)
                current_text_field = "notes"
            elif line.tag == TAGS.SOUR:
                multimedia.sources.append(line.value.strip("@"))
            elif line.tag == TAGS.CONC:
                # CONC continues the previous text field
                if current_text_field == "file_path" and multimedia.file_path:
                    multimedia.file_path += line.value
                elif current_text_field == "title" and multimedia.title:
                    multimedia.title += line.value
                elif current_text_field == "notes" and multimedia.notes:
                    multimedia.notes[-1] += line.value
            elif line.tag == TAGS.CONT:
                # CONT continues the previous text field with newline
                if current_text_field == "file_path" and multimedia.file_path:
                    multimedia.file_path += "\n" + line.value
                elif current_text_field == "title" and multimedia.title:
                    multimedia.title += "\n" + line.value
                elif current_text_field == "notes" and multimedia.notes:
                    multimedia.notes[-1] += "\n" + line.value

            current_index += 1

        return multimedia, current_index
