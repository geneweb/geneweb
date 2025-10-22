from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomSource
from ..tokenizer import GedcomLine
from .base import RecordParser
from ..tags import TAGS


class SourceParser(RecordParser):
    """Parser for GEDCOM source records."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.SOUR

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse source record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        source = GedcomSource(xref=lines[start_index].xref_id or "")
        current_index = start_index + 1
        base_level = lines[start_index].level

        current_data_record = None
        current_note_index = None

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            source.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == TAGS.TITL:
                source.title = line.value
            elif line.tag == TAGS.AUTH:
                source.author = line.value
            elif line.tag == TAGS.PUBL:
                source.publication = line.value
            elif line.tag == TAGS.REPO:
                source.repository = line.value.strip("@")
            elif line.tag == TAGS.TEXT:
                source.text = line.value
            elif line.tag == TAGS.CONC:
                if current_note_index is not None:
                    last_char = (
                        source.notes[current_note_index][-1]
                        if source.notes[current_note_index]
                        else ""
                    )
                    if last_char and last_char not in " \n":
                        source.notes[current_note_index] += " " + line.value
                    else:
                        source.notes[current_note_index] += line.value
                else:
                    last_char = source.text[-1] if source.text else ""
                    if last_char and last_char not in " \n":
                        source.text += " " + line.value
                    else:
                        source.text += line.value
            elif line.tag == TAGS.CONT:
                if current_note_index is not None:
                    source.notes[current_note_index] += "\n" + line.value
                else:
                    source.text += "\n" + line.value
            elif line.tag == TAGS.NOTE:
                source.notes.append(line.value.strip("@"))
                current_note_index = len(source.notes) - 1
            elif line.tag == TAGS.SOUR:
                source.sources.append(line.value.strip("@"))
            elif line.tag == TAGS.PAGE:
                source.pages.append(line.value)
            elif line.tag == TAGS.DATA:
                data_record = {"value": line.value}
                source.data_records.append(data_record)
                current_data_record = data_record
            elif line.tag == TAGS.TEXT and current_data_record is not None:
                current_data_record["text"] = line.value
            elif (
                line.tag == TAGS.CONC
                and current_data_record is not None
                and "text" in current_data_record
            ):
                current_data_record["text"] += line.value
            elif (
                line.tag == TAGS.CONT
                and current_data_record is not None
                and "text" in current_data_record
            ):
                current_data_record["text"] += "\n" + line.value
            elif line.tag == TAGS.LINK:
                source.private_links.append(line.value)

            current_index += 1

        return source, current_index
