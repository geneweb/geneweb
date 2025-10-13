from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomSource
from ..tokenizer import GedcomLine
from .base import RecordParser


class SourceParser(RecordParser):
    """Parser for GEDCOM source records."""

    def can_parse(self, tag: str) -> bool:
        return tag == "SOUR"

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse source record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        source = GedcomSource(xref=lines[start_index].xref_id)
        current_index = start_index + 1
        base_level = lines[start_index].level

        current_data_record = None

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            source.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == "TITL":
                source.title = line.value
            elif line.tag == "AUTH":
                source.author = line.value
            elif line.tag == "PUBL":
                source.publication = line.value
            elif line.tag == "REPO":
                source.repository = line.value.strip("@")
            elif line.tag == "TEXT":
                source.text = line.value
            elif line.tag == "CONC":
                # Continue previous line
                source.text += line.value
            elif line.tag == "CONT":
                # New line
                source.text += "\n" + line.value
            elif line.tag == "NOTE":
                source.notes.append(line.value.strip("@"))
            elif line.tag == "SOUR":
                source.sources.append(line.value.strip("@"))
            elif line.tag == "PAGE":
                source.pages.append(line.value)
            elif line.tag == "DATA":
                data_record = {"value": line.value}
                source.data_records.append(data_record)
                current_data_record = data_record
            elif line.tag == "TEXT" and current_data_record is not None:
                current_data_record["text"] = line.value
            elif (
                line.tag == "CONC"
                and current_data_record is not None
                and "text" in current_data_record
            ):
                current_data_record["text"] += line.value
            elif (
                line.tag == "CONT"
                and current_data_record is not None
                and "text" in current_data_record
            ):
                current_data_record["text"] += "\n" + line.value
            elif line.tag == "_LINK":
                source.private_links.append(line.value)

            current_index += 1

        return source, current_index
