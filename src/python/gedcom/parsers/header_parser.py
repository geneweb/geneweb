"""
Header Parser

This module contains the parser for GEDCOM header records.
"""

from typing import List, Tuple

from ..exceptions import GedcomParseError
from ..models import GedcomHeader
from ..tokenizer import GedcomLine
from .base import RecordParser
from ..tags import TAGS


class HeaderParser(RecordParser):
    """Parser for GEDCOM header."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.HEAD

    def parse(
        self, lines: List[GedcomLine], start_index: int
    ) -> Tuple[GedcomHeader, int]:
        """Parse header record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        header = GedcomHeader()
        current_index = start_index + 1
        base_level = lines[start_index].level

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break
            header.raw_lines.append((line.level, line.tag, line.value))
            if line.tag == TAGS.SOUR:
                header.source = line.value
            elif line.tag == TAGS.DEST:
                header.destination = line.value
            elif line.tag == TAGS.DATE:
                header.date = line.value
            elif line.tag == TAGS.FILE:
                header.filename = line.value
            elif line.tag == TAGS.CHAR:
                header.charset = line.value
            elif line.tag == TAGS.SUBM:
                header.submitter = line.value
            elif line.tag == TAGS.VERS:
                header.version = line.value

            current_index += 1

        return header, current_index
