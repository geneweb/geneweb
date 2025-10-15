from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomEvent, GedcomFamily
from ..tokenizer import GedcomLine
from .base import RecordParser
from .utils import ParserUtils
from ..tags import TAGS, GROUPS


class FamilyParser(RecordParser):
    """Parser for GEDCOM family records."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.FAM

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse family record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        start_line = lines[start_index]
        if not start_line.xref_id:
            raise GedcomParseError(
                f"Family record missing XREF at line {start_line.line_num}"
            )

        family = GedcomFamily(xref=start_line.xref_id)
        current_index = start_index + 1
        base_level = start_line.level
        current_event = None

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            if line.level == base_level + 1:
                if line.tag == TAGS.HUSB:
                    family.husband = line.value.strip("@")
                elif line.tag == TAGS.WIFE:
                    family.wife = line.value.strip("@")
                elif line.tag == TAGS.CHIL:
                    family.children.append(line.value.strip("@"))
                elif line.tag in self._get_family_event_tags():
                    current_event = GedcomEvent(tag=line.tag, sources=[])
                    self._assign_event_to_family(family, current_event, line.tag)
                elif line.tag == TAGS.NOTE:
                    family.notes.append(line.value)
                elif line.tag == TAGS.SOUR:
                    family.sources.append(line.value.strip("@"))
                else:
                    current_event = None

            elif line.level == base_level + 2 and current_event:
                self._parse_event_details(current_event, line)

            current_index += 1

        return family, current_index

    def _parse_event_details(self, event: GedcomEvent, line: GedcomLine):
        """Parse event details."""
        if line.tag == TAGS.DATE:
            event.date = ParserUtils.parse_date(line.value)
        elif line.tag == TAGS.PLAC:
            event.place = ParserUtils.parse_place(line.value)
        elif line.tag == TAGS.NOTE:
            event.note = line.value
        elif line.tag == TAGS.SOUR:
            if not hasattr(event, "sources"):
                event.sources = []
            event.sources.append(line.value)

    def _get_family_event_tags(self) -> List[str]:
        """Get list of all family event tags."""
        return GROUPS.get_family_event_tags()

    def _assign_event_to_family(
        self, family: GedcomFamily, event: GedcomEvent, tag: str
    ):
        """Assign event to appropriate family attribute."""
        if tag == TAGS.MARR:
            family.marriage = event
        elif tag == TAGS.ENGA:
            family.engagement = event
        elif tag == TAGS.MARB:
            family.marriage_banns = event
        elif tag == TAGS.MARC:
            family.marriage_contract = event
        elif tag == TAGS.MARL:
            family.marriage_license = event
        elif tag == TAGS.MARS:
            family.marriage_settlement = event

        elif tag == TAGS.DIV:
            family.divorce = event
        elif tag == TAGS.ANUL:
            family.annulment = event

        elif tag == TAGS.CENS:
            family.census.append(event)

        elif tag == TAGS.EVEN:
            family.events.append(event)
        else:
            family.events.append(event)
