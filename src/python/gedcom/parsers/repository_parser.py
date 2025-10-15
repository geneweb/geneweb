from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomAddress, GedcomRepository
from ..tokenizer import GedcomLine
from .base import RecordParser
from ..tags import TAGS, GROUPS


class RepositoryParser(RecordParser):
    """Parser for GEDCOM repository."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.REPO

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse repository record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        repository = GedcomRepository(xref=lines[start_index].xref_id or "")
        current_index = start_index + 1
        base_level = lines[start_index].level

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            repository.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == TAGS.NAME:
                repository.name = line.value
            elif line.tag == TAGS.ADDR:
                repository.address = self._parse_address(line.value)
            elif line.tag == TAGS.NOTE:
                repository.notes.append(line.value)
            elif line.tag == TAGS.SOUR:
                repository.sources.append(line.value.strip("@"))
            elif GROUPS.is_contact_tag(line.tag) and repository.address:
                self._parse_address_details(repository.address, line)
            elif (
                line.level == base_level + 2
                and repository.address
                and line.tag
                in [
                    "ADR1",
                    "ADR2",
                    "CITY",
                    "STAE",
                    "POST",
                    "CTRY",
                    "PHON",
                    "EMAIL",
                    "FAX",
                    "WWW",
                ]
            ):
                self._parse_address_details(repository.address, line)

            current_index += 1

        return repository, current_index

    def _parse_address(self, address_str: str) -> GedcomAddress:
        """Parse GEDCOM address."""
        return GedcomAddress(value=address_str)

    def _parse_address_details(self, address: GedcomAddress, line: GedcomLine) -> None:
        """Parse address sub-details."""
        if line.tag == TAGS.ADR1:
            address.value = line.value
        elif line.tag == TAGS.ADR2:
            address.address_line2 = line.value
        elif line.tag == TAGS.CITY:
            address.city = line.value
        elif line.tag == TAGS.STAE:
            address.state = line.value
        elif line.tag == TAGS.POST:
            address.postal_code = line.value
        elif line.tag == TAGS.CTRY:
            address.country = line.value
        elif line.tag == TAGS.PHON:
            address.phone.append(line.value)
        elif line.tag == TAGS.EMAIL:
            address.email.append(line.value)
        elif line.tag == TAGS.FAX:
            address.fax.append(line.value)
        elif line.tag == TAGS.WWW:
            address.website.append(line.value)
