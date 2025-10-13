from typing import List

from ..exceptions import GedcomParseError
from ..models import GedcomAddress, GedcomRepository
from ..tokenizer import GedcomLine
from .base import RecordParser


class RepositoryParser(RecordParser):
    """Parser for GEDCOM repository."""

    def can_parse(self, tag: str) -> bool:
        return tag == "REPO"

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse repository record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        repository = GedcomRepository(xref=lines[start_index].xref_id)
        current_index = start_index + 1
        base_level = lines[start_index].level

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            repository.raw_structure.append((line.level, line.tag, line.value))

            if line.tag == "NAME":
                repository.name = line.value
            elif line.tag == "ADDR":
                repository.address = self._parse_address(line.value)
            elif line.tag == "NOTE":
                repository.notes.append(line.value)
            elif line.tag == "SOUR":
                repository.sources.append(line.value.strip("@"))
            elif line.tag in ["PHON", "EMAIL", "FAX", "WWW"] and repository.address:
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
        if line.tag == "ADR1":
            address.value = line.value
        elif line.tag == "ADR2":
            address.address_line2 = line.value
        elif line.tag == "CITY":
            address.city = line.value
        elif line.tag == "STAE":
            address.state = line.value
        elif line.tag == "POST":
            address.postal_code = line.value
        elif line.tag == "CTRY":
            address.country = line.value
        elif line.tag == "PHON":
            address.phone.append(line.value)
        elif line.tag == "EMAIL":
            address.email.append(line.value)
        elif line.tag == "FAX":
            address.fax.append(line.value)
        elif line.tag == "WWW":
            address.website.append(line.value)
