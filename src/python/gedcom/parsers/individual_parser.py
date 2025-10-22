from typing import List, Optional

from ..exceptions import GedcomParseError
from ..models import (
    GedcomAddress,
    GedcomDate,
    GedcomEvent,
    GedcomIndividual,
    GedcomName,
    GedcomPlace,
)
from ..tags import TAGS, GROUPS
from ..tokenizer import GedcomLine
from .base import RecordParser


class IndividualParser(RecordParser):
    """Parser for GEDCOM individual records."""

    def can_parse(self, tag: str) -> bool:
        return tag == TAGS.INDI

    def parse(self, lines: List[GedcomLine], start_index: int) -> tuple:
        """Parse individual record."""
        if start_index >= len(lines):
            raise GedcomParseError("Unexpected end of file")

        start_line = lines[start_index]
        if not start_line.xref_id:
            raise GedcomParseError(
                f"Individual record missing XREF at line {start_line.line_num}"
            )

        individual = GedcomIndividual(xref=start_line.xref_id)
        current_index = start_index + 1
        base_level = start_line.level
        current_event = None
        current_attribute = None
        current_note_index = None

        while current_index < len(lines):
            line = lines[current_index]

            if line.level <= base_level:
                break

            if line.level == base_level + 1:
                if line.tag == TAGS.NAME:
                    individual.names.append(self._parse_name(line.value))
                elif line.tag == TAGS.SEX:
                    individual.sex = line.value
                elif GROUPS.is_individual_event(line.tag):
                    current_event = GedcomEvent(tag=line.tag, sources=[])
                    current_attribute = None
                    self._assign_event_to_individual(
                        individual, current_event, line.tag
                    )
                    # Don't reset current_event here - keep it for parsing sub-details
                elif line.tag == TAGS.OCCU:
                    individual.occupations.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.TITL:
                    individual.titles.append(line.value)
                    current_event = None
                    current_attribute = None
                elif GROUPS.is_individual_attribute(line.tag):
                    self._parse_individual_attribute(individual, line)
                    current_event = None
                    current_attribute = line.tag
                elif line.tag == TAGS.ADDR:
                    individual.address = self._parse_address(line.value)
                    current_event = None
                    current_attribute = None
                elif GROUPS.is_contact_subtag(line.tag) and individual.address:
                    self._parse_address_details(individual.address, line)
                elif line.tag == TAGS.FAMC:
                    individual.famc.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.FAMS:
                    individual.fams.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.NOTE:
                    individual.notes.append(line.value)
                    current_event = None
                    current_attribute = None
                    current_note_index = len(individual.notes) - 1
                elif line.tag == TAGS.SOUR:
                    source_xref = line.value.strip("@")
                    individual.sources.append(source_xref)
                    from gedcom.models import GedcomSourceCitation

                    citation = GedcomSourceCitation(
                        xref=source_xref, sour_level=line.level
                    )
                    scan_index = current_index + 1
                    while scan_index < len(lines):
                        sub = lines[scan_index]
                        if sub.level <= line.level:
                            break
                        citation.sub_lines.append((sub.level, sub.tag, sub.value))
                        scan_index += 1
                    individual.source_citations.append(citation)
                    current_index = scan_index
                    continue
                elif line.tag == TAGS.REFN:
                    individual.reference_numbers.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.LINK:
                    if (
                        individual.source_citations
                        and line.level > individual.source_citations[-1].sour_level
                    ):
                        last_citation = individual.source_citations[-1]
                        last_citation.sub_lines.append(
                            (line.level, line.tag, line.value)
                        )
                    else:
                        individual.private_links.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.PHOTO:
                    individual.private_photos.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.OBJE:
                    individual.multimedia.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.DATE_CUSTOM:
                    individual.private_dates.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.TEXT_CUSTOM:
                    individual.private_texts.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.FREL:
                    individual.family_relationships.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == TAGS.MREL:
                    individual.marriage_relationships.append(line.value)
                    current_event = None
                    current_attribute = None
                else:
                    current_event = None
                    current_attribute = None

            elif line.level == base_level + 2:
                if line.tag == TAGS.SOUR and current_event:
                    # SOUR within an event
                    self._parse_event_details(current_event, line)
                elif line.tag == TAGS.SOUR:
                    # SOUR at individual level
                    source_xref = line.value.strip("@")
                    individual.sources.append(source_xref)
                    from gedcom.models import GedcomSourceCitation

                    citation = GedcomSourceCitation(
                        xref=source_xref, sour_level=line.level
                    )
                    scan_index = current_index + 1
                    while scan_index < len(lines):
                        sub = lines[scan_index]
                        if sub.level <= line.level:
                            break
                        citation.sub_lines.append((sub.level, sub.tag, sub.value))
                        scan_index += 1
                    individual.source_citations.append(citation)
                    current_index = scan_index
                    continue
                elif current_event:
                    self._parse_event_details(current_event, line)
                elif current_attribute:
                    self._parse_attribute_details(individual, line, current_attribute)
                elif individual.address and line.tag in [
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
                ]:
                    self._parse_address_details(individual.address, line)
                elif (
                    line.tag
                    in [
                        "GIVN",
                        "SURN",
                        "NPFX",
                        "NSFX",
                        "NICK",
                        "ROMN",
                        "FONE",
                        "TRAN",
                        "TYPE",
                    ]
                    and individual.names
                ):
                    self._parse_name_details(individual.names[-1], line)
                elif (
                    GROUPS.is_note_continuation(line.tag)
                    and current_note_index is not None
                ):
                    if line.tag == TAGS.CONT:
                        individual.notes[current_note_index] += "\n" + line.value
                    elif line.tag == TAGS.CONC:
                        last_char = individual.notes[current_note_index][-1] if individual.notes[current_note_index] else ""
                        if last_char and last_char not in " \n":
                            individual.notes[current_note_index] += " " + line.value
                        else:
                            individual.notes[current_note_index] += line.value
            elif line.level == base_level + 3:
                if current_event and current_event.place:
                    if GROUPS.is_coordinate_tag(line.tag):
                        if line.tag == TAGS.LATI:
                            current_event.place.latitude = line.value
                        elif line.tag == TAGS.LONG:
                            current_event.place.longitude = line.value
                    elif line.tag == TAGS.MAP:
                        if current_event.place.map is None:
                            from gedcom.models import GedcomMap

                            current_event.place.map = GedcomMap()
                elif (
                    current_event
                    and current_event.place
                    and current_event.place.map
                    and GROUPS.is_coordinate_tag(line.tag)
                ):
                    if line.tag == TAGS.LATI:
                        current_event.place.map.latitude = line.value
                    elif line.tag == TAGS.LONG:
                        current_event.place.map.longitude = line.value
                elif line.tag == TAGS.SOUR:
                    source_xref = line.value.strip("@")
                    individual.sources.append(source_xref)
                    from gedcom.models import GedcomSourceCitation

                    citation = GedcomSourceCitation(
                        xref=source_xref, sour_level=line.level
                    )
                    scan_index = current_index + 1
                    while scan_index < len(lines):
                        sub = lines[scan_index]
                        if sub.level <= line.level:
                            break
                        citation.sub_lines.append((sub.level, sub.tag, sub.value))
                        scan_index += 1
                    individual.source_citations.append(citation)
                    current_index = scan_index
                    continue
                elif line.tag == TAGS.LINK:
                    if individual.source_citations:
                        last_citation = individual.source_citations[-1]
                        if last_citation.sour_level >= line.level - 1:
                            last_citation.sub_lines.append(
                                (line.level, line.tag, line.value)
                            )
                        else:
                            individual.private_links.append(line.value)
                    else:
                        individual.private_links.append(line.value)
            elif line.level == base_level + 4:
                if (
                    current_event
                    and current_event.place
                    and current_event.place.map
                    and GROUPS.is_coordinate_tag(line.tag)
                ):
                    if line.tag == TAGS.LATI:
                        current_event.place.map.latitude = line.value
                    elif line.tag == TAGS.LONG:
                        current_event.place.map.longitude = line.value
                elif line.tag == TAGS.SOUR:
                    source_xref = line.value.strip("@")
                    individual.sources.append(source_xref)
                    from gedcom.models import GedcomSourceCitation

                    citation = GedcomSourceCitation(
                        xref=source_xref, sour_level=line.level
                    )
                    scan_index = current_index + 1
                    while scan_index < len(lines):
                        sub = lines[scan_index]
                        if sub.level <= line.level:
                            break
                        citation.sub_lines.append((sub.level, sub.tag, sub.value))
                        scan_index += 1
                    individual.source_citations.append(citation)
                    current_index = scan_index
                    continue

            current_index += 1

        return individual, current_index

    def _parse_name(self, name_value: str) -> GedcomName:
        """Parse a GEDCOM name."""
        import re

        match = re.match(r"^([^/]*)\s*/([^/]*)/\s*(.*)$", name_value)

        if match:
            given = match.group(1).strip() or None
            surname = match.group(2).strip() or None
            suffix = match.group(3).strip() or None

            return GedcomName(
                full=name_value, given=given, surname=surname, suffix=suffix
            )
        else:
            return GedcomName(full=name_value)

    def _get_individual_event_tags(self) -> List[str]:
        """Get list of all individual event tags."""
        return GROUPS.get_individual_event_tags()

    def _assign_event_to_individual(
        self, individual: GedcomIndividual, event: GedcomEvent, tag: str
    ):
        """Assign event to appropriate individual attribute."""
        # Life events
        if tag == TAGS.BIRT:
            individual.birth = event
        elif tag == TAGS.DEAT:
            individual.death = event
        elif GROUPS.is_baptism_tag(tag):
            individual.baptism = event
        elif GROUPS.is_burial_tag(tag):
            individual.burial = event

        # Religious events
        elif tag == TAGS.CONF:
            individual.confirmation = event
        elif tag == TAGS.CHRA:
            individual.adult_christening = event
        elif tag == TAGS.BARM:
            individual.bar_mitzvah = event
        elif tag == TAGS.BASM:
            individual.bas_mitzvah = event
        elif tag == TAGS.BLES:
            individual.blessing = event
        elif tag == TAGS.ORDN:
            individual.ordination = event

        # Legal events
        elif tag == TAGS.ADOP:
            individual.adoption = event
        elif tag == TAGS.NATU:
            individual.naturalization = event
        elif tag == TAGS.PROB:
            individual.probate = event
        elif tag == TAGS.WILL:
            individual.will = event

        # Migration events
        elif tag == TAGS.EMIG:
            individual.emigration = event
        elif tag == TAGS.IMMI:
            individual.immigration = event

        # Census events (multiple allowed)
        elif tag == TAGS.CENS:
            individual.census.append(event)

        # Marriage events (can appear in individual records)
        elif GROUPS.is_marriage_related_tag(tag):
            individual.events.append(event)

        # Other events
        elif tag == TAGS.RETI:
            individual.retirement = event
        elif tag == TAGS.EVEN:
            individual.events.append(event)
        else:
            # Fallback for any other event types
            individual.events.append(event)

    def _get_individual_attribute_tags(self) -> List[str]:
        """Get list of all individual attribute tags."""
        return GROUPS.get_individual_attribute_tags()

    def _parse_individual_attribute(
        self, individual: GedcomIndividual, line: GedcomLine
    ):
        """Parse individual attribute."""
        if line.tag == TAGS.CAST:
            individual.caste = line.value
        elif line.tag == TAGS.DSCR:
            individual.physical_description = line.value
        elif line.tag == TAGS.EDUC:
            individual.education.append(line.value)
        elif line.tag == TAGS.IDNO:
            individual.identification_numbers.append({"value": line.value, "type": ""})
        elif line.tag == TAGS.NATI:
            individual.nationality.append(line.value)
        elif line.tag == TAGS.NCHI:
            individual.number_of_children = line.value
        elif line.tag == TAGS.NMR:
            individual.number_of_marriages = line.value
        elif line.tag == TAGS.PROP:
            individual.properties.append(line.value)
        elif line.tag == TAGS.RELI:
            individual.religion.append(line.value)
        elif line.tag == TAGS.RESI:
            residence_event = GedcomEvent(tag="RESI", sources=[])
            individual.residence.append(residence_event)
        elif line.tag == TAGS.SSN:
            individual.social_security_number = line.value

    def _parse_event_details(self, event: GedcomEvent, line: GedcomLine):
        """Parse event details like date, place, etc."""
        if line.tag == TAGS.DATE:
            event.date = self._parse_date(line.value)
        elif line.tag == TAGS.PLAC:
            event.place = self._parse_place(line.value)
        elif line.tag == TAGS.LATI and event.place:
            event.place.latitude = line.value
        elif line.tag == TAGS.LONG and event.place:
            event.place.longitude = line.value
        elif line.tag == TAGS.MAP and event.place:
            if event.place.map is None:
                from gedcom.models import GedcomMap

                event.place.map = GedcomMap()
        elif line.tag == TAGS.AGE:
            event.age = line.value
        elif line.tag == TAGS.CAUS:
            event.cause = line.value
        elif line.tag == TAGS.TYPE:
            event.attributes["TYPE"] = line.value
        elif line.tag == TAGS.NOTE:
            event.note = line.value
        elif line.tag == TAGS.SOUR:
            source_xref = line.value.strip("@")
            event.sources.append(source_xref)
        elif line.tag == TAGS.CONC:
            if event.note:
                event.note += line.value
            elif event.place and event.place.name:
                event.place.name += line.value
        elif line.tag == TAGS.CONT:
            if event.note:
                event.note += "\n" + line.value
            elif event.place and event.place.name:
                event.place.name += "\n" + line.value
        elif line.tag == TAGS.SOUR:
            if not hasattr(event, "sources"):
                event.sources = []
            event.sources.append(line.value)

    def _parse_attribute_details(
        self, individual: GedcomIndividual, line: GedcomLine, current_attribute: str
    ):
        """Parse attribute details like TYPE for IDNO, etc."""
        if current_attribute == "IDNO" and line.tag == TAGS.TYPE:
            if individual.identification_numbers:
                individual.identification_numbers[-1]["type"] = line.value
        elif current_attribute == "RESI":
            if individual.residence:
                self._parse_event_details(individual.residence[-1], line)

    def _parse_date(self, date_str: str) -> GedcomDate:
        """Parse GEDCOM date with full 5.5.1 support."""
        date = GedcomDate(raw=date_str)

        upper = date_str.upper().strip()

        if upper.startswith("ABT "):
            date.is_approximate = True
            date_str = date_str[4:].strip()
        elif upper.startswith("CAL "):
            date.is_calculated = True
            date_str = date_str[4:].strip()
        elif upper.startswith("EST "):
            date.is_estimated = True
            date_str = date_str[4:].strip()
        elif upper.startswith("BEF "):
            date.is_before = True
            date_str = date_str[4:].strip()
        elif upper.startswith("AFT "):
            date.is_after = True
            date_str = date_str[4:].strip()

        if upper.startswith("BET ") and " AND " in upper:
            date.is_range = True
            date.range_type = "BET"
            bet_parts = upper[4:].split(" AND ")
            if len(bet_parts) == 2:
                date.start_date = self._parse_simple_date(bet_parts[0].strip())
                date.end_date = self._parse_simple_date(bet_parts[1].strip())
            return date
        elif upper.startswith("FROM ") and " TO " in upper:
            date.is_range = True
            date.range_type = "FROM...TO"
            from_parts = upper[5:].split(" TO ")
            if len(from_parts) == 2:
                date.start_date = self._parse_simple_date(from_parts[0].strip())
                date.end_date = self._parse_simple_date(from_parts[1].strip())
            return date
        elif upper.startswith("FROM "):
            date.is_range = True
            date.range_type = "FROM"
            date.start_date = self._parse_simple_date(upper[5:].strip())
            return date
        elif upper.startswith("TO "):
            date.is_range = True
            date.range_type = "TO"
            date.end_date = self._parse_simple_date(upper[3:].strip())
            return date

        # Parse simple date
        return self._parse_simple_date(date_str, date)

    def _parse_simple_date(
        self, date_str: str, date: Optional[GedcomDate] = None
    ) -> GedcomDate:
        """Parse a simple date without qualifiers."""
        if date is None:
            date = GedcomDate(raw=date_str)

        months = {
            "JAN": 1,
            "FEB": 2,
            "MAR": 3,
            "APR": 4,
            "MAY": 5,
            "JUN": 6,
            "JUL": 7,
            "AUG": 8,
            "SEP": 9,
            "OCT": 10,
            "NOV": 11,
            "DEC": 12,
        }

        parts = date_str.strip().split()

        try:
            if len(parts) == 3:
                # Full date: DD MMM YYYY
                date.day = int(parts[0])
                date.month = months.get(parts[1].upper())
                date.year = int(parts[2])
                date.has_day = True
                date.has_month = True
                date.has_year = True
            elif len(parts) == 2:
                # Month and year: MMM YYYY
                date.month = months.get(parts[0].upper())
                date.year = int(parts[1])
                date.has_month = True
                date.has_year = True
            elif len(parts) == 1:
                # Year only: YYYY
                date.year = int(parts[0])
                date.has_year = True
        except (ValueError, IndexError):
            pass

        return date

    def _parse_place(self, place_str: str) -> GedcomPlace:
        """Parse GEDCOM place."""
        parts = [p.strip() for p in place_str.split(",")]
        return GedcomPlace(name=place_str, parts=parts)

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

    def _parse_name_details(self, name: GedcomName, line: GedcomLine) -> None:
        """Parse name sub-details."""
        if line.tag == TAGS.GIVN:
            name.given = line.value
        elif line.tag == TAGS.SURN:
            name.surname = line.value
        elif line.tag == TAGS.NPFX:
            name.prefix = line.value
        elif line.tag == TAGS.NSFX:
            name.suffix = line.value
        elif line.tag == TAGS.NICK:
            name.nickname = line.value
        elif line.tag == TAGS.ROMN:
            name.romanized = line.value
        elif line.tag == TAGS.FONE:
            name.phonetic = line.value
        elif line.tag == TAGS.TRAN:
            name.translation = line.value
        elif line.tag == TAGS.TYPE:
            name.name_type = line.value

    def _parse_place_geographic_details(
        self, place: GedcomPlace, line: GedcomLine
    ) -> None:
        """Parse place geographic sub-details."""
        if line.tag == TAGS.LATI:
            place.latitude = line.value
        elif line.tag == TAGS.LONG:
            place.longitude = line.value
        elif line.tag == TAGS.MAP:
            if place.map is None:
                from gedcom.models import GedcomMap

                place.map = GedcomMap()
            # MAP can have LATI and LONG sub-tags
            # This will be handled by the next level parsing
