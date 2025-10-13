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
from ..tokenizer import GedcomLine
from .base import RecordParser


class IndividualParser(RecordParser):
    """Parser for GEDCOM individual records."""

    def can_parse(self, tag: str) -> bool:
        return tag == "INDI"

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
                if line.tag == "NAME":
                    individual.names.append(self._parse_name(line.value))
                elif line.tag == "SEX":
                    individual.sex = line.value
                elif line.tag in self._get_individual_event_tags():
                    current_event = GedcomEvent(tag=line.tag, sources=[])
                    current_attribute = None
                    self._assign_event_to_individual(
                        individual, current_event, line.tag
                    )
                    # Don't reset current_event here - keep it for parsing sub-details
                elif line.tag == "OCCU":
                    individual.occupations.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == "TITL":
                    individual.titles.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag in self._get_individual_attribute_tags():
                    self._parse_individual_attribute(individual, line)
                    current_event = None
                    current_attribute = line.tag
                elif line.tag == "ADDR":
                    individual.address = self._parse_address(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag in ["PHON", "EMAIL", "FAX", "WWW"] and individual.address:
                    self._parse_address_details(individual.address, line)
                elif line.tag == "FAMC":
                    individual.famc.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == "FAMS":
                    individual.fams.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == "NOTE":
                    individual.notes.append(line.value)
                    current_event = None
                    current_attribute = None
                    current_note_index = len(individual.notes) - 1
                elif line.tag == "SOUR":
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
                elif line.tag == "REFN":
                    individual.reference_numbers.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == "_LINK":
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
                elif line.tag == "_PHOTO":
                    individual.private_photos.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == "OBJE":
                    individual.multimedia.append(line.value.strip("@"))
                    current_event = None
                    current_attribute = None
                elif line.tag == "_DATE":
                    individual.private_dates.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == "_TEXT":
                    individual.private_texts.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == "_FREL":
                    individual.family_relationships.append(line.value)
                    current_event = None
                    current_attribute = None
                elif line.tag == "_MREL":
                    individual.marriage_relationships.append(line.value)
                    current_event = None
                    current_attribute = None
                else:
                    current_event = None
                    current_attribute = None

            elif line.level == base_level + 2:
                if line.tag == "SOUR" and current_event:
                    # SOUR within an event
                    self._parse_event_details(current_event, line)
                elif line.tag == "SOUR":
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
                elif line.tag in ["CONT", "CONC"] and current_note_index is not None:
                    if line.tag == "CONT":
                        individual.notes[current_note_index] += "\n" + line.value
                    elif line.tag == "CONC":
                        individual.notes[current_note_index] += line.value
            elif line.level == base_level + 3:
                if current_event and current_event.place:
                    if line.tag in ["LATI", "LONG"]:
                        if line.tag == "LATI":
                            current_event.place.latitude = line.value
                        elif line.tag == "LONG":
                            current_event.place.longitude = line.value
                    elif line.tag == "MAP":
                        if current_event.place.map is None:
                            from gedcom.models import GedcomMap

                            current_event.place.map = GedcomMap()
                elif (
                    current_event
                    and current_event.place
                    and current_event.place.map
                    and line.tag in ["LATI", "LONG"]
                ):
                    if line.tag == "LATI":
                        current_event.place.map.latitude = line.value
                    elif line.tag == "LONG":
                        current_event.place.map.longitude = line.value
                elif line.tag == "SOUR":
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
                elif line.tag == "_LINK":
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
                    and line.tag in ["LATI", "LONG"]
                ):
                    if line.tag == "LATI":
                        current_event.place.map.latitude = line.value
                    elif line.tag == "LONG":
                        current_event.place.map.longitude = line.value
                elif line.tag == "SOUR":
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
        return [
            # Life events
            "BIRT",
            "DEAT",
            "BAPM",
            "CHR",
            "BURI",
            "CREM",
            # Religious events
            "CONF",
            "CHRA",
            "BARM",
            "BASM",
            "BLES",
            "ORDN",
            # Legal events
            "ADOP",
            "NATU",
            "PROB",
            "WILL",
            # Migration events
            "EMIG",
            "IMMI",
            # Census events
            "CENS",
            # Marriage events (can appear in individual records)
            "MARR",
            "DIV",
            "ANUL",
            "ENGA",
            # Other events
            "RETI",
            "EVEN",
        ]

    def _assign_event_to_individual(
        self, individual: GedcomIndividual, event: GedcomEvent, tag: str
    ):
        """Assign event to appropriate individual attribute."""
        # Life events
        if tag == "BIRT":
            individual.birth = event
        elif tag == "DEAT":
            individual.death = event
        elif tag in ["BAPM", "CHR"]:
            individual.baptism = event
        elif tag in ["BURI", "CREM"]:
            individual.burial = event

        # Religious events
        elif tag == "CONF":
            individual.confirmation = event
        elif tag == "CHRA":
            individual.adult_christening = event
        elif tag == "BARM":
            individual.bar_mitzvah = event
        elif tag == "BASM":
            individual.bas_mitzvah = event
        elif tag == "BLES":
            individual.blessing = event
        elif tag == "ORDN":
            individual.ordination = event

        # Legal events
        elif tag == "ADOP":
            individual.adoption = event
        elif tag == "NATU":
            individual.naturalization = event
        elif tag == "PROB":
            individual.probate = event
        elif tag == "WILL":
            individual.will = event

        # Migration events
        elif tag == "EMIG":
            individual.emigration = event
        elif tag == "IMMI":
            individual.immigration = event

        # Census events (multiple allowed)
        elif tag == "CENS":
            individual.census.append(event)

        # Marriage events (can appear in individual records)
        elif tag in ["MARR", "DIV", "ANUL", "ENGA"]:
            individual.events.append(event)

        # Other events
        elif tag == "RETI":
            individual.retirement = event
        elif tag == "EVEN":
            individual.events.append(event)
        else:
            # Fallback for any other event types
            individual.events.append(event)

    def _get_individual_attribute_tags(self) -> List[str]:
        """Get list of all individual attribute tags."""
        return [
            # Physical attributes
            "CAST",
            "DSCR",
            # Education and identification
            "EDUC",
            "IDNO",
            "NATI",
            # Family statistics
            "NCHI",
            "NMR",
            # Other attributes
            "PROP",
            "RELI",
            "RESI",
            "SSN",
        ]

    def _parse_individual_attribute(
        self, individual: GedcomIndividual, line: GedcomLine
    ):
        """Parse individual attribute."""
        if line.tag == "CAST":
            individual.caste = line.value
        elif line.tag == "DSCR":
            individual.physical_description = line.value
        elif line.tag == "EDUC":
            individual.education.append(line.value)
        elif line.tag == "IDNO":
            individual.identification_numbers.append({"value": line.value, "type": ""})
        elif line.tag == "NATI":
            individual.nationality.append(line.value)
        elif line.tag == "NCHI":
            individual.number_of_children = line.value
        elif line.tag == "NMR":
            individual.number_of_marriages = line.value
        elif line.tag == "PROP":
            individual.properties.append(line.value)
        elif line.tag == "RELI":
            individual.religion.append(line.value)
        elif line.tag == "RESI":
            residence_event = GedcomEvent(tag="RESI", sources=[])
            individual.residence.append(residence_event)
        elif line.tag == "SSN":
            individual.social_security_number = line.value

    def _parse_event_details(self, event: GedcomEvent, line: GedcomLine):
        """Parse event details like date, place, etc."""
        if line.tag == "DATE":
            event.date = self._parse_date(line.value)
        elif line.tag == "PLAC":
            event.place = self._parse_place(line.value)
        elif line.tag == "LATI" and event.place:
            event.place.latitude = line.value
        elif line.tag == "LONG" and event.place:
            event.place.longitude = line.value
        elif line.tag == "MAP" and event.place:
            if event.place.map is None:
                from gedcom.models import GedcomMap

                event.place.map = GedcomMap()
        elif line.tag == "AGE":
            event.age = line.value
        elif line.tag == "CAUS":
            event.cause = line.value
        elif line.tag == "TYPE":
            event.attributes["TYPE"] = line.value
        elif line.tag == "NOTE":
            event.note = line.value
        elif line.tag == "SOUR":
            source_xref = line.value.strip("@")
            event.sources.append(source_xref)
        elif line.tag == "CONC":
            if event.note:
                event.note += line.value
            elif event.place and event.place.name:
                event.place.name += line.value
        elif line.tag == "CONT":
            if event.note:
                event.note += "\n" + line.value
            elif event.place and event.place.name:
                event.place.name += "\n" + line.value
        elif line.tag == "SOUR":
            if not hasattr(event, "sources"):
                event.sources = []
            event.sources.append(line.value)

    def _parse_attribute_details(
        self, individual: GedcomIndividual, line: GedcomLine, current_attribute: str
    ):
        """Parse attribute details like TYPE for IDNO, etc."""
        if current_attribute == "IDNO" and line.tag == "TYPE":
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

    def _parse_name_details(self, name: GedcomName, line: GedcomLine) -> None:
        """Parse name sub-details."""
        if line.tag == "GIVN":
            name.given = line.value
        elif line.tag == "SURN":
            name.surname = line.value
        elif line.tag == "NPFX":
            name.prefix = line.value
        elif line.tag == "NSFX":
            name.suffix = line.value
        elif line.tag == "NICK":
            name.nickname = line.value
        elif line.tag == "ROMN":
            name.romanized = line.value
        elif line.tag == "FONE":
            name.phonetic = line.value
        elif line.tag == "TRAN":
            name.translation = line.value
        elif line.tag == "TYPE":
            name.name_type = line.value

    def _parse_place_geographic_details(
        self, place: GedcomPlace, line: GedcomLine
    ) -> None:
        """Parse place geographic sub-details."""
        if line.tag == "LATI":
            place.latitude = line.value
        elif line.tag == "LONG":
            place.longitude = line.value
        elif line.tag == "MAP":
            if place.map is None:
                from gedcom.models import GedcomMap

                place.map = GedcomMap()
            # MAP can have LATI and LONG sub-tags
            # This will be handled by the next level parsing
