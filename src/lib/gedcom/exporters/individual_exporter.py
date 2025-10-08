from typing import TextIO
from ..models import (
    GedcomIndividual, GedcomEvent, GedcomName, GedcomDate
)
from .base import RecordExporter


class IndividualExporter(RecordExporter):
    """Exporter for GEDCOM individual records."""

    def can_export(self, record_type: str) -> bool:
        return record_type == 'INDI'

    def export(self, file: TextIO, xref: str, individual: GedcomIndividual) -> None:
        """Export individual to GEDCOM format."""
        # Ensure XREF has @ symbols
        if not xref.startswith('@'):
            xref = f"@{xref}@"
        file.write(f"0 {xref} INDI\n")

        # Names
        for name in individual.names:
            self._write_name(file, name)

        # Sex
        if individual.sex:
            file.write(f"1 SEX {individual.sex}\n")

        # Life events
        if individual.birth:
            self._write_event(file, "BIRT", individual.birth)

        if individual.baptism:
            tag = "BAPM" if individual.baptism.tag == "BAPM" else "CHR"
            self._write_event(file, tag, individual.baptism)

        if individual.death:
            self._write_event(file, "DEAT", individual.death)

        if individual.burial:
            tag = "BURI" if individual.burial.tag == "BURI" else "CREM"
            self._write_event(file, tag, individual.burial)

        # Religious events
        if individual.confirmation:
            self._write_event(file, "CONF", individual.confirmation)

        if individual.adult_christening:
            self._write_event(file, "CHRA", individual.adult_christening)

        if individual.bar_mitzvah:
            self._write_event(file, "BARM", individual.bar_mitzvah)

        if individual.bas_mitzvah:
            self._write_event(file, "BASM", individual.bas_mitzvah)

        if individual.blessing:
            self._write_event(file, "BLES", individual.blessing)

        if individual.ordination:
            self._write_event(file, "ORDN", individual.ordination)

        # Legal events
        if individual.adoption:
            self._write_event(file, "ADOP", individual.adoption)

        if individual.naturalization:
            self._write_event(file, "NATU", individual.naturalization)

        if individual.probate:
            self._write_event(file, "PROB", individual.probate)

        if individual.will:
            self._write_event(file, "WILL", individual.will)

        # Migration events
        if individual.emigration:
            self._write_event(file, "EMIG", individual.emigration)

        if individual.immigration:
            self._write_event(file, "IMMI", individual.immigration)

        # Census events
        for census_event in individual.census:
            self._write_event(file, "CENS", census_event)

        # Other events
        if individual.retirement:
            self._write_event(file, "RETI", individual.retirement)

        # Generic events
        for event in individual.events:
            self._write_event(file, event.tag, event)

        # Attributes
        for occupation in individual.occupations:
            file.write(f"1 OCCU {occupation}\n")

        for title in individual.titles:
            file.write(f"1 TITL {title}\n")

        # Physical attributes
        if individual.caste:
            file.write(f"1 CAST {individual.caste}\n")

        if individual.physical_description:
            file.write(f"1 DSCR {individual.physical_description}\n")

        # Education and identification
        for education in individual.education:
            file.write(f"1 EDUC {education}\n")

        for idno in individual.identification_numbers:
            file.write(f"1 IDNO {idno['value']}\n")
            if idno['type']:
                file.write(f"2 TYPE {idno['type']}\n")

        for nationality in individual.nationality:
            file.write(f"1 NATI {nationality}\n")

        # Family statistics
        if individual.number_of_children:
            file.write(f"1 NCHI {individual.number_of_children}\n")

        if individual.number_of_marriages:
            file.write(f"1 NMR {individual.number_of_marriages}\n")

        # Other attributes
        for prop in individual.properties:
            file.write(f"1 PROP {prop}\n")

        for religion in individual.religion:
            file.write(f"1 RELI {religion}\n")

        for residence in individual.residence:
            self._write_event(file, "RESI", residence)

        if individual.social_security_number:
            file.write(f"1 SSN {individual.social_security_number}\n")

        # Address
        if individual.address:
            file.write(f"1 ADDR {individual.address.value}\n")
            if individual.address.city:
                file.write(f"2 CITY {individual.address.city}\n")
            if individual.address.state:
                file.write(f"2 STAE {individual.address.state}\n")
            if individual.address.postal_code:
                file.write(f"2 POST {individual.address.postal_code}\n")
            if individual.address.country:
                file.write(f"2 CTRY {individual.address.country}\n")

            # Contact information in address
            for phone in individual.address.phone:
                file.write(f"2 PHON {phone}\n")
            for email in individual.address.email:
                file.write(f"2 EMAIL {email}\n")
            for fax in individual.address.fax:
                file.write(f"2 FAX {fax}\n")
            for website in individual.address.website:
                file.write(f"2 WWW {website}\n")

        # Family links
        for famc in individual.famc:
            file.write(f"1 FAMC @{famc}@\n")

        for fams in individual.fams:
            file.write(f"1 FAMS @{fams}@\n")

        # Notes
        for note in individual.notes:
            self._write_multiline(file, "1 NOTE", note)

        if getattr(individual, 'source_citations', None):
            self._write_source_citations(file, individual)
        else:
            for source in individual.sources:
                file.write(f"1 SOUR {source}\n")

        for refn in individual.reference_numbers:
            file.write(f"1 REFN {refn}\n")
        for link in individual.private_links:
            file.write(f"1 _LINK {link}\n")
        for photo in individual.private_photos:
            file.write(f"1 _PHOTO {photo}\n")
        for multimedia in individual.multimedia:
            file.write(f"1 OBJE @{multimedia}@\n")
        for date in individual.private_dates:
            file.write(f"1 _DATE {date}\n")
        for text in individual.private_texts:
            file.write(f"1 _TEXT {text}\n")
        for frel in individual.family_relationships:
            file.write(f"1 _FREL {frel}\n")
        for mrel in individual.marriage_relationships:
            file.write(f"1 _MREL {mrel}\n")

    def _write_source_citations(self, file: TextIO, individual: GedcomIndividual) -> None:
        """Write source citations for an individual without altering content or levels."""
        for citation in individual.source_citations:

            # citation.xref may or may not already have @ symbols
            xref = citation.xref
            if not xref.startswith('@'):
                xref = f"@{xref}@"
            file.write(f"2 SOUR {xref}\n")
            for level, tag, value in citation.sub_lines:
                file.write(f"{level} {tag}{(' ' + value) if value else ''}\n")

    def _write_name(self, file: TextIO, name: GedcomName) -> None:
        """Write a name record."""
        file.write(f"1 NAME {name.full}\n")

        if name.given:
            file.write(f"2 GIVN {name.given}\n")
        if name.surname:
            file.write(f"2 SURN {name.surname}\n")
        if name.prefix:
            file.write(f"2 NPFX {name.prefix}\n")
        if name.suffix:
            file.write(f"2 NSFX {name.suffix}\n")
        if name.nickname:
            file.write(f"2 NICK {name.nickname}\n")

        if name.romanized:
            file.write(f"2 ROMN {name.romanized}\n")
        if name.phonetic:
            file.write(f"2 FONE {name.phonetic}\n")
        if name.translation:
            file.write(f"2 TRAN {name.translation}\n")
        if name.name_type:
            file.write(f"2 TYPE {name.name_type}\n")

    def _write_event(self, file: TextIO, tag: str, event: GedcomEvent) -> None:
        """Write an event record."""
        # Utiliser la structure brute si disponible pour une conservation maximale
        if hasattr(event, 'raw_structure') and event.raw_structure:
            for level, tag_name, value in event.raw_structure:
                if value:
                    file.write(f"{level} {tag_name} {value}\n")
                else:
                    file.write(f"{level} {tag_name}\n")
            return

        file.write(f"1 {tag}\n")

        if event.date:
            date_str = self._format_date(event.date)
            file.write(f"2 DATE {date_str}\n")

        if event.place:
            file.write(f"2 PLAC {event.place.name}\n")
            if event.place.latitude:
                file.write(f"3 LATI {event.place.latitude}\n")
            if event.place.longitude:
                file.write(f"3 LONG {event.place.longitude}\n")
            if event.place.map:
                file.write(f"3 MAP\n")
                if event.place.map.latitude:
                    file.write(f"4 LATI {event.place.map.latitude}\n")
                if event.place.map.longitude:
                    file.write(f"4 LONG {event.place.map.longitude}\n")

        if event.age:
            file.write(f"2 AGE {event.age}\n")

        if event.cause:
            file.write(f"2 CAUS {event.cause}\n")

        if event.attributes.get('TYPE'):
            file.write(f"2 TYPE {event.attributes['TYPE']}\n")

        if event.note:
            self._write_multiline(file, "2 NOTE", event.note)

        for source in event.sources:
            file.write(f"2 SOUR {source}\n")

    def _format_date(self, date: GedcomDate) -> str:
        """Format date for GEDCOM export with full 5.5.1 support."""
        if not date.is_valid:
            return date.raw

        # Handle date ranges
        if date.is_range:
            if date.range_type == 'BET' and date.start_date and date.end_date:
                start_str = self._format_simple_date(date.start_date)
                end_str = self._format_simple_date(date.end_date)
                return f"BET {start_str} AND {end_str}"
            elif date.range_type == 'FROM...TO' and date.start_date and date.end_date:
                start_str = self._format_simple_date(date.start_date)
                end_str = self._format_simple_date(date.end_date)
                return f"FROM {start_str} TO {end_str}"
            elif date.range_type == 'FROM' and date.start_date:
                start_str = self._format_simple_date(date.start_date)
                return f"FROM {start_str}"
            elif date.range_type == 'TO' and date.end_date:
                end_str = self._format_simple_date(date.end_date)
                return f"TO {end_str}"

        # Handle simple dates with qualifiers
        date_str = self._format_simple_date(date)

        # Add qualifiers
        if date.is_approximate:
            return f"ABT {date_str}"
        elif date.is_calculated:
            return f"CAL {date_str}"
        elif date.is_estimated:
            return f"EST {date_str}"
        elif date.is_before:
            return f"BEF {date_str}"
        elif date.is_after:
            return f"AFT {date_str}"

        return date_str

    def _format_simple_date(self, date: GedcomDate) -> str:
        """Format a simple date without qualifiers."""
        parts = []

        # Add date components
        if date.day:
            parts.append(str(date.day))

        if date.month:
            months = ['', 'JAN', 'FEB', 'MAR', 'APR', 'MAY', 'JUN',
                     'JUL', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC']
            if 1 <= date.month <= 12:
                parts.append(months[date.month])

        if date.year:
            parts.append(str(date.year))

        return ' '.join(parts) if parts else date.raw

    def _write_multiline(self, file: TextIO, tag: str, text: str) -> None:
        """Write multiline text using CONC and CONT tags."""
        if not text:
            return

        lines = text.split('\n')
        level = tag.split()[0]

        # First line
        if lines:
            first_line = lines[0]
            self._write_line_with_conc(file, tag, first_line)
            lines = lines[1:]

        # Additional lines
        for line in lines:
            if not line:
                file.write(f"{level} CONT\n")
            else:
                self._write_line_with_conc(file, f"{level} CONT", line)

    def _write_line_with_conc(self, file: TextIO, tag: str, text: str) -> None:
        """Write a line using CONC for long lines."""
        max_length = 248  # GEDCOM line length limit
        level = tag.split()[0]

        if len(text) <= max_length:
            file.write(f"{tag} {text}\n")
        else:
            # Write first chunk
            file.write(f"{tag} {text[:max_length]}\n")
            remaining = text[max_length:]

            # Write remaining chunks with CONC
            while remaining:
                chunk = remaining[:max_length]
                file.write(f"{level} CONC {chunk}\n")
                remaining = remaining[max_length:]

