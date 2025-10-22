"""
Conversion module for GEDCOM to GeneWeb.

This module contains all conversion functions from GEDCOM to GeneWeb data structures.
"""

from typing import List, Tuple
from lib.db.core.enums import DivorceStatus, RelationKind, Sex
from lib.db.core.types import Iper, dummy_iper
from lib.db.models.events import Date
from lib.db.models.family import GenFamily
from lib.db.models.person import GenPerson
from lib.db.models.relations import GenCouple


class GedcomConverter:
    """Converter for GEDCOM data structures to GeneWeb format."""

    def __init__(self, logger, options=None):
        """Initialize converter with logger and options."""
        self.logger = logger
        self.options = options

    def convert_individual(self, individual) -> GenPerson:
        """Convert GEDCOM individual to GenPerson."""
        try:
            first_name = ""
            surname = ""
            if individual.names:
                primary_name = individual.names[0]
                first_name = primary_name.given or ""
                surname = primary_name.surname or ""

                # Apply name processing options
                first_name = self._process_first_name(first_name)
                surname = self._process_surname(surname)

            sex = Sex.NEUTER
            if individual.sex == "M":
                sex = Sex.MALE
            elif individual.sex == "F":
                sex = Sex.FEMALE

            birth_date = None
            if individual.birth and individual.birth.date:
                birth_date = self.convert_date(individual.birth.date)

            baptism_date = None
            if individual.baptism and individual.baptism.date:
                baptism_date = self.convert_date(individual.baptism.date)

            death_date = None
            if individual.death and individual.death.date:
                death_date = self.convert_date(individual.death.date)

            burial_date = None
            if individual.burial and individual.burial.date:
                burial_date = self.convert_date(individual.burial.date)

            # Convert notes and sources
            notes_text = ""
            if hasattr(individual, "notes") and individual.notes:
                notes_text = "\n".join(individual.notes)
                self.logger.info(f"Converted notes for {first_name} {surname}: {notes_text}")

            sources_text = ""
            if hasattr(individual, "sources") and individual.sources:
                sources_text = "\n".join(individual.sources)
                self.logger.info(f"Converted sources for {first_name} {surname}: {sources_text}")
            elif hasattr(individual, "source_citations") and individual.source_citations:
                # Convert source citations to text
                source_citations = []
                for citation in individual.source_citations:
                    if hasattr(citation, "source") and citation.source:
                        source_citations.append(citation.source)
                sources_text = "\n".join(source_citations)
                self.logger.info(f"Converted source citations for {first_name} {surname}: {sources_text}")

            person = GenPerson(
                first_name=first_name,
                surname=surname,
                sex=sex,
                birth=birth_date,
                baptism=baptism_date,
                death=death_date,
                burial=burial_date,
                notes=notes_text,
                sources=sources_text,
            )

            # Apply default source if specified and no sources exist
            if self.options and self.options.default_source:
                # Check if individual has any sources
                has_sources = bool(sources_text)

                if not has_sources:
                    if person.notes:
                        person.notes += f"\nDefault source: {self.options.default_source}"
                    else:
                        person.notes = f"Default source: {self.options.default_source}"

            # Handle --uin: put untreated GEDCOM tags in notes
            if self.options and self.options.uin:
                # Process untreated GEDCOM tags and add them as notes
                untreated_tags = self._extract_untreated_tags(individual)
                if untreated_tags:
                    untreated_text = "\n".join(untreated_tags)
                    if person.notes:
                        person.notes += f"\n{untreated_text}"
                    else:
                        person.notes = untreated_text

            return person
        except Exception as e:
            self.logger.error(f"Error converting individual: {e}")
            return GenPerson(first_name="?", surname="?", sex=Sex.NEUTER)

    def convert_family(self, family) -> Tuple[GenFamily, GenCouple, List]:
        """Convert GEDCOM family to GenFamily."""
        try:
            marriage_date = None
            if family.marriage and family.marriage.date:
                marriage_date = self.convert_date(family.marriage.date)

            divorce_status = DivorceStatus.NOT_DIVORCED
            if family.divorce:
                divorce_status = DivorceStatus.DIVORCED

            husband_id = dummy_iper()
            if family.husband:
                if family.husband.startswith("@I") and family.husband.endswith("@"):
                    husband_id = Iper(int(family.husband[2:-1]))
                else:
                    husband_id = Iper(hash(family.husband) % 1000000)

            wife_id = dummy_iper()
            if family.wife:
                if family.wife.startswith("@I") and family.wife.endswith("@"):
                    wife_id = Iper(int(family.wife[2:-1]))
                else:
                    wife_id = Iper(hash(family.wife) % 1000000)

            children_ids = []
            for child_ref in family.children:
                if child_ref.startswith("@I") and child_ref.endswith("@"):
                    child_id = Iper(int(child_ref[2:-1]))
                else:
                    child_id = Iper(hash(child_ref) % 1000000)
                children_ids.append(child_id)

            # Convert notes and sources
            notes_text = ""
            if hasattr(family, "notes") and family.notes:
                notes_text = "\n".join(family.notes)

            sources_text = ""
            if hasattr(family, "sources") and family.sources:
                sources_text = "\n".join(family.sources)

            geneweb_family = GenFamily(
                marriage=marriage_date,
                divorce=divorce_status,
                relation=RelationKind.MARRIED,
                notes=notes_text,
                sources=sources_text,
            )

            couple = GenCouple(father=husband_id, mother=wife_id)

            return geneweb_family, couple, children_ids

        except Exception as e:
            self.logger.error(f"Error converting family: {e}")
            return (
                GenFamily(relation=RelationKind.MARRIED),
                GenCouple(father=dummy_iper(), mother=dummy_iper()),
                [],
            )

    def convert_date(self, gedcom_date) -> Date:
        """Convert GEDCOM date to GeneWeb Date."""
        try:
            if not gedcom_date:
                return Date.none()

            if hasattr(gedcom_date, "year"):
                day = gedcom_date.day if hasattr(gedcom_date, "day") else 0
                month = gedcom_date.month if hasattr(gedcom_date, "month") else 0
                year = gedcom_date.year if hasattr(gedcom_date, "year") else 0

                if day == "":
                    day = 0
                if month == "":
                    month = 0
                if year == "":
                    year = 0

                if isinstance(day, str):
                    try:
                        day = int(day) if day else 0
                    except ValueError:
                        day = 0
                if isinstance(month, str):
                    try:
                        month = int(month) if month else 0
                    except ValueError:
                        month = 0
                if isinstance(year, str):
                    try:
                        year = int(year) if year else 0
                    except ValueError:
                        year = 0

                # Apply date processing options
                day, month, year = self._process_date_components(day, month, year)

                return Date(day=day or 0, month=month or 0, year=year or 0)

            return Date.none()
        except Exception as e:
            self.logger.error(f"Error converting date: {e}")
            return Date.none()

    def _process_date_components(
        self, day: int, month: int, year: int
    ) -> tuple[int, int, int]:
        """Process date components according to options."""
        if not self.options:
            return day, month, year

        # Handle --dates-dm: day/month/year interpretation
        if self.options.dates_dm and not self.options.dates_md:
            # If we have both day and month, and day > 12, swap them
            if day and month and day > 12 and month <= 12:
                day, month = month, day

        # Handle --dates-md: month/day/year interpretation
        elif self.options.dates_md and not self.options.dates_dm:
            # If we have both day and month, and month > 12, swap them
            if day and month and month > 12 and day <= 12:
                day, month = month, day

        # Handle --no-nd: don't interpret minus as negative year
        if self.options.no_nd and year and year < 0:
            year = abs(year)

        # Handle --tnd: set negative dates when inconsistency
        if self.options.tnd and year and year < 0:
            # Keep negative years as is for --tnd
            pass

        return day, month, year

    def _process_first_name(self, first_name: str) -> str:
        """Process first name according to options."""
        if not first_name or not self.options:
            return first_name

        # Handle --efn: extract first name only
        if self.options.efn and not self.options.no_efn:
            names = first_name.split()
            if len(names) > 1:
                first_name = names[0]

        # Handle --fne: extract name between delimiters
        if self.options.fne and not self.options.no_efn:
            start_char = self.options.fne[0] if len(self.options.fne) > 0 else '"'
            end_char = self.options.fne[1] if len(self.options.fne) > 1 else '"'

            start_idx = first_name.find(start_char)
            if start_idx != -1:
                end_idx = first_name.find(end_char, start_idx + 1)
                if end_idx != -1:
                    extracted = first_name[start_idx + 1 : end_idx]
                    if extracted:
                        first_name = extracted

        # Handle --lf: lowercase first names
        if self.options.lf:
            first_name = first_name.lower()

        return first_name

    def _process_surname(self, surname: str) -> str:
        """Process surname according to options."""
        if not surname or not self.options:
            return surname

        # Handle --ls: lowercase with uppercase initials, keep particles lowercase
        if self.options.ls:
            surname = self._to_title_case_with_particles(surname)

        # Handle --us: uppercase (can be combined with --ls)
        if self.options.us:
            surname = surname.upper()

        return surname

    def _to_title_case(self, name: str) -> str:
        """Convert to title case (first letter uppercase, rest lowercase)."""
        if not name:
            return name
        return name.title()

    def _to_title_case_with_particles(self, name: str) -> str:
        """Convert to title case while keeping particles lowercase."""
        if not name:
            return name

        # Common particles to keep lowercase
        particles = {
            "de",
            "du",
            "la",
            "le",
            "des",
            "von",
            "van",
            "der",
            "den",
            "da",
            "di",
            "del",
            "della",
            "delle",
            "dello",
            "degli",
            "dei",
            "delle",
            "della",
            "dello",
            "degli",
            "dei",
        }

        words = name.split()
        result = []

        for word in words:
            if word.lower() in particles:
                result.append(word.lower())
            else:
                result.append(word.title())

        return " ".join(result)

    def _extract_untreated_tags(self, individual) -> list:
        """Extract untreated GEDCOM tags and return them as note strings."""
        untreated_tags = []

        handled_tags = {
            "NAME",
            "SEX",
            "BIRT",
            "BAPM",
            "DEAT",
            "BURI",
            "FAMC",
            "FAMS",
            "MARR",
            "DIV",
            "HUSB",
            "WIFE",
            "CHIL",
            "NOTE",
            "SOUR",
            "OBJE",
        }

        # Check for untreated tags in individual attributes
        for attr_name in dir(individual):
            if attr_name.startswith("_") or attr_name in [
                "xref",
                "names",
                "sex",
                "birth",
                "death",
                "burial",
                "baptism",
                "famc",
                "fams",
                "events",
                "notes",
                "sources",
                "source_citations",
            ]:
                continue

            attr_value = getattr(individual, attr_name)
            if attr_value and attr_name.upper() not in handled_tags:
                # This is likely an untreated tag
                if isinstance(attr_value, str):
                    untreated_tags.append(
                        f"Untreated tag {attr_name.upper()}: {attr_value}"
                    )
                elif isinstance(attr_value, list):
                    for item in attr_value:
                        if isinstance(item, str):
                            untreated_tags.append(
                                f"Untreated tag {attr_name.upper()}: {item}"
                            )
                        else:
                            untreated_tags.append(
                                f"Untreated tag {attr_name.upper()}: {str(item)}"
                            )
                else:
                    untreated_tags.append(
                        f"Untreated tag {attr_name.upper()}: {str(attr_value)}"
                    )

        return untreated_tags
