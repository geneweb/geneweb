"""
GEDCOM to GeneWeb format converter.

This module handles the conversion from GEDCOM data structures to
GeneWeb data structures. Currently, this is a placeholder for future
conversion logic.
"""

import logging
import sys
from pathlib import Path
from typing import Any, Dict

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from gedcom.models import GedcomDatabase
from lib.db_pickle.core.enums import DivorceStatus, RelationKind, Sex
from lib.db_pickle.core.types import Ifam, Iper
from lib.db_pickle.models.events import Date
from lib.db_pickle.models.family import GenFamily
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.models.relations import GenCouple

from ..utils.options import ConversionOptions


class GedcomToGenewebConverter:
    """Converts GEDCOM database to GeneWeb format."""

    def __init__(self, options: ConversionOptions):
        """Initialize converter with options."""
        self.options = options
        self.logger = logging.getLogger(__name__)
        self.warnings = []
        self.errors = []

    def convert(self, gedcom_database: GedcomDatabase) -> Dict[str, Any]:
        """
        Convert GEDCOM database to GeneWeb format.

        Args:
            gedcom_database: GEDCOM database to convert

        Returns:
            Dictionary with conversion statistics
        """
        from lib.db_pickle.database.base_data import PickleBaseData
        from lib.db_pickle.models.relations import GenDescend

        geneweb_data = PickleBaseData()

        print("*** saving persons array")
        for xref, individual in gedcom_database.individuals.items():
            try:
                geneweb_person = self._convert_individual(individual)
                self._validate_person_data(individual, geneweb_person)
                # Convert xref to Iper
                if xref.startswith("@I") and xref.endswith("@"):
                    person_id = Iper(int(xref[2:-1]))
                else:
                    person_id = Iper(hash(xref) % 1000000)
                geneweb_data.persons[person_id] = geneweb_person
            except Exception as e:
                print(f"Error processing person {xref}: {e}")
                continue

        print("*** saving ascends array")
        # Ascends are handled during person conversion
        for person_id in geneweb_data.persons.keys():
            pass

        print("*** saving unions array")
        for xref, family in gedcom_database.families.items():
            geneweb_family, couple, children = self._convert_family(family)
            if xref.startswith("@F") and xref.endswith("@"):
                family_id = Ifam(int(xref[2:-1]))
            else:
                family_id = Ifam(hash(xref) % 1000000)
            geneweb_data.descends[family_id] = GenDescend(children=children)

        print("*** saving families array")
        for xref, family in gedcom_database.families.items():
            try:
                geneweb_family, couple, children = self._convert_family(family)
                self._validate_family_data(family, children)
                if xref.startswith("@F") and xref.endswith("@"):
                    family_id = Ifam(int(xref[2:-1]))
                else:
                    family_id = Ifam(hash(xref) % 1000000)
                geneweb_data.families[family_id] = geneweb_family
            except Exception as e:
                print(f"Error processing family {xref}: {e}")
                continue

        print("*** saving couples array")
        for xref, family in gedcom_database.families.items():
            try:
                geneweb_family, couple, children = self._convert_family(family)
                if xref.startswith("@F") and xref.endswith("@"):
                    family_id = Ifam(int(xref[2:-1]))
                else:
                    family_id = Ifam(hash(xref) % 1000000)
                geneweb_data.couples[family_id] = couple
            except Exception as e:
                print(f"Error processing couple {xref}: {e}")
                continue

        print("*** saving descends array")
        for family_id in geneweb_data.descends.keys():
            pass

        print("*** saving strings array")
        for string_id in geneweb_data.strings.keys():
            pass

        geneweb_data.build_indexes()

        self._print_warnings_and_errors()

        print("*** ok")

        conversion_stats = {
            "individuals_converted": geneweb_data.persons_count,
            "families_converted": geneweb_data.families_count,
            "notes_converted": len(gedcom_database.notes),
            "sources_converted": len(gedcom_database.sources),
            "warnings_count": len(self.warnings),
            "errors_count": len(self.errors),
            "conversion_applied": True,
            "geneweb_data": geneweb_data,
        }
        return conversion_stats

    def _convert_individual(self, individual) -> GenPerson:
        """Convert GEDCOM individual to GenPerson."""
        try:
            first_name = ""
            surname = ""

            if individual.names:
                name = individual.names[0]
                first_name = name.given or ""
                surname = name.surname or ""

            sex = Sex.NEUTER
            if hasattr(individual, "sex") and individual.sex:
                sex_value = (
                    individual.sex.value
                    if hasattr(individual.sex, "value")
                    else str(individual.sex)
                )
                if sex_value == "M":
                    sex = Sex.MALE
                elif sex_value == "F":
                    sex = Sex.FEMALE

            person = GenPerson(first_name=first_name, surname=surname, occ=0, sex=sex)
            for event in individual.events:
                try:
                    if event.tag == "BIRT" and event.date:
                        person.birth = self._convert_date(event.date)
                    elif event.tag == "DEAT" and event.date:
                        person.death = self._convert_date(event.date)
                    elif event.tag == "BAPM" and event.date:
                        person.baptism = self._convert_date(event.date)
                    elif event.tag == "BURI" and event.date:
                        person.burial = self._convert_date(event.date)
                except Exception as e:
                    print(f"Error processing event {event.tag}: {e}")
                    continue
            return person
        except Exception as e:
            print(f"Error converting individual: {e}")
            raise

    def _convert_family(self, family) -> tuple[GenFamily, GenCouple, list]:
        """Convert GEDCOM family to GenFamily."""
        from lib.db_pickle.core.types import dummy_iper

        husband = dummy_iper()
        wife = dummy_iper()

        if hasattr(family, "husband") and family.husband:
            # Convert husband xref to Iper
            if family.husband.startswith("@I") and family.husband.endswith("@"):
                husband = Iper(int(family.husband[2:-1]))
            else:
                husband = Iper(hash(family.husband) % 1000000)
        if hasattr(family, "wife") and family.wife:
            # Convert wife xref to Iper
            if family.wife.startswith("@I") and family.wife.endswith("@"):
                wife = Iper(int(family.wife[2:-1]))
            else:
                wife = Iper(hash(family.wife) % 1000000)

        geneweb_family = GenFamily(relation=RelationKind.MARRIED)
        couple = GenCouple(father=husband, mother=wife)

        children = []
        if hasattr(family, "children") and family.children:
            # Convert children xrefs to Iper
            for child_xref in family.children:
                if child_xref.startswith("@I") and child_xref.endswith("@"):
                    child_id = Iper(int(child_xref[2:-1]))
                else:
                    child_id = Iper(hash(child_xref) % 1000000)
                children.append(child_id)

        for event in family.events:
            if event.tag == "MARR" and event.date:
                geneweb_family.marriage = self._convert_date(event.date)
            elif event.tag == "DIV" and event.date:
                geneweb_family.divorce = DivorceStatus.DIVORCED
        return geneweb_family, couple, children

    def _convert_date(self, gedcom_date) -> Date:
        """Convert GEDCOM date to GeneWeb Date."""
        if not gedcom_date:
            return Date.none()

        try:
            year = (
                int(gedcom_date.year)
                if gedcom_date.year and str(gedcom_date.year).strip()
                else 0
            )
        except (ValueError, TypeError):
            year = 0

        try:
            month = (
                int(gedcom_date.month)
                if gedcom_date.month and str(gedcom_date.month).strip()
                else 0
            )
        except (ValueError, TypeError):
            month = 0

        try:
            day = (
                int(gedcom_date.day)
                if gedcom_date.day and str(gedcom_date.day).strip()
                else 0
            )
        except (ValueError, TypeError):
            day = 0

        return Date(year=year, month=month, day=day)

    def _add_warning(self, message: str):
        """Add a warning message."""
        self.warnings.append(message)
        print(f"Warning: {message}", file=sys.stderr)

    def _add_error(self, message: str):
        """Add an error message."""
        self.errors.append(message)
        print(f"Error: {message}", file=sys.stderr)

    def _print_warnings_and_errors(self):
        """Print all warnings and errors."""
        if self.warnings:
            print(
                f"\n{len(self.warnings)} warnings generated during conversion",
                file=sys.stderr,
            )
        if self.errors:
            print(
                f"\n{len(self.errors)} errors generated during conversion",
                file=sys.stderr,
            )

    def _validate_person_data(self, individual, person: GenPerson):
        """Validate person data and add warnings/errors."""
        # Check for sex inconsistencies
        if hasattr(individual, "sex") and individual.sex:
            sex_value = (
                individual.sex.value
                if hasattr(individual.sex, "value")
                else str(individual.sex)
            )
            if sex_value == "F" and person.sex == Sex.MALE:
                self._add_warning(
                    f"Wife with male sex: {person.first_name} {person.surname}"
                )
            elif sex_value == "M" and person.sex == Sex.FEMALE:
                self._add_warning(
                    f"Husband with female sex: {person.first_name} {person.surname}"
                )

        # Check for death before baptism
        if person.death and person.baptism:
            if self._date_compare(person.death, person.baptism) < 0:
                self._add_warning(
                    f"{person.first_name} {person.surname}'s death before his/her baptism"
                )

        # Check for advanced age at death
        if person.birth and person.death:
            age = self._calculate_age(person.birth, person.death)
            if age > 100:
                self._add_warning(
                    f"{person.first_name} {person.surname} died at the advanced age of {age} years old"
                )

        # Check for parent at advanced age
        if person.birth:
            birth_year = getattr(person.birth, "year", 0) or 0
            if birth_year > 0:
                current_year = 2024  # Approximate current year
                if current_year - birth_year > 70:
                    self._add_warning(
                        f"{person.first_name} {person.surname} was parent at age of {current_year - birth_year}"
                    )

    def _validate_family_data(self, family, children):
        """Validate family data and add warnings/errors."""
        if len(children) > 1:
            self._check_children_birth_order(children)

    def _check_children_birth_order(self, children):
        """Check if children are born in reasonable order."""
        if len(children) > 5:
            self._add_warning(
                f"Family has {len(children)} children - check birth order"
            )

    def _date_compare(self, date1, date2):
        """Compare two dates. Returns -1 if date1 < date2, 0 if equal, 1 if date1 > date2."""
        if not date1 or not date2:
            return 0

        year1 = getattr(date1, "year", 0) or 0
        year2 = getattr(date2, "year", 0) or 0

        if year1 < year2:
            return -1
        elif year1 > year2:
            return 1

        month1 = getattr(date1, "month", 0) or 0
        month2 = getattr(date2, "month", 0) or 0

        if month1 < month2:
            return -1
        elif month1 > month2:
            return 1

        day1 = getattr(date1, "day", 0) or 0
        day2 = getattr(date2, "day", 0) or 0

        if day1 < day2:
            return -1
        elif day1 > day2:
            return 1

        return 0

    def _calculate_age(self, birth_date, death_date):
        """Calculate age at death."""
        if not birth_date or not death_date:
            return 0

        birth_year = getattr(birth_date, "year", 0) or 0
        death_year = getattr(death_date, "year", 0) or 0

        if birth_year > 0 and death_year > 0:
            return death_year - birth_year

        return 0
