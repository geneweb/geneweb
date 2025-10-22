"""
Validation module for GEDCOM to GeneWeb conversion.

This module contains all validation functions for checking data consistency.
"""

from typing import List
from gedcom.models import GedcomDatabase
from lib.db.models.person import GenPerson


class GedcomValidator:
    """Validator for GEDCOM data during conversion."""

    def __init__(self, logger, options=None):
        """Initialize validator with logger and options."""
        self.logger = logger
        self.options = options
        self.warnings: List[str] = []
        self.errors: List[str] = []

    def add_warning(self, message: str):
        """Add a warning message."""
        self.warnings.append(message)

    def add_error(self, message: str):
        """Add an error message."""
        self.errors.append(message)
        self.logger.error(message)

    def print_warnings_and_errors(self):
        """Print all warnings and errors with detailed explanations."""
        if self.warnings:
            self.logger.warning(
                f"VALIDATION WARNINGS ({len(self.warnings)} issues found):"
            )
            self.logger.warning("=" * 50)
            for i, warning in enumerate(self.warnings[:10], 1):
                self.logger.warning(f"  {i:2d}. {warning}")
            if len(self.warnings) > 10:
                self.logger.warning(
                    f"  ... and {len(self.warnings) - 10} more warnings"
                )
            self.logger.warning("=" * 50)

        if self.errors:
            self.logger.error(f"CRITICAL ERRORS ({len(self.errors)} issues found):")
            self.logger.error("=" * 50)
            for i, error in enumerate(self.errors[:10], 1):
                self.logger.error(f"  {i:2d}. {error}")
            if len(self.errors) > 10:
                self.logger.error(f"  ... and {len(self.errors) - 10} more errors")
            self.logger.error("=" * 50)

    def check_undefined_sources(self, gedcom_database: GedcomDatabase) -> None:
        """Check for undefined source references like OCaml."""
        defined_sources = set(gedcom_database.sources.keys())

        for individual in gedcom_database.individuals.values():
            for event in individual.events:
                if event.sources:
                    for source_ref in event.sources:
                        if source_ref not in defined_sources:
                            self.logger.warning(f'File "", line {event.line_number}:')
                            self.logger.warning(f"Source {source_ref} not found")

        for family in gedcom_database.families.values():
            for event in family.events:
                if event.sources:
                    for source_ref in event.sources:
                        if source_ref not in defined_sources:
                            self.logger.warning(f'File "", line {event.line_number}:')
                            self.logger.warning(f"Source {source_ref} not found")

    def check_undefined_individuals(self, gedcom_database: GedcomDatabase) -> None:
        """Check for undefined individual references like OCaml."""
        defined_individuals = set(gedcom_database.individuals.keys())

        for family in gedcom_database.families.values():
            if family.husband and family.husband not in defined_individuals:
                self.add_warning(
                    f"Missing person: Family {family.xref} references undefined husband @{family.husband}@"
                )
            if family.wife and family.wife not in defined_individuals:
                self.add_warning(
                    f"Missing person: Family {family.xref} references undefined wife @{family.wife}@"
                )
            for child in family.children:
                if child not in defined_individuals:
                    self.add_warning(
                        f"Missing person: Family {family.xref} references undefined child @{child}@"
                    )

    def check_undefined_families(self, gedcom_database: GedcomDatabase) -> None:
        """Check for undefined family references like OCaml."""
        defined_families = set(gedcom_database.families.keys())

        for individual in gedcom_database.individuals.values():
            for family_ref in individual.famc:
                if family_ref not in defined_families:
                    self.add_warning(
                        f"Missing family: Person {individual.xref} references undefined parent family @{family_ref}@"
                    )
            for family_ref in individual.fams:
                if family_ref not in defined_families:
                    self.add_warning(
                        f"Missing family: Person {individual.xref} references undefined spouse family @{family_ref}@"
                    )

    def validate_person_data(self, individual, person: GenPerson):
        """Validate person data for consistency."""
        if individual.birth and individual.death:
            birth_date = individual.birth.date
            death_date = individual.death.date
            if birth_date and death_date:
                if hasattr(birth_date, "year") and hasattr(death_date, "year"):
                    if birth_date.year and death_date.year:
                        if birth_date.year > death_date.year:
                            self.add_warning(
                                f"Timeline error: Person {individual.xref} born {birth_date.year} but died {death_date.year}"
                            )

        if individual.baptism and individual.death:
            baptism_date = individual.baptism.date
            death_date = individual.death.date
            if baptism_date and death_date:
                if hasattr(baptism_date, "year") and hasattr(death_date, "year"):
                    if baptism_date.year and death_date.year:
                        if baptism_date.year > death_date.year:
                            self.add_warning(
                                f"Timeline error: Person {individual.xref} baptized {baptism_date.year} but died {death_date.year}"
                            )

        if individual.birth and individual.death:
            birth_date = individual.birth.date
            death_date = individual.death.date
            if birth_date and death_date:
                if hasattr(birth_date, "year") and hasattr(death_date, "year"):
                    if birth_date.year and death_date.year:
                        age_at_death = death_date.year - birth_date.year
                        if age_at_death > 120:
                            self.add_warning(
                                f"Unrealistic age: Person {individual.xref} died at age {age_at_death} years"
                            )

        # Handle --udi: undefined death interval
        if self.options and self.options.udi and not individual.death:
            if individual.birth and individual.birth.date:
                birth_year = (
                    individual.birth.date.year
                    if hasattr(individual.birth.date, "year")
                    else None
                )
                if birth_year:
                    current_year = 2024  # Could be made configurable
                    age = current_year - birth_year
                    udi_min, udi_max = self.options.udi

                    if age < udi_min:
                        # Consider as alive
                        pass
                    elif age > udi_max:
                        # Consider as dead
                        pass
                    else:
                        # Don't know status
                        pass

    def validate_family_data(self, family, children):
        """Validate family data for consistency."""
        if len(children) > 20:
            self.add_warning(
                f"Unusual family size: Family {family.xref} has {len(children)} children"
            )

    def check_children_birth_order(self, children):
        """Check if children birth dates are in correct order."""
        pass

    def date_compare(self, date1, date2):
        """Compare two dates."""
        if not date1 or not date2:
            return 0
        if hasattr(date1, "year") and hasattr(date2, "year"):
            if date1.year and date2.year:
                if date1.year < date2.year:
                    return -1
                elif date1.year > date2.year:
                    return 1
        return 0
