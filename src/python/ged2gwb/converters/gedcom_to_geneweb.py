"""
GEDCOM to GeneWeb format converter.

This module handles the conversion from GEDCOM data structures to
GeneWeb data structures.
"""

import logging
import sys
from pathlib import Path
from typing import Any, Dict

sys.path.insert(0, str(Path(__file__).parent.parent.parent))

from gedcom.models import GedcomDatabase
from lib.db.core.types import Ifam, Iper, dummy_iper
from lib.db.models.relations import GenCouple

from ..utils.options import ConversionOptions
from .validation import GedcomValidator
from .conversion import GedcomConverter
from .relationship_checker import RelationshipChecker


class GedcomToGenewebConverter:
    """Converts GEDCOM database to GeneWeb format."""

    def __init__(self, options: ConversionOptions):
        """Initialize converter with options."""
        self.options = options
        self.logger = logging.getLogger(__name__)

        if not self.logger.handlers:
            self.logger.setLevel(logging.WARNING)

        # Initialize helper classes
        self.validator = GedcomValidator(self.logger, self.options)
        self.converter = GedcomConverter(self.logger, self.options)
        self.relationship_checker = RelationshipChecker(self.logger, self.options)

        # Keep backward compatibility
        self.warnings = self.validator.warnings
        self.errors = self.validator.errors

    def convert(self, gedcom_database: GedcomDatabase) -> Dict[str, Any]:
        """
        Convert GEDCOM database to GeneWeb format.

        Args:
            gedcom_database: GEDCOM database to convert

        Returns:
            Dictionary with conversion statistics
        """
        from lib.db.database.base_data import BaseData
        from lib.db.models.relations import GenDescend, GenAscend

        # Display parsing steps like OCaml
        self.logger.info("*** pass 1 (note)")
        self._process_notes(gedcom_database)

        self.logger.info("*** pass 2 (indi)")
        self._process_individuals(gedcom_database)

        self.logger.info("*** pass 3 (fam)")
        self._process_families(gedcom_database)

        self.logger.info("*** Trailer ok")

        geneweb_data = BaseData()

        # Convert persons
        self.logger.info("*** saving persons array")
        for xref, individual in gedcom_database.individuals.items():
            try:
                geneweb_person = self.converter.convert_individual(individual)
                self.validator.validate_person_data(individual, geneweb_person)

                # Convert xref to Iper
                if xref.startswith("@I") and xref.endswith("@"):
                    person_id = Iper(int(xref[2:-1]))
                else:
                    person_id = Iper(hash(xref) % 1000000)
                geneweb_data.persons[person_id] = geneweb_person
            except Exception as e:
                self.logger.error(f"Error processing person {xref}: {e}")
                continue

        self.logger.info("*** saving strings array")

        self.logger.info("*** saving descends array")
        family_children: dict[Ifam, list[Iper]] = {}
        for xref, individual in gedcom_database.individuals.items():
            for famc_ref in individual.famc:
                if famc_ref.startswith("@F") and famc_ref.endswith("@"):
                    family_id = Ifam(int(famc_ref[2:-1]))
                else:
                    family_id = Ifam(hash(famc_ref) % 1000000)

                if individual.xref.startswith("@I") and individual.xref.endswith("@"):
                    person_id = Iper(int(individual.xref[2:-1]))
                else:
                    person_id = Iper(hash(individual.xref) % 1000000)

                if family_id not in family_children:
                    family_children[family_id] = []
                family_children[family_id].append(person_id)

        for family_id, children in family_children.items():
            geneweb_data.descends[family_id] = GenDescend(children=children)
            if family_id not in geneweb_data.couples:
                father_id = dummy_iper()
                mother_id = dummy_iper()

                for xref, family in gedcom_database.families.items():
                    if xref.startswith("@F") and xref.endswith("@"):
                        gedcom_family_id = Ifam(int(xref[2:-1]))
                    else:
                        gedcom_family_id = Ifam(hash(xref) % 1000000)

                    if gedcom_family_id == family_id:
                        if family.husband:
                            if family.husband.startswith(
                                "@I"
                            ) and family.husband.endswith("@"):
                                father_id = Iper(int(family.husband[2:-1]))
                            else:
                                father_id = Iper(hash(family.husband) % 1000000)
                        if family.wife:
                            if family.wife.startswith("@I") and family.wife.endswith(
                                "@"
                            ):
                                mother_id = Iper(int(family.wife[2:-1]))
                            else:
                                mother_id = Iper(hash(family.wife) % 1000000)
                        break

                geneweb_data.couples[family_id] = GenCouple(
                    father=father_id, mother=mother_id
                )

        # Create ascends (parent relationships)
        self.logger.info("*** saving ascends array")
        for person_id, person in geneweb_data.persons.items():
            parents = None
            parent_families = []
            for family_id, descend in geneweb_data.descends.items():
                if person_id in descend.children:
                    parent_families.append(family_id)

            if parent_families:
                parents = parent_families[0]
                if len(parent_families) > 1:
                    person.additional_parents = parent_families[1:]

            geneweb_data.ascends[person_id] = GenAscend(parents=parents)

        # Process unions
        self.logger.info("*** saving unions array")
        for family_id in geneweb_data.descends.keys():
            pass

        # Convert families
        self.logger.info("*** saving families array")
        for xref, family in gedcom_database.families.items():
            try:
                geneweb_family, couple, children = self.converter.convert_family(family)
                self.validator.validate_family_data(family, children)

                if xref.startswith("@F") and xref.endswith("@"):
                    family_id = Ifam(int(xref[2:-1]))
                else:
                    family_id = Ifam(hash(xref) % 1000000)
                geneweb_data.families[family_id] = geneweb_family
            except Exception as e:
                self.logger.error(f"Error processing family {xref}: {e}")
                continue

        # Convert couples
        self.logger.info("*** saving couples array")
        for xref, family in gedcom_database.families.items():
            try:
                geneweb_family, couple, children = self.converter.convert_family(family)
                if xref.startswith("@F") and xref.endswith("@"):
                    family_id = Ifam(int(xref[2:-1]))
                else:
                    family_id = Ifam(hash(xref) % 1000000)
                geneweb_data.couples[family_id] = couple
            except Exception as e:
                self.logger.error(f"Error processing couple {xref}: {e}")
                continue

        # Descends already created
        self.logger.info("*** saving descends array")
        for family_id in geneweb_data.descends.keys():
            pass

        # Strings already created
        self.logger.info("*** saving strings array")
        for string_id in geneweb_data.strings.keys():
            pass

        # Build indexes
        geneweb_data.build_indexes()

        # Check parent-child relationships
        self.relationship_checker.check_parents_children(geneweb_data)

        # Print warnings and errors
        self.validator.print_warnings_and_errors()

        self.logger.info("*** ok")

        conversion_stats = {
            "individuals_converted": geneweb_data.persons_count,
            "families_converted": geneweb_data.families_count,
            "notes_converted": len(gedcom_database.notes),
            "sources_converted": len(gedcom_database.sources),
            "warnings_count": len(self.validator.warnings),
            "errors_count": len(self.validator.errors),
            "conversion_applied": True,
            "geneweb_data": geneweb_data,
        }
        return conversion_stats

    def _process_notes(self, gedcom_database: GedcomDatabase) -> None:
        """Process notes (pass 1) - like OCaml pass1."""
        self.validator.check_undefined_sources(gedcom_database)

    def _process_individuals(self, gedcom_database: GedcomDatabase) -> None:
        """Process individuals (pass 2) - like OCaml pass2."""
        self.validator.check_undefined_individuals(gedcom_database)

    def _process_families(self, gedcom_database: GedcomDatabase) -> None:
        """Process families (pass 3) - like OCaml pass3."""
        self.validator.check_undefined_families(gedcom_database)
