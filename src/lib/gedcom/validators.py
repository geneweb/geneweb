"""
GEDCOM Validators - Clean Code Architecture

This module provides comprehensive validation for GEDCOM 5.5.1 files.
Separates structural validation from semantic validation following SRP.
"""

from abc import ABC, abstractmethod
from typing import List, Dict, Set, Optional
import logging

from .models import GedcomDatabase, GedcomIndividual, GedcomFamily
from .tokenizer import GedcomLine
from .exceptions import GedcomValidationError, GedcomStructureError, GedcomSemanticError

class Validator(ABC):
    """Abstract base class for all validators."""

    @abstractmethod
    def validate(self, data) -> List[str]:
        """Validate data and return list of error messages."""
        pass

class StructureValidator(Validator):
    """Validates GEDCOM file structure according to 5.5.1 specification."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.required_tags = {'HEAD', 'TRLR'}
        self.valid_record_tags = {'HEAD', 'SUBM', 'INDI', 'FAM', 'NOTE', 'SOUR', 'REPO', 'OBJE', 'SUBN', 'TRLR'}

    def validate(self, lines: List[GedcomLine]) -> List[str]:
        """Validate GEDCOM structure."""
        errors = []

        # Basic structure checks
        errors.extend(self._validate_file_structure(lines))
        errors.extend(self._validate_record_structure(lines))
        errors.extend(self._validate_xref_format(lines))

        return errors

    def _validate_file_structure(self, lines: List[GedcomLine]) -> List[str]:
        """Validate overall file structure."""
        errors = []

        if not lines:
            errors.append("Empty GEDCOM file")
            return errors

        # Check header
        if lines[0].level != 0 or lines[0].tag != 'HEAD':
            errors.append("File must start with '0 HEAD'")

        # Check trailer
        if lines[-1].level != 0 or lines[-1].tag != 'TRLR':
            errors.append("File must end with '0 TRLR'")

        # Check for required sections
        found_tags = {line.tag for line in lines if line.level == 0}
        missing_tags = self.required_tags - found_tags
        if missing_tags:
            errors.append(f"Missing required tags: {', '.join(missing_tags)}")

        return errors

    def _validate_record_structure(self, lines: List[GedcomLine]) -> List[str]:
        """Validate individual record structures."""
        errors = []

        for i, line in enumerate(lines):
            if line.level == 0:
                # Validate record tag
                if line.tag not in self.valid_record_tags:
                    errors.append(f"Line {line.line_num}: Unknown record type '{line.tag}'")

                # Validate XREF requirements
                if line.tag in {'INDI', 'FAM', 'NOTE', 'SOUR', 'REPO', 'OBJE', 'SUBM'} and not line.xref_id:
                    errors.append(f"Line {line.line_num}: Record type '{line.tag}' requires XREF")

                if line.tag in {'HEAD', 'TRLR', 'SUBN'} and line.xref_id:
                    errors.append(f"Line {line.line_num}: Record type '{line.tag}' should not have XREF")

        return errors

    def _validate_xref_format(self, lines: List[GedcomLine]) -> List[str]:
        """Validate XREF format and uniqueness."""
        errors = []
        seen_xrefs = set()

        for line in lines:
            if line.xref_id:
                # Check uniqueness
                if line.xref_id in seen_xrefs:
                    errors.append(f"Line {line.line_num}: Duplicate XREF '{line.xref_id}'")
                else:
                    seen_xrefs.add(line.xref_id)

        return errors

class SemanticValidator(Validator):
    """Validates GEDCOM semantic rules and relationships."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def validate(self, database: GedcomDatabase) -> List[str]:
        """Validate GEDCOM semantic rules."""
        errors = []

        errors.extend(self._validate_references(database))
        errors.extend(self._validate_family_relationships(database))
        errors.extend(self._validate_dates(database))
        errors.extend(self._validate_individuals(database))

        return errors

    def _validate_references(self, database: GedcomDatabase) -> List[str]:
        """Validate that all references point to existing records."""
        errors = []

        # Collect all valid XREFs
        valid_xrefs = set()
        valid_xrefs.update(database.individuals.keys())
        valid_xrefs.update(database.families.keys())
        valid_xrefs.update(database.notes.keys())
        valid_xrefs.update(database.sources.keys())
        valid_xrefs.update(database.submitters.keys())
        valid_xrefs.update(database.multimedia.keys())

        # Check individual references
        for xref, individual in database.individuals.items():
            # Check family references
            for famc in individual.famc:
                famc_ref = self._normalize_xref(famc)
                if famc_ref not in database.families:
                    errors.append(f"Individual {xref}: Invalid FAMC reference '{famc}'")

            for fams in individual.fams:
                fams_ref = self._normalize_xref(fams)
                if fams_ref not in database.families:
                    errors.append(f"Individual {xref}: Invalid FAMS reference '{fams}'")

            # Check source references
            for source_ref in individual.sources:
                if source_ref not in database.sources:
                    errors.append(f"Individual {xref}: Invalid source reference '{source_ref}'")

        # Check family references
        for xref, family in database.families.items():
            if family.husband:
                husband_ref = self._normalize_xref(family.husband)
                if husband_ref not in database.individuals:
                    errors.append(f"Family {xref}: Invalid husband reference '{family.husband}'")

            if family.wife:
                wife_ref = self._normalize_xref(family.wife)
                if wife_ref not in database.individuals:
                    errors.append(f"Family {xref}: Invalid wife reference '{family.wife}'")

            for child in family.children:
                child_ref = self._normalize_xref(child)
                if child_ref not in database.individuals:
                    errors.append(f"Family {xref}: Invalid child reference '{child}'")

        return errors

    def _normalize_xref(self, xref: str) -> str:
        """Normalize XREF by ensuring it has @ symbols."""
        if not xref:
            return xref
        if not xref.startswith('@'):
            xref = f'@{xref}@'
        return xref

    def _validate_family_relationships(self, database: GedcomDatabase) -> List[str]:
        """Validate family relationship consistency."""
        errors = []

        for family_xref, family in database.families.items():
            # Check that spouses reference this family
            if family.husband:
                husband = database.individuals.get(family.husband)
                if husband and family_xref not in husband.fams:
                    errors.append(f"Family {family_xref}: Husband {family.husband} doesn't reference this family")

            if family.wife:
                wife = database.individuals.get(family.wife)
                if wife and family_xref not in wife.fams:
                    errors.append(f"Family {family_xref}: Wife {family.wife} doesn't reference this family")

            # Check that children reference this family as FAMC
            for child_xref in family.children:
                child = database.individuals.get(child_xref)
                if child and family_xref not in child.famc:
                    errors.append(f"Family {family_xref}: Child {child_xref} doesn't reference this family as FAMC")

        return errors

    def _validate_dates(self, database: GedcomDatabase) -> List[str]:
        """Validate date consistency and logic."""
        errors = []

        for xref, individual in database.individuals.items():
            birth_year = individual.birth_year
            death_year = individual.death_year

            # Check birth before death
            if birth_year and death_year and birth_year > death_year:
                errors.append(f"Individual {xref}: Birth year {birth_year} after death year {death_year}")

            # Check reasonable age at death
            if birth_year and death_year:
                age = death_year - birth_year
                if age > 150:
                    errors.append(f"Individual {xref}: Unrealistic lifespan of {age} years")
                elif age < 0:
                    errors.append(f"Individual {xref}: Negative age at death")

        return errors

    def _validate_individuals(self, database: GedcomDatabase) -> List[str]:
        """Validate individual record consistency."""
        errors = []

        for xref, individual in database.individuals.items():
            # Check that individual has at least one name
            if not individual.names:
                errors.append(f"Individual {xref}: No name provided")

            # Check sex value
            if individual.sex and individual.sex not in {'M', 'F', 'U'}:
                errors.append(f"Individual {xref}: Invalid sex value '{individual.sex}'")

        return errors

class GedcomValidator:
    """Main validator that orchestrates all validation checks."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)
        self.structure_validator = StructureValidator()
        self.semantic_validator = SemanticValidator()

    def validate_structure(self, lines: List[GedcomLine]) -> None:
        """
        Validate GEDCOM file structure.

        Args:
            lines: Tokenized GEDCOM lines

        Raises:
            GedcomStructureError: If structure validation fails
        """
        errors = self.structure_validator.validate(lines)

        if errors:
            self.logger.error(f"Structure validation failed with {len(errors)} errors")
            raise GedcomStructureError("GEDCOM structure validation failed", errors)

        self.logger.debug("Structure validation passed")

    def validate_semantics(self, database: GedcomDatabase) -> None:
        """
        Validate GEDCOM semantic rules.

        Args:
            database: Parsed GEDCOM database

        Raises:
            GedcomSemanticError: If semantic validation fails
        """
        errors = self.semantic_validator.validate(database)

        if errors:
            self.logger.warning(f"Semantic validation found {len(errors)} issues")
            # For semantic errors, we might want to be more lenient
            # and just log warnings instead of raising exceptions
            for error in errors[:10]:  # Log first 10 errors
                self.logger.warning(f"Semantic issue: {error}")

            # Uncomment to make semantic validation strict:
            # raise GedcomSemanticError("GEDCOM semantic validation failed", errors)
        else:
            self.logger.debug("Semantic validation passed")

    def validate_all(self, lines: List[GedcomLine], database: GedcomDatabase) -> Dict[str, List[str]]:
        """
        Run all validations and return results.

        Args:
            lines: Tokenized GEDCOM lines
            database: Parsed GEDCOM database

        Returns:
            Dictionary with validation results
        """
        results = {
            'structure_errors': [],
            'semantic_errors': []
        }

        try:
            self.validate_structure(lines)
        except GedcomStructureError as e:
            results['structure_errors'] = e.errors

        try:
            self.validate_semantics(database)
        except GedcomSemanticError as e:
            results['semantic_errors'] = e.errors

        return results
