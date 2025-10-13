"""
GEDCOM Tokenizer - Clean Code Architecture

This module handles the tokenization of GEDCOM content into structured lines.
Follows single responsibility principle and provides comprehensive error handling.
"""

import logging
import re
from dataclasses import dataclass
from typing import List, Optional

from .exceptions import GedcomTokenizeError


@dataclass(frozen=True)
class GedcomLine:
    """Immutable representation of a GEDCOM line."""

    level: int
    xref_id: Optional[str]
    tag: str
    value: str
    line_num: int

    def __str__(self) -> str:
        """String representation of the line."""
        parts = [str(self.level)]
        if self.xref_id:
            parts.append(self.xref_id)
        parts.append(self.tag)
        if self.value:
            parts.append(self.value)
        return " ".join(parts)

    @property
    def is_record_start(self) -> bool:
        """Check if this line starts a new record."""
        return self.level == 0 and self.xref_id is not None

    @property
    def clean_xref(self) -> Optional[str]:
        """Get XREF without @ symbols."""
        return self.xref_id.strip("@") if self.xref_id else None


class GedcomTokenizer:
    """GEDCOM tokenizer for parsing GEDCOM files into structured tokens."""

    # GEDCOM line pattern: level [xref] tag [value]
    LINE_PATTERN = re.compile(r"^(\d+)\s+(?:(@[^@]+@)\s+)?(\S+)(?:\s+(.*))?$")

    def __init__(self, preserve_bom: bool = False, preserve_notes: bool = True):
        """Initialize the tokenizer.

        Args:
            preserve_bom: If True, preserve UTF-8 BOM in content
            preserve_notes: If True, preserve NOTE records and references
        """
        self.logger = logging.getLogger(__name__)
        self.errors: List[str] = []
        self.preserve_bom = preserve_bom
        self.preserve_notes = preserve_notes

    def tokenize(self, content: str) -> List[GedcomLine]:
        """
        Tokenize GEDCOM content into structured lines.

        Args:
            content: Raw GEDCOM file content

        Returns:
            List of GedcomLine objects

        Raises:
            GedcomTokenizeError: If tokenization fails
        """
        self.errors.clear()

        try:
            # Preprocess content
            content = self._preprocess_content(content)

            # Parse lines
            lines = self._parse_lines(content)

            # Filter lines based on options
            lines = self._filter_lines(lines)

            # Validate structure
            self._validate_basic_structure(lines)

            if self.errors:
                error_msg = (
                    f"Tokenization completed with {len(self.errors)} errors:\n"
                    + "\n".join(self.errors[:5])
                )
                self.logger.warning(error_msg)
                # Don't raise exception for warnings, just log them

            self.logger.info(f"Successfully tokenized {len(lines)} lines")
            return lines

        except Exception as e:
            error_msg = f"Failed to tokenize GEDCOM content: {e}"
            self.logger.error(error_msg)
            raise GedcomTokenizeError(error_msg) from e

    def _preprocess_content(self, content: str) -> str:
        """Preprocess content before tokenization."""
        if not content.strip():
            raise GedcomTokenizeError("Empty GEDCOM file")

        # Handle BOM - always remove it for parsing, but log if preserve_bom is True
        if content.startswith("\ufeff"):
            if self.preserve_bom:
                self.logger.debug(
                    "Found UTF-8 BOM - preserving for export but removing for parsing"
                )
            else:
                self.logger.debug("Removed UTF-8 BOM from content")
            content = content[1:]  # Always remove BOM for parsing

        return content

    def _parse_lines(self, content: str) -> List[GedcomLine]:
        """Parse content into GedcomLine objects."""
        lines = []

        for line_num, raw_line in enumerate(content.splitlines(), 1):
            line = raw_line.rstrip()

            if not line:
                continue  # Skip empty lines

            try:
                gedcom_line = self._parse_single_line(line, line_num)
                if gedcom_line:
                    lines.append(gedcom_line)
            except Exception as e:
                error_msg = f"Line {line_num}: {e}"
                self.errors.append(error_msg)
                continue

        return lines

    def _parse_single_line(self, line: str, line_num: int) -> Optional[GedcomLine]:
        """Parse a single line into a GedcomLine object."""
        match = self.LINE_PATTERN.match(line)

        if not match:
            raise ValueError(f"Invalid line format: '{line}'")

        level = int(match.group(1))
        xref_id = match.group(2)  # Includes @ symbols if present
        tag = match.group(3)
        value = match.group(4) or ""

        return GedcomLine(
            level=level, xref_id=xref_id, tag=tag, value=value, line_num=line_num
        )

    def _filter_lines(self, lines: List[GedcomLine]) -> List[GedcomLine]:
        """Filter lines based on configuration options."""
        # no need to skip notes if preserve_notes is True

        if self.preserve_notes:
            return lines

        # Filter out NOTE records and NOTE references
        filtered_lines = []
        skip_until_level = None

        for line in lines:
            # If we're skipping a NOTE record
            if skip_until_level is not None:
                if line.level <= skip_until_level:
                    skip_until_level = None  # End of NOTE record
                else:
                    continue  # Skip this line (part of NOTE record)

            # Check if this is a NOTE record start
            if line.level == 0 and line.tag == "NOTE":
                skip_until_level = line.level
                continue

            # Check if this is a NOTE reference
            if line.tag == "NOTE" and line.value.startswith("@"):
                continue  # Skip NOTE references

            filtered_lines.append(line)

        if not self.preserve_notes and len(filtered_lines) < len(lines):
            removed_count = len(lines) - len(filtered_lines)
            self.logger.info(f"Filtered out {removed_count} NOTE-related lines")

        return filtered_lines

    def _validate_basic_structure(self, lines: List[GedcomLine]) -> None:
        """Validate basic GEDCOM structure."""
        if not lines:
            raise GedcomTokenizeError("No valid lines found")

        # Check for required HEAD record
        if not any(line.level == 0 and line.tag == "HEAD" for line in lines):
            self.logger.warning("No HEAD record found")

        # Check for TRLR record
        if not any(line.tag == "TRLR" for line in lines):
            self.logger.warning("No TRLR record found")

        # Validate level progression
        self._validate_level_progression(lines)

    def _validate_level_progression(self, lines: List[GedcomLine]) -> None:
        """Validate that level numbers progress correctly."""
        prev_level = -1

        for line in lines:
            if line.level > prev_level + 1:
                error_msg = f"Line {line.line_num}: Level jump from {prev_level} to {line.level}"
                self.errors.append(error_msg)

            prev_level = line.level

    def get_lines_by_tag(self, lines: List[GedcomLine], tag: str) -> List[GedcomLine]:
        """Get all lines with a specific tag."""
        return [line for line in lines if line.tag == tag]

    def get_record_lines(
        self, lines: List[GedcomLine], start_line: GedcomLine
    ) -> List[GedcomLine]:
        """Get all lines belonging to a record starting with start_line."""
        if not start_line.is_record_start:
            return [start_line]

        record_lines = [start_line]
        start_idx = lines.index(start_line)

        for line in lines[start_idx + 1 :]:
            if line.level == 0:
                break
            record_lines.append(line)

        return record_lines


def create_tokenizer(
    preserve_bom: bool = False, preserve_notes: bool = True
) -> GedcomTokenizer:
    """
    Factory function to create a GEDCOM tokenizer.

    Args:
        preserve_bom: If True, preserve UTF-8 BOM in content
        preserve_notes: If True, preserve NOTE records and references

    Returns:
        Configured GedcomTokenizer instance
    """
    return GedcomTokenizer(preserve_bom=preserve_bom, preserve_notes=preserve_notes)
