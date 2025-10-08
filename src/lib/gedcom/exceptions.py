"""
GEDCOM Exception Classes - Clean Code Architecture

This module defines all custom exceptions for the GEDCOM parser.
Follows clean code principles with clear error hierarchies and meaningful messages.
"""

from typing import Optional, List

class GedcomError(Exception):
    """Base exception for all GEDCOM-related errors."""

    def __init__(self, message: str, line_num: Optional[int] = None):
        self.message = message
        self.line_num = line_num
        super().__init__(self._format_message())

    def _format_message(self) -> str:
        """Format error message with optional line number."""
        if self.line_num:
            return f"Line {self.line_num}: {self.message}"
        return self.message

class GedcomTokenizeError(GedcomError):
    """Exception raised during GEDCOM tokenization."""
    pass

class GedcomParseError(GedcomError):
    """Exception raised during GEDCOM parsing."""

    def __init__(self, message: str, line_num: Optional[int] = None, record_type: Optional[str] = None):
        self.record_type = record_type
        super().__init__(message, line_num)

    def _format_message(self) -> str:
        """Format error message with optional record type and line number."""
        parts = []
        if self.line_num:
            parts.append(f"Line {self.line_num}")
        if self.record_type:
            parts.append(f"Record {self.record_type}")

        if parts:
            return f"{', '.join(parts)}: {self.message}"
        return self.message

class GedcomValidationError(GedcomError):
    """Exception raised during GEDCOM validation."""

    def __init__(self, message: str, errors: Optional[List[str]] = None):
        self.errors = errors or []
        super().__init__(message)

    def _format_message(self) -> str:
        """Format error message with validation errors."""
        if self.errors:
            error_list = '\n'.join(f"  - {error}" for error in self.errors[:10])
            if len(self.errors) > 10:
                error_list += f"\n  ... and {len(self.errors) - 10} more errors"
            return f"{self.message}\nValidation errors:\n{error_list}"
        return self.message

class GedcomExportError(GedcomError):
    """Exception raised during GEDCOM export."""
    pass

class GedcomFileError(GedcomError):
    """Exception raised for file-related operations."""

    def __init__(self, message: str, filepath: Optional[str] = None):
        self.filepath = filepath
        super().__init__(message)

    def _format_message(self) -> str:
        """Format error message with optional filepath."""
        if self.filepath:
            return f"File '{self.filepath}': {self.message}"
        return self.message

class GedcomStructureError(GedcomValidationError):
    """Exception raised for GEDCOM structure violations."""
    pass

class GedcomSemanticError(GedcomValidationError):
    """Exception raised for GEDCOM semantic violations."""
    pass
