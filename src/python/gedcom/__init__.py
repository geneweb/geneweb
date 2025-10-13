"""
GEDCOM 5.5.1 Parser - Clean Code Architecture

A complete, production-ready GEDCOM parser following clean code principles:
- Single Responsibility Principle
- Dependency Inversion
- Comprehensive validation
- Extensible architecture
- Full GEDCOM 5.5.1 compliance

Usage:
    from gedcom import create_parser

    parser = create_parser()
    database = parser.parse_file(Path("family.ged"))

    print(f"Found {len(database.individuals)} individuals")
    for individual in database.individuals.values():
        print(f"- {individual.primary_name}")
"""

# Exceptions
from .exceptions import (
    GedcomError,
    GedcomExportError,
    GedcomFileError,
    GedcomParseError,
    GedcomSemanticError,
    GedcomStructureError,
    GedcomTokenizeError,
    GedcomValidationError,
)

# Exporter
from .exporter import GedcomExporter, create_exporter

# Individual exporters for backward compatibility
from .exporters import (
    FamilyExporter,
    HeaderExporter,
    IndividualExporter,
    MultimediaExporter,
    NoteExporter,
    RawStructureExporter,
    RepositoryExporter,
    SourceExporter,
    SubmitterExporter,
)

# Core models
from .models import (
    GedcomAddress,
    GedcomDatabase,
    GedcomDate,
    GedcomEvent,
    GedcomFamily,
    GedcomHeader,
    GedcomIndividual,
    GedcomMultimedia,
    GedcomName,
    GedcomPlace,
    GedcomRepository,
    GedcomStructuredText,
    GedcomSubmitter,
)

# Parser and related components
from .parser import (
    FamilyParser,
    GedcomParser,
    HeaderParser,
    IndividualParser,
    RecordParser,
    create_parser,
)

# Tokenizer
from .tokenizer import GedcomLine, GedcomTokenizer

# Validators
from .validators import GedcomValidator, SemanticValidator, StructureValidator

__version__ = "1.0.0"

__all__ = [
    # Factory functions (recommended entry points)
    "create_parser",
    "create_exporter",
    # Core models
    "GedcomDatabase",
    "GedcomIndividual",
    "GedcomFamily",
    "GedcomHeader",
    "GedcomEvent",
    "GedcomDate",
    "GedcomName",
    "GedcomPlace",
    "GedcomStructuredText",
    "GedcomAddress",
    "GedcomSubmitter",
    "GedcomMultimedia",
    "GedcomRepository",
    # Main classes
    "GedcomParser",
    "GedcomTokenizer",
    "GedcomValidator",
    "GedcomExporter",
    "GedcomLine",
    # Parser components
    "RecordParser",
    "IndividualParser",
    "FamilyParser",
    "HeaderParser",
    # Exporter components
    "HeaderExporter",
    "IndividualExporter",
    "FamilyExporter",
    "RawStructureExporter",
    "MultimediaExporter",
    "RepositoryExporter",
    "SubmitterExporter",
    "NoteExporter",
    "SourceExporter",
    # Validators
    "StructureValidator",
    "SemanticValidator",
    # Exceptions
    "GedcomError",
    "GedcomParseError",
    "GedcomValidationError",
    "GedcomTokenizeError",
    "GedcomExportError",
    "GedcomFileError",
    "GedcomStructureError",
    "GedcomSemanticError",
]
