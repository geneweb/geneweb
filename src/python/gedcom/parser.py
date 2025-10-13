"""
GEDCOM 5.5.1 Parser - Clean Code Architecture

This module provides a complete GEDCOM 5.5.1 parser following clean code principles:
- Single Responsibility Principle
- Dependency Inversion
- Clear separation of concerns
- Comprehensive error handling
"""

import logging
from pathlib import Path
from typing import List, Optional

from .exceptions import GedcomParseError
from .models import GedcomDatabase, GedcomHeader
from .parsers import (
    DefaultFileReader,
    FamilyParser,
    FileReader,
    HeaderParser,
    IndividualParser,
    MultimediaParser,
    NoteParser,
    RecordParser,
    RepositoryParser,
    SourceParser,
    SubmitterParser,
)
from .tokenizer import GedcomLine, GedcomTokenizer
from .validators import GedcomValidator


class GedcomParser:
    """Main GEDCOM parser following clean code principles."""

    def __init__(
        self,
        file_reader: Optional[FileReader] = None,
        validator: Optional[GedcomValidator] = None,
        preserve_bom: bool = True,
        preserve_notes: bool = True,
    ):
        """Initialize parser with dependencies.

        Args:
            file_reader: File reader implementation
            validator: Validator implementation
            preserve_bom: If True, preserve UTF-8 BOM in content (default: True)
            preserve_notes: If True, preserve NOTE records and references (default: True)
        """
        self.file_reader = file_reader or DefaultFileReader()
        self.validator = validator or GedcomValidator()
        self.tokenizer = GedcomTokenizer(
            preserve_bom=preserve_bom, preserve_notes=preserve_notes
        )
        self.logger = logging.getLogger(__name__)
        self.preserve_bom = preserve_bom
        self.preserve_notes = preserve_notes

        self.record_parsers: List[RecordParser] = [
            HeaderParser(),
            IndividualParser(),
            FamilyParser(),
            RepositoryParser(),
            MultimediaParser(),
            SubmitterParser(),
            NoteParser(),
            SourceParser(),
        ]

    def parse_file(self, filepath: Path, encoding: str = "utf-8") -> GedcomDatabase:
        """Parse a GEDCOM file."""
        self.logger.info(f"Parsing GEDCOM file: {filepath}")

        try:
            content = self.file_reader.read_file(filepath, encoding)
            return self.parse_content(content)
        except Exception as e:
            self.logger.error(f"Failed to parse file {filepath}: {e}")
            raise GedcomParseError(f"Failed to parse file {filepath}: {e}") from e

    def parse_content(self, content: str) -> GedcomDatabase:
        """Parse GEDCOM content string."""
        try:
            lines = self.tokenizer.tokenize(content)

            self.validator.validate_structure(lines)
            database = self._parse_records(lines)
            self._ensure_stub_sources(database)
            self._normalize_source_refs(database)

            self.validator.validate_semantics(database)

            self.logger.info(
                f"Successfully parsed {len(database.individuals)} individuals and {len(database.families)} families"
            )
            return database

        except Exception as e:
            self.logger.error(f"Failed to parse content: {e}")
            raise GedcomParseError(f"Failed to parse content: {e}") from e

    def _parse_records(self, lines: List[GedcomLine]) -> GedcomDatabase:
        """Parse all records from tokenized lines."""
        database = GedcomDatabase(header=GedcomHeader())
        current_index = 0

        while current_index < len(lines):
            line = lines[current_index]

            if line.level == 0:
                parser = self._find_parser(line.tag)
                if parser:
                    try:
                        parsed_object, next_index = parser.parse(lines, current_index)
                        self._add_to_database(
                            database, line.tag, line.xref_id, parsed_object
                        )
                        current_index = next_index
                    except Exception as e:
                        self.logger.warning(
                            f"Failed to parse {line.tag} record at line {line.line_num}: {e}"
                        )
                        current_index += 1
                elif line.tag == "TRLR":
                    break
                else:
                    self.logger.debug(f"Skipping unsupported record type: {line.tag}")
                    current_index += 1
            else:
                current_index += 1

        return database

    def _find_parser(self, tag: str) -> Optional[RecordParser]:
        """Find appropriate parser for the given tag."""
        for parser in self.record_parsers:
            if parser.can_parse(tag):
                return parser
        return None

    def _ensure_stub_sources(self, database: GedcomDatabase) -> None:
        """Ensure that all referenced SOUR XREFs exist in database.sources."""
        from .models import GedcomSource

        referenced: set[str] = set()

        def collect_from_event(event) -> None:
            if not event:
                return
            for sx in getattr(event, "sources", []) or []:
                if sx:
                    referenced.add(sx.strip("@"))

        def collect_from_events_container(obj) -> None:
            for attr_name in dir(obj):
                if attr_name.startswith("_"):
                    continue
                try:
                    value = getattr(obj, attr_name)
                except Exception:
                    continue
                if value.__class__.__name__ == "GedcomEvent":
                    collect_from_event(value)
                elif isinstance(value, list) and value:
                    first = value[0]
                    if first.__class__.__name__ == "GedcomEvent":
                        for ev in value:
                            collect_from_event(ev)

        for ind in database.individuals.values():
            for s in getattr(ind, "sources", []):
                if s:
                    clean_xref = s.strip("@")
                    referenced.add(clean_xref)
            for cit in getattr(ind, "source_citations", []):
                if cit and cit.xref:
                    clean_xref = cit.xref.strip("@")
                    referenced.add(clean_xref)
            collect_from_events_container(ind)

        for fam in database.families.values():
            for s in getattr(fam, "sources", []):
                if s:
                    clean_xref = s.strip("@")
                    referenced.add(clean_xref)
            collect_from_events_container(fam)

        for xref in referenced:
            key_with_at = f"@{xref}@"
            key_without_at = xref

            if (
                key_with_at not in database.sources
                and key_without_at not in database.sources
            ):
                database.sources[key_with_at] = GedcomSource(xref=key_with_at)

    def _normalize_source_refs(self, database: GedcomDatabase) -> None:
        """Normalize all source references to the canonical form @XREF@ across the database."""

        def norm(x: str) -> str:
            if not x:
                return x
            core = x.strip("@")
            return f"@{core}@"

        if database.sources:
            to_add = {}
            to_del = []
            for key, src in list(database.sources.items()):
                nk = norm(key)
                if nk != key:
                    to_add[nk] = src
                    src.xref = nk
                    to_del.append(key)
            for k in to_del:
                database.sources.pop(k, None)
            database.sources.update(to_add)

        for ind in database.individuals.values():
            if hasattr(ind, "sources"):
                ind.sources = [norm(s) for s in ind.sources]

            for attr_name in dir(ind):
                if attr_name.startswith("_"):
                    continue
                try:
                    value = getattr(ind, attr_name)
                except Exception:
                    continue
                if value.__class__.__name__ == "GedcomEvent":
                    if getattr(value, "sources", None) is not None:
                        value.sources = [norm(s) for s in value.sources]
                elif isinstance(value, list) and value:
                    first = value[0]
                    if first.__class__.__name__ == "GedcomEvent":
                        for ev in value:
                            if getattr(ev, "sources", None) is not None:
                                ev.sources = [norm(s) for s in ev.sources]

        for fam in database.families.values():
            if hasattr(fam, "sources"):
                fam.sources = [norm(s) for s in fam.sources]
            for attr_name in dir(fam):
                if attr_name.startswith("_"):
                    continue
                try:
                    value = getattr(fam, attr_name)
                except Exception:
                    continue
                if value.__class__.__name__ == "GedcomEvent":
                    if getattr(value, "sources", None) is not None:
                        value.sources = [norm(s) for s in value.sources]
                elif isinstance(value, list) and value:
                    first = value[0]
                    if first.__class__.__name__ == "GedcomEvent":
                        for ev in value:
                            if getattr(ev, "sources", None) is not None:
                                ev.sources = [norm(s) for s in ev.sources]

        for obj_map in (database.multimedia, database.repositories, database.notes):
            for obj in obj_map.values():
                if hasattr(obj, "sources") and isinstance(obj.sources, list):
                    obj.sources = [norm(s) for s in obj.sources]

    def _add_to_database(
        self, database: GedcomDatabase, tag: str, xref_id: Optional[str], parsed_object
    ):
        """Add parsed object to database."""
        if tag == "HEAD":
            database.header = parsed_object
        elif tag == "INDI" and xref_id:
            database.individuals[xref_id] = parsed_object
            database.record_order.append(("INDI", xref_id))
        elif tag == "FAM" and xref_id:
            database.families[xref_id] = parsed_object
            database.record_order.append(("FAM", xref_id))
        elif tag == "REPO" and xref_id:
            database.repositories[xref_id] = parsed_object
            database.record_order.append(("REPO", xref_id))
        elif tag == "OBJE" and xref_id:
            database.multimedia[xref_id] = parsed_object
            database.record_order.append(("OBJE", xref_id))
        elif tag == "SUBM" and xref_id:
            database.submitters[xref_id] = parsed_object
            database.record_order.append(("SUBM", xref_id))
        elif tag == "NOTE" and xref_id:
            database.notes[xref_id] = parsed_object
            database.record_order.append(("NOTE", xref_id))
        elif tag == "SOUR" and xref_id:
            database.sources[xref_id] = parsed_object
            database.record_order.append(("SOUR", xref_id))


def create_parser(
    file_reader: Optional[FileReader] = None,
    validator: Optional[GedcomValidator] = None,
    preserve_bom: bool = True,
    preserve_notes: bool = True,
) -> GedcomParser:
    """Create a GEDCOM parser with optional dependencies.

    Args:
        file_reader: File reader implementation
        validator: Validator implementation
        preserve_bom: If True, preserve UTF-8 BOM in content (default: True)
        preserve_notes: If True, preserve NOTE records and references (default: True)

    Returns:
        Configured GedcomParser instance
    """
    return GedcomParser(
        file_reader=file_reader,
        validator=validator,
        preserve_bom=preserve_bom,
        preserve_notes=preserve_notes,
    )
