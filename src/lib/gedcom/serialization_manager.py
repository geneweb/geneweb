"""
GEDCOM Serialization Manager - Pickle & MessagePack Support

This module provides high-performance serialization and deserialization
of GEDCOM data using both pickle and msgpack formats.
"""

import pickle
import msgpack
import json
import gzip
from pathlib import Path
from typing import Union, Dict, Any, Optional
from dataclasses import asdict
import time
import logging

from .models import GedcomDatabase
from .parser import create_parser
from .exporter import create_exporter

class SerializationManager:
    """Manages serialization and deserialization of GEDCOM data."""

    def __init__(self, compression: bool = True):
        """Initialize the serialization manager.

        Args:
            compression: If True, use gzip compression for better storage efficiency
        """
        self.compression = compression
        self.logger = logging.getLogger(__name__)

    def save_pickle(self, database: GedcomDatabase, filepath: Path) -> Dict[str, Any]:
        """Save GEDCOM database to pickle format.

        Args:
            database: GedcomDatabase instance to serialize
            filepath: Path where to save the pickle file

        Returns:
            Dictionary with serialization statistics
        """
        start_time = time.time()

        try:
            # Prepare data for serialization
            serializable_data = self._prepare_for_serialization(database)

            # Serialize with optional compression
            if self.compression:
                filepath = filepath.with_suffix('.pkl.gz')
                with gzip.open(filepath, 'wb') as f:
                    pickle.dump(serializable_data, f, protocol=pickle.HIGHEST_PROTOCOL)
            else:
                filepath = filepath.with_suffix('.pkl')
                with open(filepath, 'wb') as f:
                    pickle.dump(serializable_data, f, protocol=pickle.HIGHEST_PROTOCOL)

            # Calculate statistics
            file_size = filepath.stat().st_size
            serialization_time = time.time() - start_time

            stats = {
                'format': 'pickle',
                'compressed': self.compression,
                'file_path': str(filepath),
                'file_size': file_size,
                'serialization_time': serialization_time,
                'individuals_count': len(database.individuals),
                'families_count': len(database.families),
                'throughput_mb_s': file_size / serialization_time / 1024 / 1024
            }

            self.logger.info(f"Pickle saved: {file_size:,} bytes in {serialization_time:.3f}s")
            return stats

        except Exception as e:
            self.logger.error(f"Failed to save pickle: {e}")
            raise

    def save_msgpack(self, database: GedcomDatabase, filepath: Path) -> Dict[str, Any]:
        """Save GEDCOM database to MessagePack format.

        Args:
            database: GedcomDatabase instance to serialize
            filepath: Path where to save the msgpack file

        Returns:
            Dictionary with serialization statistics
        """
        start_time = time.time()

        try:
            # Prepare data for serialization
            serializable_data = self._prepare_for_serialization(database)

            # Convert to msgpack-compatible format
            msgpack_data = self._convert_to_msgpack_format(serializable_data)

            # Serialize with optional compression
            if self.compression:
                filepath = filepath.with_suffix('.msgpack.gz')
                with gzip.open(filepath, 'wb') as f:
                    msgpack.pack(msgpack_data, f, use_bin_type=True)
            else:
                filepath = filepath.with_suffix('.msgpack')
                with open(filepath, 'wb') as f:
                    msgpack.pack(msgpack_data, f, use_bin_type=True)

            # Calculate statistics
            file_size = filepath.stat().st_size
            serialization_time = time.time() - start_time

            stats = {
                'format': 'msgpack',
                'compressed': self.compression,
                'file_path': str(filepath),
                'file_size': file_size,
                'serialization_time': serialization_time,
                'individuals_count': len(database.individuals),
                'families_count': len(database.families),
                'throughput_mb_s': file_size / serialization_time / 1024 / 1024
            }

            self.logger.info(f"MessagePack saved: {file_size:,} bytes in {serialization_time:.3f}s")
            return stats

        except Exception as e:
            self.logger.error(f"Failed to save msgpack: {e}")
            raise

    def load_pickle(self, filepath: Path) -> GedcomDatabase:
        """Load GEDCOM database from pickle format.

        Args:
            filepath: Path to the pickle file

        Returns:
            GedcomDatabase instance
        """
        start_time = time.time()

        try:
            # Determine if file is compressed
            is_compressed = filepath.suffix == '.gz' or '.pkl.gz' in str(filepath)

            # Load data
            if is_compressed:
                with gzip.open(filepath, 'rb') as f:
                    serializable_data = pickle.load(f)
            else:
                with open(filepath, 'rb') as f:
                    serializable_data = pickle.load(f)

            # Reconstruct database
            database = self._reconstruct_from_serialization(serializable_data)

            load_time = time.time() - start_time
            self.logger.info(f"Pickle loaded: {len(database.individuals)} individuals in {load_time:.3f}s")

            return database

        except Exception as e:
            self.logger.error(f"Failed to load pickle: {e}")
            raise

    def load_msgpack(self, filepath: Path) -> GedcomDatabase:
        """Load GEDCOM database from MessagePack format.

        Args:
            filepath: Path to the msgpack file

        Returns:
            GedcomDatabase instance
        """
        start_time = time.time()

        try:
            # Determine if file is compressed
            is_compressed = filepath.suffix == '.gz' or '.msgpack.gz' in str(filepath)

            # Load data
            if is_compressed:
                with gzip.open(filepath, 'rb') as f:
                    msgpack_data = msgpack.unpack(f, raw=False)
            else:
                with open(filepath, 'rb') as f:
                    msgpack_data = msgpack.unpack(f, raw=False)

            # Convert from msgpack format and reconstruct database
            serializable_data = self._convert_from_msgpack_format(msgpack_data)
            database = self._reconstruct_from_serialization(serializable_data)

            load_time = time.time() - start_time
            self.logger.info(f"MessagePack loaded: {len(database.individuals)} individuals in {load_time:.3f}s")

            return database

        except Exception as e:
            self.logger.error(f"Failed to load msgpack: {e}")
            raise

    def export_to_gedcom(self, database: GedcomDatabase, output_path: Path) -> Dict[str, Any]:
        """Export database back to GEDCOM format.

        Args:
            database: GedcomDatabase instance to export
            output_path: Path where to save the GEDCOM file

        Returns:
            Dictionary with export statistics
        """
        start_time = time.time()

        try:
            exporter = create_exporter()
            exporter.export_file(output_path, database)

            # Calculate statistics
            file_size = output_path.stat().st_size
            export_time = time.time() - start_time

            stats = {
                'format': 'gedcom',
                'file_path': str(output_path),
                'file_size': file_size,
                'export_time': export_time,
                'individuals_count': len(database.individuals),
                'families_count': len(database.families),
                'throughput_individuals_s': len(database.individuals) / export_time
            }

            self.logger.info(f"GEDCOM exported: {file_size:,} bytes in {export_time:.3f}s")
            return stats

        except Exception as e:
            self.logger.error(f"Failed to export GEDCOM: {e}")
            raise

    def _prepare_for_serialization(self, database: GedcomDatabase) -> Dict[str, Any]:
        """Prepare database for serialization by converting to dict format."""
        return {
            'header': asdict(database.header) if database.header else None,
            'individuals': {xref: asdict(individual) for xref, individual in database.individuals.items()},
            'families': {xref: asdict(family) for xref, family in database.families.items()},
            'notes': {xref: asdict(note) for xref, note in database.notes.items()},
            'sources': {xref: asdict(source) for xref, source in database.sources.items()},
            'submitters': {xref: asdict(submitter) for xref, submitter in database.submitters.items()},
            'record_order': database.record_order,
            'metadata': {
                'serialization_version': '1.0',
                'timestamp': time.time(),
                'total_individuals': len(database.individuals),
                'total_families': len(database.families)
            }
        }

    def _convert_to_msgpack_format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Convert data to MessagePack-compatible format."""
        # MessagePack doesn't handle some Python types well, so we need to convert them
        def convert_value(value):
            if isinstance(value, dict):
                return {k: convert_value(v) for k, v in value.items()}
            elif isinstance(value, list):
                return [convert_value(item) for item in value]
            elif isinstance(value, tuple):
                return list(value)  # Convert tuples to lists
            elif hasattr(value, '__dict__'):
                return asdict(value) if hasattr(value, '__dataclass_fields__') else str(value)
            else:
                return value

        return convert_value(data)

    def _convert_from_msgpack_format(self, data: Dict[str, Any]) -> Dict[str, Any]:
        """Convert data from MessagePack format back to Python format."""
        # Convert lists back to tuples where needed (like in record_order)
        if 'record_order' in data and isinstance(data['record_order'], list):
            data['record_order'] = [tuple(item) if isinstance(item, list) else item
                                   for item in data['record_order']]
        return data

    def _reconstruct_from_serialization(self, data: Dict[str, Any]) -> GedcomDatabase:
        """Reconstruct GedcomDatabase from serialized data."""
        from .models import (GedcomDatabase, GedcomHeader, GedcomIndividual, GedcomFamily,
                           GedcomSubmitter, GedcomStructuredText, GedcomName, GedcomEvent,
                           GedcomDate, GedcomPlace, GedcomAddress)

        # Reconstruct header first
        header = GedcomHeader()
        if data.get('header'):
            header = self._reconstruct_object(data['header'], GedcomHeader)

        # Create new database with header
        database = GedcomDatabase(header=header)

        # Reconstruct individuals
        for xref, individual_data in data.get('individuals', {}).items():
            try:
                database.individuals[xref] = self._reconstruct_object(individual_data, GedcomIndividual)
            except Exception as e:
                self.logger.warning(f"Failed to reconstruct individual {xref}: {e}")
                # Create a minimal individual as fallback
                database.individuals[xref] = GedcomIndividual(xref=xref)

        # Reconstruct families
        for xref, family_data in data.get('families', {}).items():
            try:
                database.families[xref] = self._reconstruct_object(family_data, GedcomFamily)
            except Exception as e:
                self.logger.warning(f"Failed to reconstruct family {xref}: {e}")
                # Create a minimal family as fallback
                database.families[xref] = GedcomFamily(xref=xref)

        # Reconstruct notes (as structured text for now)
        for xref, note_data in data.get('notes', {}).items():
            try:
                if isinstance(note_data, dict):
                    database.notes[xref] = self._reconstruct_object(note_data, GedcomStructuredText)
                else:
                    database.notes[xref] = GedcomStructuredText(main_value=str(note_data))
            except Exception as e:
                self.logger.warning(f"Failed to reconstruct note {xref}: {e}")
                database.notes[xref] = GedcomStructuredText(main_value="")

        # Reconstruct sources (as structured text for now)
        for xref, source_data in data.get('sources', {}).items():
            try:
                if isinstance(source_data, dict):
                    database.sources[xref] = self._reconstruct_object(source_data, GedcomStructuredText)
                else:
                    database.sources[xref] = GedcomStructuredText(main_value=str(source_data))
            except Exception as e:
                self.logger.warning(f"Failed to reconstruct source {xref}: {e}")
                database.sources[xref] = GedcomStructuredText(main_value="")

        # Reconstruct submitters
        for xref, submitter_data in data.get('submitters', {}).items():
            try:
                database.submitters[xref] = self._reconstruct_object(submitter_data, GedcomSubmitter)
            except Exception as e:
                self.logger.warning(f"Failed to reconstruct submitter {xref}: {e}")
                database.submitters[xref] = GedcomSubmitter()

        # Restore record order
        database.record_order = data.get('record_order', [])

        return database

    def _reconstruct_object(self, data: Dict[str, Any], target_class):
        """Recursively reconstruct objects from serialized data."""
        if not isinstance(data, dict):
            return data

        from .models import (GedcomName, GedcomEvent, GedcomDate, GedcomPlace,
                           GedcomAddress, GedcomStructuredText)

        # Create a copy to avoid modifying the original
        reconstructed_data = {}

        for key, value in data.items():
            if isinstance(value, dict):
                # Try to determine the correct type based on field names
                if key == 'date' and ('year' in value or 'raw' in value):
                    reconstructed_data[key] = self._reconstruct_object(value, GedcomDate)
                elif key == 'place' and ('name' in value or 'parts' in value):
                    reconstructed_data[key] = self._reconstruct_object(value, GedcomPlace)
                elif key == 'address':
                    reconstructed_data[key] = self._reconstruct_object(value, GedcomAddress)
                elif key in ['birth', 'death', 'marriage'] or key.endswith('_event'):
                    reconstructed_data[key] = self._reconstruct_object(value, GedcomEvent)
                else:
                    # For unknown dict types, try to reconstruct recursively
                    reconstructed_data[key] = self._reconstruct_object(value, dict)
            elif isinstance(value, list):
                # Handle lists of objects
                reconstructed_list = []
                for item in value:
                    if isinstance(item, dict):
                        # Determine type based on context
                        if key == 'names' and 'full' in item:
                            reconstructed_list.append(self._reconstruct_object(item, GedcomName))
                        elif key == 'events' and 'tag' in item:
                            reconstructed_list.append(self._reconstruct_object(item, GedcomEvent))
                        else:
                            reconstructed_list.append(self._reconstruct_object(item, dict))
                    else:
                        reconstructed_list.append(item)
                reconstructed_data[key] = reconstructed_list
            else:
                reconstructed_data[key] = value

        # Try to create the target object
        try:
            if target_class == dict:
                return reconstructed_data
            else:
                return target_class(**reconstructed_data)
        except Exception as e:
            self.logger.debug(f"Failed to reconstruct {target_class.__name__}: {e}")
            # Fallback: return the data as-is or create with available fields
            try:
                # Try with only the fields that the class accepts
                import inspect
                sig = inspect.signature(target_class.__init__)
                valid_fields = {k: v for k, v in reconstructed_data.items()
                              if k in sig.parameters}
                return target_class(**valid_fields)
            except:
                return reconstructed_data

class GedcomCache:
    """High-level cache manager for GEDCOM files."""

    def __init__(self, cache_dir: Path = None, compression: bool = True):
        """Initialize the cache manager.

        Args:
            cache_dir: Directory to store cached files (default: ./cache)
            compression: Use compression for better storage efficiency
        """
        self.cache_dir = cache_dir or Path('./cache')
        self.cache_dir.mkdir(exist_ok=True)
        self.serialization_manager = SerializationManager(compression=compression)
        self.logger = logging.getLogger(__name__)

    def get_cache_path(self, original_path: Path, format: str = 'pickle') -> Path:
        """Get cache file path for an original GEDCOM file."""
        cache_name = f"{original_path.stem}_{format}"
        if self.serialization_manager.compression:
            cache_name += '.gz'
        return self.cache_dir / cache_name

    def is_cache_valid(self, original_path: Path, cache_path: Path) -> bool:
        """Check if cache is newer than original file."""
        if not cache_path.exists():
            return False

        original_mtime = original_path.stat().st_mtime
        cache_mtime = cache_path.stat().st_mtime

        return cache_mtime > original_mtime

    def load_or_parse(self, gedcom_path: Path, format: str = 'pickle',
                      force_refresh: bool = False) -> GedcomDatabase:
        """Load from cache or parse GEDCOM file if cache is invalid.

        Args:
            gedcom_path: Path to the original GEDCOM file
            format: Cache format ('pickle' or 'msgpack')
            force_refresh: Force re-parsing even if cache is valid

        Returns:
            GedcomDatabase instance
        """
        cache_path = self.get_cache_path(gedcom_path, format)

        # Check if we can use cache
        if not force_refresh and self.is_cache_valid(gedcom_path, cache_path):
            self.logger.info(f"Loading from cache: {cache_path}")
            if format == 'pickle':
                return self.serialization_manager.load_pickle(cache_path)
            else:
                return self.serialization_manager.load_msgpack(cache_path)

        # Parse original file and cache it
        self.logger.info(f"Parsing and caching: {gedcom_path}")
        parser = create_parser()
        database = parser.parse_file(gedcom_path)

        # Save to cache
        if format == 'pickle':
            self.serialization_manager.save_pickle(database, cache_path)
        else:
            self.serialization_manager.save_msgpack(database, cache_path)

        return database

def create_serialization_manager(compression: bool = True) -> SerializationManager:
    """Factory function to create a serialization manager."""
    return SerializationManager(compression=compression)

def create_gedcom_cache(cache_dir: Path = None, compression: bool = True) -> GedcomCache:
    """Factory function to create a GEDCOM cache manager."""
    return GedcomCache(cache_dir=cache_dir, compression=compression)
