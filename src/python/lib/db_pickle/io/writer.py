"""
Writer for pickle database.

Handles saving pickle database to disk with progress messages.
"""

import gzip
import pickle
from pathlib import Path
from typing import Any, Dict

from ..database.base_data import PickleBaseData


class PickleWriter:
    """Writer for pickle database files."""

    def __init__(self, verbose: bool = False):
        """
        Initialize pickle writer.

        Args:
            verbose: Enable verbose output
        """
        self.verbose = verbose

    def trace(self, message: str) -> None:
        """Print trace message if verbose."""
        if self.verbose:
            print(f"*** {message}")

    def save_database(
        self, data: PickleBaseData, filepath, compress: bool = False
    ) -> Dict[str, Any]:
        """
        Save pickle database to file.

        Args:
            data: Database data to save
            filepath: Output file path
            compress: Whether to compress with gzip

        Returns:
            Dictionary with save statistics
        """
        import time

        start_time = time.time()

        # Convert to Path if needed
        if isinstance(filepath, str):
            filepath = Path(filepath)

        # Determine final filepath
        if compress:
            if filepath.suffix == ".gz" or filepath.name.endswith(".pkl.gz"):
                final_path = filepath
            else:
                final_path = filepath.with_suffix(".pkl.gz")
        else:
            if filepath.suffix == ".pkl":
                final_path = filepath
            else:
                final_path = filepath.with_suffix(".pkl")

        # Create output directory if needed
        final_path.parent.mkdir(parents=True, exist_ok=True)

        # Serialize data
        if compress:
            with gzip.open(final_path, "wb", compresslevel=6) as f:
                pickle.dump(data, f, protocol=pickle.HIGHEST_PROTOCOL)
        else:
            with open(final_path, "wb") as f:
                pickle.dump(data, f, protocol=pickle.HIGHEST_PROTOCOL)

        # Calculate statistics
        file_size = final_path.stat().st_size
        save_time = time.time() - start_time

        self.trace("ok")

        return {
            "file_path": str(final_path),
            "file_size": file_size,
            "save_time": save_time,
            "compressed": compress,
            "persons_count": data.persons_count,
            "families_count": data.families_count,
            "strings_count": data.strings_count,
        }
