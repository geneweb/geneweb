"""
Reader for pickle database.

Handles loading pickle database from disk.
"""

import gzip
import pickle
from pathlib import Path

from ..database.base_data import PickleBaseData


class PickleReader:
    """Reader for pickle database files."""

    def __init__(self, verbose: bool = False):
        """
        Initialize pickle reader.

        Args:
            verbose: Enable verbose output
        """
        self.verbose = verbose

    def trace(self, message: str) -> None:
        """Print trace message if verbose."""
        if self.verbose:
            print(f"*** {message}")

    def load_database(self, filepath: Path) -> PickleBaseData:
        """
        Load pickle database from file.

        Args:
            filepath: Input file path

        Returns:
            Loaded database data
        """
        import time

        start_time = time.time()

        # Determine if file is compressed
        if filepath.suffix == ".gz" or filepath.name.endswith(".pkl.gz"):
            self.trace("loading compressed pickle database" + str(filepath))
            with gzip.open(filepath, "rb") as f:
                data = pickle.load(f)
        else:
            self.trace("loading pickle database" + str(filepath))
            with open(filepath, "rb") as f:
                data = pickle.load(f)

        load_time = time.time() - start_time
        self.trace(
            f"loaded {data.persons_count} persons, {data.families_count} families in {load_time:.3f}s"
        )

        return data
