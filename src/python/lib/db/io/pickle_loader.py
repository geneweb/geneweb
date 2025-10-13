"""
Pickle Database Loader for GeneWeb

This module provides functionality to load GeneWeb databases
saved in pickle format instead of the native .gwb format.
"""

import gzip
import logging
import pickle
from pathlib import Path
from typing import Any, Dict

from ..database.base import Base
from ..database.base_data import BaseData
from ..database.base_func import BaseFunc


class PickleDatabaseLoader:
    """Loader for GeneWeb databases saved in pickle format."""

    def __init__(self):
        self.logger = logging.getLogger(__name__)

    def load_database(self, pickle_path: Path) -> Base:
        """
        Load a GeneWeb database from pickle file.

        Args:
            pickle_path: Path to the pickle file (.pkl or .pkl.gz)

        Returns:
            Base: Loaded GeneWeb database
        """
        try:
            if pickle_path.suffix == ".gz" or str(pickle_path).endswith(".pkl.gz"):
                with gzip.open(pickle_path, "rb") as f:
                    data = pickle.load(f)
            else:
                with open(pickle_path, "rb") as f:
                    data = pickle.load(f)

            base_data = self._reconstruct_base_data(data)
            base_func = self._reconstruct_base_func(base_data)

            return Base(base_data, base_func)

        except Exception as e:
            self.logger.error(f"Failed to load pickle database {pickle_path}: {e}")
            raise

    def _reconstruct_base_data(self, data: Dict[str, Any]) -> BaseData:
        """Reconstruct BaseData from pickle data."""
        return BaseData(
            persons=data.get("persons", {}),
            families=data.get("families", {}),
            strings=data.get("strings", {}),
            ascends=data.get("ascends", {}),
            unions=data.get("unions", {}),
            couples=data.get("couples", {}),
            descends=data.get("descends", {}),
            bdir=data.get("bdir", ""),
        )

    def _reconstruct_base_func(self, base_data: BaseData) -> BaseFunc:
        """Reconstruct BaseFunc from BaseData."""
        return BaseFunc(base_data)

    def get_database_info(self, pickle_path: Path) -> Dict[str, Any]:
        """
        Get information about a pickle database without fully loading it.

        Args:
            pickle_path: Path to the pickle file

        Returns:
            Dictionary with database information
        """
        try:
            if pickle_path.suffix == ".gz" or str(pickle_path).endswith(".pkl.gz"):
                with gzip.open(pickle_path, "rb") as f:
                    data = pickle.load(f)
            else:
                with open(pickle_path, "rb") as f:
                    data = pickle.load(f)

            return {
                "persons_count": len(data.get("persons", {})),
                "families_count": len(data.get("families", {})),
                "strings_count": len(data.get("strings", {})),
                "file_size": pickle_path.stat().st_size,
                "last_modified": pickle_path.stat().st_mtime,
            }

        except Exception as e:
            self.logger.error(f"Failed to get database info {pickle_path}: {e}")
            return {}
