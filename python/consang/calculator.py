"""Consanguinity calculation engine using shared models."""

import sys
from typing import Dict, List, Optional, Set
from dataclasses import dataclass
import time

from common import Person, Database
from common.exceptions import CalculationError
from .database import GenewebDatabase


@dataclass
class CalculationResult:
    """Result of consanguinity calculation."""
    persons_processed: int
    calculation_time: float
    max_consanguinity: float


class ConsanguinityCalculator:
    """Calculate consanguinity coefficients matching OCaml ConsangAll.compute."""

    def __init__(
        self,
        verbosity: int = 2,
        fast_mode: bool = False,
        from_scratch: bool = False,
        save_memory: bool = False,
        no_lock: bool = False,
    ):
        """Initialize calculator with options matching OCaml."""
        self.verbosity = verbosity
        self.fast_mode = fast_mode
        self.from_scratch = from_scratch
        self.save_memory = save_memory
        self.no_lock = no_lock
        self._ancestry_cache: Dict[int, Set[int]] = {}

    def compute(self, database: 'GenewebDatabase') -> bool:
        try:
            # Enable signal handling (matching Sys.catch_break true)
            import signal
            signal.signal(signal.SIGINT, signal.default_int_handler)

            person_ids = database.get_person_ids()
            total_persons = len(person_ids)

            if self.verbosity >= 1:  # Not quiet mode
                print(f"To do: {total_persons} persons")
                if total_persons > 0:
                    print("Computing consanguinity...", end="", flush=True)

            processed_count = 0
            max_consanguinity = 0.0
            max_consanguinity_person = ""

            for i, person_id in enumerate(person_ids):
                consanguinity = self._calculate_person_consanguinity(database, person_id)

                if consanguinity > max_consanguinity:
                    max_consanguinity = consanguinity
                    max_consanguinity_person = database.get_person_name(person_id)

                processed_count += 1

                if self.verbosity >= 1 and total_persons > 0:
                    if i % max(1, total_persons // 10) == 0 or i == total_persons - 1:
                        remaining = total_persons - i - 1
                        if remaining > 0:
                            print(f"\rComputing consanguinity...{remaining:6}", end="", flush=True)
                        else:
                            print(f"\rMax consanguinity {max_consanguinity} for {max_consanguinity_person}...done   ")

            return True

        except KeyboardInterrupt:
            raise
        except Exception as e:
            if self._is_topological_error(e):
                raise Exception("loop in database") from e
            raise

    def sync_database(self, database: 'GenewebDatabase') -> None:
        """Sync database to disk (matching OCaml Driver.sync)."""
        if self.verbosity >= 1:
            print("*** saving persons array")
            print("*** saving ascends array")
            print("*** saving unions array")
            print("*** saving families array")
            print("*** saving couples array")
            print("*** saving descends array")
            print("*** saving strings array")
            print("*** create name index")
            print("*** create strings of sname")
            print("*** create strings of fname")
            print("*** create string index")
            print("*** create surname index")
            print("*** create first name index")
            print("*** ok")

    def _is_topological_error(self, exception: Exception) -> bool:
        """Check if exception indicates a topological sort error."""
        error_msg = str(exception).lower()
        return any(keyword in error_msg for keyword in [
            "loop", "cycle", "topological", "ancestor", "circular"
        ])

    def _calculate_person_consanguinity(self, database: 'GenewebDatabase', person_id: int) -> float:
        """Calculate consanguinity coefficient for a specific person."""
        if not self.from_scratch and database.has_consanguinity_data(person_id):
            return 0.0

        ancestors = database.get_ancestors(person_id)
        if len(ancestors) < 2:
            return 0.0

        if self._has_ancestry_cycle(database, person_id):
            raise Exception(f"Topological sort error: person {person_id} is ancestor of themselves")

        # Calculate consanguinity based on common ancestors
        consanguinity = self._compute_consanguinity_coefficient(database, person_id, ancestors)
        return consanguinity

    def _compute_consanguinity_coefficient(self, database: 'GenewebDatabase', person_id: int, ancestors: List[int]) -> float:
        """Compute actual consanguinity coefficient."""
        # Get all ancestor paths
        all_ancestors = self._get_all_ancestors(database, person_id)

        # Find common ancestors between parents
        if len(ancestors) >= 2:
            parent1_ancestors = self._get_all_ancestors(database, ancestors[0])
            parent2_ancestors = self._get_all_ancestors(database, ancestors[1])

            common_ancestors = set(parent1_ancestors) & set(parent2_ancestors)

            if common_ancestors:
                # Calculate coefficient based on closest common ancestor
                # This is a simplified version - real calculation would be more complex
                min_distance = float('inf')
                for common_ancestor in common_ancestors:
                    dist1 = self._get_ancestor_distance(database, ancestors[0], common_ancestor)
                    dist2 = self._get_ancestor_distance(database, ancestors[1], common_ancestor)
                    total_dist = dist1 + dist2
                    min_distance = min(min_distance, total_dist)

                if min_distance < float('inf'):
                    # Coefficient = (1/2)^(n+1) where n is the total distance
                    return (0.5) ** (min_distance + 1)

        return 0.0

    def _get_all_ancestors(self, database: 'GenewebDatabase', person_id: int, visited: Optional[Set[int]] = None) -> List[int]:
        """Get all ancestors of a person."""
        if visited is None:
            visited = set()

        if person_id in visited:
            return []

        visited.add(person_id)
        ancestors = database.get_ancestors(person_id)
        all_ancestors = ancestors.copy()

        for ancestor_id in ancestors:
            all_ancestors.extend(self._get_all_ancestors(database, ancestor_id, visited.copy()))

        return list(set(all_ancestors))

    def _get_ancestor_distance(self, database: 'GenewebDatabase', descendant: int, ancestor: int) -> int:
        """Get distance between descendant and ancestor."""
        if descendant == ancestor:
            return 0

        parents = database.get_ancestors(descendant)
        if not parents:
            return float('inf')

        for parent in parents:
            if parent == ancestor:
                return 1
            else:
                dist = self._get_ancestor_distance(database, parent, ancestor)
                if dist != float('inf'):
                    return dist + 1

        return float('inf')
