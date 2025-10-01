"""Consanguinity calculation engine using shared models."""

import sys
from typing import Dict, List, Optional, Set
from dataclasses import dataclass
import time

from geneweb_common import Person, Database
from geneweb_common.exceptions import CalculationError
from .database import GenewebDatabase


@dataclass
class CalculationResult:
    """Result of consanguinity calculation."""
    persons_processed: int
    persons_updated: int
    calculation_time: float


class ConsanguinityCalculator:
    """Calculate consanguinity coefficients using shared genealogical models."""

    def __init__(self, quiet_level: int = 0, fast_mode: bool = False,
                 from_scratch: bool = False, save_memory: bool = False,
                 no_lock: bool = False):
        """Initialize calculator with options."""
        self.quiet_level = quiet_level
        self.fast_mode = fast_mode
        self.from_scratch = from_scratch
        self.save_memory = save_memory
        self.no_lock = no_lock

        self._ancestry_cache: Dict[int, Set[int]] = {}

    def calculate(self, database: GenewebDatabase) -> CalculationResult:
        """Calculate consanguinity for entire database."""
        start_time = time.time()

        if not self.no_lock:
            database.lock_database()

        try:
            return self._perform_calculation(database, start_time)
        finally:
            if not self.no_lock:
                database.unlock_database()

    def _perform_calculation(self, database: GenewebDatabase, start_time: float) -> CalculationResult:
        """Perform the actual calculation."""
        persons_to_process = self._get_persons_to_process(database)

        if self.quiet_level == 0:
            self._print_initial_status(len(persons_to_process))

        if len(persons_to_process) == 0:
            if self.quiet_level < 2:
                print("To do: 0 persons", file=sys.stderr)
            return CalculationResult(0, 0, time.time() - start_time)

        # Process persons
        updated_count = 0
        for person in persons_to_process:
            if self._should_calculate_person(person):
                consanguinity = self._calculate_person_consanguinity(person, database)
                database.update_person_consanguinity(person.id, consanguinity)
                updated_count += 1

                if self.quiet_level == 0:
                    self._print_person_progress(person, consanguinity)

        database.save_changes()

        if self.quiet_level < 2:
            final_count = len(persons_to_process) - updated_count
            print(f"To do: {final_count} persons", file=sys.stderr)

        calculation_time = time.time() - start_time
        return CalculationResult(len(persons_to_process), updated_count, calculation_time)

    def _get_persons_to_process(self, database: GenewebDatabase) -> List[Person]:
        """Get list of persons that need processing."""
        if self.from_scratch:
            # Reset all consanguinity values
            for person in database.get_all_persons():
                person.consanguinity = None

        if self.fast_mode:
            # In fast mode, skip persons that already have values
            return []

        return database.get_persons_needing_calculation()

    def _should_calculate_person(self, person: Person) -> bool:
        """Determine if person needs calculation."""
        if self.fast_mode and person.consanguinity is not None:
            return False

        if self.from_scratch:
            return True

        return person.consanguinity is None and person.parents is not None

    def _calculate_person_consanguinity(self, person: Person, database: GenewebDatabase) -> float:
        """Calculate consanguinity coefficient using relationship utilities."""
        if not person.parents:
            return 0.0

        father_id, mother_id = person.parents

        # Use database interface
        if database.database:
            # Get common ancestors
            father_ancestors = database.database.get_ancestors(father_id)
            mother_ancestors = database.database.get_ancestors(mother_id)
            common_ancestors = father_ancestors.intersection(mother_ancestors)

            if not common_ancestors:
                return 0.0

            # Calculate consanguinity based on common ancestors
            consanguinity = 0.0
            for ancestor_id in common_ancestors:
                father_distance = self._get_generation_distance(father_id, ancestor_id, database.database)
                mother_distance = self._get_generation_distance(mother_id, ancestor_id, database.database)

                if father_distance > 0 and mother_distance > 0:
                    coefficient = (0.5 ** (father_distance + mother_distance + 1))
                    consanguinity += coefficient

            return consanguinity

        return 0.0

    def _get_generation_distance(self, person_id: int, ancestor_id: int, database: Database) -> int:
        """Get generation distance using simple traversal."""
        if person_id == ancestor_id:
            return 0

        distance = 0
        current_id = person_id
        visited = set()

        while current_id and current_id not in visited:
            visited.add(current_id)
            person = database.get_person(current_id)

            if not person or not person.parents:
                return -1

            father_id, mother_id = person.parents
            distance += 1

            if father_id == ancestor_id or mother_id == ancestor_id:
                return distance

            # Try father first, then mother if needed
            ancestors = database.get_ancestors(ancestor_id)
            if father_id in ancestors:
                current_id = father_id
            elif mother_id in ancestors:
                current_id = mother_id
            else:
                return -1

        return -1

    def _print_initial_status(self, person_count: int) -> None:
        """Print initial calculation status."""
        if person_count > 0:
            print(f"To do: {person_count} persons", file=sys.stderr)
            print("Computing consanguinities...", file=sys.stderr)

    def _print_person_progress(self, person: Person, consanguinity: float) -> None:
        """Print progress using shared name formatting."""
        full_name = person.get_full_name()
        print(f"{full_name}: {consanguinity:.6f}", file=sys.stderr)
