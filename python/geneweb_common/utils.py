"""Utility functions for genealogical operations."""

from typing import Dict, List, Optional, Set, Tuple, Any
import re
from datetime import datetime
from .models import Person, Family, Database, Date, Name
from .exceptions import ValidationError


class ValidationUtils:
    """Utility functions for data validation."""

    @staticmethod
    def validate_person_id(person_id: int) -> bool:
        """Validate person ID format."""
        return isinstance(person_id, int) and person_id >= 0

    @staticmethod
    def validate_name(name: str) -> bool:
        """Validate name format."""
        if not isinstance(name, str):
            return False

        # Basic validation - no control characters
        return not any(ord(c) < 32 for c in name)

    @staticmethod
    def validate_date_string(date_str: str) -> bool:
        """Validate date string format."""
        if not date_str:
            return True

        # Common date patterns
        patterns = [
            r'^\d{4}$',  # YYYY
            r'^\d{1,2}/\d{4}$',  # MM/YYYY
            r'^\d{1,2}/\d{1,2}/\d{4}$',  # DD/MM/YYYY
            r'^(about|before|after)\s+\d{4}$'  # precision + year
        ]

        return any(re.match(pattern, date_str.strip()) for pattern in patterns)

    @staticmethod
    def validate_sex(sex: str) -> bool:
        """Validate sex value."""
        return sex in ('M', 'F', 'U')


class NameUtils:
    """Utility functions for name handling."""

    @staticmethod
    def parse_full_name(full_name: str) -> Name:
        """Parse full name into components."""
        parts = full_name.strip().split()
        if not parts:
            return Name()

        if len(parts) == 1:
            return Name(first_name=parts[0])

        # Simple heuristic: last part is surname
        first_name = " ".join(parts[:-1])
        surname = parts[-1]

        return Name(first_name=first_name, surname=surname)

    @staticmethod
    def normalize_name(name: str) -> str:
        """Normalize name for comparison."""
        # Remove extra spaces, convert to title case
        normalized = " ".join(name.split()).title()

        # Handle special cases (French particles, etc.)
        particles = ['de', 'du', 'des', 'la', 'le', 'les', 'van', 'von', 'di', 'da']
        words = normalized.split()

        for i, word in enumerate(words):
            if word.lower() in particles and i > 0:
                words[i] = word.lower()

        return " ".join(words)

    @staticmethod
    def get_soundex(name: str) -> str:
        """Generate Soundex code for name matching."""
        if not name:
            return "0000"

        name = name.upper().strip()
        if not name:
            return "0000"

        # Soundex algorithm
        soundex_map = {
            'B': '1', 'F': '1', 'P': '1', 'V': '1',
            'C': '2', 'G': '2', 'J': '2', 'K': '2', 'Q': '2', 'S': '2', 'X': '2', 'Z': '2',
            'D': '3', 'T': '3',
            'L': '4',
            'M': '5', 'N': '5',
            'R': '6'
        }

        # Keep first letter
        result = name[0]

        # Convert remaining letters
        for char in name[1:]:
            if char in soundex_map:
                code = soundex_map[char]
                if result[-1] != code:  # No consecutive duplicates
                    result += code

        # Pad or truncate to 4 characters
        result = (result + "000")[:4]

        return result


class DateUtils:
    """Utility functions for date handling."""

    @staticmethod
    def parse_date_string(date_str: str) -> Date:
        """Parse date string into Date object."""
        if not date_str:
            return Date()

        date_str = date_str.strip()

        # Handle precision markers
        precision = "exact"
        for marker in ["about", "before", "after"]:
            if date_str.startswith(marker):
                precision = marker
                date_str = date_str[len(marker):].strip()
                break

        # Parse date components
        parts = date_str.split('/')
        year = month = day = None

        try:
            if len(parts) == 1:  # YYYY
                year = int(parts[0])
            elif len(parts) == 2:  # MM/YYYY
                month, year = int(parts[0]), int(parts[1])
            elif len(parts) == 3:  # DD/MM/YYYY
                day, month, year = int(parts[0]), int(parts[1]), int(parts[2])
        except ValueError:
            pass

        return Date(year=year, month=month, day=day, precision=precision)

    @staticmethod
    def format_date(date_obj: Date) -> str:
        """Format Date object as string."""
        return str(date_obj)

    @staticmethod
    def compare_dates(date1: Date, date2: Date) -> int:
        """Compare two dates. Returns -1, 0, or 1."""
        def date_to_tuple(d: Date) -> Tuple[int, int, int]:
            return (d.year or 0, d.month or 0, d.day or 0)

        tuple1 = date_to_tuple(date1)
        tuple2 = date_to_tuple(date2)

        if tuple1 < tuple2:
            return -1
        elif tuple1 > tuple2:
            return 1
        else:
            return 0


class RelationshipUtils:
    """Utility functions for calculating relationships."""

    @staticmethod
    def get_relationship(person1_id: int, person2_id: int, database: Database) -> Optional[str]:
        """Get relationship description between two persons."""
        # Find common ancestors and calculate relationship
        path1 = RelationshipUtils._get_path_to_root(person1_id, database)
        path2 = RelationshipUtils._get_path_to_root(person2_id, database)

        # Find common ancestor
        common_ancestors = set(path1.keys()) & set(path2.keys())
        if not common_ancestors:
            return None

        # Use closest common ancestor
        closest_ancestor = min(common_ancestors, key=lambda x: path1[x] + path2[x])

        dist1 = path1[closest_ancestor]
        dist2 = path2[closest_ancestor]

        return RelationshipUtils._format_relationship(dist1, dist2)

    @staticmethod
    def _get_path_to_root(person_id: int, database: Database) -> Dict[int, int]:
        """Get path distances to all ancestors."""
        paths = {}
        queue = [(person_id, 0)]
        visited = set()

        while queue:
            current_id, distance = queue.pop(0)

            if current_id in visited:
                continue

            visited.add(current_id)
            paths[current_id] = distance

            person = database.get_person(current_id)
            if person and person.parents:
                father_id, mother_id = person.parents
                if father_id:
                    queue.append((father_id, distance + 1))
                if mother_id:
                    queue.append((mother_id, distance + 1))

        return paths

    @staticmethod
    def _format_relationship(dist1: int, dist2: int) -> str:
        """Format relationship description."""
        if dist1 == 0 and dist2 == 0:
            return "same person"
        elif dist1 == 1 and dist2 == 1:
            return "siblings"
        elif dist1 == 1 and dist2 == 0:
            return "parent"
        elif dist1 == 0 and dist2 == 1:
            return "child"
        elif dist1 == 2 and dist2 == 0:
            return "grandparent"
        elif dist1 == 0 and dist2 == 2:
            return "grandchild"
        elif dist1 == 2 and dist2 == 2:
            return "first cousins"
        else:
            # Generic description
            min_dist = min(dist1, dist2)
            max_dist = max(dist1, dist2)

            if min_dist == 1:
                return f"{max_dist - 1} times removed"
            else:
                cousin_level = min_dist - 1
                removed = abs(dist1 - dist2)

                if removed == 0:
                    return f"{RelationshipUtils._ordinal(cousin_level)} cousins"
                else:
                    return f"{RelationshipUtils._ordinal(cousin_level)} cousins {removed} times removed"

    @staticmethod
    def _ordinal(n: int) -> str:
        """Convert number to ordinal string."""
        if n == 1:
            return "first"
        elif n == 2:
            return "second"
        elif n == 3:
            return "third"
        else:
            return f"{n}th"

    @staticmethod
    def _get_generation_distance(person_id: int, ancestor_id: int, database: Database) -> Optional[int]:
        """Get generation distance between person and ancestor."""
        if person_id == ancestor_id:
            return 0

        distance = 0
        current_id = person_id
        visited = set()

        while current_id and current_id not in visited:
            visited.add(current_id)
            person = database.get_person(current_id)

            if not person or not person.parents:
                return None

            father_id, mother_id = person.parents
            distance += 1

            if father_id == ancestor_id or mother_id == ancestor_id:
                return distance

            # Continue with father (arbitrary choice)
            current_id = father_id

        return None


class DatabaseUtils:
    """Utility functions for database operations."""

    @staticmethod
    def validate_database(database: Database) -> List[str]:
        """Validate database integrity and return list of issues."""
        issues = []

        # Check for missing person references
        all_person_ids = set(database.persons.keys())

        for family in database.families.values():
            # Check parent references
            if family.father_id and family.father_id not in all_person_ids:
                issues.append(f"Family {family.id}: missing father {family.father_id}")

            if family.mother_id and family.mother_id not in all_person_ids:
                issues.append(f"Family {family.id}: missing mother {family.mother_id}")

            # Check child references
            for child_id in family.children:
                if child_id not in all_person_ids:
                    issues.append(f"Family {family.id}: missing child {child_id}")

        # Check for orphaned persons
        referenced_persons = set()
        for family in database.families.values():
            if family.father_id:
                referenced_persons.add(family.father_id)
            if family.mother_id:
                referenced_persons.add(family.mother_id)
            referenced_persons.update(family.children)

        # Check person family references
        for person in database.persons.values():
            for family_id in person.families:
                if family_id not in database.families:
                    issues.append(f"Person {person.id}: references missing family {family_id}")

        return issues

    @staticmethod
    def get_statistics(database: Database) -> Dict[str, Any]:
        """Get database statistics."""
        stats = {
            'total_persons': len(database.persons),
            'total_families': len(database.families),
            'males': sum(1 for p in database.persons.values() if p.sex == 'M'),
            'females': sum(1 for p in database.persons.values() if p.sex == 'F'),
            'unknown_sex': sum(1 for p in database.persons.values() if p.sex == 'U'),
            'persons_with_birth': sum(1 for p in database.persons.values() if p.birth),
            'persons_with_death': sum(1 for p in database.persons.values() if p.death),
            'persons_with_parents': sum(1 for p in database.persons.values() if p.parents),
            'families_with_children': sum(1 for f in database.families.values() if f.children),
        }

        return stats
