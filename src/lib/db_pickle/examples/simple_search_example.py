#!/usr/bin/env python3
"""
Simple example of using search indexes in db_pickle.

Quick demonstration of basic search functionality.
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.core.types import Iper
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.core.enums import Sex

def main():
    """Simple search demonstration."""
    print("=== Simple Search Example ===\n")

    # Create database
    data = PickleBaseData()

    # Add some persons
    data.persons[Iper(1)] = GenPerson(first_name="Jean", surname="Dupont", sex=Sex.MALE)
    data.persons[Iper(2)] = GenPerson(first_name="Marie", surname="Martin", sex=Sex.FEMALE)
    data.persons[Iper(3)] = GenPerson(first_name="Jean", surname="Martin", sex=Sex.MALE)
    data.persons[Iper(4)] = GenPerson(first_name="Pierre", surname="Dupont", sex=Sex.MALE)

    # Add some strings
    data.strings[1] = "Paris"
    data.strings[2] = "Lyon"
    data.strings[3] = "Marseille"

    print(f"Database created with {data.persons_count} persons and {data.strings_count} strings")

    # Build indexes
    print("\nBuilding indexes...")
    data.build_indexes()

    # Search examples
    print("\n=== Search Examples ===")

    # Search by first name
    print("1. Search by first name 'Jean':")
    jean_persons = data.search_persons_by_first_name("Jean")
    for iper in jean_persons:
        person = data.persons[iper]
        print(f"   - {person.first_name} {person.surname}")

    # Search by surname
    print("\n2. Search by surname 'Dupont':")
    dupont_persons = data.search_persons_by_surname("Dupont")
    for iper in dupont_persons:
        person = data.persons[iper]
        print(f"   - {person.first_name} {person.surname}")

    # Search by full name
    print("\n3. Search by full name 'Jean Dupont':")
    jean_dupont = data.search_persons_by_full_name("Jean Dupont")
    for iper in jean_dupont:
        person = data.persons[iper]
        print(f"   - {person.first_name} {person.surname}")

    # Search strings
    print("\n4. Search strings containing 'Paris':")
    paris_strings = data.search_strings_by_content("Paris")
    for istr in paris_strings:
        print(f"   - String ID {istr}: '{data.strings[istr]}'")

    # Case-insensitive search
    print("\n5. Case-insensitive search for 'JEAN':")
    jean_upper = data.search_persons_by_first_name("JEAN")
    for iper in jean_upper:
        person = data.persons[iper]
        print(f"   - {person.first_name} {person.surname}")

    print("\n=== Search Complete ===")

if __name__ == '__main__':
    main()
