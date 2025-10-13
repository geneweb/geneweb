#!/usr/bin/env python3
"""
Example of searching descendants and using all indexes in db_pickle.

Demonstrates how to:
- Search for descendants of a person
- Use all search indexes (first name, surname, full name, strings)
- Build family trees
- Perform complex genealogical queries
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from lib.db_pickle.core.enums import RelationKind, Sex
from lib.db_pickle.core.types import Ifam, Iper
from lib.db_pickle.database.base_data import PickleBaseData
from lib.db_pickle.models.family import GenFamily
from lib.db_pickle.models.person import GenPerson
from lib.db_pickle.models.relations import GenCouple, GenDescend


def create_family_tree():
    """Create a sample family tree for demonstration."""
    data = PickleBaseData()

    # Generation 1 - Grandparents
    data.persons[Iper(1)] = GenPerson(first_name="Jean", surname="Dupont", sex=Sex.MALE)

    data.persons[Iper(2)] = GenPerson(
        first_name="Marie", surname="Martin", sex=Sex.FEMALE
    )

    # Generation 2 - Parents
    data.persons[Iper(3)] = GenPerson(
        first_name="Pierre", surname="Dupont", sex=Sex.MALE
    )

    data.persons[Iper(4)] = GenPerson(
        first_name="Sophie", surname="Bernard", sex=Sex.FEMALE
    )

    data.persons[Iper(5)] = GenPerson(first_name="Paul", surname="Martin", sex=Sex.MALE)

    data.persons[Iper(6)] = GenPerson(
        first_name="Claire", surname="Dupont", sex=Sex.FEMALE
    )

    # Generation 3 - Children
    data.persons[Iper(7)] = GenPerson(
        first_name="Lucas", surname="Dupont", sex=Sex.MALE
    )

    data.persons[Iper(8)] = GenPerson(
        first_name="Emma", surname="Dupont", sex=Sex.FEMALE
    )

    data.persons[Iper(9)] = GenPerson(
        first_name="Thomas", surname="Martin", sex=Sex.MALE
    )

    data.persons[Iper(10)] = GenPerson(
        first_name="Léa", surname="Martin", sex=Sex.FEMALE
    )

    # Generation 4 - Grandchildren
    data.persons[Iper(11)] = GenPerson(
        first_name="Noah", surname="Dupont", sex=Sex.MALE
    )

    data.persons[Iper(12)] = GenPerson(
        first_name="Chloé", surname="Dupont", sex=Sex.FEMALE
    )

    # Create families
    # Family 1: Jean + Marie (grandparents)
    data.families[Ifam(1)] = GenFamily(relation=RelationKind.MARRIED)
    data.couples[Ifam(1)] = GenCouple(father=Iper(1), mother=Iper(2))
    data.descends[Ifam(1)] = GenDescend(children=[Iper(3), Iper(6)])

    # Family 2: Pierre + Sophie (parents of Lucas/Emma)
    data.families[Ifam(2)] = GenFamily(relation=RelationKind.MARRIED)
    data.couples[Ifam(2)] = GenCouple(father=Iper(3), mother=Iper(4))
    data.descends[Ifam(2)] = GenDescend(children=[Iper(7), Iper(8)])

    # Family 3: Paul + Claire (parents of Thomas/Léa)
    data.families[Ifam(3)] = GenFamily(relation=RelationKind.MARRIED)
    data.couples[Ifam(3)] = GenCouple(father=Iper(5), mother=Iper(6))
    data.descends[Ifam(3)] = GenDescend(children=[Iper(9), Iper(10)])

    # Family 4: Lucas + Emma (parents of Noah/Chloé)
    data.families[Ifam(4)] = GenFamily(relation=RelationKind.MARRIED)
    data.couples[Ifam(4)] = GenCouple(father=Iper(7), mother=Iper(8))
    data.descends[Ifam(4)] = GenDescend(children=[Iper(11), Iper(12)])

    # Add some strings (places, events, etc.)
    data.strings[1] = "Paris"
    data.strings[2] = "Lyon"
    data.strings[3] = "Marseille"
    data.strings[4] = "Naissance"
    data.strings[5] = "Mariage"
    data.strings[6] = "Décès"

    return data


def find_descendants(
    data: PickleBaseData, person_id: Iper, max_generations: int = 10
) -> list[Iper]:
    """Find all descendants of a person."""
    descendants = []
    to_process = [person_id]
    generation = 0

    while to_process and generation < max_generations:
        current_generation = to_process.copy()
        to_process = []

        for person in current_generation:
            # Find families where this person is a parent
            for family_id, couple in data.couples.items():
                if couple.father == person or couple.mother == person:
                    # Get children from this family
                    if family_id in data.descends:
                        children = data.descends[family_id].children
                        descendants.extend(children)
                        to_process.extend(children)

        generation += 1

    return descendants


def find_ancestors(
    data: PickleBaseData, person_id: Iper, max_generations: int = 10
) -> list[Iper]:
    """Find all ancestors of a person."""
    ancestors = []
    to_process = [person_id]
    generation = 0

    while to_process and generation < max_generations:
        current_generation = to_process.copy()
        to_process = []

        for person in current_generation:
            # Find families where this person is a child
            for family_id, descend in data.descends.items():
                if person in descend.children:
                    # Get parents from this family
                    if family_id in data.couples:
                        couple = data.couples[family_id]
                        if couple.father != Iper(0):  # Not dummy
                            ancestors.append(couple.father)
                            to_process.append(couple.father)
                        if couple.mother != Iper(0):  # Not dummy
                            ancestors.append(couple.mother)
                            to_process.append(couple.mother)

        generation += 1

    return ancestors


def demonstrate_search():
    """Demonstrate all search capabilities."""
    print("=== Genealogical Search Demonstration ===\n")

    # Create family tree
    data = create_family_tree()
    print(
        f"Created family tree with {data.persons_count} persons and {data.families_count} families"
    )

    # Build indexes
    print("\nBuilding search indexes...")
    data.build_indexes()
    print("Indexes built successfully!")

    # Demonstrate person searches
    print("\n=== Person Search Examples ===")

    # Search by first name
    jean_persons = data.search_persons_by_first_name("Jean")
    print(f"Persons with first name 'Jean': {len(jean_persons)}")
    for iper in jean_persons:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Search by surname
    dupont_persons = data.search_persons_by_surname("Dupont")
    print(f"\nPersons with surname 'Dupont': {len(dupont_persons)}")
    for iper in dupont_persons:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Search by full name
    pierre_dupont = data.search_persons_by_full_name("Pierre Dupont")
    print(f"\nPersons with full name 'Pierre Dupont': {len(pierre_dupont)}")
    for iper in pierre_dupont:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Demonstrate descendant search
    print("\n=== Descendant Search Examples ===")

    # Find descendants of Jean Dupont (grandfather)
    jean_descendants = find_descendants(data, Iper(1))
    print(f"Descendants of Jean Dupont: {len(jean_descendants)}")
    for iper in jean_descendants:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Find descendants of Pierre Dupont (father)
    pierre_descendants = find_descendants(data, Iper(3))
    print(f"\nDescendants of Pierre Dupont: {len(pierre_descendants)}")
    for iper in pierre_descendants:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Find descendants of Lucas Dupont (son)
    lucas_descendants = find_descendants(data, Iper(7))
    print(f"\nDescendants of Lucas Dupont: {len(lucas_descendants)}")
    for iper in lucas_descendants:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Demonstrate ancestor search
    print("\n=== Ancestor Search Examples ===")

    # Find ancestors of Noah Dupont (grandchild)
    noah_ancestors = find_ancestors(data, Iper(11))
    print(f"Ancestors of Noah Dupont: {len(noah_ancestors)}")
    for iper in noah_ancestors:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Demonstrate string searches
    print("\n=== String Search Examples ===")

    # Search strings by content
    paris_strings = data.search_strings_by_content("Paris")
    print(f"Strings containing 'Paris': {len(paris_strings)}")
    for istr in paris_strings:
        print(f"  - String ID {istr}: '{data.strings[istr]}'")

    # Complex genealogical queries
    print("\n=== Complex Queries ===")

    # Find all male descendants of Jean Dupont
    jean_male_descendants = []
    for iper in find_descendants(data, Iper(1)):
        person = data.persons[iper]
        if person.sex == Sex.MALE:
            jean_male_descendants.append(iper)

    print(f"Male descendants of Jean Dupont: {len(jean_male_descendants)}")
    for iper in jean_male_descendants:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Find all people with "Dupont" surname who are descendants of Jean
    dupont_descendants = []
    for iper in find_descendants(data, Iper(1)):
        person = data.persons[iper]
        if person.surname == "Dupont":
            dupont_descendants.append(iper)

    print(f"\nDupont descendants of Jean: {len(dupont_descendants)}")
    for iper in dupont_descendants:
        person = data.persons[iper]
        print(f"  - {person.first_name} {person.surname} (ID: {iper})")

    # Performance test
    print("\n=== Performance Test ===")
    import time

    # Test descendant search performance
    start_time = time.time()
    for _ in range(1000):
        find_descendants(data, Iper(1))
    descendant_time = time.time() - start_time

    # Test index search performance
    start_time = time.time()
    for _ in range(1000):
        data.search_persons_by_surname("Dupont")
    index_time = time.time() - start_time

    print(f"Descendant search (1000 iterations): {descendant_time:.4f}s")
    print(f"Index search (1000 iterations): {index_time:.4f}s")
    print(
        f"Index search is {descendant_time / index_time:.1f}x faster for simple lookups"
    )


if __name__ == "__main__":
    demonstrate_search()
