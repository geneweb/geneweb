"""
Demonstration tests for GeneWeb database.

These tests demonstrate real-world usage scenarios:
- Building a complete family tree
- Searching and filtering
- Name variations and indexing
- Complex relationship queries
"""

import pytest
from pathlib import Path

from geneweb import (
    GenPerson, GenFamily, GenAscend, GenUnion, GenCouple, GenDescend,
    Sex, RelationKind, DivorceStatus, Date, Iper, Ifam, Istr,
    Collection, Marker
)
from geneweb.database import Base, BaseData, BaseFunc
from geneweb.utils import NameUtils


class TestBuildFamilyTree:
    """
    Demonstrate building a complete 3-generation family tree.

    Tree structure:
        Grandfather Jean MARTIN (1920) × Grandmother Marie DURAND (1922)
            |
            +-- Father Pierre MARTIN (1950) × Mother Sophie BERNARD (1952)
                    |
                    +-- Son Thomas MARTIN (1980)
                    +-- Daughter Julie MARTIN (1982)
    """

    @pytest.fixture
    def family_tree_base(self):
        """Build a complete 3-generation family tree."""
        # Initialize strings
        strings = [
            "",              # 0 - empty
            "?",             # 1 - quest
            "Jean",          # 2
            "MARTIN",        # 3
            "Marie",         # 4
            "DURAND",        # 5
            "Pierre",        # 6
            "Sophie",        # 7
            "BERNARD",       # 8
            "Thomas",        # 9
            "Julie",         # 10
            "Paris",         # 11
            "Lyon",          # 12
            "Marseille",     # 13
        ]

        # Use test_output directory in project
        from pathlib import Path
        project_root = Path(__file__).parent.parent
        test_db_path = project_root / "test_output" / "test_demo"

        data = BaseData(
            bdir=str(test_db_path),
            strings=strings,
            persons=[],
            ascends=[],
            unions=[],
            families=[],
            couples=[],
            descends=[]
        )
        func = BaseFunc(data)
        base = Base(data, func)

        # === GENERATION 1: Grandparents ===

        # Grandfather Jean MARTIN
        grandfather = GenPerson(
            first_name=2, surname=3, occ=0,  # Jean MARTIN.0
            sex=Sex.MALE,
            birth=Date(year=1920, month=3, day=15),
            birth_place=11,  # Paris
            key_index=0
        )
        base.patch_person(0, grandfather)
        base.patch_ascend(0, GenAscend())
        base.patch_union(0, GenUnion(family=[0]))

        # Grandmother Marie DURAND
        grandmother = GenPerson(
            first_name=4, surname=5, occ=0,  # Marie DURAND.0
            sex=Sex.FEMALE,
            birth=Date(year=1922, month=8, day=20),
            birth_place=11,  # Paris
            key_index=1
        )
        base.patch_person(1, grandmother)
        base.patch_ascend(1, GenAscend())
        base.patch_union(1, GenUnion(family=[0]))

        # Grandparents' family
        gp_family = GenFamily(
            marriage=Date(year=1945, month=6, day=15),
            marriage_place=11,  # Paris
            relation=RelationKind.MARRIED,
            divorce=DivorceStatus.NOT_DIVORCED,
            fam_index=0
        )
        base.patch_family(0, gp_family)
        base.patch_couple(0, GenCouple(father=0, mother=1))
        base.patch_descend(0, GenDescend(children=[2]))  # Pierre

        # === GENERATION 2: Parents ===

        # Father Pierre MARTIN
        father = GenPerson(
            first_name=6, surname=3, occ=0,  # Pierre MARTIN.0
            sex=Sex.MALE,
            birth=Date(year=1950, month=11, day=5),
            birth_place=12,  # Lyon
            key_index=2
        )
        base.patch_person(2, father)
        base.patch_ascend(2, GenAscend(parents=0))  # Child of family 0
        base.patch_union(2, GenUnion(family=[1]))

        # Mother Sophie BERNARD
        mother = GenPerson(
            first_name=7, surname=8, occ=0,  # Sophie BERNARD.0
            sex=Sex.FEMALE,
            birth=Date(year=1952, month=2, day=18),
            birth_place=12,  # Lyon
            key_index=3
        )
        base.patch_person(3, mother)
        base.patch_ascend(3, GenAscend())
        base.patch_union(3, GenUnion(family=[1]))

        # Parents' family
        parents_family = GenFamily(
            marriage=Date(year=1975, month=9, day=20),
            marriage_place=12,  # Lyon
            relation=RelationKind.MARRIED,
            divorce=DivorceStatus.NOT_DIVORCED,
            fam_index=1
        )
        base.patch_family(1, parents_family)
        base.patch_couple(1, GenCouple(father=2, mother=3))
        base.patch_descend(1, GenDescend(children=[4, 5]))  # Thomas & Julie

        # === GENERATION 3: Children ===

        # Son Thomas MARTIN
        son = GenPerson(
            first_name=9, surname=3, occ=0,  # Thomas MARTIN.0
            sex=Sex.MALE,
            birth=Date(year=1980, month=5, day=12),
            birth_place=13,  # Marseille
            key_index=4
        )
        base.patch_person(4, son)
        base.patch_ascend(4, GenAscend(parents=1))  # Child of family 1
        base.patch_union(4, GenUnion(family=[]))

        # Daughter Julie MARTIN
        daughter = GenPerson(
            first_name=10, surname=3, occ=0,  # Julie MARTIN.0
            sex=Sex.FEMALE,
            birth=Date(year=1982, month=7, day=8),
            birth_place=13,  # Marseille
            key_index=5
        )
        base.patch_person(5, daughter)
        base.patch_ascend(5, GenAscend(parents=1))  # Child of family 1
        base.patch_union(5, GenUnion(family=[]))

        return base

    def test_family_tree_structure(self, family_tree_base):
        """Test the complete family tree structure."""
        base = family_tree_base

        # Verify counts
        assert base.nb_of_persons() == 6
        assert base.nb_of_families() == 2

        print("\n=== Family Tree Structure ===")
        print(f"Total persons: {base.nb_of_persons()}")
        print(f"Total families: {base.nb_of_families()}")

    def test_query_grandparents(self, family_tree_base):
        """Query grandparents information."""
        base = family_tree_base

        grandfather = base.poi(0)
        grandmother = base.poi(1)

        print("\n=== Grandparents ===")
        print(f"Grandfather: {base.p_first_name(grandfather)} "
              f"{base.p_surname(grandfather)}")
        print(f"  Born: {grandfather.get_birth().year}")
        print(f"  Sex: {grandfather.get_sex().value}")

        print(f"Grandmother: {base.p_first_name(grandmother)} "
              f"{base.p_surname(grandmother)}")
        print(f"  Born: {grandmother.get_birth().year}")
        print(f"  Sex: {grandmother.get_sex().value}")

        # Verify marriage
        gp_family = base.foi(0)
        assert gp_family.get_father() == 0
        assert gp_family.get_mother() == 1
        assert gp_family.get_marriage().year == 1945

        print(f"\nMarriage: {gp_family.get_marriage().year}")

    def test_query_children_of_grandparents(self, family_tree_base):
        """Query children of grandparents."""
        base = family_tree_base

        grandfather = base.poi(0)
        children = base.children_of_person(grandfather)

        print("\n=== Children of Grandfather ===")
        assert len(children) == 1

        for child_id in children:
            child = base.poi(child_id)
            print(f"  - {base.p_first_name(child)} {base.p_surname(child)} "
                  f"(born {child.get_birth().year})")

    def test_query_grandchildren(self, family_tree_base):
        """Query grandchildren."""
        base = family_tree_base

        # Get grandfather's grandchildren through his children
        grandfather = base.poi(0)
        children = base.children_of_person(grandfather)

        grandchildren = []
        for child_id in children:
            child = base.poi(child_id)
            grandchildren.extend(base.children_of_person(child))

        print("\n=== Grandchildren ===")
        assert len(grandchildren) == 2

        for gc_id in grandchildren:
            gc = base.poi(gc_id)
            print(f"  - {base.p_first_name(gc)} {base.p_surname(gc)} "
                  f"(born {gc.get_birth().year})")

    def test_query_siblings(self, family_tree_base):
        """Query siblings."""
        base = family_tree_base

        son = base.poi(4)  # Thomas
        ascend = son.gen_ascend()

        print("\n=== Siblings of Thomas ===")

        if ascend.parents is not None:
            family = base.foi(ascend.parents)
            children = family.get_children()
            siblings = [c for c in children if c != son.iper]

            assert len(siblings) == 1

            for sib_id in siblings:
                sib = base.poi(sib_id)
                print(f"  - {base.p_first_name(sib)} {base.p_surname(sib)} "
                      f"(born {sib.get_birth().year})")

    def test_query_ancestors(self, family_tree_base):
        """Query all ancestors of a person."""
        base = family_tree_base

        son = base.poi(4)  # Thomas

        print("\n=== Ancestors of Thomas ===")

        # Get parents
        ascend = son.gen_ascend()
        if ascend.parents is not None:
            parents_family = base.foi(ascend.parents)
            father_id = parents_family.get_father()
            mother_id = parents_family.get_mother()

            father = base.poi(father_id)
            mother = base.poi(mother_id)

            print(f"\nParents:")
            print(f"  Father: {base.p_first_name(father)} {base.p_surname(father)}")
            print(f"  Mother: {base.p_first_name(mother)} {base.p_surname(mother)}")

            # Get grandparents through father
            father_ascend = father.gen_ascend()
            if father_ascend.parents is not None:
                gp_family = base.foi(father_ascend.parents)
                gf = base.poi(gp_family.get_father())
                gm = base.poi(gp_family.get_mother())

                print(f"\nGrandparents:")
                print(f"  Grandfather: {base.p_first_name(gf)} {base.p_surname(gf)}")
                print(f"  Grandmother: {base.p_first_name(gm)} {base.p_surname(gm)}")

class TestCollectionOperations:
    """Demonstrate collection operations on family data."""

    @pytest.fixture
    def base_with_multiple_persons(self):
        """Create base with multiple persons for collection tests."""
        strings = ["", "?"] + [f"Name{i}" for i in range(20)]

        # Use test_output directory in project
        from pathlib import Path
        project_root = Path(__file__).parent.parent
        test_db_path = project_root / "test_output" / "test_collections"

        data = BaseData(
            bdir=str(test_db_path),
            strings=strings,
            persons=[],
            ascends=[],
            unions=[],
            families=[],
            couples=[],
            descends=[]
        )
        func = BaseFunc(data)
        base = Base(data, func)

        # Create 10 persons
        for i in range(10):
            person = GenPerson(
                first_name=2+i,
                surname=12,
                occ=i,
                sex=Sex.MALE if i % 2 == 0 else Sex.FEMALE,
                birth=Date(year=1950 + i * 3),
                key_index=i
            )
            base.patch_person(i, person)
            base.patch_ascend(i, GenAscend())
            base.patch_union(i, GenUnion(family=[]))

        return base

    def test_iterate_all_persons(self, base_with_multiple_persons):
        """Iterate over all persons in collection."""
        base = base_with_multiple_persons

        print("\n=== All Persons ===")

        persons = base.persons()
        count = 0

        def print_person(p):
            nonlocal count
            count += 1
            print(f"  {count}. {base.p_first_name(p)} {base.p_surname(p)} "
                  f"({p.get_sex().value})")

        persons.iter(print_person)
        assert count == 10

    def test_filter_by_sex(self, base_with_multiple_persons):
        """Filter persons by sex."""
        base = base_with_multiple_persons

        print("\n=== Male Persons Only ===")

        persons = base.persons()
        males = []

        persons.iter(lambda p: males.append(p) if p.get_sex() == Sex.MALE else None)

        assert len(males) == 5

        for person in males:
            print(f"  - {base.p_first_name(person)} {base.p_surname(person)}")

    def test_count_by_decade(self, base_with_multiple_persons):
        """Count persons by birth decade."""
        base = base_with_multiple_persons

        print("\n=== Persons by Birth Decade ===")

        persons = base.persons()
        decades = {}

        def count_decade(p):
            year = p.get_birth().year
            if year:
                decade = (year // 10) * 10
                decades[decade] = decades.get(decade, 0) + 1

        persons.iter(count_decade)

        for decade in sorted(decades.keys()):
            print(f"  {decade}s: {decades[decade]} persons")

    def test_fold_operations(self, base_with_multiple_persons):
        """Demonstrate fold operations."""
        base = base_with_multiple_persons

        print("\n=== Fold Operations ===")

        ipers = base.ipers()

        # Count total
        count = ipers.fold(lambda acc, _: acc + 1, 0)
        print(f"Total count: {count}")
        assert count == 10

        # Sum of IDs
        sum_ids = ipers.fold(lambda acc, i: acc + i, 0)
        print(f"Sum of IDs: {sum_ids}")
        assert sum_ids == sum(range(10))

        # Collect even IDs
        evens = ipers.fold(
            lambda acc, i: acc + [i] if i % 2 == 0 else acc,
            []
        )
        print(f"Even IDs: {evens}")
        assert len(evens) == 5


class TestMarkerOperations:
    """Demonstrate marker usage for traversal algorithms."""

    @pytest.fixture
    def simple_base(self):
        """Create simple base for marker tests."""
        # Use test_output directory in project
        from pathlib import Path
        project_root = Path(__file__).parent.parent
        test_db_path = project_root / "test_output" / "test_markers"

        data = BaseData(
            bdir=str(test_db_path),
            strings=["", "?", "Test"],
            persons=[],
            ascends=[],
            unions=[],
            families=[],
            couples=[],
            descends=[]
        )
        func = BaseFunc(data)
        base = Base(data, func)

        # Create 5 persons
        for i in range(5):
            person = GenPerson(
                first_name=2, surname=2, occ=i,
                sex=Sex.MALE, key_index=i
            )
            base.patch_person(i, person)
            base.patch_ascend(i, GenAscend())
            base.patch_union(i, GenUnion(family=[]))

        return base

    def test_visited_marker(self, simple_base):
        """Use marker to track visited persons."""
        base = simple_base

        print("\n=== Visited Marker ===")

        ipers = base.ipers()
        visited = Marker(lambda i: i, ipers, False)

        # Visit some persons
        visited.set(0, True)
        visited.set(2, True)
        visited.set(4, True)

        # Check visited
        for i in range(5):
            status = "visited" if visited.get(i) else "not visited"
            print(f"  Person {i}: {status}")

            if i in [0, 2, 4]:
                assert visited.get(i) == True
            else:
                assert visited.get(i) == False

    def test_distance_marker(self, simple_base):
        """Use marker to track distances in graph traversal."""
        base = simple_base

        print("\n=== Distance Marker ===")

        ipers = base.ipers()
        distances = Marker(lambda i: i, ipers, -1)

        # Set distances from root
        distances.set(0, 0)   # Root
        distances.set(1, 1)   # Distance 1
        distances.set(2, 1)   # Distance 1
        distances.set(3, 2)   # Distance 2
        distances.set(4, 2)   # Distance 2

        # Display distances
        for i in range(5):
            dist = distances.get(i)
            if dist == -1:
                print(f"  Person {i}: not reachable")
            else:
                print(f"  Person {i}: distance {dist}")


class TestNameOperations:
    """Demonstrate name normalization and searching."""

    def test_name_variations(self):
        """Test name normalization and variations."""
        print("\n=== Name Normalization ===")

        names = [
            "Jean-François de La Fontaine",
            "Marie-Antoinette",
            "José María García",
            "François-Xavier O'Brien",
        ]

        for name in names:
            normalized = NameUtils.normalize(name)
            crushed = NameUtils.crush_lower(name)

            print(f"\nOriginal:   '{name}'")
            print(f"Normalized: '{normalized}'")
            print(f"Crushed:    '{crushed}'")

    def test_name_splitting(self):
        """Test name splitting for indexing."""
        print("\n=== Name Splitting ===")

        first_names = [
            "Jean-François",
            "Marie Antoinette",
            "José María",
        ]

        for name in first_names:
            parts = NameUtils.split_fname(name)
            print(f"'{name}' -> {parts}")

        surnames = [
            "de La Fontaine",
            "von Habsburg",
            "van den Berg",
        ]

        for name in surnames:
            parts = NameUtils.split_sname(name)
            print(f"'{name}' -> {parts}")


if __name__ == "__main__":
    pytest.main([__file__, "-v", "-s"])
