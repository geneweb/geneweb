"""
Test DatabaseWriter functionality.
Creates a sample genealogical database and writes it to disk.
"""

from pathlib import Path
import tempfile
import shutil
from geneweb import (
    GenPerson, GenFamily, GenCouple, GenDescend, GenAscend, GenUnion,
    Sex, RelationKind, Date, DatabaseWriter
)


def create_sample_database():
    """Create a sample family database for testing."""

    # String table
    strings = [
        "",           # 0 - empty
        "?",          # 1 - unknown
        "Jean",       # 2
        "MARTIN",     # 3
        "Marie",      # 4
        "DURAND",     # 5
        "Pierre",     # 6
        "Sophie",     # 7
        "Paris",      # 8
        "Lyon",       # 9
    ]

    # Create persons
    persons = []

    # Grandfather
    grandfather = GenPerson(
        first_name=2,  # Jean
        surname=3,     # MARTIN
        occ=0,
        sex=Sex.MALE,
        birth=Date(year=1920, month=1, day=15),
        birth_place=8,  # Paris
        key_index=0
    )
    persons.append(grandfather)

    # Grandmother
    grandmother = GenPerson(
        first_name=4,  # Marie
        surname=5,     # DURAND
        occ=0,
        sex=Sex.FEMALE,
        birth=Date(year=1922, month=3, day=20),
        birth_place=8,  # Paris
        key_index=1
    )
    persons.append(grandmother)

    # Father
    father = GenPerson(
        first_name=6,  # Pierre
        surname=3,     # MARTIN
        occ=0,
        sex=Sex.MALE,
        birth=Date(year=1950, month=6, day=10),
        birth_place=9,  # Lyon
        key_index=2
    )
    persons.append(father)

    # Mother
    mother = GenPerson(
        first_name=7,  # Sophie
        surname=5,     # DURAND
        occ=0,
        sex=Sex.FEMALE,
        birth=Date(year=1952, month=8, day=25),
        birth_place=9,  # Lyon
        key_index=3
    )
    persons.append(mother)

    # Child
    child = GenPerson(
        first_name=2,  # Jean
        surname=3,     # MARTIN
        occ=1,         # Jean MARTIN.1 (to distinguish from grandfather)
        sex=Sex.MALE,
        birth=Date(year=1980, month=12, day=5),
        birth_place=8,  # Paris
        key_index=4
    )
    persons.append(child)

    # Create families
    families = []
    couples = []
    descends = []

    # Grandparents family
    gp_family = GenFamily(
        marriage=Date(year=1945, month=5, day=1),
        marriage_place=8,  # Paris
        relation=RelationKind.MARRIED,
        fam_index=0
    )
    families.append(gp_family)
    couples.append(GenCouple(father=0, mother=1))
    descends.append(GenDescend(children=[2]))  # Father is child

    # Parents family
    parents_family = GenFamily(
        marriage=Date(year=1975, month=7, day=15),
        marriage_place=9,  # Lyon
        relation=RelationKind.MARRIED,
        fam_index=1
    )
    families.append(parents_family)
    couples.append(GenCouple(father=2, mother=3))
    descends.append(GenDescend(children=[4]))  # Child

    # Create ascendants and unions
    ascends = [
        GenAscend(parents=None, consang=0.0),  # Grandfather
        GenAscend(parents=None, consang=0.0),  # Grandmother
        GenAscend(parents=0, consang=0.0),     # Father (child of family 0)
        GenAscend(parents=None, consang=0.0),  # Mother
        GenAscend(parents=1, consang=0.0),     # Child (child of family 1)
    ]

    unions = [
        GenUnion(family=[0]),      # Grandfather in family 0
        GenUnion(family=[0]),      # Grandmother in family 0
        GenUnion(family=[1]),      # Father in family 1
        GenUnion(family=[1]),      # Mother in family 1
        GenUnion(family=[]),       # Child (no family yet)
    ]

    return {
        'strings': strings,
        'persons': persons,
        'families': families,
        'couples': couples,
        'descends': descends,
        'ascends': ascends,
        'unions': unions
    }


def test_database_writer():
    """Test DatabaseWriter with sample data."""
    print("=" * 70)
    print("DATABASE WRITER TEST")
    print("=" * 70)

    # Create test directory in project root
    project_root = Path(__file__).parent
    test_output_dir = project_root / "test_output"

    # Clean up if exists
    if test_output_dir.exists():
        shutil.rmtree(test_output_dir)

    test_output_dir.mkdir()
    base_path = test_output_dir / "test_base"
    base_path.mkdir()

    print(f"\n✓ Created test database at: {base_path}")
    print(f"   (Relative path: ./test_output/test_base/)")

    try:
        # Create sample data
        print("\n1. Creating sample database...")
        data = create_sample_database()

        print(f"   ✓ Created {len(data['persons'])} persons")
        print(f"   ✓ Created {len(data['families'])} families")
        print(f"   ✓ Created {len(data['strings'])} strings")

        # Initialize writer
        writer = DatabaseWriter(str(base_path), verbose=True)

        # Test string hash output
        print("\n2. Writing string hash table...")
        strings_hash_path = base_path / "strings.inx"
        writer.output_strings_hash(data['strings'], str(strings_hash_path))
        print(f"   ✓ String hash size: {strings_hash_path.stat().st_size} bytes")

        # Test name index creation
        print("\n3. Creating name indexes...")

        def get_misc_names(person):
            """Get all name variations for a person."""
            first = data['strings'][person.first_name]
            last = data['strings'][person.surname]
            return [
                f"{first} {last}",
                f"{last} {first}",
                first,
                last
            ]

        name_index = writer.make_name_index(data['persons'], get_misc_names)
        print(f"   ✓ Created name index with {len(name_index)} buckets")

        non_empty = sum(1 for bucket in name_index if bucket)
        print(f"   ✓ Non-empty buckets: {non_empty}")

        # Test surname index
        print("\n4. Creating surname index...")

        def get_surname(person):
            return person.surname

        def compare_surname(a, b):
            return (a > b) - (a < b)

        surname_idx_path = base_path / "snames.inx"
        surname_dat_path = base_path / "snames.dat"

        writer.output_name_index(
            data['persons'],
            get_surname,
            compare_surname,
            str(surname_idx_path),
            str(surname_dat_path)
        )

        print(f"   ✓ Surname index size: {surname_idx_path.stat().st_size} bytes")
        print(f"   ✓ Surname data size: {surname_dat_path.stat().st_size} bytes")

        # Test firstname index
        print("\n5. Creating firstname index...")

        def get_firstname(person):
            return person.first_name

        firstname_idx_path = base_path / "fnames.inx"
        firstname_dat_path = base_path / "fnames.dat"

        writer.output_name_index(
            data['persons'],
            get_firstname,
            compare_surname,
            str(firstname_idx_path),
            str(firstname_dat_path)
        )

        print(f"   ✓ Firstname index size: {firstname_idx_path.stat().st_size} bytes")
        print(f"   ✓ Firstname data size: {firstname_dat_path.stat().st_size} bytes")

        # Test particles file
        print("\n6. Writing particles file...")
        particles = ["de", "von", "van", "du", "de la"]
        particles_path = base_path / "particles.txt"
        writer.output_particles_file(particles, str(particles_path))
        print(f"   ✓ Particles file size: {particles_path.stat().st_size} bytes")

        # Display sample data
        print("\n7. Sample data created:")
        print("\n   Persons:")
        for person in data['persons']:
            first = data['strings'][person.first_name]
            last = data['strings'][person.surname]
            sex = person.sex.value if person.sex else "?"
            birth_year = person.birth.year if person.birth else "?"
            print(f"     [{person.key_index}] {first} {last}.{person.occ} ({sex}) born {birth_year}")

        print("\n   Families:")
        for i, family in enumerate(data['families']):
            couple = data['couples'][i]
            descend = data['descends'][i]
            father = data['persons'][couple.father]
            mother = data['persons'][couple.mother]

            father_name = f"{data['strings'][father.first_name]} {data['strings'][father.surname]}"
            mother_name = f"{data['strings'][mother.first_name]} {data['strings'][mother.surname]}"

            marriage_year = family.marriage.year if family.marriage else "?"

            print(f"     [{family.fam_index}] {father_name} × {mother_name} ({marriage_year})")
            print(f"        Children: {len(descend.children)}")

        # List created files
        print("\n8. Created files:")
        for file in sorted(base_path.glob("*")):
            size = file.stat().st_size
            rel_path = file.relative_to(project_root)
            print(f"   ✓ {file.name}: {size} bytes ({rel_path})")

        print("\n" + "=" * 70)
        print("✅ DATABASE WRITER TEST COMPLETED SUCCESSFULLY")
        print("=" * 70)
        print(f"\n📁 Test files are in: ./test_output/test_base/")
        print("   (Run 'rm -rf test_output' to clean up)")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()


def main():
    """Run database writer tests."""
    print("\n")
    print("╔" + "=" * 68 + "╗")
    print("║" + " " * 18 + "DatabaseWriter Test" + " " * 31 + "║")
    print("╚" + "=" * 68 + "╝")
    print()

    test_database_writer()

    print()


if __name__ == "__main__":
    main()
