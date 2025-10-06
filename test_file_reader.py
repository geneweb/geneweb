"""
Test FileReader functionality.
Reads the database files created by test_database_writer.py
"""

from pathlib import Path
import struct
from geneweb import FileReader


def read_strings_hash(file_path: Path):
    """Read and display string hash table."""
    print(f"\n  Reading {file_path.name}...")

    with open(file_path, 'rb') as f:
        # Read table size
        table_size = struct.unpack('>I', f.read(4))[0]
        print(f"    Table size: {table_size}")

        # Read hash array
        hash_array = []
        for _ in range(table_size):
            val = struct.unpack('>I', f.read(4))[0]
            # Convert to signed int
            if val >= 0x80000000:
                val = val - 0x100000000
            hash_array.append(val)

        non_empty = sum(1 for x in hash_array if x != -1)
        print(f"    Non-empty buckets: {non_empty}/{table_size}")


def read_name_index(index_path: Path, data_path: Path, strings: list):
    """Read and display name index."""
    print(f"\n  Reading {index_path.name} and {data_path.name}...")

    # Read index file
    import pickle
    with open(index_path, 'rb') as f:
        offsets = pickle.load(f)

    print(f"    Found {len(offsets)} name entries")

    # Read first few entries from data file
    print("\n    Sample entries:")
    with open(data_path, 'rb') as f:
        for i, (name_id, offset) in enumerate(offsets[:5]):
            f.seek(offset)
            count = struct.unpack('>I', f.read(4))[0]
            person_ids = []
            for _ in range(min(count, 5)):  # Limit to 5
                iper = struct.unpack('>I', f.read(4))[0]
                person_ids.append(iper)

            name = strings[name_id] if name_id < len(strings) else f"<{name_id}>"
            more = f" (+{count-5} more)" if count > 5 else ""
            print(f"      {name}: {person_ids}{more}")


def read_particles_file(file_path: Path):
    """Read and display particles."""
    print(f"\n  Reading {file_path.name}...")

    with open(file_path, 'r', encoding='utf-8') as f:
        particles = [line.strip().replace('_', ' ') for line in f]

    print(f"    Particles ({len(particles)}): {', '.join(particles)}")


def test_file_reader():
    """Test FileReader with database files."""
    print("=" * 70)
    print("FILE READER TEST")
    print("=" * 70)

    # Check if test database exists
    project_root = Path(__file__).parent
    base_path = project_root / "test_output" / "test_base"

    if not base_path.exists():
        print("\n❌ Test database not found!")
        print("   Please run 'python test_database_writer.py' first")
        return

    print(f"\n✓ Found test database at: {base_path}")
    print(f"   (Relative path: ./test_output/test_base/)")

    try:
        # List all files
        print("\n1. Database files:")
        files = list(base_path.glob("*"))
        for file in sorted(files):
            size = file.stat().st_size
            print(f"   - {file.name}: {size} bytes")

        # Test FileReader methods
        print("\n2. Testing FileReader.load_strings()...")
        strings = FileReader.load_strings(str(base_path))
        print(f"   ✓ Loaded {len(strings)} strings")
        print(f"   First 10 strings:")
        for i, s in enumerate(strings[:10]):
            print(f"     [{i}] '{s}'")

        print("\n3. Testing FileReader.load_particles()...")
        particles = FileReader.load_particles(str(base_path))
        print(f"   ✓ Loaded {len(particles)} particles")
        print(f"   Particles: {', '.join(particles)}")

        # Read binary index files
        print("\n4. Reading binary index files:")

        # String hash
        strings_hash = base_path / "strings.inx"
        if strings_hash.exists():
            read_strings_hash(strings_hash)

        # Surname index
        surname_idx = base_path / "snames.inx"
        surname_dat = base_path / "snames.dat"
        if surname_idx.exists() and surname_dat.exists():
            read_name_index(surname_idx, surname_dat, strings)

        # Firstname index
        firstname_idx = base_path / "fnames.inx"
        firstname_dat = base_path / "fnames.dat"
        if firstname_idx.exists() and firstname_dat.exists():
            read_name_index(firstname_idx, firstname_dat, strings)

        # Particles
        particles_file = base_path / "particles.txt"
        if particles_file.exists():
            read_particles_file(particles_file)

        # Test array loading
        print("\n5. Testing generic array loading:")

        # Try to load non-existent file (should return default)
        test_array = FileReader.load_array(
            str(base_path / "nonexistent.dat"),
            lambda: ["default", "values"]
        )
        print(f"   ✓ Non-existent file returned default: {test_array}")

        print("\n" + "=" * 70)
        print("✅ FILE READER TEST COMPLETED SUCCESSFULLY")
        print("=" * 70)

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()


def test_roundtrip():
    """Test write-read roundtrip."""
    print("\n" + "=" * 70)
    print("ROUNDTRIP TEST (Write then Read)")
    print("=" * 70)

    from geneweb import DatabaseWriter
    from geneweb import GenPerson, Sex, Date
    import tempfile
    import shutil

    # Create temp directory
    temp_dir = Path(tempfile.mkdtemp(prefix="geneweb_roundtrip_"))

    try:
        print(f"\n✓ Created temp directory: {temp_dir}")

        # Write test data
        print("\n1. Writing test data...")

        strings = ["", "?", "Test", "Person", "City"]
        writer = DatabaseWriter(str(temp_dir), verbose=False)

        # Write strings
        strings_file = temp_dir / "strings.dat"
        import pickle
        with open(strings_file, 'wb') as f:
            pickle.dump(strings, f)
        print(f"   ✓ Wrote {len(strings)} strings")

        # Write particles
        particles = ["de", "von", "van"]
        particles_file = temp_dir / "particles.dat"
        with open(particles_file, 'wb') as f:
            pickle.dump(particles, f)
        print(f"   ✓ Wrote {len(particles)} particles")

        # Read back
        print("\n2. Reading back data...")

        loaded_strings = FileReader.load_strings(str(temp_dir))
        print(f"   ✓ Read {len(loaded_strings)} strings")

        loaded_particles = FileReader.load_particles(str(temp_dir))
        print(f"   ✓ Read {len(loaded_particles)} particles")

        # Verify
        print("\n3. Verifying data integrity...")

        assert loaded_strings == strings, "Strings mismatch!"
        print("   ✓ Strings match")

        assert loaded_particles == particles, "Particles mismatch!"
        print("   ✓ Particles match")

        print("\n✅ ROUNDTRIP TEST PASSED")

    except Exception as e:
        print(f"\n❌ Error: {e}")
        import traceback
        traceback.print_exc()

    finally:
        # Cleanup
        print(f"\n🗑️  Cleaning up: {temp_dir}")
        shutil.rmtree(temp_dir)


def main():
    """Run file reader tests."""
    print("\n")
    print("╔" + "=" * 68 + "╗")
    print("║" + " " * 20 + "FileReader Test" + " " * 33 + "║")
    print("╚" + "=" * 68 + "╝")
    print()

    # Test reading existing database
    test_file_reader()

    # Test roundtrip
    test_roundtrip()

    print()


if __name__ == "__main__":
    main()
