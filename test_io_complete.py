"""
Complete I/O test: Write database with DatabaseWriter, then read with FileReader.
"""

from pathlib import Path
import shutil

print("\n")
print("╔" + "=" * 68 + "╗")
print("║" + " " * 18 + "Complete I/O Test" + " " * 33 + "║")
print("╚" + "=" * 68 + "╝")
print()

print("This test will:")
print("  1. Create a test database with DatabaseWriter")
print("  2. Read it back with FileReader")
print("  3. Verify data integrity")
print()

# Step 1: Write database
print("=" * 70)
print("STEP 1: Writing database...")
print("=" * 70)

import test_database_writer
test_database_writer.test_database_writer()

# Step 2: Read database
print("\n\n")
print("=" * 70)
print("STEP 2: Reading database...")
print("=" * 70)

import test_file_reader
test_file_reader.test_file_reader()

# Summary
print("\n\n")
print("=" * 70)
print("✅ COMPLETE I/O TEST FINISHED")
print("=" * 70)
print()
print("Summary:")
print("  ✓ Database written successfully")
print("  ✓ Database read successfully")
print("  ✓ All data verified")
print()
print("📁 Test database remains at: ./test_output/test_base/")
print("   Run 'rm -rf test_output' to clean up")
print()
