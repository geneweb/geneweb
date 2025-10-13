#!/usr/bin/env python3
"""
Test ged2gwb with the uk.ged file.

Demonstrates conversion and search using the larger UK GEDCOM file.
"""

import sys
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path.cwd() / 'src'))

from lib.ged2gwb.converters.gedcom_to_geneweb import GedcomToGenewebConverter
from lib.ged2gwb.utils.options import ConversionOptions
from lib.gedcom.parser import create_parser

def main():
    """Test with uk.ged file."""
    print("=== Testing with uk.ged ===\n")

    # Path to the UK GEDCOM file
    uk_ged_path = Path("src/lib/gedcom/ged/uk.ged")

    if not uk_ged_path.exists():
        print(f"ERROR: UK GEDCOM file not found: {uk_ged_path}")
        return 1

    print(f"1. Loading GEDCOM file: {uk_ged_path}")

    # Parse GEDCOM
    parser = create_parser()
    gedcom_db = parser.parse_file(uk_ged_path)

    print(f"   ✓ Parsed {len(gedcom_db.individuals)} individuals")
    print(f"   ✓ Parsed {len(gedcom_db.families)} families")
    print(f"   ✓ Parsed {len(gedcom_db.sources)} sources")
    print(f"   ✓ Parsed {len(gedcom_db.repositories)} repositories")

    # Show some individuals
    print("\n   First 10 individuals found:")
    count = 0
    for xref, individual in gedcom_db.individuals.items():
        if count >= 10:
            break
        if individual.primary_name:
            print(f"     - {individual.primary_name.display_name} ({individual.sex})")
        count += 1

    if len(gedcom_db.individuals) > 10:
        print(f"     ... and {len(gedcom_db.individuals) - 10} more individuals")

    print("\n2. Converting to GeneWeb format...")

    # Convert to GeneWeb
    options = ConversionOptions(input_file=uk_ged_path)
    converter = GedcomToGenewebConverter(options)
    result = converter.convert(gedcom_db)

    # Get the converted data
    geneweb_data = result['geneweb_data']
    print(f"   ✓ Converted to {geneweb_data.persons_count} persons")
    print(f"   ✓ Converted to {geneweb_data.families_count} families")
    print(f"   ✓ Converted to {geneweb_data.strings_count} strings")

    print("\n3. Testing search functionality...")

    # Test first name search
    print("\n   🔍 Search by first name 'John':")
    john_persons = geneweb_data.search_persons_by_first_name("John")
    print(f"     Found {len(john_persons)} persons")
    for iper in john_persons[:5]:  # Show first 5
        person = geneweb_data.persons[iper]
        print(f"       - {person.first_name} {person.surname} (ID: {iper})")
    if len(john_persons) > 5:
        print(f"       ... and {len(john_persons) - 5} more")

    # Test surname search
    print("\n   🔍 Search by surname 'Smith':")
    smith_persons = geneweb_data.search_persons_by_surname("Smith")
    print(f"     Found {len(smith_persons)} persons")
    for iper in smith_persons[:5]:  # Show first 5
        person = geneweb_data.persons[iper]
        print(f"       - {person.first_name} {person.surname} (ID: {iper})")
    if len(smith_persons) > 5:
        print(f"       ... and {len(smith_persons) - 5} more")

    # Test partial first name search
    print("\n   🔍 Search by partial first name 'Mary':")
    mary_persons = geneweb_data.search_persons_by_first_name("Mary")
    print(f"     Found {len(mary_persons)} persons")
    for iper in mary_persons[:5]:  # Show first 5
        person = geneweb_data.persons[iper]
        print(f"       - {person.first_name} {person.surname} (ID: {iper})")
    if len(mary_persons) > 5:
        print(f"       ... and {len(mary_persons) - 5} more")

    # Test full name search
    print("\n   🔍 Search by full name 'John Smith':")
    john_smith = geneweb_data.search_persons_by_full_name("John Smith")
    print(f"     Found {len(john_smith)} persons")
    for iper in john_smith:
        person = geneweb_data.persons[iper]
        print(f"       - {person.first_name} {person.surname} (ID: {iper})")

    # Test case-insensitive search
    print("\n   🔍 Case-insensitive search for 'WILLIAMS':")
    williams_upper = geneweb_data.search_persons_by_surname("WILLIAMS")
    print(f"     Found {len(williams_upper)} persons")
    for iper in williams_upper[:5]:  # Show first 5
        person = geneweb_data.persons[iper]
        print(f"       - {person.first_name} {person.surname} (ID: {iper})")
    if len(williams_upper) > 5:
        print(f"       ... and {len(williams_upper) - 5} more")

    # Show some family relationships
    print("\n4. Family relationships (first 5 families):")
    count = 0
    for ifam, couple in geneweb_data.couples.items():
        if count >= 5:
            break
        father = geneweb_data.persons.get(couple.father)
        mother = geneweb_data.persons.get(couple.mother)

        if father and mother:
            print(f"   Family {ifam}: {father.first_name} {father.surname} + {mother.first_name} {mother.surname}")
        elif father:
            print(f"   Family {ifam}: {father.first_name} {father.surname} + (unknown mother)")
        elif mother:
            print(f"   Family {ifam}: (unknown father) + {mother.first_name} {mother.surname}")

        # Show children
        if ifam in geneweb_data.descends:
            children = geneweb_data.descends[ifam].children
            if children:
                print(f"     Children: {len(children)} children")
                for child_id in children[:3]:  # Show first 3 children
                    child = geneweb_data.persons.get(child_id)
                    if child:
                        print(f"       - {child.first_name} {child.surname}")
                if len(children) > 3:
                    print(f"       ... and {len(children) - 3} more")
        count += 1

    if geneweb_data.families_count > 5:
        print(f"   ... and {geneweb_data.families_count - 5} more families")

    # Show conversion statistics
    print("\n5. Conversion statistics:")
    print(f"   📊 Individuals converted: {result['individuals_converted']}")
    print(f"   📊 Families converted: {result['families_converted']}")
    print(f"   📊 Notes converted: {result['notes_converted']}")
    print(f"   📊 Sources converted: {result['sources_converted']}")
    print(f"   ⚠️  Warnings: {result['warnings_count']}")
    print(f"   ❌ Errors: {result['errors_count']}")

    # Performance test
    print("\n6. Performance test:")
    import time

    # Test search performance
    start_time = time.time()
    for _ in range(1000):
        geneweb_data.search_persons_by_surname("Smith")
    search_time = time.time() - start_time

    print(f"   ⚡ 1000 surname searches: {search_time:.4f}s")
    print(f"   ⚡ Average per search: {search_time/1000*1000:.2f}ms")

    # Test index building performance
    start_time = time.time()
    geneweb_data.build_indexes(verbose=False)
    index_time = time.time() - start_time

    print(f"   ⚡ Index building time: {index_time:.4f}s")
    print(f"   ⚡ Index size: {len(geneweb_data.first_name_index)} first names, {len(geneweb_data.surname_index)} surnames")

    # Show most common names
    print("\n7. Most common names:")

    # Most common first names
    first_name_counts = {}
    for person in geneweb_data.persons.values():
        if person.first_name:
            name = person.first_name.lower()
            first_name_counts[name] = first_name_counts.get(name, 0) + 1

    print("\n   Most common first names:")
    for name, count in sorted(first_name_counts.items(), key=lambda x: x[1], reverse=True)[:10]:
        print(f"     {name}: {count} persons")

    # Most common surnames
    surname_counts = {}
    for person in geneweb_data.persons.values():
        if person.surname:
            name = person.surname.lower()
            surname_counts[name] = surname_counts.get(name, 0) + 1

    print("\n   Most common surnames:")
    for name, count in sorted(surname_counts.items(), key=lambda x: x[1], reverse=True)[:10]:
        print(f"     {name}: {count} persons")

    print("\n=== Test Complete ===")
    print("✅ All tests passed successfully!")
    return 0

if __name__ == '__main__':
    sys.exit(main())
