#!/usr/bin/env python3
"""
Compare conversion results between sample.ged and uk.ged.

Shows the differences in performance and data between small and large GEDCOM files.
"""

import sys
import time
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path.cwd() / "src"))

from ged2gwb.converters.gedcom_to_geneweb import GedcomToGenewebConverter
from ged2gwb.utils.options import ConversionOptions
from gedcom.parser import create_parser


def test_file(gedcom_path, file_name):
    """Test a GEDCOM file and return statistics."""
    print(f"\n=== Testing {file_name} ===")

    # Parse GEDCOM
    start_time = time.time()
    parser = create_parser()
    gedcom_db = parser.parse_file(gedcom_path)
    parse_time = time.time() - start_time

    print(f"   📁 File: {gedcom_path}")
    print(f"   ⏱️  Parse time: {parse_time:.4f}s")
    print(f"   👥 Individuals: {len(gedcom_db.individuals)}")
    print(f"   👨‍👩‍👧‍👦 Families: {len(gedcom_db.families)}")
    print(f"   📚 Sources: {len(gedcom_db.sources)}")
    print(f"   📝 Notes: {len(gedcom_db.notes)}")

    # Convert to GeneWeb
    start_time = time.time()
    options = ConversionOptions(input_file=gedcom_path)
    converter = GedcomToGenewebConverter(options)
    result = converter.convert(gedcom_db)
    convert_time = time.time() - start_time

    geneweb_data = result["geneweb_data"]

    print(f"   ⏱️  Convert time: {convert_time:.4f}s")
    print(f"   👥 Converted persons: {geneweb_data.persons_count}")
    print(f"   👨‍👩‍👧‍👦 Converted families: {geneweb_data.families_count}")
    print(f"   📝 Converted notes: {result['notes_converted']}")
    print(f"   📚 Converted sources: {result['sources_converted']}")
    print(f"   ⚠️  Warnings: {result['warnings_count']}")
    print(f"   ❌ Errors: {result['errors_count']}")

    # Test search performance
    start_time = time.time()
    for _ in range(1000):
        geneweb_data.search_persons_by_surname("Smith")
    search_time = time.time() - start_time

    print(f"   ⚡ 1000 searches: {search_time:.4f}s")
    print(f"   ⚡ Avg per search: {search_time / 1000 * 1000:.2f}ms")

    # Test index building
    start_time = time.time()
    geneweb_data.build_indexes(verbose=False)
    index_time = time.time() - start_time

    print(f"   🔍 Index build time: {index_time:.4f}s")
    print(f"   🔍 First names indexed: {len(geneweb_data.first_name_index)}")
    print(f"   🔍 Surnames indexed: {len(geneweb_data.surname_index)}")

    # Show some search results
    print(
        f"   🔍 Search 'John': {len(geneweb_data.search_persons_by_first_name('John'))} results"
    )
    print(
        f"   🔍 Search 'Smith': {len(geneweb_data.search_persons_by_surname('Smith'))} results"
    )
    print(
        f"   🔍 Search 'Williams': {len(geneweb_data.search_persons_by_surname('Williams'))} results"
    )

    return {
        "file_name": file_name,
        "parse_time": parse_time,
        "convert_time": convert_time,
        "total_time": parse_time + convert_time,
        "individuals": len(gedcom_db.individuals),
        "families": len(gedcom_db.families),
        "sources": len(gedcom_db.sources),
        "notes": len(gedcom_db.notes),
        "converted_persons": geneweb_data.persons_count,
        "converted_families": geneweb_data.families_count,
        "warnings": result["warnings_count"],
        "errors": result["errors_count"],
        "search_time": search_time,
        "index_time": index_time,
        "first_names": len(geneweb_data.first_name_index),
        "surnames": len(geneweb_data.surname_index),
    }


def main():
    """Compare sample.ged and uk.ged."""
    print("=== GEDCOM File Comparison ===\n")

    # Test files
    files = [
        ("src/lib/gedcom/ged/sample.ged", "sample.ged"),
        ("src/lib/gedcom/ged/uk.ged", "uk.ged"),
    ]

    results = []

    for gedcom_path, file_name in files:
        if Path(gedcom_path).exists():
            result = test_file(gedcom_path, file_name)
            results.append(result)
        else:
            print(f"❌ File not found: {gedcom_path}")

    # Comparison summary
    if len(results) >= 2:
        print("\n=== Comparison Summary ===")

        sample = results[0]
        uk = results[1]

        print(f"\n📊 Data Size Comparison:")
        print(
            f"   Individuals: {sample['individuals']:,} vs {uk['individuals']:,} ({uk['individuals'] / sample['individuals']:.1f}x larger)"
        )
        print(
            f"   Families: {sample['families']:,} vs {uk['families']:,} ({uk['families'] / sample['families']:.1f}x larger)"
        )
        print(
            f"   Sources: {sample['sources']:,} vs {uk['sources']:,} ({uk['sources'] / sample['sources']:.1f}x larger)"
        )

        print(f"\n⏱️  Performance Comparison:")
        print(
            f"   Parse time: {sample['parse_time']:.4f}s vs {uk['parse_time']:.4f}s ({uk['parse_time'] / sample['parse_time']:.1f}x slower)"
        )
        print(
            f"   Convert time: {sample['convert_time']:.4f}s vs {uk['convert_time']:.4f}s ({uk['convert_time'] / sample['convert_time']:.1f}x slower)"
        )
        print(
            f"   Total time: {sample['total_time']:.4f}s vs {uk['total_time']:.4f}s ({uk['total_time'] / sample['total_time']:.1f}x slower)"
        )

        print(f"\n🔍 Search Performance:")
        print(
            f"   1000 searches: {sample['search_time']:.4f}s vs {uk['search_time']:.4f}s"
        )
        print(f"   Index build: {sample['index_time']:.4f}s vs {uk['index_time']:.4f}s")

        print(f"\n📈 Index Size:")
        print(
            f"   First names: {sample['first_names']} vs {uk['first_names']} ({uk['first_names'] / sample['first_names']:.1f}x more)"
        )
        print(
            f"   Surnames: {sample['surnames']} vs {uk['surnames']} ({uk['surnames'] / sample['surnames']:.1f}x more)"
        )

        print(f"\n⚠️  Quality:")
        print(f"   Warnings: {sample['warnings']} vs {uk['warnings']}")
        print(f"   Errors: {sample['errors']} vs {uk['errors']}")

        # Performance per person
        print(f"\n📊 Performance per Person:")
        sample_per_person = sample["total_time"] / sample["individuals"]
        uk_per_person = uk["total_time"] / uk["individuals"]
        print(
            f"   Time per person: {sample_per_person * 1000:.2f}ms vs {uk_per_person * 1000:.2f}ms"
        )
        print(
            f"   Efficiency: {'Better' if uk_per_person < sample_per_person else 'Worse'} with larger files"
        )

    print("\n=== Comparison Complete ===")
    return 0


if __name__ == "__main__":
    sys.exit(main())
