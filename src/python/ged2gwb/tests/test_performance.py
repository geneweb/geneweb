#!/usr/bin/env python3
"""
Performance tests for ged2gwb.

These tests verify that the converter performs well with various data sizes
and under different conditions.
"""

import sys
import os
import tempfile
import time
from pathlib import Path

# Add the src directory to the path
sys.path.insert(0, str(Path(__file__).parent.parent.parent.parent))

from ged2gwb.core.converter import Ged2GwbConverter
from ged2gwb.utils.options import ConversionOptions


class TestPerformance:
    """Performance tests for ged2gwb."""

    def setup_method(self):
        """Set up test environment."""
        self.temp_dir = tempfile.mkdtemp()
        self.test_dir = Path(self.temp_dir)

    def teardown_method(self):
        """Clean up test environment."""
        import shutil

        shutil.rmtree(self.temp_dir, ignore_errors=True)

    def create_test_gedcom(self, content: str, filename: str = "test.ged") -> Path:
        """Create a test GEDCOM file."""
        file_path = self.test_dir / filename
        with open(file_path, "w", encoding="utf-8") as f:
            f.write(content)
        return file_path

    def load_msgpack_data(self, file_path: Path):
        """Load MessagePack data."""
        from lib.db.io.msgpack import MessagePackReader
        reader = MessagePackReader(str(file_path.parent))
        return reader.load_database(file_path.stem)

    def test_small_file_performance(self):
        """Test performance with small files (< 10 individuals)."""
        print("\n=== Testing Small File Performance ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 5 individuals
        for i in range(1, 6):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "small.pkl"
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options)
        result = converter.convert()
        end_time = time.time()

        execution_time = end_time - start_time

        print(f"Small file conversion time: {execution_time:.3f}s")
        print(f"Individuals processed: {result['individuals_count']}")
        print(
            f"Processing rate: {result['individuals_count'] / execution_time:.1f} individuals/second"
        )

        # Should complete quickly
        assert execution_time < 2.0, f"Small file took too long: {execution_time:.3f}s"
        assert result["conversion_successful"] is True

    def test_medium_file_performance(self):
        """Test performance with medium files (10-100 individuals)."""
        print("\n=== Testing Medium File Performance ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 50 individuals
        for i in range(1, 51):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
1 DEAT
2 DATE {i % 28 + 1} JAN {2000 + i}
"""

        # Add 25 families
        for i in range(1, 26):
            husband_id = i * 2
            wife_id = i * 2 + 1
            gedcom_content += f"""0 @F{i}@ FAM
1 HUSB @I{husband_id}@
1 WIFE @I{wife_id}@
1 MARR
2 DATE {i % 28 + 1} JUN {2000 + i}
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "medium.pkl"
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options)
        result = converter.convert()
        end_time = time.time()

        execution_time = end_time - start_time

        print(f"Medium file conversion time: {execution_time:.3f}s")
        print(f"Individuals processed: {result['individuals_count']}")
        print(f"Families processed: {result['families_count']}")
        print(
            f"Processing rate: {result['individuals_count'] / execution_time:.1f} individuals/second"
        )

        # Should complete in reasonable time
        assert execution_time < 10.0, (
            f"Medium file took too long: {execution_time:.3f}s"
        )
        assert result["conversion_successful"] is True

    def test_large_file_performance(self):
        """Test performance with large files (100+ individuals)."""
        print("\n=== Testing Large File Performance ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 200 individuals
        for i in range(1, 201):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
1 DEAT
2 DATE {i % 28 + 1} JAN {2000 + i}
1 NOTE This is a note for person {i}
"""

        # Add 100 families
        for i in range(1, 101):
            husband_id = i * 2
            wife_id = i * 2 + 1
            gedcom_content += f"""0 @F{i}@ FAM
1 HUSB @I{husband_id}@
1 WIFE @I{wife_id}@
1 MARR
2 DATE {i % 28 + 1} JUN {2000 + i}
1 NOTE This is a note for family {i}
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "large.pkl"
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options)
        result = converter.convert()
        end_time = time.time()

        execution_time = end_time - start_time

        print(f"Large file conversion time: {execution_time:.3f}s")
        print(f"Individuals processed: {result['individuals_count']}")
        print(f"Families processed: {result['families_count']}")
        print(
            f"Processing rate: {result['individuals_count'] / execution_time:.1f} individuals/second"
        )

        # Should complete in reasonable time
        assert execution_time < 30.0, f"Large file took too long: {execution_time:.3f}s"
        assert result["conversion_successful"] is True

    def test_concurrent_conversions(self):
        """Test performance with multiple concurrent conversions."""
        print("\n=== Testing Concurrent Conversions ===")

        import threading
        import queue

        def convert_file(file_id, results_queue):
            """Convert a single file and put result in queue."""
            gedcom_content = f"""0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Person{file_id} /Surname{file_id}/
1 SEX M
1 BIRT
2 DATE 15 MAR 1980
0 TRLR
"""

            gedcom_file = self.create_test_gedcom(
                gedcom_content, f"concurrent_{file_id}.ged"
            )
            options = ConversionOptions(
                input_file=gedcom_file,
                output_file=self.test_dir / f"concurrent_{file_id}.pkl",
            )

            start_time = time.time()
            converter = Ged2GwbConverter(options)
            result = converter.convert()
            end_time = time.time()

            results_queue.put(
                {
                    "file_id": file_id,
                    "success": result["conversion_successful"],
                    "time": end_time - start_time,
                }
            )

        # Run 5 concurrent conversions
        results_queue = queue.Queue()
        threads = []

        start_time = time.time()

        for i in range(5):
            thread = threading.Thread(target=convert_file, args=(i, results_queue))
            threads.append(thread)
            thread.start()

        # Wait for all threads to complete
        for thread in threads:
            thread.join()

        total_time = time.time() - start_time

        # Collect results
        results = []
        while not results_queue.empty():
            results.append(results_queue.get())

        print(f"Total time for 5 concurrent conversions: {total_time:.3f}s")
        print(f"Average time per conversion: {total_time / 5:.3f}s")

        # All conversions should succeed
        for result in results:
            assert result["success"] is True, f"Conversion {result['file_id']} failed"

        print(f"All {len(results)} concurrent conversions completed successfully")

    def test_error_handling_performance(self):
        """Test performance when handling errors."""
        print("\n=== Testing Error Handling Performance ===")

        # Test with invalid GEDCOM
        gedcom_file = self.create_test_gedcom("Invalid GEDCOM content")
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "error.pkl"
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options)

        try:
            converter.convert()
            assert False, "Expected conversion to fail"
        except Exception as e:
            end_time = time.time()
            error_handling_time = end_time - start_time

            print(f"Error handling time: {error_handling_time:.3f}s")

            # Error handling should be fast
            assert error_handling_time < 1.0, (
                f"Error handling took too long: {error_handling_time:.3f}s"
            )


def run_performance_tests():
    """Run all performance tests."""
    print("=== Performance Tests for ged2gwb ===\n")

    test_instance = TestPerformance()
    test_methods = [
        method
        for method in dir(test_instance)
        if method.startswith("test_") and callable(getattr(test_instance, method))
    ]

    passed = 0
    total = len(test_methods)

    for test_method in test_methods:
        try:
            test_instance.setup_method()
            getattr(test_instance, test_method)()
            test_instance.teardown_method()
            print(f"PASS: {test_method}")
            passed += 1
        except Exception as e:
            print(f"FAIL: {test_method} - {e}")
            test_instance.teardown_method()

    print(f"\nPerformance Test Results: {passed}/{total}")

    if passed == total:
        print("SUCCESS: All performance tests passed!")
        return 0
    else:
        print("FAILURE: Some performance tests failed.")
        return 1


if __name__ == "__main__":
    sys.exit(run_performance_tests())
