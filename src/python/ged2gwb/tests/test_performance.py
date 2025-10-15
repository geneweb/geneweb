#!/usr/bin/env python3
"""
Performance tests for ged2gwb.

These tests verify that the converter performs well with various data sizes
and under different conditions.
"""

import sys
import os
import tempfile
import pickle
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

    def load_pickle_data(self, file_path: Path):
        """Load pickle data, handling compression."""
        if file_path.suffix == ".gz":
            import gzip

            with gzip.open(file_path, "rb") as f:
                return pickle.load(f)
        else:
            with open(file_path, "rb") as f:
                return pickle.load(f)

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

    def test_compression_performance(self):
        """Test performance impact of compression."""
        print("\n=== Testing Compression Performance ===")

        gedcom_content = """0 HEAD
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE
1 CHAR UTF-8
"""

        # Add 100 individuals with notes to create larger files
        for i in range(1, 101):
            gedcom_content += f"""0 @I{i}@ INDI
1 NAME Person{i} /Surname{i}/
1 SEX {"M" if i % 2 == 0 else "F"}
1 BIRT
2 DATE {i % 28 + 1} MAR {1900 + i}
1 NOTE This is a detailed note for person {i} with additional information
1 NOTE Another note for person {i} with more details
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)

        # Test without compression
        options_uncompressed = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "uncompressed.pkl",
            compress=False,
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options_uncompressed)
        result_uncompressed = converter.convert()
        uncompressed_time = time.time() - start_time

        # Test with compression
        options_compressed = ConversionOptions(
            input_file=gedcom_file,
            output_file=self.test_dir / "compressed.pkl",
            compress=True,
        )

        start_time = time.time()
        converter = Ged2GwbConverter(options_compressed)
        result_compressed = converter.convert()
        compressed_time = time.time() - start_time

        print(f"Uncompressed conversion time: {uncompressed_time:.3f}s")
        print(f"Compressed conversion time: {compressed_time:.3f}s")
        print(
            f"Compression overhead: {((compressed_time - uncompressed_time) / uncompressed_time) * 100:.1f}%"
        )

        # Check file sizes
        uncompressed_file = options_uncompressed.output_file
        compressed_file = options_compressed.output_file.with_suffix(".pkl.gz")

        uncompressed_size = uncompressed_file.stat().st_size
        compressed_size = compressed_file.stat().st_size

        compression_ratio = (1 - compressed_size / uncompressed_size) * 100

        print(f"Uncompressed file size: {uncompressed_size:,} bytes")
        print(f"Compressed file size: {compressed_size:,} bytes")
        print(f"Compression ratio: {compression_ratio:.1f}%")

        # Both should succeed
        assert result_uncompressed["conversion_successful"] is True
        assert result_compressed["conversion_successful"] is True

        # Compression should provide space savings
        assert compressed_size < uncompressed_size

    def test_memory_usage(self):
        """Test memory usage during conversion."""
        print("\n=== Testing Memory Usage ===")

        try:
            import psutil
            import os
        except ImportError:
            print("SKIP: psutil not available for memory testing")
            return

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
1 NOTE This is a note for person {i} with some additional information
"""

        gedcom_content += "0 TRLR\n"

        gedcom_file = self.create_test_gedcom(gedcom_content)
        options = ConversionOptions(
            input_file=gedcom_file, output_file=self.test_dir / "memory.pkl"
        )

        # Get initial memory usage
        process = psutil.Process(os.getpid())
        initial_memory = process.memory_info().rss / 1024 / 1024  # MB

        converter = Ged2GwbConverter(options)
        result = converter.convert()

        # Get final memory usage
        final_memory = process.memory_info().rss / 1024 / 1024  # MB
        memory_used = final_memory - initial_memory

        print(f"Initial memory usage: {initial_memory:.1f} MB")
        print(f"Final memory usage: {final_memory:.1f} MB")
        print(f"Memory used during conversion: {memory_used:.1f} MB")

        # Memory usage should be reasonable
        assert memory_used < 100, f"Memory usage too high: {memory_used:.1f} MB"
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
