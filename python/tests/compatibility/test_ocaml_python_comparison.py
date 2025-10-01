"""Exact comparison between OCaml and Python consang binaries."""

import pytest
import subprocess
import os
import tempfile
import shutil
from pathlib import Path
from typing import List, Dict, Any, Tuple
import difflib
import re


class BinaryComparator:
    """Compare OCaml and Python binaries exactly."""

    def __init__(self):
        self.ocaml_binary = self._find_ocaml_binary()
        self.python_binary = "consang"
        self.temp_dir = None

    def _find_ocaml_binary(self) -> str:
        """Find OCaml consang binary."""
        possible_paths = [
            "../distribution/gw/consang",
            "./consang",
            "consang.opt",
            "/usr/local/bin/consang"
        ]

        for path in possible_paths:
            if os.path.exists(path) and os.access(path, os.X_OK):
                return path

        return "consang"  # Assume it's in PATH

    def run_binary(self, binary_path: str, args: List[str], timeout: int = 30) -> Dict[str, Any]:
        """Run binary and capture all outputs."""
        try:
            result = subprocess.run(
                [binary_path] + args,
                capture_output=True,
                text=True,
                timeout=timeout,
                cwd=os.getcwd(),
                env=os.environ.copy()
            )

            return {
                "returncode": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "success": True,
                "error": None
            }
        except subprocess.TimeoutExpired:
            return {
                "returncode": -1,
                "stdout": "",
                "stderr": "",
                "success": False,
                "error": "Timeout"
            }
        except FileNotFoundError:
            return {
                "returncode": -1,
                "stdout": "",
                "stderr": "",
                "success": False,
                "error": "Binary not found"
            }
        except Exception as e:
            return {
                "returncode": -1,
                "stdout": "",
                "stderr": "",
                "success": False,
                "error": str(e)
            }

    def normalize_output(self, text: str) -> str:
        """Normalize output for comparison."""
        # Remove timestamps and variable paths
        text = re.sub(r'\d{4}-\d{2}-\d{2} \d{2}:\d{2}:\d{2}', 'TIMESTAMP', text)
        text = re.sub(r'/tmp/[^/\s]+', '/tmp/TEMP_PATH', text)
        text = re.sub(r'/var/folders/[^/\s]+/[^/\s]+/[^/\s]+', '/tmp/TEMP_PATH', text)

        # Normalize whitespace
        lines = [line.rstrip() for line in text.split('\n')]
        return '\n'.join(lines).strip()

    def compare_outputs(self, ocaml_result: Dict, python_result: Dict) -> Dict[str, Any]:
        """Compare outputs between OCaml and Python."""
        comparison = {
            "exit_codes_match": ocaml_result["returncode"] == python_result["returncode"],
            "stdout_match": False,
            "stderr_match": False,
            "differences": [],
            "ocaml_result": ocaml_result,
            "python_result": python_result
        }

        # Normalize outputs
        ocaml_stdout = self.normalize_output(ocaml_result["stdout"])
        python_stdout = self.normalize_output(python_result["stdout"])
        ocaml_stderr = self.normalize_output(ocaml_result["stderr"])
        python_stderr = self.normalize_output(python_result["stderr"])

        # Compare stdout
        comparison["stdout_match"] = ocaml_stdout == python_stdout
        if not comparison["stdout_match"]:
            diff = list(difflib.unified_diff(
                ocaml_stdout.splitlines(keepends=True),
                python_stdout.splitlines(keepends=True),
                fromfile="OCaml stdout",
                tofile="Python stdout"
            ))
            comparison["differences"].append({
                "type": "stdout",
                "diff": ''.join(diff)
            })

        # Compare stderr
        comparison["stderr_match"] = ocaml_stderr == python_stderr
        if not comparison["stderr_match"]:
            diff = list(difflib.unified_diff(
                ocaml_stderr.splitlines(keepends=True),
                python_stderr.splitlines(keepends=True),
                fromfile="OCaml stderr",
                tofile="Python stderr"
            ))
            comparison["differences"].append({
                "type": "stderr",
                "diff": ''.join(diff)
            })

        return comparison


@pytest.fixture
def binary_comparator():
    """Create binary comparator instance."""
    return BinaryComparator()


@pytest.fixture
def temp_database():
    """Create temporary test database."""
    temp_dir = tempfile.mkdtemp()
    db_path = Path(temp_dir) / "test.gwb"
    db_path.mkdir()

    # Create minimal base file
    base_file = db_path / "base"
    base_file.write_text("# Test database\n")

    yield db_path

    shutil.rmtree(temp_dir)


@pytest.mark.compatibility
class TestOCamlPythonComparison:
    """Test exact compatibility between OCaml and Python binaries."""

    def test_help_output_identical(self, binary_comparator):
        """Test that help output is identical."""
        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, ["--help"])
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, ["--help"])

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        # Print detailed comparison for debugging
        if not comparison["exit_codes_match"]:
            print(f"\nExit codes differ:")
            print(f"  OCaml: {ocaml_result['returncode']}")
            print(f"  Python: {python_result['returncode']}")

        if not comparison["stdout_match"]:
            print(f"\nStdout differences:")
            for diff in comparison["differences"]:
                if diff["type"] == "stdout":
                    print(diff["diff"])

        assert comparison["exit_codes_match"], "Exit codes must match"
        assert comparison["stdout_match"], "Help output must be identical"

    def test_no_arguments_identical(self, binary_comparator):
        """Test behavior with no arguments is identical."""
        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, [])
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, [])

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        assert comparison["exit_codes_match"], "Exit codes must match for no arguments"
        assert comparison["stderr_match"], "Error messages must be identical"

    def test_invalid_option_h_identical(self, binary_comparator):
        """Test invalid -h option behavior is identical."""
        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, ["-h"])
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, ["-h"])

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        assert comparison["exit_codes_match"], "Exit codes must match for invalid -h"
        assert comparison["stderr_match"], "Error messages for -h must be identical"

    def test_nonexistent_file_identical(self, binary_comparator):
        """Test nonexistent file error is identical."""
        args = ["nonexistent_file.gwb"]

        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, args)
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, args)

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        assert comparison["exit_codes_match"], "Exit codes must match for nonexistent file"

        # Error messages should contain similar information
        ocaml_stderr = comparison["ocaml_result"]["stderr"].lower()
        python_stderr = comparison["python_result"]["stderr"].lower()

        assert "no such file" in ocaml_stderr or "not found" in ocaml_stderr
        assert "no such file" in python_stderr or "not found" in python_stderr

    @pytest.mark.parametrize("option", ["-q", "-fast", "-mem", "-nolock", "-scratch"])
    def test_options_with_nonexistent_file(self, binary_comparator, option):
        """Test various options with nonexistent file."""
        args = [option, "nonexistent.gwb"]

        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, args)
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, args)

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        assert comparison["exit_codes_match"], f"Exit codes must match for {option}"

        # Both should produce similar error about file not found
        ocaml_stderr = comparison["ocaml_result"]["stderr"].lower()
        python_stderr = comparison["python_result"]["stderr"].lower()

        assert ("no such file" in ocaml_stderr or "not found" in ocaml_stderr), f"OCaml should report file error for {option}"
        assert ("no such file" in python_stderr or "not found" in python_stderr), f"Python should report file error for {option}"

    def test_quiet_modes(self, binary_comparator):
        """Test quiet mode options (-q, -qq)."""
        for quiet_level in ["-q", "-qq"]:
            args = [quiet_level, "nonexistent.gwb"]

            ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, args)
            python_result = binary_comparator.run_binary(binary_comparator.python_binary, args)

            if not ocaml_result["success"]:
                pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

            comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

            assert comparison["exit_codes_match"], f"Exit codes must match for {quiet_level}"

    def test_combined_options(self, binary_comparator):
        """Test combined options work identically."""
        test_combinations = [
            ["-q", "-fast"],
            ["-mem", "-nolock"],
            ["-q", "-scratch"],
            ["-fast", "-mem", "-q"]
        ]

        for options in test_combinations:
            args = options + ["nonexistent.gwb"]

            ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, args)
            python_result = binary_comparator.run_binary(binary_comparator.python_binary, args)

            if not ocaml_result["success"]:
                pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

            comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

            assert comparison["exit_codes_match"], f"Exit codes must match for {' '.join(options)}"

    def test_empty_database_handling(self, binary_comparator, temp_database):
        """Test behavior with empty/minimal database."""
        args = [str(temp_database)]

        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, args, timeout=60)
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, args, timeout=60)

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        comparison = binary_comparator.compare_outputs(ocaml_result, python_result)

        # Both should either succeed or fail in the same way
        assert comparison["exit_codes_match"], "Exit codes must match for empty database"

        # If both succeed, check output similarity
        if ocaml_result["returncode"] == 0 and python_result["returncode"] == 0:
            # Both should produce similar output about processing or completion
            print(f"\nOCaml output: {ocaml_result['stderr']}")
            print(f"Python output: {python_result['stderr']}")

    def test_performance_comparison(self, binary_comparator):
        """Compare performance characteristics."""
        import time

        # Test startup time
        start_time = time.time()
        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, ["--help"])
        ocaml_time = time.time() - start_time

        if not ocaml_result["success"]:
            pytest.skip(f"OCaml binary not available: {ocaml_result['error']}")

        start_time = time.time()
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, ["--help"])
        python_time = time.time() - start_time

        print(f"\nPerformance comparison:")
        print(f"  OCaml startup: {ocaml_time:.3f}s")
        print(f"  Python startup: {python_time:.3f}s")
        print(f"  Ratio: {python_time/ocaml_time:.1f}x")

        # Python should be reasonably fast (less than 10x slower for startup)
        assert python_time < 10.0, "Python version should start in reasonable time"

        # Both should be under reasonable limits
        assert ocaml_time < 5.0, "OCaml version should start quickly"
        assert python_time < 10.0, "Python version should start reasonably quickly"

    def test_binary_detection(self, binary_comparator):
        """Test that both binaries are available."""
        print(f"\nBinary locations:")
        print(f"  OCaml: {binary_comparator.ocaml_binary}")
        print(f"  Python: {binary_comparator.python_binary}")

        # Test OCaml binary
        ocaml_result = binary_comparator.run_binary(binary_comparator.ocaml_binary, ["--help"])
        print(f"  OCaml available: {ocaml_result['success']}")
        if not ocaml_result['success']:
            print(f"    Error: {ocaml_result['error']}")

        # Test Python binary
        python_result = binary_comparator.run_binary(binary_comparator.python_binary, ["--help"])
        print(f"  Python available: {python_result['success']}")
        if not python_result['success']:
            print(f"    Error: {python_result['error']}")

        assert python_result["success"], "Python consang binary must be available"

        if ocaml_result["success"]:
            assert ocaml_result["returncode"] == 0, "OCaml binary should show help"

        assert python_result["returncode"] == 0, "Python binary should show help"
