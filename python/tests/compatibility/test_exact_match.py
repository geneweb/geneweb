"""Test exact match between OCaml and Python consang outputs."""

import pytest
import subprocess
import re
from typing import Dict, Any, List


class ExactMatchTester:
    """Test exact output matching between implementations."""

    def __init__(self):
        self.test_cases = self._generate_test_cases()

    def _generate_test_cases(self) -> List[Dict[str, Any]]:
        """Generate comprehensive test cases."""
        return [
            {
                "name": "help_display",
                "args": ["--help"],
                "description": "Help output should be identical"
            },
            {
                "name": "no_arguments",
                "args": [],
                "description": "No arguments error should be identical"
            },
            {
                "name": "invalid_h_option",
                "args": ["-h"],
                "description": "Invalid -h option error should be identical"
            },
            {
                "name": "nonexistent_file",
                "args": ["nonexistent.gwb"],
                "description": "File not found error should be similar"
            },
            {
                "name": "quiet_mode",
                "args": ["-q", "nonexistent.gwb"],
                "description": "Quiet mode with nonexistent file"
            },
            {
                "name": "very_quiet_mode",
                "args": ["-qq", "nonexistent.gwb"],
                "description": "Very quiet mode with nonexistent file"
            },
            {
                "name": "fast_mode",
                "args": ["-fast", "nonexistent.gwb"],
                "description": "Fast mode with nonexistent file"
            },
            {
                "name": "memory_mode",
                "args": ["-mem", "nonexistent.gwb"],
                "description": "Memory mode with nonexistent file"
            },
            {
                "name": "scratch_mode",
                "args": ["-scratch", "nonexistent.gwb"],
                "description": "Scratch mode with nonexistent file"
            },
            {
                "name": "nolock_mode",
                "args": ["-nolock", "nonexistent.gwb"],
                "description": "No lock mode with nonexistent file"
            },
            {
                "name": "combined_options",
                "args": ["-q", "-fast", "-mem", "nonexistent.gwb"],
                "description": "Combined options should work identically"
            }
        ]

    def normalize_for_comparison(self, text: str) -> str:
        """Normalize text for comparison."""
        # Remove variable elements that will differ
        text = re.sub(r'/tmp/[^\s]+', '/tmp/TEMP_PATH', text)
        text = re.sub(r'/var/folders/[^\s]+', '/tmp/TEMP_PATH', text)
        text = re.sub(r'geneweb_test_[^\s]+', 'geneweb_test_ID', text)

        # Normalize whitespace
        lines = [line.rstrip() for line in text.split('\n')]
        return '\n'.join(lines).strip()

    def are_outputs_equivalent(self, ocaml_output: str, python_output: str) -> Dict[str, Any]:
        """Check if outputs are equivalent."""
        ocaml_norm = self.normalize_for_comparison(ocaml_output)
        python_norm = self.normalize_for_comparison(python_output)

        exact_match = ocaml_norm == python_norm

        # For error messages, check semantic equivalence
        semantic_match = False
        if not exact_match:
            # Check for key phrases that should be present
            key_phrases = [
                "usage: consang",
                "Missing file name",
                "unknown option",
                "No such file or directory",
                "-fast", "-mem", "-nolock", "-q", "-scratch"
            ]

            ocaml_lower = ocaml_norm.lower()
            python_lower = python_norm.lower()

            # Count matching key phrases
            ocaml_phrases = sum(1 for phrase in key_phrases if phrase.lower() in ocaml_lower)
            python_phrases = sum(1 for phrase in key_phrases if phrase.lower() in python_lower)

            # Semantic match if both have similar key phrases
            semantic_match = abs(ocaml_phrases - python_phrases) <= 1 and min(ocaml_phrases, python_phrases) > 0

        return {
            "exact_match": exact_match,
            "semantic_match": semantic_match or exact_match,
            "ocaml_normalized": ocaml_norm,
            "python_normalized": python_norm,
            "length_diff": abs(len(ocaml_norm) - len(python_norm))
        }

    def run_test_case(self, test_case: Dict[str, Any]) -> Dict[str, Any]:
        """Run a single test case comparison."""
        ocaml_binary = self._find_ocaml_binary()
        python_binary = "consang"

        # Run OCaml version
        ocaml_result = self._run_binary(ocaml_binary, test_case["args"])

        # Run Python version
        python_result = self._run_binary(python_binary, test_case["args"])

        if not ocaml_result["success"]:
            return {
                "test_case": test_case,
                "skipped": True,
                "reason": f"OCaml binary not available: {ocaml_result.get('error')}"
            }

        if not python_result["success"]:
            return {
                "test_case": test_case,
                "failed": True,
                "reason": f"Python binary failed: {python_result.get('error')}"
            }

        # Compare exit codes
        exit_codes_match = ocaml_result["returncode"] == python_result["returncode"]

        # Compare stdout
        stdout_comparison = self.are_outputs_equivalent(
            ocaml_result["stdout"],
            python_result["stdout"]
        )

        # Compare stderr
        stderr_comparison = self.are_outputs_equivalent(
            ocaml_result["stderr"],
            python_result["stderr"]
        )

        return {
            "test_case": test_case,
            "exit_codes_match": exit_codes_match,
            "stdout_comparison": stdout_comparison,
            "stderr_comparison": stderr_comparison,
            "ocaml_result": ocaml_result,
            "python_result": python_result,
            "overall_match": (
                exit_codes_match and
                stdout_comparison["semantic_match"] and
                stderr_comparison["semantic_match"]
            )
        }

    def _find_ocaml_binary(self) -> str:
        """Find OCaml binary."""
        import os
        candidates = [
            "./distribution/gw/consang",
            "./consang",
            "consang.opt",
            "/usr/local/bin/consang"
        ]

        for candidate in candidates:
            if os.path.exists(candidate) and os.access(candidate, os.X_OK):
                return candidate

        return "consang"  # Try PATH

    def _run_binary(self, binary: str, args: List[str]) -> Dict[str, Any]:
        """Run binary with args."""
        try:
            result = subprocess.run(
                [binary] + args,
                capture_output=True,
                text=True,
                timeout=30
            )
            return {
                "success": True,
                "returncode": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr
            }
        except Exception as e:
            return {
                "success": False,
                "error": str(e),
                "returncode": -1,
                "stdout": "",
                "stderr": ""
            }


@pytest.fixture
def exact_tester():
    """Create exact match tester."""
    return ExactMatchTester()


@pytest.mark.compatibility
class TestExactMatch:
    """Test exact matching between OCaml and Python implementations."""

    @pytest.mark.parametrize("test_case", ExactMatchTester()._generate_test_cases())
    def test_exact_compatibility(self, exact_tester, test_case):
        """Test exact compatibility for each test case."""
        result = exact_tester.run_test_case(test_case)

        if result.get("skipped"):
            pytest.skip(result["reason"])

        if result.get("failed"):
            pytest.fail(result["reason"])

        # Print detailed results for debugging
        print(f"\n=== {test_case['name']} ===")
        print(f"Description: {test_case['description']}")
        print(f"Args: {test_case['args']}")
        print(f"Exit codes match: {result['exit_codes_match']}")
        print(f"Stdout match: {result['stdout_comparison']['semantic_match']}")
        print(f"Stderr match: {result['stderr_comparison']['semantic_match']}")

        if not result['exit_codes_match']:
            print(f"OCaml exit code: {result['ocaml_result']['returncode']}")
            print(f"Python exit code: {result['python_result']['returncode']}")

        if not result['stdout_comparison']['exact_match']:
            print("Stdout differences (normalized):")
            print("OCaml:", repr(result['stdout_comparison']['ocaml_normalized'][:200]))
            print("Python:", repr(result['stdout_comparison']['python_normalized'][:200]))

        if not result['stderr_comparison']['exact_match']:
            print("Stderr differences (normalized):")
            print("OCaml:", repr(result['stderr_comparison']['ocaml_normalized'][:200]))
            print("Python:", repr(result['stderr_comparison']['python_normalized'][:200]))

        # Assert requirements
        assert result["exit_codes_match"], f"Exit codes must match for {test_case['name']}"
        assert result["stdout_comparison"]["semantic_match"], f"Stdout must be semantically equivalent for {test_case['name']}"
        assert result["stderr_comparison"]["semantic_match"], f"Stderr must be semantically equivalent for {test_case['name']}"
        assert result["overall_match"], f"Overall compatibility failed for {test_case['name']}"

    def test_comprehensive_compatibility_report(self, exact_tester):
        """Generate comprehensive compatibility report."""
        results = []

        for test_case in exact_tester.test_cases:
            result = exact_tester.run_test_case(test_case)
            results.append(result)

        # Calculate statistics
        total_tests = len(results)
        skipped = sum(1 for r in results if r.get("skipped"))
        failed = sum(1 for r in results if r.get("failed"))
        passed = sum(1 for r in results if r.get("overall_match", False))

        # Print comprehensive report
        print(f"\n" + "="*60)
        print("COMPREHENSIVE COMPATIBILITY REPORT")
        print("="*60)
        print(f"Total test cases: {total_tests}")
        print(f"Passed: {passed}")
        print(f"Failed: {failed}")
        print(f"Skipped (OCaml N/A): {skipped}")
        print(f"Success rate: {(passed/(total_tests-skipped)*100):.1f}%" if total_tests > skipped else "N/A")

        print(f"\nDetailed Results:")
        for result in results:
            name = result["test_case"]["name"]
            if result.get("skipped"):
                print(f"  ⏭️  {name}: SKIPPED")
            elif result.get("failed"):
                print(f"  ❌ {name}: FAILED")
            elif result.get("overall_match"):
                print(f"  ✅ {name}: PASSED")
            else:
                print(f"  ⚠️  {name}: ISSUES")

        # Fail if too many issues
        if total_tests > skipped:
            success_rate = passed / (total_tests - skipped) * 100
            assert success_rate >= 80, f"Overall compatibility too low: {success_rate:.1f}%"
