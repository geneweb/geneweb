"""JSON-based Golden Master runner."""

import subprocess
from typing import Dict, List, Any
from pathlib import Path


class JsonGoldenRunner:
    """Simple JSON-based Golden Master runner."""

    def __init__(self, config_file: Path):
        self.config_file = config_file
        self.binary_name = "consang"

    def run_binary(self, args: List[str], timeout: int = 10) -> Dict[str, Any]:
        """Execute binary and capture output."""
        try:
            result = subprocess.run(
                [self.binary_name] + args,
                capture_output=True,
                text=True,
                timeout=timeout
            )
            return {
                "exit_code": result.returncode,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "success": True
            }
        except Exception as e:
            return {
                "exit_code": -1,
                "stdout": "",
                "stderr": str(e),
                "success": False
            }

    def run_all_tests(self) -> Dict[str, Any]:
        """Execute all configured tests."""
        test_cases = [
            {"name": "help", "args": ["--help"], "expected_exit": 0},
            {"name": "no_args", "args": [], "expected_exit": 2},
            {"name": "invalid_h", "args": ["-h"], "expected_exit": 2},
        ]

        results = {
            "binary": self.binary_name,
            "total_tests": len(test_cases),
            "passed": 0,
            "failed": 0,
        }

        for test_case in test_cases:
            result = self.run_binary(test_case["args"])
            if result["exit_code"] == test_case["expected_exit"]:
                results["passed"] += 1
            else:
                results["failed"] += 1

        return results

    def print_results(self, results: Dict[str, Any]) -> None:
        """Display test results."""
        total = results["total_tests"]
        passed = results["passed"]
        success_rate = (passed / total * 100) if total > 0 else 0

        print(f"JSON Golden Results: {passed}/{total} ({success_rate:.1f}%)")
