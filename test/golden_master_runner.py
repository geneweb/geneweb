#!/usr/bin/env python3

import os
import sys
import subprocess
import argparse
import json
from pathlib import Path
from typing import Dict, List, Tuple

class GoldenMasterTester:
    def __init__(self, binary_name: str):
        self.binary_name = binary_name
        self.test_data_dir = Path("test/scena")  # Reverted back to test/scena
        self.results_dir = Path(f"test/results/{binary_name}")
        self.results_dir.mkdir(parents=True, exist_ok=True)

    def run_ocaml_reference(self, test_case: Dict) -> Tuple[str, str, int]:
        """Exécute la version OCaml de référence"""
        ocaml_binary_path = f"distribution/gw/{self.binary_name}"

        if not os.path.exists(ocaml_binary_path):
            return "", f"OCAML_BINARY_NOT_FOUND: {ocaml_binary_path}", -1

        cmd = [ocaml_binary_path] + test_case.get("args", [])

        try:
            result = subprocess.run(
                cmd,
                input=test_case.get("input", ""),
                capture_output=True,
                text=True,
                timeout=30
            )
            return result.stdout, result.stderr, result.returncode
        except subprocess.TimeoutExpired:
            return "", "TIMEOUT", -1
        except FileNotFoundError:
            return "", f"OCAML_BINARY_NOT_FOUND: {ocaml_binary_path}", -1

    def run_python_version(self, test_case: Dict) -> Tuple[str, str, int]:
        """Exécute la version Python"""
        possible_commands = [
            f"python3 python/{self.binary_name}/{self.binary_name}.py",
            f"python3 python/{self.binary_name}/run_{self.binary_name}.py",
            f"python/{self.binary_name}/{self.binary_name}"
        ]

        for cmd_template in possible_commands:
            if cmd_template.startswith("python3 "):
                # Split python3 command
                cmd_parts = cmd_template.split()
                cmd = cmd_parts + test_case.get("args", [])
            else:
                # Single executable
                if os.path.exists(cmd_template):
                    cmd = [cmd_template] + test_case.get("args", [])
                else:
                    continue

            try:
                result = subprocess.run(
                    cmd,
                    input=test_case.get("input", ""),
                    capture_output=True,
                    text=True,
                    timeout=30
                )
                return result.stdout, result.stderr, result.returncode
            except (subprocess.TimeoutExpired, FileNotFoundError):
                continue

        # If all methods fail, return error
        return "", "PYTHON_VERSION_NOT_FOUND", -1

    def load_test_cases(self) -> List[Dict]:
        """Charge les cas de test depuis le fichier JSON"""
        test_file = self.test_data_dir / f"{self.binary_name}.json"
        if not test_file.exists():
            print(f"Warning: No test file found at {test_file}")
            return []

        with open(test_file, 'r') as f:
            return json.load(f)

    def compare_outputs(self, ocaml_out: Tuple, python_out: Tuple) -> bool:
        """Compare les sorties OCaml et Python"""
        ocaml_stdout, ocaml_stderr, ocaml_code = ocaml_out
        python_stdout, python_stderr, python_code = python_out

        # Normalisation des sorties pour comparaison
        return (
            ocaml_stdout.strip() == python_stdout.strip() and
            ocaml_code == python_code
        )

    def run_all_tests(self) -> bool:
        """Exécute tous les tests Golden Master"""
        test_cases = self.load_test_cases()
        if not test_cases:
            print(f"No test cases found for {self.binary_name}")
            return True

        passed = 0
        total = len(test_cases)
        results = []

        print(f"Running {total} Golden Master tests for {self.binary_name}...")

        for i, test_case in enumerate(test_cases):
            print(f"Test {i+1}/{total}: {test_case.get('name', f'test_{i+1}')}")

            ocaml_result = self.run_ocaml_reference(test_case)
            python_result = self.run_python_version(test_case)

            test_passed = self.compare_outputs(ocaml_result, python_result)

            result = {
                "name": test_case.get("name", f"test_{i+1}"),
                "passed": test_passed,
                "ocaml_output": ocaml_result,
                "python_output": python_result
            }
            results.append(result)

            if test_passed:
                passed += 1
                print("  ✓ PASSED")
            else:
                print("  ✗ FAILED")
                print(f"    OCaml: {ocaml_result[0][:100]}...")
                print(f"    Python: {python_result[0][:100]}...")

        # Sauvegarde des résultats
        with open(self.results_dir / "results.json", 'w') as f:
            json.dump(results, f, indent=2)

        success_rate = (passed / total) * 100
        print(f"\nResults: {passed}/{total} tests passed ({success_rate:.1f}%)")

        return success_rate == 100.0

def main():
    parser = argparse.ArgumentParser(description="Run Golden Master tests")
    parser.add_argument("--binary", required=True, help="Binary name to test")

    args = parser.parse_args()

    tester = GoldenMasterTester(args.binary)
    success = tester.run_all_tests()

    sys.exit(0 if success else 1)

if __name__ == "__main__":
    main()
