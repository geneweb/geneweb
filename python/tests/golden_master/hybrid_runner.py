"""Runner Golden Master utilisant la configuration hybride."""

import subprocess
import time
from typing import Dict, List, Any, Tuple
from dataclasses import dataclass
from tests.common.hybrid_config import hybrid_config, TestCase


@dataclass
class TestResult:
    """Résultat d'un test."""
    name: str
    success: bool
    execution_time: float
    exit_code: int
    stdout: str
    stderr: str
    errors: List[str]


@dataclass
class GoldenMasterResults:
    """Résultats complets des tests Golden Master."""
    binary_name: str
    total_tests: int
    passed: int
    failed: int
    execution_time: float
    test_results: List[TestResult]
    success_rate: float


class HybridGoldenRunner:
    """Runner Golden Master basé sur configuration hybride."""

    def __init__(self, binary_name: str):
        """Initialiser pour un binaire spécifique."""
        self.binary_name = binary_name

        if not hybrid_config.is_binary_enabled(binary_name):
            raise ValueError(f"Binaire {binary_name} non activé dans la configuration")

        self.test_cases = hybrid_config.get_test_cases(binary_name)

        if not self.test_cases:
            raise ValueError(f"Aucun cas de test trouvé pour {binary_name}")

    def run_single_test(self, test_case: TestCase) -> TestResult:
        """Exécuter un seul cas de test."""
        start_time = time.perf_counter()

        try:
            result = subprocess.run(
                [self.binary_name] + test_case.args,
                capture_output=True,
                text=True,
                timeout=test_case.timeout
            )

            success, errors = self._check_expectations(result, test_case)

            return TestResult(
                name=test_case.name,
                success=success,
                execution_time=time.perf_counter() - start_time,
                exit_code=result.returncode,
                stdout=result.stdout,
                stderr=result.stderr,
                errors=errors
            )

        except subprocess.TimeoutExpired:
            return TestResult(
                name=test_case.name,
                success=False,
                execution_time=time.perf_counter() - start_time,
                exit_code=-1,
                stdout="",
                stderr="",
                errors=[f"Timeout après {test_case.timeout}s"]
            )

        except Exception as e:
            return TestResult(
                name=test_case.name,
                success=False,
                execution_time=time.perf_counter() - start_time,
                exit_code=-1,
                stdout="",
                stderr="",
                errors=[f"Erreur d'exécution: {e}"]
            )

    def _check_expectations(self, result: subprocess.CompletedProcess, test_case: TestCase) -> Tuple[bool, List[str]]:
        """Vérifier si le résultat correspond aux attentes."""
        errors = []

        # Vérifier le code de sortie
        if result.returncode != test_case.expected_exit_code:
            errors.append(f"Code de sortie: attendu {test_case.expected_exit_code}, obtenu {result.returncode}")

        # Vérifier stdout
        if test_case.stdout_must_be_empty and result.stdout.strip():
            errors.append(f"Stdout devrait être vide, obtenu: '{result.stdout.strip()[:50]}...'")

        for text in test_case.stdout_must_contain:
            if text not in result.stdout:
                errors.append(f"Stdout manque: '{text}'")

        # Vérifier stderr
        if test_case.stderr_must_be_empty and result.stderr.strip():
            errors.append(f"Stderr devrait être vide, obtenu: '{result.stderr.strip()[:50]}...'")

        for text in test_case.stderr_must_contain:
            if text not in result.stderr:
                errors.append(f"Stderr manque: '{text}'")

        return len(errors) == 0, errors

    def run_all_tests(self) -> GoldenMasterResults:
        """Exécuter tous les tests configurés."""
        start_time = time.perf_counter()
        test_results = []
        passed = 0

        print(f"🏆 Exécution des tests Golden Master pour {self.binary_name}")
        print(f"   Nombre de tests: {len(self.test_cases)}")

        for i, test_case in enumerate(self.test_cases, 1):
            print(f"   [{i:2d}/{len(self.test_cases)}] {test_case.name}...", end=" ")

            result = self.run_single_test(test_case)
            test_results.append(result)

            if result.success:
                passed += 1
                print(f"✅ ({result.execution_time:.3f}s)")
            else:
                print(f"❌ ({result.execution_time:.3f}s)")
                for error in result.errors:
                    print(f"       → {error}")

        total_time = time.perf_counter() - start_time
        success_rate = (passed / len(self.test_cases) * 100) if self.test_cases else 0

        return GoldenMasterResults(
            binary_name=self.binary_name,
            total_tests=len(self.test_cases),
            passed=passed,
            failed=len(self.test_cases) - passed,
            execution_time=total_time,
            test_results=test_results,
            success_rate=success_rate
        )

    def print_summary(self, results: GoldenMasterResults) -> None:
        """Afficher un résumé des résultats."""
        print(f"\n📊 Résumé Golden Master - {results.binary_name}")
        print(f"   Total: {results.total_tests}")
        print(f"   Réussis: {results.passed}")
        print(f"   Échoués: {results.failed}")
        print(f"   Taux de réussite: {results.success_rate:.1f}%")
        print(f"   Temps d'exécution: {results.execution_time:.3f}s")

        if results.failed > 0:
            print(f"\n❌ Tests échoués:")
            for result in results.test_results:
                if not result.success:
                    print(f"   - {result.name}: {', '.join(result.errors)}")

    def save_results(self, results: GoldenMasterResults, output_file: str = None) -> None:
        """Sauvegarder les résultats dans un fichier JSON."""
        import json
        from pathlib import Path

        if output_file is None:
            reports_dir = Path(hybrid_config.get_global_config().reports_dir)
            reports_dir.mkdir(exist_ok=True)
            output_file = reports_dir / f"golden_master_{self.binary_name}.json"

        # Convertir en dictionnaire
        results_dict = {
            "binary_name": results.binary_name,
            "timestamp": time.strftime("%Y-%m-%d %H:%M:%S"),
            "summary": {
                "total_tests": results.total_tests,
                "passed": results.passed,
                "failed": results.failed,
                "success_rate": results.success_rate,
                "execution_time": results.execution_time
            },
            "test_results": [
                {
                    "name": result.name,
                    "success": result.success,
                    "execution_time": result.execution_time,
                    "exit_code": result.exit_code,
                    "errors": result.errors
                }
                for result in results.test_results
            ]
        }

        with open(output_file, 'w', encoding='utf-8') as f:
            json.dump(results_dict, f, indent=2, ensure_ascii=False)

        print(f"📄 Résultats sauvegardés: {output_file}")
