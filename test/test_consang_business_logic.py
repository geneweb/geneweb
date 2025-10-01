#!/usr/bin/env python3
"""
Tests métier pour le binaire consang - Tests des fonctionnalités de calcul de consanguinité
"""

import subprocess
import json
import os
import sys
import time
from pathlib import Path

def run_consang_test(binary_path, args, input_data="", timeout=60):
    """Exécute consang avec les arguments donnés et retourne le résultat avec mesure de performance"""
    cmd = [binary_path] + args

    start_time = time.time()
    try:
        result = subprocess.run(
            cmd,
            input=input_data,
            capture_output=True,
            text=True,
            timeout=timeout
        )

        execution_time = time.time() - start_time

        return {
            "success": True,
            "returncode": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "command": " ".join(cmd),
            "execution_time": execution_time
        }

    except subprocess.TimeoutExpired:
        execution_time = time.time() - start_time
        return {
            "success": False,
            "error": "TIMEOUT",
            "returncode": -1,
            "stdout": "",
            "stderr": "Process timed out",
            "command": " ".join(cmd),
            "execution_time": execution_time
        }

    except Exception as e:
        execution_time = time.time() - start_time
        return {
            "success": False,
            "error": str(e),
            "returncode": -1,
            "stdout": "",
            "stderr": str(e),
            "command": " ".join(cmd),
            "execution_time": execution_time
        }

def test_consanguinity_calculation(binary_path):
    """Test les calculs de consanguinité avec différents modes"""
    print("  Testing consanguinity calculation modes...")

    if not os.path.exists("test-consang-db.gwb"):
        print("    ⏭️  test-consang-db.gwb not found, skipping business logic tests")
        return {}

    test_cases = [
        (["test-consang-db"], "normal_mode"),
        (["-q", "test-consang-db"], "quiet_mode"),
        (["-qq", "test-consang-db"], "very_quiet_mode"),
        (["-fast", "test-consang-db"], "fast_mode"),
        (["-mem", "test-consang-db"], "memory_save_mode"),
        (["-nolock", "test-consang-db"], "no_lock_mode"),
    ]

    results = {}

    for args, test_name in test_cases:
        result = run_consang_test(binary_path, args)
        results[test_name] = result

        if result["success"]:
            status = "✅" if result["returncode"] == 0 else "⚠️"
            exec_time = f"{result['execution_time']:.3f}s"
            stdout_lines = len(result["stdout"].splitlines()) if result["stdout"] else 0
            stderr_lines = len(result["stderr"].splitlines()) if result["stderr"] else 0

            print(f"    {test_name}: {status} Exit:{result['returncode']} Time:{exec_time} Lines:{stdout_lines}+{stderr_lines}")

            # Analyser le contenu spécifique aux calculs de consanguinité
            analyze_consanguinity_output(result, test_name)
        else:
            print(f"    {test_name}: ❌ {result['error']}")

    return results

def test_scratch_mode_operations(binary_path):
    """Test les opérations de recalcul complet (-scratch)"""
    print("  Testing scratch mode operations...")

    if not os.path.exists("test-consang-db.gwb"):
        print("    ⏭️  test-consang-db.gwb not found, skipping scratch mode tests")
        return {}

    test_cases = [
        (["-scratch", "test-consang-db"], "scratch_normal"),
        (["-scratch", "-q", "test-consang-db"], "scratch_quiet"),
        (["-scratch", "-fast", "test-consang-db"], "scratch_fast"),
        (["-scratch", "-mem", "test-consang-db"], "scratch_memory_save"),
    ]

    results = {}

    for args, test_name in test_cases:
        result = run_consang_test(binary_path, args)
        results[test_name] = result

        if result["success"]:
            status = "✅" if result["returncode"] == 0 else "⚠️"
            exec_time = f"{result['execution_time']:.3f}s"
            print(f"    {test_name}: {status} Exit:{result['returncode']} Time:{exec_time}")

            # Vérifier que le mode scratch fait bien un recalcul complet
            if "Computing consanguinity" in result["stderr"]:
                print(f"      ✓ Scratch mode: Full recalculation detected")
            else:
                print(f"      ? Scratch mode: No clear recalculation indicator")

        else:
            print(f"    {test_name}: ❌ {result['error']}")

    return results

def test_performance_combinations(binary_path):
    """Test les combinaisons d'options de performance"""
    print("  Testing performance option combinations...")

    if not os.path.exists("test-consang-db.gwb"):
        print("    ⏭️  test-consang-db.gwb not found, skipping performance tests")
        return {}

    test_cases = [
        (["-fast", "-q", "test-consang-db"], "fast_quiet"),
        (["-mem", "-qq", "test-consang-db"], "memory_very_quiet"),
        (["-fast", "-mem", "test-consang-db"], "fast_memory"),
        (["-fast", "-nolock", "test-consang-db"], "fast_no_lock"),
        (["-fast", "-mem", "-nolock", "test-consang-db"], "all_performance_flags"),
        (["-scratch", "-fast", "-q", "test-consang-db"], "complete_workflow"),
    ]

    results = {}
    baseline_time = None

    for args, test_name in test_cases:
        result = run_consang_test(binary_path, args)
        results[test_name] = result

        if result["success"]:
            status = "✅" if result["returncode"] == 0 else "⚠️"
            exec_time = result['execution_time']

            # Comparer avec le temps de base si disponible
            if baseline_time is None:
                baseline_time = exec_time
                perf_indicator = "baseline"
            else:
                if exec_time < baseline_time * 0.8:
                    perf_indicator = f"🚀 {(baseline_time/exec_time):.1f}x faster"
                elif exec_time > baseline_time * 1.2:
                    perf_indicator = f"🐌 {(exec_time/baseline_time):.1f}x slower"
                else:
                    perf_indicator = "≈ similar"

            print(f"    {test_name}: {status} Exit:{result['returncode']} Time:{exec_time:.3f}s ({perf_indicator})")
        else:
            print(f"    {test_name}: ❌ {result['error']}")

    return results

def analyze_consanguinity_output(result, test_name):
    """Analyse la sortie pour extraire des informations sur les calculs de consanguinité"""
    stderr = result["stderr"]

    # Rechercher des indicateurs de calcul de consanguinité
    patterns = {
        "persons_processed": r"To do: (\d+) persons",
        "max_consanguinity": r"Max consanguinity (\d+(?:\.\d+)?)",
        "computing_phase": r"Computing consanguinity",
        "saving_phase": r"saving (\w+) array",
    }

    findings = {}
    for pattern_name, pattern in patterns.items():
        import re
        matches = re.findall(pattern, stderr)
        if matches:
            findings[pattern_name] = matches

    if findings:
        details = []
        if "persons_processed" in findings:
            details.append(f"Persons: {findings['persons_processed'][0]}")
        if "max_consanguinity" in findings:
            details.append(f"Max consanguinity: {findings['max_consanguinity'][0]}")
        if "saving_phase" in findings:
            details.append(f"Saved {len(findings['saving_phase'])} arrays")

        if details:
            print(f"      📊 {', '.join(details)}")

def compare_mode_outputs(all_results):
    """Compare les sorties entre différents modes pour détecter les incohérences"""
    print("\n🔍 Comparing outputs between modes...")

    # Grouper les résultats par type d'opération
    normal_results = {}
    quiet_results = {}

    for category, results in all_results.items():
        for test_name, result in results.items():
            if not result["success"] or result["returncode"] != 0:
                continue

            if "quiet" in test_name:
                quiet_results[test_name] = result
            elif "normal" in test_name or ("quiet" not in test_name and "fast" not in test_name):
                normal_results[test_name] = result

    # Vérifier que les modes quiet produisent moins de sortie
    quiet_vs_normal = []
    for quiet_name, quiet_result in quiet_results.items():
        base_name = quiet_name.replace("_quiet", "").replace("_very_quiet", "")

        # Chercher un résultat normal correspondant
        normal_result = None
        for normal_name, nr in normal_results.items():
            if base_name in normal_name:
                normal_result = nr
                break

        if normal_result:
            normal_stderr_len = len(normal_result["stderr"])
            quiet_stderr_len = len(quiet_result["stderr"])

            if quiet_stderr_len < normal_stderr_len:
                reduction = ((normal_stderr_len - quiet_stderr_len) / normal_stderr_len) * 100
                quiet_vs_normal.append(f"✓ {quiet_name}: {reduction:.1f}% less verbose")
            else:
                quiet_vs_normal.append(f"? {quiet_name}: Not quieter than normal mode")

    if quiet_vs_normal:
        print("  Quiet mode effectiveness:")
        for comparison in quiet_vs_normal:
            print(f"    {comparison}")

    return quiet_vs_normal

def generate_business_logic_report(all_results, comparisons, output_dir):
    """Génère un rapport détaillé des tests métier"""
    report_file = output_dir / "business_logic_analysis.md"

    with open(report_file, 'w') as f:
        f.write("# Consang Business Logic Analysis Report\n\n")
        f.write("This report analyzes the consanguinity calculation functionality.\n\n")

        f.write("## Performance Analysis\n\n")

        # Analyse des performances par catégorie
        for category, results in all_results.items():
            f.write(f"### {category.replace('_', ' ').title()}\n\n")

            f.write("| Test | Exit Code | Execution Time | STDERR Lines | Status |\n")
            f.write("|------|-----------|----------------|--------------|--------|\n")

            for test_name, result in results.items():
                if result["success"]:
                    exec_time = f"{result.get('execution_time', 0):.3f}s"
                    stderr_lines = len(result["stderr"].splitlines()) if result["stderr"] else 0
                    exit_code = result["returncode"]
                    status = "✅" if exit_code == 0 else "⚠️"
                else:
                    exec_time = "ERROR"
                    stderr_lines = 0
                    exit_code = "ERROR"
                    status = "❌"

                f.write(f"| {test_name} | {exit_code} | {exec_time} | {stderr_lines} | {status} |\n")

            f.write("\n")

        f.write("## Mode Comparisons\n\n")
        if comparisons:
            for comparison in comparisons:
                f.write(f"- {comparison}\n")
        else:
            f.write("No mode comparisons available.\n")

        f.write("\n## Key Findings\n\n")

        # Analyser les findings généraux
        successful_tests = sum(
            1 for results in all_results.values()
            for result in results.values()
            if result.get("success", False) and result.get("returncode", -1) == 0
        )

        total_tests = sum(len(results) for results in all_results.values())

        f.write(f"- **Total business logic tests**: {total_tests}\n")
        f.write(f"- **Successful executions**: {successful_tests}\n")
        f.write(f"- **Success rate**: {(successful_tests/total_tests)*100:.1f}%\n")

        # Analyser les temps d'exécution
        execution_times = []
        for results in all_results.values():
            for result in results.values():
                if result.get("success", False) and result.get("returncode", -1) == 0:
                    execution_times.append(result.get("execution_time", 0))

        if execution_times:
            avg_time = sum(execution_times) / len(execution_times)
            f.write(f"- **Average execution time**: {avg_time:.3f}s\n")
            f.write(f"- **Fastest execution**: {min(execution_times):.3f}s\n")
            f.write(f"- **Slowest execution**: {max(execution_times):.3f}s\n")

    print(f"📄 Business logic report generated: {report_file}")

def main():
    """Point d'entrée principal pour les tests métier"""
    print("🏢 Starting consang business logic tests...")

    # Vérifier le binaire
    binary_path = "distribution/gw/consang"
    if not os.path.exists(binary_path):
        print(f"❌ Binary not found: {binary_path}")
        print("Please build the OCaml binaries first with: make distrib")
        return 1

    print(f"✅ Found binary: {binary_path}")

    # Créer le dossier de sortie
    output_dir = Path("test/snapshots/consang")
    output_dir.mkdir(parents=True, exist_ok=True)

    # Exécuter les tests métier
    all_results = {}

    print("\n🔍 Running business logic tests...")

    all_results["consanguinity_calculation"] = test_consanguinity_calculation(binary_path)
    all_results["scratch_mode_operations"] = test_scratch_mode_operations(binary_path)
    all_results["performance_combinations"] = test_performance_combinations(binary_path)

    # Comparer les sorties entre modes
    comparisons = compare_mode_outputs(all_results)

    # Générer le rapport
    generate_business_logic_report(all_results, comparisons, output_dir)

    # Calculer les statistiques
    total_tests = sum(len(results) for results in all_results.values())
    successful_tests = sum(
        1 for results in all_results.values()
        for result in results.values()
        if result.get("success", False)
    )

    working_tests = sum(
        1 for results in all_results.values()
        for result in results.values()
        if result.get("success", False) and result.get("returncode", -1) == 0
    )

    print(f"\n📊 Business Logic Test Summary:")
    print(f"   Total tests: {total_tests}")
    print(f"   Executed successfully: {successful_tests}")
    print(f"   Working correctly: {working_tests}")
    print(f"   Failed to execute: {total_tests - successful_tests}")

    if total_tests > 0:
        success_rate = (working_tests / total_tests) * 100
        print(f"   Success rate: {success_rate:.1f}%")

        if success_rate >= 80:
            print("🎉 Business logic tests show good functionality!")
            return 0
        elif success_rate >= 50:
            print("⚠️  Business logic tests show partial functionality")
            return 1
        else:
            print("❌ Business logic tests show poor functionality")
            return 2
    else:
        print("⚠️  No business logic tests could be executed")
        return 1

if __name__ == "__main__":
    exit(main())
