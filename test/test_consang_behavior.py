#!/usr/bin/env python3
"""
Tests comportementaux pour le binaire consang
"""

import subprocess
import json
import os
import sys
from pathlib import Path

def run_consang_test(binary_path, args, input_data="", timeout=30):
    """Exécute consang avec les arguments donnés et retourne le résultat"""
    cmd = [binary_path] + args

    try:
        result = subprocess.run(
            cmd,
            input=input_data,
            capture_output=True,
            text=True,
            timeout=timeout
        )

        return {
            "success": True,
            "returncode": result.returncode,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "command": " ".join(cmd)
        }

    except subprocess.TimeoutExpired:
        return {
            "success": False,
            "error": "TIMEOUT",
            "returncode": -1,
            "stdout": "",
            "stderr": "Process timed out",
            "command": " ".join(cmd)
        }

    except Exception as e:
        return {
            "success": False,
            "error": str(e),
            "returncode": -1,
            "stdout": "",
            "stderr": str(e),
            "command": " ".join(cmd)
        }

def test_help_behavior(binary_path):
    """Test que l'aide s'affiche correctement"""
    print("  Testing help behavior...")

    help_variations = ["-help", "--help", "-h"]
    results = {}

    for help_arg in help_variations:
        result = run_consang_test(binary_path, [help_arg])
        results[help_arg] = result

        # Vérifications
        if result["success"]:
            # L'aide devrait avoir du contenu
            has_content = len(result["stdout"]) > 0 or len(result["stderr"]) > 0
            # Exit code devrait être 0 pour l'aide
            correct_exit = result["returncode"] == 0

            status = "✅" if (has_content and correct_exit) else "⚠️"
            print(f"    {help_arg}: {status} Exit:{result['returncode']} Output:{len(result['stdout'])}+{len(result['stderr'])} chars")
        else:
            print(f"    {help_arg}: ❌ {result['error']}")

    return results

def test_argument_validation(binary_path):
    """Test la validation des arguments"""
    print("  Testing argument validation...")

    test_cases = [
        ([], "no_args"),
        (["nonexistent_db"], "invalid_db"),
        (["-invalid_flag", "test_db"], "invalid_flag"),
        (["-q"], "missing_db_after_flag"),
        (["-fast", "-qq", "nonexistent_test"], "multiple_flags"),
    ]

    results = {}

    for args, test_name in test_cases:
        result = run_consang_test(binary_path, args)
        results[test_name] = result

        if result["success"]:
            status = "📝" if result["returncode"] != 0 else "✅"
            print(f"    {test_name}: {status} Exit:{result['returncode']}")
        else:
            print(f"    {test_name}: ❌ {result['error']}")

    return results

def test_database_operations(binary_path):
    """Test les opérations sur base de données"""
    print("  Testing database operations...")

    results = {}

    # Test avec base vide (nom vide)
    result = run_consang_test(binary_path, [""])
    results["empty_db_name"] = result

    status = "📝" if not result["success"] or result["returncode"] != 0 else "⚠️"
    print(f"    empty_db: {status} Exit:{result.get('returncode', 'N/A')}")

    # Test avec base inexistante
    result = run_consang_test(binary_path, ["nonexistent_database"])
    results["nonexistent_db"] = result

    status = "📝" if not result["success"] or result["returncode"] != 0 else "⚠️"
    print(f"    nonexistent_db: {status} Exit:{result.get('returncode', 'N/A')}")

    # Test avec base de test si elle existe
    if os.path.exists("test-consang-db.gwb"):
        result = run_consang_test(binary_path, ["test-consang-db"])
        results["existing_test_db"] = result

        status = "✅" if result["success"] and result["returncode"] == 0 else "📝"
        print(f"    existing_test_db: {status} Exit:{result.get('returncode', 'N/A')}")
    else:
        print(f"    existing_test_db: ⏭️  test-consang-db.gwb not found, skipping")

    return results

def test_flag_combinations(binary_path):
    """Test les combinaisons de flags"""
    print("  Testing flag combinations...")

    flag_combinations = [
        (["-q", "nonexistent_test"], "quiet_mode"),
        (["-qq", "nonexistent_test"], "very_quiet_mode"),
        (["-fast", "nonexistent_test"], "fast_mode"),
        (["-scratch", "nonexistent_test"], "scratch_mode"),
        (["-mem", "nonexistent_test"], "memory_save_mode"),
        (["-nolock", "nonexistent_test"], "no_lock_mode"),
        (["-q", "-fast", "nonexistent_test"], "quiet_fast"),
        (["-qq", "-mem", "nonexistent_test"], "very_quiet_mem"),
    ]

    results = {}

    for args, test_name in flag_combinations:
        result = run_consang_test(binary_path, args)
        results[test_name] = result

        if result["success"]:
            print(f"    {test_name}: 📝 Exit:{result['returncode']}")
        else:
            print(f"    {test_name}: ❌ {result['error']}")

    return results

def analyze_behavioral_patterns(all_results):
    """Analyse les patterns comportementaux"""
    print("\n🔍 Analyzing behavioral patterns...")

    patterns = {
        "help_exits_zero": [],
        "invalid_args_non_zero": [],
        "missing_db_behavior": [],
        "flag_compatibility": []
    }

    for category, results in all_results.items():
        for test_name, result in results.items():
            if not result["success"]:
                continue

            exit_code = result["returncode"]
            has_stdout = len(result["stdout"]) > 0
            has_stderr = len(result["stderr"]) > 0

            # Analyser les patterns
            if "help" in test_name and exit_code == 0:
                patterns["help_exits_zero"].append(test_name)

            if any(word in test_name for word in ["invalid", "nonexistent", "missing"]) and exit_code != 0:
                patterns["invalid_args_non_zero"].append(test_name)

            if "empty_db" in test_name or "nonexistent_db" in test_name:
                patterns["missing_db_behavior"].append((test_name, exit_code, has_stderr))

    # Rapporter les patterns
    for pattern_name, matches in patterns.items():
        if matches:
            print(f"  ✅ {pattern_name}: {len(matches)} cases")
        else:
            print(f"  ⚠️  {pattern_name}: No matches found")

    return patterns

def generate_behavior_report(all_results, patterns, output_dir):
    """Génère un rapport détaillé des comportements"""
    report_file = output_dir / "behavior_analysis.md"

    with open(report_file, 'w') as f:
        f.write("# Consang Behavioral Analysis Report\n\n")

        f.write("## Test Categories\n\n")
        for category, results in all_results.items():
            f.write(f"### {category.replace('_', ' ').title()}\n\n")
            f.write(f"Executed {len(results)} test cases.\n\n")

            f.write("| Test | Exit Code | STDOUT | STDERR | Status |\n")
            f.write("|------|-----------|--------|--------|---------|\n")

            for test_name, result in results.items():
                if result["success"]:
                    stdout_len = len(result["stdout"])
                    stderr_len = len(result["stderr"])
                    exit_code = result["returncode"]
                    status = "✅" if exit_code == 0 else "⚠️"
                else:
                    stdout_len = stderr_len = 0
                    exit_code = "ERROR"
                    status = "❌"

                f.write(f"| {test_name} | {exit_code} | {stdout_len} | {stderr_len} | {status} |\n")

            f.write("\n")

        f.write("## Behavioral Patterns\n\n")
        for pattern_name, matches in patterns.items():
            f.write(f"### {pattern_name.replace('_', ' ').title()}\n")
            if matches:
                f.write(f"Found {len(matches)} matching cases:\n")
                for match in matches:
                    f.write(f"- {match}\n")
            else:
                f.write("No matching cases found.\n")
            f.write("\n")

    print(f"📄 Behavior report generated: {report_file}")

def main():
    """Point d'entrée principal pour les tests comportementaux"""
    print("🧪 Starting consang behavioral tests...")

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

    # Exécuter les tests
    all_results = {}

    print("\n🔍 Running behavioral tests...")

    all_results["help_behavior"] = test_help_behavior(binary_path)
    all_results["argument_validation"] = test_argument_validation(binary_path)
    all_results["database_operations"] = test_database_operations(binary_path)
    all_results["flag_combinations"] = test_flag_combinations(binary_path)

    # Analyser les patterns
    patterns = analyze_behavioral_patterns(all_results)

    # Générer le rapport
    generate_behavior_report(all_results, patterns, output_dir)

    # Calculer les statistiques
    total_tests = sum(len(results) for results in all_results.values())
    successful_tests = sum(
        1 for results in all_results.values()
        for result in results.values()
        if result["success"]
    )

    print(f"\n📊 Test Summary:")
    print(f"   Total tests: {total_tests}")
    print(f"   Successful: {successful_tests}")
    print(f"   Failed: {total_tests - successful_tests}")

    if successful_tests == total_tests:
        print("🎉 All behavioral tests completed successfully!")
        return 0
    else:
        print("⚠️  Some tests encountered issues (see report for details)")
        return 1

if __name__ == "__main__":
    exit(main())
