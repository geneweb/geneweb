#!/usr/bin/env python3
"""
Validation des snapshots Golden Master
"""

import json
import os
from pathlib import Path

def validate_snapshot_file(snapshot_file):
    """Valide un fichier de snapshot"""
    try:
        with open(snapshot_file, 'r') as f:
            data = json.load(f)

        if not isinstance(data, dict):
            return False, "Root should be a dictionary"

        required_fields = ['command', 'args', 'input', 'stdout', 'stderr', 'returncode', 'description']
        validation_errors = []

        for test_name, test_data in data.items():
            # Vérifier la présence des champs requis
            for field in required_fields:
                if field not in test_data:
                    validation_errors.append(f"Missing field '{field}' in test '{test_name}'")

            # Vérifier les types
            if 'returncode' in test_data and not isinstance(test_data['returncode'], int):
                validation_errors.append(f"returncode should be int in test '{test_name}', got {type(test_data['returncode'])}")

            # Vérifier que les chaînes sont bien des chaînes
            string_fields = ['command', 'input', 'stdout', 'stderr', 'description']
            for field in string_fields:
                if field in test_data and not isinstance(test_data[field], str):
                    validation_errors.append(f"{field} should be string in test '{test_name}', got {type(test_data[field])}")

            # Vérifier que args est une liste
            if 'args' in test_data and not isinstance(test_data['args'], list):
                validation_errors.append(f"args should be list in test '{test_name}', got {type(test_data['args'])}")

        if validation_errors:
            return False, "; ".join(validation_errors)

        return True, f"Valid snapshot with {len(data)} test cases"

    except json.JSONDecodeError as e:
        return False, f"Invalid JSON: {e}"
    except Exception as e:
        return False, f"Error: {e}"

def analyze_snapshot_content(snapshot_file):
    """Analyse le contenu des snapshots pour des statistiques"""
    try:
        with open(snapshot_file, 'r') as f:
            data = json.load(f)

        stats = {
            'total_tests': len(data),
            'exit_codes': {},
            'has_stdout': 0,
            'has_stderr': 0,
            'empty_output': 0,
            'timeout_tests': 0,
            'error_tests': 0
        }

        for test_name, test_data in data.items():
            # Analyser les codes de sortie
            exit_code = test_data.get('returncode', 'unknown')
            stats['exit_codes'][exit_code] = stats['exit_codes'].get(exit_code, 0) + 1

            # Analyser les sorties
            stdout = test_data.get('stdout', '')
            stderr = test_data.get('stderr', '')

            if stdout:
                stats['has_stdout'] += 1
            if stderr:
                stats['has_stderr'] += 1
            if not stdout and not stderr:
                stats['empty_output'] += 1

            # Détecter les timeouts et erreurs
            if 'TIMEOUT' in stderr:
                stats['timeout_tests'] += 1
            if exit_code == -1:
                stats['error_tests'] += 1

        return stats

    except Exception as e:
        return None

def main():
    """Valide tous les snapshots"""
    snapshots_dir = Path("test/snapshots")

    if not snapshots_dir.exists():
        print("❌ No snapshots directory found")
        return 1

    print("🔍 Validating snapshots...")

    total_files = 0
    valid_files = 0
    all_stats = {}

    for binary_dir in snapshots_dir.iterdir():
        if binary_dir.is_dir():
            snapshot_file = binary_dir / "reference_outputs.json"

            if snapshot_file.exists():
                total_files += 1
                binary_name = binary_dir.name

                valid, message = validate_snapshot_file(snapshot_file)

                if valid:
                    valid_files += 1
                    print(f"✅ {binary_name}: {message}")

                    # Analyser le contenu
                    stats = analyze_snapshot_content(snapshot_file)
                    if stats:
                        all_stats[binary_name] = stats
                        print(f"   📊 Exit codes: {dict(list(stats['exit_codes'].items())[:3])}")
                        print(f"   📄 Output: {stats['has_stdout']} with stdout, {stats['has_stderr']} with stderr")
                        if stats['timeout_tests'] > 0:
                            print(f"   ⏱️  {stats['timeout_tests']} timeout(s)")
                        if stats['error_tests'] > 0:
                            print(f"   ❌ {stats['error_tests']} error(s)")
                else:
                    print(f"❌ {binary_name}: {message}")
            else:
                print(f"⚠️  {binary_dir.name}: No reference_outputs.json")

    # Résumé global
    print(f"\n📊 Summary: {valid_files}/{total_files} valid snapshot files")

    if all_stats:
        print(f"\n📈 Content Analysis:")
        total_tests = sum(stats['total_tests'] for stats in all_stats.values())
        total_timeouts = sum(stats['timeout_tests'] for stats in all_stats.values())
        total_errors = sum(stats['error_tests'] for stats in all_stats.values())

        print(f"   Total test cases: {total_tests}")
        if total_timeouts > 0:
            print(f"   Total timeouts: {total_timeouts}")
        if total_errors > 0:
            print(f"   Total errors: {total_errors}")

        # Analyse des codes de sortie globaux
        global_exit_codes = {}
        for stats in all_stats.values():
            for code, count in stats['exit_codes'].items():
                global_exit_codes[code] = global_exit_codes.get(code, 0) + count

        print(f"   Exit code distribution: {dict(sorted(global_exit_codes.items()))}")

    return 0 if valid_files == total_files else 1

if __name__ == "__main__":
    exit(main())
