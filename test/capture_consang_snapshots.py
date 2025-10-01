#!/usr/bin/env python3

import subprocess
import json
import os
from pathlib import Path

def create_test_database():
    """Crée une base de données de test pour consang si nécessaire"""

    print("🔧 Creating test database for consang...")

    # Créer un fichier GEDCOM plus complexe avec des relations de consanguinité
    test_ged = """0 HEAD
1 SOUR test
1 GEDC
2 VERS 5.5.1
2 FORM LINEAGE-LINKED
1 CHAR UTF-8
0 @I1@ INDI
1 NAME Pierre /MARTIN/
1 SEX M
1 BIRT
2 DATE 1 JAN 1920
0 @I2@ INDI
1 NAME Marie /DURAND/
1 SEX F
1 BIRT
2 DATE 1 JAN 1925
0 @I3@ INDI
1 NAME Jean /MARTIN/
1 SEX M
1 BIRT
2 DATE 1 JAN 1945
1 FAMC @F1@
0 @I4@ INDI
1 NAME Anne /MARTIN/
1 SEX F
1 BIRT
2 DATE 1 JAN 1947
1 FAMC @F1@
0 @I5@ INDI
1 NAME Paul /BERNARD/
1 SEX M
1 BIRT
2 DATE 1 JAN 1940
0 @I6@ INDI
1 NAME Lucie /PETIT/
1 SEX F
1 BIRT
2 DATE 1 JAN 1942
0 @I7@ INDI
1 NAME Michel /BERNARD/
1 SEX M
1 BIRT
2 DATE 1 JAN 1965
1 FAMC @F2@
0 @I8@ INDI
1 NAME Sophie /BERNARD/
1 SEX F
1 BIRT
2 DATE 1 JAN 1967
1 FAMC @F2@
0 @I9@ INDI
1 NAME Pierre /MARTIN/
1 SEX M
1 BIRT
2 DATE 1 JAN 1970
1 FAMC @F3@
0 @I10@ INDI
1 NAME Julie /MARTIN/
1 SEX F
1 BIRT
2 DATE 1 JAN 1972
1 FAMC @F3@
0 @I11@ INDI
1 NAME Thomas /MARTIN/
1 SEX M
1 BIRT
2 DATE 1 JAN 1995
1 FAMC @F4@
0 @I12@ INDI
1 NAME Emma /MARTIN/
1 SEX F
1 BIRT
2 DATE 1 JAN 1997
1 FAMC @F4@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
1 CHIL @I4@
1 MARR
2 DATE 1 JAN 1944
0 @F2@ FAM
1 HUSB @I5@
1 WIFE @I6@
1 CHIL @I7@
1 CHIL @I8@
1 MARR
2 DATE 1 JAN 1964
0 @F3@ FAM
1 HUSB @I3@
1 WIFE @I8@
1 CHIL @I9@
1 CHIL @I10@
1 MARR
2 DATE 1 JAN 1969
0 @F4@ FAM
1 HUSB @I9@
1 WIFE @I10@
1 CHIL @I11@
1 CHIL @I12@
1 MARR
2 DATE 1 JAN 1994
0 TRLR"""

    # Sauvegarder le GEDCOM
    os.makedirs("test/fixtures", exist_ok=True)
    with open("test/fixtures/consang_test.ged", 'w') as f:
        f.write(test_ged)

    # Convertir en base GeneWeb si ged2gwb existe
    if os.path.exists("distribution/gw/ged2gwb"):
        try:
            # Supprimer l'ancienne base si elle existe et est accessible
            if os.path.exists("test-consang-db.gwb"):
                try:
                    import shutil
                    if os.path.isdir("test-consang-db.gwb"):
                        shutil.rmtree("test-consang-db.gwb")
                    else:
                        os.remove("test-consang-db.gwb")
                    print("🗑️  Removed existing test database")
                except (PermissionError, OSError) as e:
                    print(f"⚠️  Cannot remove existing database: {e}")
                    print("   Proceeding with existing database")
                    return True  # Use existing database

            result = subprocess.run([
                "distribution/gw/ged2gwb",
                "-o", "test-consang-db",
                "-f",
                "test/fixtures/consang_test.ged"
            ], check=True, capture_output=True, text=True)

            print("✅ Test database created: test-consang-db.gwb")
            return True

        except subprocess.CalledProcessError as e:
            print(f"⚠️  Could not create test database: {e}")
            print(f"STDOUT: {e.stdout}")
            print(f"STDERR: {e.stderr}")

            # Si la base existe déjà, on peut l'utiliser
            if os.path.exists("test-consang-db.gwb"):
                print("📁 Using existing test database")
                return True
            return False
    else:
        print("⚠️  ged2gwb not found, skipping database creation")
        return False

def capture_consang_snapshots():
    """Capture les snapshots de référence pour le binaire consang"""

    print("📸 Capturing consang reference snapshots...")

    # Vérifier que le binaire existe
    consang_binary = "distribution/gw/consang"
    if not os.path.exists(consang_binary):
        print(f"❌ Binary not found: {consang_binary}")
        print("Please build the OCaml binaries first with: make distrib")
        return False

    # Charger les cas de test
    test_file = "test/scena/consang.json"
    if not os.path.exists(test_file):
        print(f"❌ Test file not found: {test_file}")
        return False

    with open(test_file, 'r') as f:
        test_cases = json.load(f)

    # Créer le dossier de snapshots
    snapshots_dir = Path("test/snapshots/consang")
    snapshots_dir.mkdir(parents=True, exist_ok=True)
    snapshots = {}
    print(f"Running {len(test_cases)} test cases...")

    for i, test_case in enumerate(test_cases):
        test_name = test_case["name"]
        args = test_case["args"]
        input_data = test_case.get("input", "")

        print(f"  {i+1}/{len(test_cases)}: {test_name}")

        # Construire la commande
        cmd = [consang_binary] + args

        try:
            # Exécuter le binaire
            result = subprocess.run(
                cmd,
                input=input_data,
                capture_output=True,
                text=True,
                timeout=30
            )

            # Capturer le snapshot
            snapshot = {
                "command": " ".join(cmd),
                "args": args,
                "input": input_data,
                "stdout": result.stdout,
                "stderr": result.stderr,
                "returncode": result.returncode,
                "description": test_case.get("description", "")
            }

            snapshots[test_name] = snapshot

            # Afficher un résumé
            status = "✅" if result.returncode == 0 else "📝"
            stdout_len = len(result.stdout)
            stderr_len = len(result.stderr)
            print(f"    {status} Exit: {result.returncode}, STDOUT: {stdout_len} chars, STDERR: {stderr_len} chars")

            # Afficher les premières lignes si il y a de la sortie
            if result.stdout:
                first_line = result.stdout.split('\n')[0]
                print(f"      STDOUT: {first_line[:60]}{'...' if len(first_line) > 60 else ''}")
            if result.stderr:
                first_line = result.stderr.split('\n')[0]
                print(f"      STDERR: {first_line[:60]}{'...' if len(first_line) > 60 else ''}")

        except subprocess.TimeoutExpired:
            print(f"    ❌ TIMEOUT")
            snapshot = {
                "command": " ".join(cmd),
                "args": args,
                "input": input_data,
                "stdout": "",
                "stderr": "TIMEOUT",
                "returncode": -1,
                "description": test_case.get("description", "")
            }
            snapshots[test_name] = snapshot

        except Exception as e:
            print(f"    ❌ ERROR: {e}")
            snapshot = {
                "command": " ".join(cmd),
                "args": args,
                "input": input_data,
                "stdout": "",
                "stderr": str(e),
                "returncode": -1,
                "description": test_case.get("description", "")
            }
            snapshots[test_name] = snapshot

    # Sauvegarder les snapshots
    snapshots_file = snapshots_dir / "reference_outputs.json"
    with open(snapshots_file, 'w', encoding='utf-8') as f:
        json.dump(snapshots, f, indent=2, ensure_ascii=False)

    print(f"\n✅ Snapshots saved to: {snapshots_file}")
    print(f"📊 Captured {len(snapshots)} test cases")

    # Générer un rapport
    generate_report(snapshots, snapshots_dir)

    return True

def generate_report(snapshots, output_dir):
    """Génère un rapport des snapshots capturés"""

    report_file = output_dir / "snapshot_report.md"

    with open(report_file, 'w') as f:
        f.write("# Consang Reference Snapshots Report\n\n")
        f.write(f"Generated snapshots for {len(snapshots)} test cases.\n\n")

        f.write("## Test Cases Summary\n\n")
        f.write("| Test Name | Exit Code | STDOUT | STDERR | Description |\n")
        f.write("|-----------|-----------|--------|--------|--------------|\n")

        for name, snapshot in snapshots.items():
            stdout_len = len(snapshot["stdout"])
            stderr_len = len(snapshot["stderr"])
            exit_code = snapshot["returncode"]
            desc = snapshot["description"][:50] + "..." if len(snapshot["description"]) > 50 else snapshot["description"]

            f.write(f"| {name} | {exit_code} | {stdout_len} chars | {stderr_len} chars | {desc} |\n")

        f.write("\n## Detailed Outputs\n\n")

        for name, snapshot in snapshots.items():
            f.write(f"### {name}\n\n")
            f.write(f"**Command:** `{snapshot['command']}`\n\n")
            f.write(f"**Description:** {snapshot['description']}\n\n")
            f.write(f"**Exit Code:** {snapshot['returncode']}\n\n")

            if snapshot["stdout"]:
                f.write("**STDOUT:**\n```\n")
                f.write(snapshot["stdout"][:500])  # Limit to 500 chars
                if len(snapshot["stdout"]) > 500:
                    f.write("\n... (truncated)")
                f.write("\n```\n\n")
            else:
                f.write("**STDOUT:** (empty)\n\n")

            if snapshot["stderr"]:
                f.write("**STDERR:**\n```\n")
                f.write(snapshot["stderr"][:500])  # Limit to 500 chars
                if len(snapshot["stderr"]) > 500:
                    f.write("\n... (truncated)")
                f.write("\n```\n\n")
            else:
                f.write("**STDERR:** (empty)\n\n")

            f.write("---\n\n")

    print(f"📄 Report generated: {report_file}")

if __name__ == "__main__":
    print("🚀 Starting consang snapshot capture...")

    # Créer une base de test d'abord
    db_created = create_test_database()
    if db_created:
        print("✅ Test database ready for consang testing")
    else:
        print("⚠️  Proceeding without test database (will test error cases)")

    # Capturer les snapshots
    success = capture_consang_snapshots()

    if success:
        print("\n🎉 Snapshot capture completed successfully!")
        print("\nNext steps:")
        print("1. Review the snapshots in test/snapshots/consang/")
        print("2. Create Python implementation in python/consang/")
        print("3. Run Golden Master tests with: python test/run_golden_tests.py consang")
    else:
        print("\n❌ Snapshot capture failed!")

    exit(0 if success else 1)
