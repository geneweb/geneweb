#!/usr/bin/env python3

import os
import subprocess
import sys

def check_build_status():
    """Vérifie le statut de build de GeneWeb"""

    print("🔍 Checking GeneWeb build status...")

    # 1. Vérifier l'environnement OCaml
    try:
        result = subprocess.run(["ocaml", "-version"], capture_output=True, text=True)
        print(f"✅ OCaml version: {result.stdout.strip()}")
    except FileNotFoundError:
        print("❌ OCaml not found")
        return False

    # 2. Vérifier opam
    try:
        result = subprocess.run(["opam", "--version"], capture_output=True, text=True)
        print(f"✅ OPAM version: {result.stdout.strip()}")
    except FileNotFoundError:
        print("❌ OPAM not found")
        return False

    # 3. Lister les packages installés
    try:
        result = subprocess.run(["opam", "list", "--installed"], capture_output=True, text=True)
        installed_packages = result.stdout
        print(f"📦 Installed packages: {len(installed_packages.split()) // 3} packages")
    except Exception as e:
        print(f"⚠️ Could not list packages: {e}")

    # 4. Vérifier les fichiers de configuration
    config_files = ["configure.ml", "Makefile"]
    for config_file in config_files:
        if os.path.exists(config_file):
            print(f"✅ Found: {config_file}")
        else:
            print(f"❌ Missing: {config_file}")
            return False

    # 5. Vérifier la structure du projet
    expected_dirs = ["lib", "bin", "test"]
    for expected_dir in expected_dirs:
        if os.path.exists(expected_dir):
            print(f"✅ Found directory: {expected_dir}")
        else:
            print(f"❌ Missing directory: {expected_dir}")

    # 6. Essayer une compilation test
    print("\n🔨 Testing compilation...")
    try:
        # Source opam environment
        os.environ.update(dict(line.split('=', 1) for line in
                              subprocess.check_output(['opam', 'env']).decode().split('\n')
                              if '=' in line))

        # Try configure
        result = subprocess.run(["ocaml", "./configure.ml"],
                              capture_output=True, text=True, timeout=60)
        if result.returncode == 0:
            print("✅ Configure succeeded")
        else:
            print(f"❌ Configure failed: {result.stderr}")
            return False

        # Try make clean
        result = subprocess.run(["make", "clean"],
                              capture_output=True, text=True, timeout=60)
        print("✅ Make clean completed")

        return True

    except Exception as e:
        print(f"❌ Compilation test failed: {e}")
        return False

if __name__ == "__main__":
    success = check_build_status()
    sys.exit(0 if success else 1)
