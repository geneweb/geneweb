#!/usr/bin/env python3

import subprocess
import sys
import os

def install_geneweb_dependencies():
    """Installe toutes les dépendances OCaml nécessaires pour GeneWeb"""

    print("🔧 Installing all GeneWeb OCaml dependencies...")

    # Toutes les dépendances détectées depuis les erreurs de build
    all_deps = [
        # Build tools
        "dune",
        "ocamlfind",
        "camlp5",
        "cppo",

        # Core libraries
        "fmt",
        "re",
        "zarith",
        "num",

        # PPX libraries
        "ppx_import",
        "ppx_deriving",
        "pp_loc",

        # Web libraries
        "uri",
        "lwt",
        "lwt_ssl",
        "ocurl",
        "yojson",

        # Text processing
        "markup",
        "uunf",
        "unidecode",
        "pcre",

        # Compression
        "camlzip",

        # Templating
        "jingoo",

        # Date/time
        "calendars",

        # Crypto
        "digestif",
        "cryptokit",
        "base64",
        "uuidm"
    ]

    print(f"Installing {len(all_deps)} packages...")

    # Install all dependencies in one command for efficiency
    try:
        cmd = ["opam", "install", "--yes", "--assume-depexts"] + all_deps
        print(f"Running: {' '.join(cmd)}")

        result = subprocess.run(cmd, check=True, capture_output=True, text=True)
        print("✅ All dependencies installed successfully!")
        print(f"Output: {result.stdout[-500:]}")  # Last 500 chars

    except subprocess.CalledProcessError as e:
        print(f"❌ Installation failed: {e}")
        print(f"STDOUT: {e.stdout}")
        print(f"STDERR: {e.stderr}")

        # Try installing problematic packages individually
        print("\n🔄 Trying individual installation for failed packages...")

        failed_deps = []
        for dep in all_deps:
            try:
                result = subprocess.run([
                    "opam", "install", "--yes", "--assume-depexts", dep
                ], check=True, capture_output=True, text=True, timeout=120)
                print(f"✅ {dep}")
            except Exception as e:
                print(f"❌ {dep}: {e}")
                failed_deps.append(dep)

        if failed_deps:
            print(f"\n⚠️ Failed to install: {failed_deps}")
            print("These packages might not be critical for basic functionality.")

    # Verify installation
    print("\n📦 Verifying critical packages...")
    critical_packages = ["dune", "camlp5", "uri", "uunf", "camlzip", "jingoo"]

    for pkg in critical_packages:
        try:
            result = subprocess.run([
                "opam", "list", "--installed", pkg
            ], check=True, capture_output=True, text=True)
            if pkg in result.stdout:
                print(f"✅ {pkg} is installed")
            else:
                print(f"❌ {pkg} is missing")
        except Exception:
            print(f"❌ Could not verify {pkg}")

if __name__ == "__main__":
    # Make sure opam environment is loaded
    try:
        subprocess.run(["opam", "env"], check=True, capture_output=True)
        install_geneweb_dependencies()
    except FileNotFoundError:
        print("❌ OPAM not found. Please install OPAM first.")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Error setting up OPAM environment: {e}")
        sys.exit(1)
