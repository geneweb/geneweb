#!/usr/bin/env python3

import subprocess
import sys
import os

def install_missing_ocaml_deps():
    """Installe les dépendances OCaml manquantes détectées dans l'erreur de build"""

    print("🔧 Installing missing OCaml dependencies...")

    # Dépendances détectées depuis l'erreur de build
    missing_deps = [
        "cppo",
        "ppx_import",
        "ppx_deriving",
        "pp_loc",
        "fmt",
        "markup",
        "calendars",
        "zarith",
        "unidecode",
        "ocurl",
        "yojson",
        "lwt",
        "lwt_ssl",
        "digestif",
        "pcre"
    ]

    for dep in missing_deps:
        print(f"Installing {dep}...")
        try:
            result = subprocess.run([
                "opam", "install", "--yes", "--assume-depexts", dep
            ], check=True, capture_output=True, text=True)
            print(f"✅ {dep} installed successfully")
        except subprocess.CalledProcessError as e:
            print(f"❌ Failed to install {dep}: {e}")
            print(f"STDOUT: {e.stdout}")
            print(f"STDERR: {e.stderr}")

    print("🏁 Dependency installation completed")

if __name__ == "__main__":
    # Sourcer l'environnement opam
    os.system("eval $(opam env)")
    install_missing_ocaml_deps()
