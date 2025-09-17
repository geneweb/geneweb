#!/usr/bin/env python3

import subprocess
import sys

def verify_geneweb_deps():
    """Vérifie que toutes les dépendances GeneWeb sont installées"""

    required_deps = [
        "uri", "camlzip", "camlp5", "digestif", "camlp-streams",
        "jingoo", "ocamlfind", "ppx_import", "ppx_deriving", "re",
        "fmt", "pp_loc", "uunf", "markup", "yojson",
        "unidecode", "calendars", "dune"
    ]

    print("🔍 Verifying GeneWeb OCaml dependencies...")

    missing_deps = []
    installed_deps = []

    for dep in required_deps:
        try:
            result = subprocess.run([
                "opam", "list", "--installed", dep
            ], capture_output=True, text=True, check=True)

            if dep in result.stdout:
                installed_deps.append(dep)
                print(f"✅ {dep}")
            else:
                missing_deps.append(dep)
                print(f"❌ {dep}")
        except subprocess.CalledProcessError:
            missing_deps.append(dep)
            print(f"❌ {dep} (error checking)")

    print(f"\n📊 Summary:")
    print(f"  Installed: {len(installed_deps)}/{len(required_deps)}")
    print(f"  Missing: {len(missing_deps)}")

    if missing_deps:
        print(f"\n⚠️ Missing dependencies:")
        for dep in missing_deps:
            print(f"  - {dep}")

        print(f"\n🔧 To install missing dependencies:")
        print(f"opam install {' '.join(missing_deps)}")
        return False
    else:
        print(f"\n🎉 All dependencies are installed!")
        return True

if __name__ == "__main__":
    success = verify_geneweb_deps()
    sys.exit(0 if success else 1)
