"""
Lister et ouvrir les artefacts de tests (screenshots / html / logs).

Exemples:
  - python view_artifacts.py --list
  - python view_artifacts.py --latest
  - python view_artifacts.py --open my_test.setup.20250909T180000Z.png
"""
from pathlib import Path
import sys
import argparse
import subprocess
import platform

ROOT = Path(__file__).resolve().parents[1]  # tests/e2e
ARTIFACTS_DIR = ROOT / "reports" / "artifacts"


def list_artifacts():
    if not ARTIFACTS_DIR.exists():
        print(f"No artifacts dir: {ARTIFACTS_DIR}")
        return 1
    files = sorted(ARTIFACTS_DIR.iterdir(), key=lambda p: p.stat().st_mtime, reverse=True)
    if not files:
        print("No artifacts found.")
        return 0
    for f in files:
        print(f.name)
    return 0


def open_file(path: Path):
    if not path.exists():
        print(f"File not found: {path}")
        return 2
    system = platform.system()
    try:
        if system == "Darwin":
            subprocess.run(["open", str(path)])
        elif system == "Windows":
            subprocess.run(["start", str(path)], shell=True)
        else:
            # assume Linux-like
            subprocess.run(["xdg-open", str(path)])
    except Exception as e:
        print(f"Failed to open {path}: {e}")
        return 3
    return 0


def open_latest():
    if not ARTIFACTS_DIR.exists():
        print(f"No artifacts dir: {ARTIFACTS_DIR}")
        return 1
    files = sorted(ARTIFACTS_DIR.glob("*"), key=lambda p: p.stat().st_mtime, reverse=True)
    if not files:
        print("No artifacts found.")
        return 0
    latest = files[0]
    print(f"Opening latest artifact: {latest.name}")
    return open_file(latest)


def main():
    p = argparse.ArgumentParser()
    p.add_argument("--list", action="store_true", help="Lister les artefacts")
    p.add_argument("--latest", action="store_true", help="Ouvrir le dernier artefact")
    p.add_argument("--open", metavar="FILENAME", help="Ouvrir un fichier précis (nom exact)")
    args = p.parse_args()

    if args.list:
        sys.exit(list_artifacts())
    if args.latest:
        sys.exit(open_latest())
    if args.open:
        sys.exit(open_file(ARTIFACTS_DIR / args.open))

    p.print_help()
    sys.exit(0)


if __name__ == "__main__":
    main()
