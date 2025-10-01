#!/bin/bash

echo "🔍 OCaml vs Python Compatibility Tests"
echo "======================================"

# Go to python/ directory (from python/scripts/)
cd "$(dirname "$0")/.."

GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m'

print_status() { echo -e "${BLUE}[INFO]${NC} $1"; }
print_success() { echo -e "${GREEN}[SUCCESS]${NC} $1"; }
print_error() { echo -e "${RED}[ERROR]${NC} $1"; }
print_warning() { echo -e "${YELLOW}[WARNING]${NC} $1"; }

# Check if OCaml binary exists
print_status "Looking for OCaml consang binary..."

OCAML_CONSANG=""
POSSIBLE_PATHS=(
    "../distribution/gw/consang"
    "./consang"
    "consang.opt"
    "/usr/local/bin/consang"
)

for path in "${POSSIBLE_PATHS[@]}"; do
    if [ -x "$path" ]; then
        OCAML_CONSANG="$path"
        print_success "OCaml binary found: $path"
        break
    fi
done

if [ -z "$OCAML_CONSANG" ]; then
    print_warning "OCaml consang binary not found in standard locations"
    print_status "Trying 'consang' in PATH..."
    if command -v consang >/dev/null 2>&1; then
        OCAML_CONSANG="consang"
        print_success "OCaml binary found in PATH"
    else
        print_error "No OCaml binary found"
        print_status "Limited compatibility tests (Python only)"
    fi
fi

print_status "Checking Python binary..."
if command -v consang >/dev/null 2>&1; then
    print_success "Python consang binary available"
else
    print_error "Python consang binary not found"
    print_status "Recommended installation: pip install -e python/"
    exit 1
fi

export PYTHONPATH=".:$PYTHONPATH"

print_status "Running compatibility tests..."
export CONSANG_OCAML_HELP=1

if [ -n "$OCAML_CONSANG" ]; then
    print_status "Complete OCaml vs Python tests..."
    CONSANG_OCAML_HELP=1 pytest tests/compatibility/test_ocaml_python_comparison.py -v -s \
        --tb=short
else
    print_warning "Limited tests (OCaml not available)"
    pytest tests/compatibility/test_ocaml_python_comparison.py -v -s \
        --tb=short \
        -k "not (help_output_identical or no_arguments_identical or invalid_option)"
fi

print_status "Generating compatibility report..."

python3 << 'EOF'
import subprocess
import json
from pathlib import Path

def test_binary(binary_name, args=[]):
    """Test a binary and return results."""
    try:
        result = subprocess.run(
            [binary_name] + args,
            capture_output=True,
            text=True,
            timeout=10
        )
        return {
            "available": True,
            "returncode": result.returncode,
            "stdout_lines": len(result.stdout.splitlines()),
            "stderr_lines": len(result.stderr.splitlines()),
            "help_contains_usage": "usage:" in result.stdout.lower()
        }
    except Exception as e:
        return {
            "available": False,
            "error": str(e)
        }

print("\n📊 RAPPORT DE COMPATIBILITÉ")
print("=" * 50)

ocaml_paths = [
    "./distribution/gw/consang",
    "./consang",
    "consang.opt",
    "/usr/local/bin/consang",
    "consang"
]

ocaml_result = None
ocaml_binary = None
for path in ocaml_paths:
    result = test_binary(path, ["--help"])
    if result["available"] and result["returncode"] == 0:
        ocaml_result = result
        ocaml_binary = path
        break

python_result = test_binary("consang", ["--help"])

print(f"\n🔧 BINAIRES:")
if ocaml_result:
    print(f"  OCaml ({ocaml_binary}): ✅ Disponible")
    print(f"    - Exit code: {ocaml_result['returncode']}")
    print(f"    - Stdout lines: {ocaml_result['stdout_lines']}")
    print(f"    - Has usage: {ocaml_result['help_contains_usage']}")
else:
    print(f"  OCaml: ❌ Non disponible")

if python_result["available"]:
    print(f"  Python (consang): ✅ Disponible")
    print(f"    - Exit code: {python_result['returncode']}")
    print(f"    - Stdout lines: {python_result['stdout_lines']}")
    print(f"    - Has usage: {python_result['help_contains_usage']}")
else:
    print(f"  Python (consang): ❌ Non disponible")
    print(f"    - Error: {python_result.get('error', 'Unknown')}")

print(f"\n🎯 ÉVALUATION:")
if ocaml_result and python_result["available"]:
    if (ocaml_result["returncode"] == python_result["returncode"] and
        ocaml_result["help_contains_usage"] == python_result["help_contains_usage"]):
        print("  ✅ Compatibilité de base: OK")
    else:
        print("  ⚠️  Compatibilité de base: Différences détectées")

    print("  📋 Tests détaillés disponibles via pytest")
else:
    print("  ⚠️  Comparaison limitée (un binaire manquant)")

print(f"\n💡 RECOMMANDATIONS:")
if not ocaml_result:
    print("  - Compiler le binaire OCaml pour tests complets")
    print("  - Vérifier: make && ls -la distribution/gw/consang")

if not python_result["available"]:
    print("  - Installer le binaire Python: pip install -e python/")

print("\n🚀 Pour tests détaillés:")
print("  pytest tests/compatibility/ -v -s")

EOF

print_success "Tests de compatibilité terminés!"

echo ""
echo "📁 Rapports disponibles:"
echo "   - Sortie console ci-dessus"
echo "   - Logs pytest détaillés"
echo ""
echo "💡 Pour réexécuter:"
echo "   ./run_compatibility_tests.sh"
