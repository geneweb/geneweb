#!/bin/bash

echo "🧪 Geneweb Tests - Optimized Hybrid Architecture"
echo "================================================"

# Go to python/ directory (from python/scripts/)
cd "$(dirname "$0")/.."

# Colors for display
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Colored display functions
print_status() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

print_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

print_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

print_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

# Check dependencies
print_status "Checking dependencies..."

if ! python3 -c "import yaml" 2>/dev/null; then
    print_warning "PyYAML missing. Installing..."
    pip install PyYAML
fi

if ! python3 -c "import pytest" 2>/dev/null; then
    print_error "pytest missing. Installing..."
    pip install pytest pytest-cov pytest-html
fi

# Configure environment (we are already in python/)
export PYTHONPATH=".:$PYTHONPATH"
# Ensure integration/hybrid tests use Python binary name in help banner
unset CONSANG_OCAML_HELP

# Validate configuration
print_status "Validating hybrid configuration..."
# Use python from virtual environment if available, otherwise python3
PYTHON_CMD="python3"
if [ -n "$VIRTUAL_ENV" ]; then
    PYTHON_CMD="python"
fi
$PYTHON_CMD -c "
from tests.common.hybrid_config import hybrid_config

# Valider la configuration
errors = hybrid_config.validate_config()
if errors:
    print('❌ Erreurs de configuration:')
    for error in errors:
        print(f'   - {error}')
    exit(1)

# Afficher le résumé
summary = hybrid_config.get_summary()
print('✅ Configuration valide')
print(f'📋 Projet: {summary.get(\"project\", {}).get(\"name\", \"N/A\")}')
print(f'📊 Binaires activés: {summary.get(\"enabled_binaries\", [])}')
print(f'🧪 Total cas de test: {summary.get(\"total_test_cases\", 0)}')

for binary, suites in summary.get('test_suites', {}).items():
    print(f'   {binary}: {', '.join(suites)}')
"

if [ $? -ne 0 ]; then
    exit 1
fi

print_success "Configuration validée"

# Aller au répertoire python/ (depuis python/scripts/)
cd "$(dirname "$0")/.."

# Créer le répertoire des rapports
mkdir -p tests/reports

# Exécuter les tests par suite
echo ""
print_status "Exécution des tests unitaires..."
pytest tests/unit/python/test_consang/test_core_functionality.py -v --tb=short
echo ""
print_status "Exécution des tests d'intégration..."
# Ensure integration tests use Python binary name in help banner
unset CONSANG_OCAML_HELP
pytest tests/integration/test_consang_simple.py -v --tb=short

echo ""
print_status "Exécution des tests Golden Master hybrides..."
pytest tests/golden_master/test_hybrid_golden.py -v --tb=short

echo ""
print_status "Exécution des tests de performance..."
pytest tests/performance/test_consang_simple.py -v --tb=short

# Générer un rapport complet
echo ""
print_status "Génération du rapport complet..."
export CONSANG_OCAML_HELP=1
pytest tests/ -v \
    --html=tests/reports/hybrid_report.html \
    --self-contained-html \
    --cov=python \
    --cov-report=html:tests/reports/coverage \
    --cov-report=term-missing \
    --tb=short \
    -m "not slow"
# Unset it again after the comprehensive tests
unset CONSANG_OCAML_HELP

# Afficher le résumé final
echo ""
echo "📊 RÉSUMÉ FINAL"
echo "==============="

$PYTHON_CMD -c "
from pathlib import Path
import json

# Lire les résultats Golden Master si disponibles
reports_dir = Path('tests/reports')
golden_files = list(reports_dir.glob('golden_master_*.json'))

if golden_files:
    print('🏆 Résultats Golden Master:')
    for file in golden_files:
        with open(file) as f:
            data = json.load(f)
        binary = data['binary_name']
        summary = data['summary']
        print(f'   {binary}: {summary[\"passed\"]}/{summary[\"total_tests\"]} ({summary[\"success_rate\"]:.1f}%)')

# Vérifier les rapports
html_report = reports_dir / 'hybrid_report.html'
if html_report.exists():
    print(f'📄 Rapport HTML: {html_report}')

coverage_dir = reports_dir / 'coverage'
if coverage_dir.exists():
    print(f'📈 Couverture: {coverage_dir / \"index.html\"}')
"

print_success "Tests hybrides terminés!"

echo ""
echo "📁 Rapports disponibles:"
echo "   - tests/reports/hybrid_report.html (rapport principal)"
echo "   - tests/reports/coverage/index.html (couverture de code)"
echo "   - tests/reports/golden_master_*.json (résultats Golden Master)"
