#!/bin/bash

echo "🔧 Installation de l'architecture de tests hybride"
echo "================================================="

cd "$(dirname "$0")"

# Couleurs pour l'affichage
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

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

# Vérifier Python
print_status "Vérification de l'environnement Python..."
if ! command -v python &> /dev/null; then
    print_error "Python non trouvé. Veuillez installer Python 3.8+"
    exit 1
fi

PYTHON_VERSION=$(python --version 2>&1 | cut -d' ' -f2 | cut -d'.' -f1,2)
print_success "Python $PYTHON_VERSION détecté"

# Installer le package consang
print_status "Installation du package consang Python..."
if [ -d "python/" ]; then
    pip install -e python/
    if [ $? -eq 0 ]; then
        print_success "Package consang installé"
    else
        print_error "Échec de l'installation du package consang"
        exit 1
    fi
else
    print_warning "Répertoire python/ non trouvé, sautant l'installation"
fi

# Installer les dépendances de test
print_status "Installation des dépendances de test..."
DEPENDENCIES=(
    "PyYAML>=6.0"
    "pytest>=7.0"
    "pytest-cov>=4.0"
    "pytest-html>=3.1"
    "pytest-mock>=3.10"
)

for dep in "${DEPENDENCIES[@]}"; do
    print_status "Installation de $dep..."
    pip install "$dep"
done

print_success "Dépendances installées"

# Créer les répertoires nécessaires
print_status "Création de la structure de répertoires..."
mkdir -p tests/reports
mkdir -p tests/config
mkdir -p tests/common
mkdir -p tests/unit/python/test_consang
mkdir -p tests/unit/python/test_geneweb_common
mkdir -p tests/integration
mkdir -p tests/golden_master
mkdir -p tests/performance

print_success "Structure de répertoires créée"

# Rendre les scripts exécutables
print_status "Configuration des permissions des scripts..."
for script in run_hybrid_tests.sh cleanup_tests_final.sh run_optimal_tests.sh run_clean_consang_tests.sh; do
    if [ -f "$script" ]; then
        chmod +x "$script"
        print_success "Script $script rendu exécutable"
    else
        print_warning "Script $script non trouvé"
    fi
done

# Nettoyer les anciens fichiers si demandé
echo ""
print_status "Nettoyage des anciens fichiers de test..."
read -p "🗑️  Supprimer les anciens fichiers de test ? (y/N) " -n 1 -r
echo ""

if [[ $REPLY =~ ^[Yy]$ ]]; then
    if [ -f "cleanup_tests_final.sh" ]; then
        print_status "Exécution du nettoyage..."
        ./cleanup_tests_final.sh
        print_success "Nettoyage terminé"
    else
        print_warning "Script de nettoyage non trouvé"
    fi
else
    print_status "Nettoyage sauté"
fi

# Vérifier que consang est installé et fonctionne
echo ""
print_status "Vérification de l'installation de consang..."
if command -v consang &> /dev/null; then
    print_success "Commande consang disponible"

    # Test rapide
    if consang --help > /dev/null 2>&1; then
        print_success "consang fonctionne correctement"
    else
        print_warning "consang installé mais ne fonctionne pas correctement"
    fi
else
    print_warning "Commande consang non disponible dans PATH"
    print_status "Essayez: pip install -e python/"
fi

# Valider la configuration hybride
print_status "Validation de l'architecture hybride..."
if [ -f "tests/config/main.yaml" ] && [ -f "tests/config/golden_data.json" ]; then
    python -c "
import sys
sys.path.insert(0, '.')
try:
    from tests.common.hybrid_config import hybrid_config

    # Tester le chargement de configuration
    errors = hybrid_config.validate_config()
    if errors:
        print('❌ Erreurs de configuration:')
        for error in errors:
            print(f'   - {error}')
        sys.exit(1)

    # Afficher le résumé
    summary = hybrid_config.get_summary()
    print('✅ Configuration hybride valide')
    print(f'📋 Projet: {summary.get(\"project\", {}).get(\"name\", \"N/A\")}')
    print(f'📊 Binaires activés: {summary.get(\"enabled_binaries\", [])}')
    print(f'🧪 Total cas de test: {summary.get(\"total_test_cases\", 0)}')

    for binary, suites in summary.get('test_suites', {}).items():
        print(f'   {binary}: {\", \".join(suites)}')

except Exception as e:
    print(f'❌ Erreur lors de la validation: {e}')
    print('💡 Vérifiez que tous les fichiers de configuration sont présents')
    sys.exit(1)
"

    if [ $? -eq 0 ]; then
        print_success "Architecture hybride validée"
    else
        print_error "Problème avec la configuration hybride"
        exit 1
    fi
else
    print_error "Fichiers de configuration manquants:"
    print_error "  - tests/config/main.yaml"
    print_error "  - tests/config/golden_data.json"
    exit 1
fi

# Créer un fichier de test rapide
print_status "Création d'un test de validation..."
cat > test_installation.py << 'EOF'
#!/usr/bin/env python3
"""Test rapide de validation de l'installation."""

import sys
import subprocess

def test_imports():
    """Teste les imports principaux."""
    try:
        import yaml
        import pytest
        print("✅ Dépendances Python importées")
        return True
    except ImportError as e:
        print(f"❌ Erreur d'import: {e}")
        return False

def test_config():
    """Teste la configuration hybride."""
    try:
        sys.path.insert(0, '.')
        from tests.common.hybrid_config import hybrid_config

        config = hybrid_config.load_main_config()
        golden = hybrid_config.load_golden_data()

        print("✅ Configuration hybride chargée")
        return True
    except Exception as e:
        print(f"❌ Erreur de configuration: {e}")
        return False

def test_consang_command():
    """Teste la commande consang."""
    try:
        result = subprocess.run(['consang', '--help'],
                              capture_output=True, text=True, timeout=5)
        if result.returncode == 0:
            print("✅ Commande consang fonctionne")
            return True
        else:
            print(f"❌ consang retourne le code {result.returncode}")
            return False
    except Exception as e:
        print(f"❌ Erreur avec consang: {e}")
        return False

if __name__ == "__main__":
    print("🧪 Test de validation de l'installation")
    print("=" * 40)

    tests = [
        ("Imports Python", test_imports),
        ("Configuration", test_config),
        ("Commande consang", test_consang_command),
    ]

    passed = 0
    total = len(tests)

    for name, test_func in tests:
        print(f"\n📋 Test: {name}")
        if test_func():
            passed += 1

    print(f"\n📊 Résultat: {passed}/{total} tests réussis")

    if passed == total:
        print("🎉 Installation validée avec succès !")
        sys.exit(0)
    else:
        print("❌ Problèmes détectés dans l'installation")
        sys.exit(1)
EOF

chmod +x test_installation.py

# Exécuter le test de validation
echo ""
print_status "Exécution du test de validation..."
python test_installation.py

validation_result=$?

# Nettoyer le fichier de test
rm -f test_installation.py

# Résumé final
echo ""
echo "📊 RÉSUMÉ DE L'INSTALLATION"
echo "=========================="

if [ $validation_result -eq 0 ]; then
    print_success "Installation hybride terminée avec succès !"

    echo ""
    echo "🚀 UTILISATION:"
    echo "  ./run_hybrid_tests.sh              # Tous les tests"
    echo "  pytest tests/golden_master/ -v     # Golden Master seulement"
    echo "  pytest tests/unit/ -v              # Tests unitaires seulement"
    echo ""
    echo "📁 STRUCTURE CRÉÉE:"
    echo "  tests/config/main.yaml             # Configuration principale"
    echo "  tests/config/golden_data.json      # Données de test"
    echo "  tests/reports/                     # Rapports générés"
    echo ""
    echo "📄 RAPPORTS:"
    echo "  tests/reports/hybrid_report.html   # Rapport principal"
    echo "  tests/reports/coverage/            # Couverture de code"

else
    print_error "Installation échouée"
    echo ""
    echo "🔧 DÉPANNAGE:"
    echo "  1. Vérifiez que Python 3.8+ est installé"
    echo "  2. Installez les dépendances: pip install PyYAML pytest"
    echo "  3. Installez consang: pip install -e python/"
    echo "  4. Vérifiez les fichiers de configuration"

    exit 1
fi

echo ""
print_success "Installation hybride terminée !"
echo "💡 Lancez './run_hybrid_tests.sh' pour tester votre installation"
