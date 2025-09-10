#!/bin/bash

# Script pour exécuter les tests E2E GeneWeb avec Selenium

set -e

echo "🧪 Démarrage des tests E2E GeneWeb avec Selenium"

# Vérifier que GeneWeb est compilé
if [ ! -f "../../distribution/gw/gwd" ]; then
    echo "❌ GeneWeb n'est pas compilé. Veuillez exécuter 'make distrib' d'abord."
    exit 1
fi

# Créer le répertoire de rapports
mkdir -p reports

# Variables d'environnement
export GENEWEB_PORT=${GENEWEB_PORT:-2317}
export GENEWEB_BASE=${GENEWEB_BASE:-galichet}
export HEADLESS=${HEADLESS:-true}

echo "🚀 Exécution des tests avec Selenium..."
echo "   Port: $GENEWEB_PORT"
echo "   Base: $GENEWEB_BASE"
echo "   Headless: $HEADLESS"

# Exécuter les tests
pytest "$@" --html=reports/report.html --self-contained-html

echo "✅ Tests terminés. Rapport disponible dans reports/report.html"
