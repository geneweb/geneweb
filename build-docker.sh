#!/bin/bash
#
# Script de build Docker avec gestion des permissions
#

set -e

echo "🐳 Build Docker GeneWeb avec gestion des permissions"
echo "===================================================="

# Couleurs
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m'

log_info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_success() { echo -e "${GREEN}✅ $1${NC}"; }

# S'assurer que les permissions sont correctes sur les fichiers source
log_info "Vérification des permissions des fichiers source..."
chmod +x docker/geneweb-launch.sh
log_success "Permissions corrigées"

# Build OCaml
log_info "Build image OCaml (USE_PYTHON_BINARIES=false)..."
if docker build --build-arg USE_PYTHON_BINARIES=false -t geneweb:test-ocaml -f docker/Dockerfile .; then
    log_success "Image OCaml construite"
else
    echo "❌ Échec build OCaml"
    exit 1
fi

# Build Python
log_info "Build image Python (USE_PYTHON_BINARIES=true)..."
if docker build --build-arg USE_PYTHON_BINARIES=true -t geneweb:test-python -f docker/Dockerfile .; then
    log_success "Image Python construite"
else
    echo "❌ Échec build Python"
    exit 1
fi

echo ""
log_success "Build terminé avec succès !"
echo ""
echo "📋 Images créées:"
docker images | grep geneweb:test

echo ""
echo "🚀 Pour tester:"
echo "  ./test-docker-final.sh"
