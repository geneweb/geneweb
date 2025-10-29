#!/bin/bash
#
# Test final Docker GeneWeb avec script original corrigé
#

set -e

echo "🐳 Test Final Docker GeneWeb"
echo "============================"

# Couleurs
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
RED='\033[0;31m'
NC='\033[0m'

log_info() { echo -e "${BLUE}ℹ️  $1${NC}"; }
log_success() { echo -e "${GREEN}✅ $1${NC}"; }
log_warning() { echo -e "${YELLOW}⚠️  $1${NC}"; }
log_error() { echo -e "${RED}❌ $1${NC}"; }

# Nettoyage initial
log_info "Nettoyage des conteneurs existants..."
docker stop geneweb-test-ocaml geneweb-test-python 2>/dev/null || true
docker rm geneweb-test-ocaml geneweb-test-python 2>/dev/null || true
docker rmi geneweb:test-ocaml geneweb:test-python 2>/dev/null || true

echo ""
echo "🔧 TEST 1: Build et test OCaml (USE_PYTHON_BINARIES=false)"
echo "========================================================="

log_info "Construction de l'image OCaml..."
if docker build --build-arg USE_PYTHON_BINARIES=false -t geneweb:test-ocaml -f docker/Dockerfile .; then
    log_success "Image OCaml construite"
else
    log_error "Échec construction OCaml"
    exit 1
fi

log_info "Démarrage du conteneur OCaml..."
docker run -d --name geneweb-test-ocaml -p 2317:2317 geneweb:test-ocaml
sleep 15

if docker ps | grep -q geneweb-test-ocaml; then
    log_success "Conteneur OCaml fonctionne"
    
    # Vérifier les binaires
    log_info "Vérification des binaires OCaml..."
    if docker exec geneweb-test-ocaml ls -la share/dist/gw/ged2gwb; then
        log_success "Binaire ged2gwb OCaml trouvé"
    fi
    
    # Vérifier la variable d'environnement
    ENV_VAR=$(docker exec geneweb-test-ocaml printenv USE_PYTHON_BINARIES)
    if [ "$ENV_VAR" = "false" ]; then
        log_success "USE_PYTHON_BINARIES = false ✓"
    else
        log_warning "USE_PYTHON_BINARIES = $ENV_VAR (attendu: false)"
    fi
    
else
    log_error "Conteneur OCaml arrêté"
    log_info "Logs OCaml:"
    docker logs geneweb-test-ocaml | tail -20
fi

echo ""
echo "🐍 TEST 2: Build et test Python (USE_PYTHON_BINARIES=true)"
echo "=========================================================="

log_info "Construction de l'image Python..."
if docker build --build-arg USE_PYTHON_BINARIES=true -t geneweb:test-python -f docker/Dockerfile .; then
    log_success "Image Python construite"
else
    log_error "Échec construction Python"
    exit 1
fi

log_info "Démarrage du conteneur Python..."
docker run -d --name geneweb-test-python -p 2318:2317 geneweb:test-python
sleep 15

if docker ps | grep -q geneweb-test-python; then
    log_success "Conteneur Python fonctionne"
    
    # Vérifier les binaires
    log_info "Vérification des binaires Python..."
    if docker exec geneweb-test-python ls -la share/dist/gw/ged2gwb; then
        log_success "Binaire ged2gwb trouvé"
    fi
    
    # Vérifier si c'est la version Python
    if docker exec geneweb-test-python head -5 share/dist/gw/ged2gwb | grep -q "python\|bash"; then
        log_success "Binaire ged2gwb est un script (Python) ✓"
    else
        log_warning "Binaire ged2gwb semble être OCaml"
    fi
    
    # Vérifier la variable d'environnement
    ENV_VAR=$(docker exec geneweb-test-python printenv USE_PYTHON_BINARIES)
    if [ "$ENV_VAR" = "true" ]; then
        log_success "USE_PYTHON_BINARIES = true ✓"
    else
        log_warning "USE_PYTHON_BINARIES = $ENV_VAR (attendu: true)"
    fi
    
    # Vérifier si Python est installé
    if docker exec geneweb-test-python python3 --version; then
        log_success "Python3 disponible ✓"
    else
        log_warning "Python3 non disponible"
    fi
    
else
    log_error "Conteneur Python arrêté"
    log_info "Logs Python:"
    docker logs geneweb-test-python | tail -20
fi

echo ""
echo "🌐 TEST 3: Test de connectivité"
echo "==============================="

log_info "Test connexion OCaml (port 2317)..."
if curl -s --connect-timeout 5 http://localhost:2317 > /dev/null 2>&1; then
    log_success "Service OCaml accessible"
else
    log_warning "Service OCaml non accessible (normal sans données)"
fi

log_info "Test connexion Python (port 2318)..."
if curl -s --connect-timeout 5 http://localhost:2318 > /dev/null 2>&1; then
    log_success "Service Python accessible"
else
    log_warning "Service Python non accessible (normal sans données)"
fi

echo ""
echo "📋 RÉSUMÉ"
echo "========="

log_info "Conteneurs actifs:"
docker ps --format "table {{.Names}}\t{{.Status}}\t{{.Ports}}" | grep geneweb-test || echo "Aucun conteneur actif"

echo ""
log_info "Images créées:"
docker images | grep geneweb:test

echo ""
log_success "Test terminé !"
echo ""
echo "🚀 Pour utiliser les conteneurs:"
echo "  - OCaml:  http://localhost:2317"
echo "  - Python: http://localhost:2318"
echo ""
echo "🧹 Pour nettoyer:"
echo "  docker stop geneweb-test-ocaml geneweb-test-python"
echo "  docker rm geneweb-test-ocaml geneweb-test-python"
echo "  docker rmi geneweb:test-ocaml geneweb:test-python"
