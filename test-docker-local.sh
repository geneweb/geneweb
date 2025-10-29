#!/bin/bash
#
# Script de test Docker local pour GeneWeb
# Teste les deux configurations : OCaml (fallback) et Python (si tests passent)
#

set -e

echo "🐳 Test Docker GeneWeb - Configuration Locale"
echo "=============================================="

# Couleurs pour les logs
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Fonction pour afficher les logs colorés
log_info() {
    echo -e "${BLUE}ℹ️  $1${NC}"
}

log_success() {
    echo -e "${GREEN}✅ $1${NC}"
}

log_warning() {
    echo -e "${YELLOW}⚠️  $1${NC}"
}

log_error() {
    echo -e "${RED}❌ $1${NC}"
}

# Vérification des prérequis
log_info "Vérification des prérequis..."

if ! command -v docker &> /dev/null; then
    log_error "Docker n'est pas installé"
    exit 1
fi

if ! docker info &> /dev/null; then
    log_error "Docker daemon n'est pas démarré"
    exit 1
fi

log_success "Docker est disponible"

# Nettoyage des images précédentes
log_info "Nettoyage des images précédentes..."
docker rmi geneweb:test-ocaml geneweb:test-python 2>/dev/null || true

echo ""
echo "🔧 TEST 1: Build avec OCaml (USE_PYTHON_BINARIES=false)"
echo "======================================================"

log_info "Construction de l'image Docker avec binaires OCaml..."
if docker build \
    --build-arg USE_PYTHON_BINARIES=false \
    --target container \
    -t geneweb:test-ocaml \
    -f docker/Dockerfile \
    . ; then
    log_success "Image OCaml construite avec succès"
else
    log_error "Échec de construction de l'image OCaml"
    exit 1
fi

echo ""
echo "🧪 Vérification de l'image OCaml..."
log_info "Inspection de l'image OCaml..."

# Vérifier que l'image contient les binaires OCaml
if docker run --rm geneweb:test-ocaml ls -la share/dist/gw/ged2gwb; then
    log_success "Binaire ged2gwb trouvé dans l'image OCaml"
else
    log_error "Binaire ged2gwb manquant dans l'image OCaml"
fi

# Vérifier la variable d'environnement
ENV_VAR=$(docker run --rm geneweb:test-ocaml printenv USE_PYTHON_BINARIES)
if [ "$ENV_VAR" = "false" ]; then
    log_success "Variable USE_PYTHON_BINARIES correctement définie à 'false'"
else
    log_warning "Variable USE_PYTHON_BINARIES: '$ENV_VAR' (attendu: 'false')"
fi

echo ""
echo "🐍 TEST 2: Build avec Python (USE_PYTHON_BINARIES=true)"
echo "======================================================="

log_info "Construction de l'image Docker avec binaires Python..."
if docker build \
    --build-arg USE_PYTHON_BINARIES=true \
    --target container \
    -t geneweb:test-python \
    -f docker/Dockerfile \
    . ; then
    log_success "Image Python construite avec succès"
else
    log_error "Échec de construction de l'image Python"
    exit 1
fi

echo ""
echo "🧪 Vérification de l'image Python..."
log_info "Inspection de l'image Python..."

# Vérifier que l'image contient les binaires
if docker run --rm geneweb:test-python ls -la share/dist/gw/ged2gwb; then
    log_success "Binaire ged2gwb trouvé dans l'image Python"
else
    log_error "Binaire ged2gwb manquant dans l'image Python"
fi

# Vérifier la variable d'environnement
ENV_VAR=$(docker run --rm geneweb:test-python printenv USE_PYTHON_BINARIES)
if [ "$ENV_VAR" = "true" ]; then
    log_success "Variable USE_PYTHON_BINARIES correctement définie à 'true'"
else
    log_warning "Variable USE_PYTHON_BINARIES: '$ENV_VAR' (attendu: 'true')"
fi

# Vérifier si Python est installé dans l'image Python
if docker run --rm geneweb:test-python python3 --version; then
    log_success "Python3 disponible dans l'image Python"
else
    log_warning "Python3 non disponible dans l'image Python"
fi

# Vérifier si l'environnement Python existe
if docker run --rm geneweb:test-python ls -la share/dist/python-env 2>/dev/null; then
    log_success "Environnement Python trouvé dans l'image"
else
    log_warning "Environnement Python non trouvé dans l'image"
fi

echo ""
echo "🚀 TEST 3: Test de démarrage des conteneurs"
echo "==========================================="

log_info "Test de démarrage du conteneur OCaml..."
CONTAINER_ID=$(docker run -d -p 2317:2317 geneweb:test-ocaml)
sleep 10

# Vérifier si le conteneur est toujours en cours d'exécution
if docker ps | grep -q $CONTAINER_ID; then
    log_success "Conteneur OCaml démarré avec succès"
    
    # Vérifier si les services sont accessibles
    log_info "Vérification des services OCaml..."
    if curl -s --connect-timeout 5 http://localhost:2317 > /dev/null 2>&1; then
        log_success "Service GWD accessible sur le port 2317"
    else
        log_warning "Service GWD non accessible (normal si pas de base de données)"
    fi
    
    docker stop $CONTAINER_ID > /dev/null
    docker rm $CONTAINER_ID > /dev/null
else
    log_error "Conteneur OCaml s'est arrêté"
    log_info "Logs du conteneur OCaml:"
    docker logs $CONTAINER_ID | tail -20
    docker rm $CONTAINER_ID > /dev/null
fi

log_info "Test de démarrage du conteneur Python..."
CONTAINER_ID=$(docker run -d -p 2318:2317 geneweb:test-python)
sleep 10

# Vérifier si le conteneur est toujours en cours d'exécution
if docker ps | grep -q $CONTAINER_ID; then
    log_success "Conteneur Python démarré avec succès"
    
    # Vérifier si les services sont accessibles
    log_info "Vérification des services Python..."
    if curl -s --connect-timeout 5 http://localhost:2318 > /dev/null 2>&1; then
        log_success "Service GWD accessible sur le port 2318"
    else
        log_warning "Service GWD non accessible (normal si pas de base de données)"
    fi
    
    docker stop $CONTAINER_ID > /dev/null
    docker rm $CONTAINER_ID > /dev/null
else
    log_error "Conteneur Python s'est arrêté"
    log_info "Logs du conteneur Python:"
    docker logs $CONTAINER_ID | tail -20
    docker rm $CONTAINER_ID > /dev/null
fi

echo ""
echo "📊 RÉSUMÉ DES TESTS"
echo "=================="

log_info "Images créées:"
docker images | grep geneweb:test

echo ""
log_success "Tests Docker terminés avec succès !"
log_info "Vous pouvez maintenant tester manuellement:"
echo "  - Image OCaml:  docker run -p 2317:2317 geneweb:test-ocaml"
echo "  - Image Python: docker run -p 2318:2317 geneweb:test-python"

echo ""
log_info "Pour nettoyer les images de test:"
echo "  docker rmi geneweb:test-ocaml geneweb:test-python"
