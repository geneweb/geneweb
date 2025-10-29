#!/bin/bash
#
# Test simple des conteneurs Docker GeneWeb
#

set -e

echo "🐳 Test Simple Docker GeneWeb"
echo "============================="

# Build et test OCaml
echo "1. Build image OCaml..."
docker build --build-arg USE_PYTHON_BINARIES=false -t geneweb:test-ocaml -f docker/Dockerfile .

echo "2. Test conteneur OCaml..."
docker run -d --name geneweb-test-ocaml -p 2317:2317 geneweb:test-ocaml
sleep 15

echo "3. Vérification conteneur OCaml..."
if docker ps | grep geneweb-test-ocaml; then
    echo "✅ Conteneur OCaml fonctionne"
    echo "📋 Logs OCaml (dernières lignes):"
    docker logs geneweb-test-ocaml | tail -10
else
    echo "❌ Conteneur OCaml arrêté"
    docker logs geneweb-test-ocaml
fi

echo "4. Nettoyage OCaml..."
docker stop geneweb-test-ocaml 2>/dev/null || true
docker rm geneweb-test-ocaml 2>/dev/null || true

echo ""
echo "5. Build image Python..."
docker build --build-arg USE_PYTHON_BINARIES=true -t geneweb:test-python -f docker/Dockerfile .

echo "6. Test conteneur Python..."
docker run -d --name geneweb-test-python -p 2318:2317 geneweb:test-python
sleep 15

echo "7. Vérification conteneur Python..."
if docker ps | grep geneweb-test-python; then
    echo "✅ Conteneur Python fonctionne"
    echo "📋 Logs Python (dernières lignes):"
    docker logs geneweb-test-python | tail -10
else
    echo "❌ Conteneur Python arrêté"
    docker logs geneweb-test-python
fi

echo "8. Nettoyage Python..."
docker stop geneweb-test-python 2>/dev/null || true
docker rm geneweb-test-python 2>/dev/null || true

echo ""
echo "✅ Test terminé !"
echo "Les deux images sont disponibles:"
echo "  - geneweb:test-ocaml"
echo "  - geneweb:test-python"
