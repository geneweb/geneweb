# Tests E2E GeneWeb

Suite de tests End-to-End (E2E) pour GeneWeb utilisant Python et Selenium.

## Installation

```bash
pip install -r requirements.txt
```

## Exécution des tests

### Tests de base (recommandé pour commencer)
```bash
./run_tests.sh test_geneweb_simple.py
```

### Tous les tests
```bash
./run_tests.sh
```

### Tests spécifiques
```bash
pytest test_basic_navigation.py -v
pytest test_api_endpoints.py --html=reports/report.html
```

### Mode non-headless (voir le navigateur)
```bash
HEADLESS=false pytest test_geneweb_simple.py -v -s
```

## Structure

- `conftest.py` : Configuration et fixtures Selenium
- `test_geneweb_simple.py` : Tests de base fonctionnels ✅
- `test_basic_navigation.py` : Tests de navigation
- `test_data_management.py` : Tests de gestion des données
- `test_api_endpoints.py` : Tests des endpoints API
- `test_performance.py` : Tests de performance
- `test_accessibility.py` : Tests d'accessibilité
- `reports/` : Rapports HTML générés

## Prérequis

- GeneWeb compilé (`make distrib`)
- Base de données de test dans `./distribution/bases/`
- Chrome/Chromium installé

## Configuration

Variables d'environnement :
- `GENEWEB_PORT` : Port du serveur (défaut: 2317)
- `GENEWEB_BASE` : Nom de la base (défaut: galichet)
- `HEADLESS` : Mode headless (défaut: true)
