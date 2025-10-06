# Tests GeneWeb - Guide Ultra Simple

## 📁 Structure
```
tests/
├── conftest.py           # Configuration pytest
└── binaries/consang/     # Tests consang
    ├── unit/             # Tests unitaires
    ├── integration/      # Tests d'intégration
    ├── compatibility/    # Tests compatibilité
    └── performance/      # Tests performance
```

## 🚀 Utilisation

```bash
# Tous les tests
pytest tests/ -v

# Par type
pytest -m unit -v
pytest -m integration -v
pytest -m compatibility -v
pytest -m performance -v

# Tests rapides seulement
pytest -m "not slow" -v

# Consang seulement
pytest tests/binaries/consang/ -v
```

## 📝 Écrire un test

### Exemple simple
```python
import subprocess

def test_something():
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    assert result.returncode == 0
    assert "usage" in result.stdout
```

### Outils utiles
- `subprocess.run()` pour exécuter des binaires
- `capture_output=True, text=True` pour capturer stdout/stderr
- `assert result.returncode == 0` pour vérifier le succès
- `pytest.mark.slow` pour les tests lents
- `pytest.skip()` pour ignorer un test

## 🏷️ Marqueurs
- `@pytest.mark.consang` : Test pour consang
- `@pytest.mark.unit` : Test unitaire
- `@pytest.mark.integration` : Test d'intégration
- `@pytest.mark.compatibility` : Test compatibilité
- `@pytest.mark.performance` : Test performance
- `@pytest.mark.slow` : Test lent

C'est tout ! Simple et efficace.
