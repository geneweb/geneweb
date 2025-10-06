# Golden Master Tests - Framework Générique

Framework générique de tests Golden Master pour **comparer n'importe quels deux exécutables** avec pytest, indépendamment du langage de programmation.

## 🎯 Principe

Compare deux exécutables (référence vs candidat) en :
1. Exécutant la **version de référence** pour créer les golden masters
2. Exécutant la **version candidate** avec les mêmes paramètres  
3. Comparant **toutes les sorties** : stdout, stderr, fichiers, codes de retour

## ⚙️ Configuration Complète (JSON)

Tout est configurable dans `test_config.json` :

```json
{
  "test_config": {
    "executables": {
      "reference": {
        "path": "path/to/old/binary",
        "type": "binary",
        "description": "Version de référence"
      },
      "candidate": {
        "path": "path/to/new/script.py", 
        "type": "script",
        "interpreter": "python3",
        "description": "Version candidate"
      }
    },
    "environment": {
      "timeout_seconds": 30,
      "encoding": "utf-8",
      "env_vars": {"VAR": "value"}
    },
    "comparison": {
      "ignore_patterns": ["^# Generated.*"],
      "normalize_whitespace": true,
      "normalize_line_endings": true
    },
    "output_capture": {
      "stdout": true,
      "stderr": true, 
      "files": true,
      "return_code": true
    }
  },
  "test_cases": [
    {
      "name": "test_basic",
      "description": "Test basique",
      "input_files": ["input.txt"],
      "args": ["-option", "value"],
      "expected_output_files": ["*.out"],
      "working_dir": "test_basic"
    }
  ]
}
```

## 🚀 Utilisation

### Installation
```bash
make install       # Installe pytest
make check-config  # Vérifie la configuration
make check-executables  # Vérifie que les binaires existent
```

### Création des Golden Masters
```bash
make update-golden  # Crée tous les golden masters avec la référence
# ou
make update-single TEST=nom_du_test  # Un seul test
```

### Exécution des Tests
```bash
make test          # Tous les tests
make test-single TEST=nom_du_test  # Test spécifique
make test-verbose  # Mode détaillé
```

### Avec pytest directement
```bash
pytest -v golden_master.py::TestGenericGoldenMaster
pytest --update-golden golden_master.py  # Mise à jour
pytest -k "test_name" golden_master.py   # Test spécifique
```

### CLI direct (sans pytest)
```bash
python3 golden_master.py                 # Tous les tests
python3 golden_master.py --update-golden # Mise à jour
python3 golden_master.py --test nom      # Test spécifique
```

## 📋 Types d'Exécutables Supportés

### Binaire natif
```json
"reference": {
  "path": "bin/my_program",
  "type": "binary"
}
```

### Script avec interpréteur
```json
"candidate": {
  "path": "src/my_script.py",
  "type": "script", 
  "interpreter": "python3"
}
```

## 🧪 Configuration des Tests

### Test simple
```json
{
  "name": "basic_test",
  "input_files": ["input.dat"],
  "args": ["-v", "--output", "result.txt"],
  "expected_output_files": ["result.txt"],
  "working_dir": "basic"
}
```

### Test avec setup
```json
{
  "name": "with_setup",
  "input_files": ["data.in"],
  "args": ["-f"],
  "expected_output_files": ["*.out"],
  "setup_commands": [
    "touch existing_file.txt",
    "mkdir -p subdir"
  ]
}
```

### Test avec capture complète
```json
{
  "name": "full_capture",
  "args": ["--verbose"],
  "expected_output_files": ["*.log"],
  "capture_stdout": true,
  "capture_stderr": true
}
```

## 📊 Comparaison des Résultats

Le framework compare automatiquement :
- **Codes de retour** (exit codes)
- **Stdout** (sortie standard)
- **Stderr** (erreurs)
- **Fichiers de sortie** (selon patterns)

### Normalisation configurable :
- Suppression de patterns (timestamps, etc.)
- Normalisation des espaces/fins de ligne
- Gestion de la casse

## 🛠️ Commandes Utiles

```bash
make list-tests     # Liste tous les tests
make show-config    # Affiche la configuration
make stats          # Statistiques
make report         # Rapport HTML
make clean          # Nettoie les temporaires
make clean-golden   # Supprime les golden masters
```

## 🎯 Exemple Concret : ged2gwb

Configuration pour comparer OCaml vs Python :

```json
{
  "executables": {
    "reference": {
      "path": "distribution/gw/ged2gwb",
      "type": "binary"
    },
    "candidate": {
      "path": "python/ged2gwb/ged2gwb_cli.py",
      "type": "script",
      "interpreter": "python3"
    }
  },
  "test_cases": [
    {
      "name": "convert_basic",
      "input_files": ["sample.ged"],
      "args": [],
      "expected_output_files": ["*.gw", "*.gwb"]
    },
    {
      "name": "lowercase_names",
      "input_files": ["sample.ged"], 
      "args": ["-lf"],
      "expected_output_files": ["*.gw"]
    }
  ]
}
```

## ✅ Avantages

1. **100% Générique** - Compare n'importe quels exécutables
2. **Entièrement Configurable** - Tout dans le JSON
3. **Pytest Intégré** - Rapports, parallélisation, CI/CD
4. **Comparaison Complète** - stdout, stderr, fichiers, codes retour
5. **Normalisation Flexible** - Ignore patterns, espaces, etc.
6. **Facile à Étendre** - Nouveaux tests = ajout JSON

Ce framework peut être réutilisé pour **n'importe quel projet** nécessitant de comparer deux versions d'un programme !