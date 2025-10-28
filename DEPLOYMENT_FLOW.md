# Flux de Déploiement Master - Résumé

## 🎯 Processus Complet sur Master

### **1. Déclenchement**
```
Push sur master → build-and-deploy.yml
```

### **2. Étapes Parallèles puis Séquentielles**

#### **Étape 1A: Tests Python** (Parallèle)
- **Workflow**: `working-branch.yml` (appelé par `build-and-deploy.yml`)
- **Actions**: 
  - Setup environnement Python
  - Tests unitaires Python, Golden Master
  - Vérification qualité code (lint, format)
  - Tests CLI et intégration
- **Outputs**: 
  - `python-tests-passed`: true/false
  - `use-python-binaries`: true/false

#### **Étape 1B: Tests OCaml CI** (Parallèle)
- **Workflow**: `ci.yml` (appelé par `build-and-deploy.yml`)
- **Actions**: 
  - Build OCaml sur multiple OS/versions
  - Tests unitaires, intégration OCaml
  - Vérification performance et sécurité
- **Outputs**: 
  - `all-tests-passed`: true/false

#### **Étape 2: Build Docker**
- **Workflow**: `docker.yml` (appelé par `build-and-deploy.yml`)
- **Input**: `use_python_binaries` (basé sur résultats CI)
- **Actions**:
  - Build OCaml distribution
  - **SI tests passent**: Remplace `ged2gwb` par version Python
  - **SI tests échouent**: Garde binaires OCaml
  - Push image vers GitHub Container Registry

#### **Étape 3: Status de Déploiement**
- **Workflow**: `build-and-deploy.yml`
- **Actions**:
  - Affiche résumé des tests
  - Confirme quelle version (Python/OCaml) est utilisée
  - Status du push Docker

## 🔄 Logique de Décision

### **Tous les Tests Passent**
```
✅ Python Tests + ✅ OCaml CI → Python Binaries → Docker avec ged2gwb Python
```

### **Tests Python Échouent**
```
❌ Python Tests + ✅ OCaml CI → OCaml Fallback → Docker avec ged2gwb OCaml
```

### **Tests OCaml Échouent**
```
✅ Python Tests + ❌ OCaml CI → OCaml Fallback → Docker avec ged2gwb OCaml
```

### **Tous les Tests Échouent**
```
❌ Python Tests + ❌ OCaml CI → OCaml Fallback → Docker avec ged2gwb OCaml
```

## 📋 Workflows Actifs par Branche

### **Master Branch**
- `build-and-deploy.yml` ✅ (orchestrateur principal)
- `working-branch.yml` ✅ (tests Python - appelé par build-and-deploy)
- `ci.yml` ✅ (tests OCaml - appelé par build-and-deploy)
- `docker.yml` ❌ (uniquement via workflow_call)

### **Autres Branches**
- `working-branch.yml` ✅ (tests Python)
- `ci.yml` ✅ (sur PR vers master)
- `docker.yml` ✅ (sur PR)

### **Pull Requests**
- `ci.yml` ✅ (validation avant merge)
- `docker.yml` ✅ (test build)
- `working-branch.yml` ✅ (tests Python)

## ✅ Avantages de cette Architecture

1. **Pas de Conflit**: Un seul workflow principal sur master
2. **Décision Automatique**: Python vs OCaml basé sur tests
3. **Fallback Sûr**: OCaml toujours disponible
4. **Traçabilité**: Logs clairs de quelle version est utilisée
5. **Flexibilité**: Workflow_dispatch pour tests manuels

## 🚀 Résultat Final

L'image Docker finale contient:
- **Base OCaml**: Distribution complète
- **Binaires Conditionnels**: 
  - `ged2gwb` Python (si tests OK)
  - `ged2gwb` OCaml (si tests KO ou fallback)
- **Environnement Python**: Disponible si nécessaire
- **Monitoring**: Variables d'environnement pour debugging

Cette approche garantit un déploiement sûr avec validation automatique !
