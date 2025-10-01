# Makefile pour Golden Master Tests - Générique

PYTHON := python3
PYTEST := $(PYTHON) -m pytest
CONFIG_FILE := test_config.json

# Couleurs
RED := \033[0;31m
GREEN := \033[0;32m
YELLOW := \033[1;33m
BLUE := \033[0;34m
NC := \033[0m

.PHONY: help install test update-golden clean check-config

help: ## Affiche cette aide
	@echo "$(BLUE)Golden Master Tests - Générique$(NC)"
	@echo "$(YELLOW)Compare deux exécutables quelconques$(NC)"
	@echo ""
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "  $(GREEN)%-20s$(NC) %s\n", $$1, $$2}'

install: ## Installe les dépendances
	@echo "$(YELLOW)Installation des dépendances...$(NC)"
	$(PYTHON) -m pip install pytest pytest-xdist pytest-html
	@echo "$(GREEN)Installation terminée$(NC)"

check-config: ## Vérifie la configuration
	@echo "$(YELLOW)Vérification de la configuration...$(NC)"
	@$(PYTHON) -c "import json; json.load(open('$(CONFIG_FILE)')); print('$(GREEN)Configuration valide$(NC)')"

check-executables: check-config ## Vérifie que les exécutables existent
	@echo "$(YELLOW)Vérification des exécutables...$(NC)"
	@$(PYTHON) -c "\
import json; \
import sys; \
from pathlib import Path; \
config = json.load(open('$(CONFIG_FILE)')); \
executables = config['test_config']['executables']; \
[print(f'$(RED)ERREUR: {name} executable not found: {exec_config[\"path\"]}$(NC)') or sys.exit(1) if not Path(exec_config['path']).exists() else print(f'$(GREEN)✓ {name}: {exec_config[\"path\"]}$(NC)') for name, exec_config in executables.items()]; \
print('$(GREEN)Tous les exécutables sont présents$(NC)') \
"

test: check-executables ## Lance tous les tests
	@echo "$(YELLOW)Lancement des tests Golden Master...$(NC)"
	$(PYTEST) -v golden_master.py::TestGenericGoldenMaster

test-verbose: check-executables ## Tests en mode verbose
	@echo "$(YELLOW)Tests en mode verbose...$(NC)"
	$(PYTEST) -v -s --tb=long golden_master.py::TestGenericGoldenMaster

test-single: check-executables ## Lance un test spécifique (make test-single TEST=nom)
ifndef TEST
	@echo "$(RED)ERREUR: Spécifiez TEST=nom_du_test$(NC)"
	@$(PYTHON) -c "\
import json; \
config = json.load(open('$(CONFIG_FILE)')); \
print('$(YELLOW)Tests disponibles:$(NC)'); \
[print(f'  - {tc[\"name\"]}') for tc in config['test_cases']] \
"
	@exit 1
endif
	@echo "$(YELLOW)Test: $(TEST)$(NC)"
	$(PYTEST) -v -k "$(TEST)" golden_master.py::TestGenericGoldenMaster

update-golden: check-executables ## Met à jour les golden masters
	@echo "$(YELLOW)Mise à jour des golden masters...$(NC)"
	@echo "$(RED)ATTENTION: Écrase les golden masters existants$(NC)"
	@read -p "Continuer? [y/N] " confirm && [ "$$confirm" = "y" ] || exit 1
	$(PYTEST) --update-golden golden_master.py::TestGenericGoldenMaster
	@echo "$(GREEN)Golden masters mis à jour$(NC)"

update-single: check-executables ## Met à jour un golden master (make update-single TEST=nom)
ifndef TEST
	@echo "$(RED)ERREUR: Spécifiez TEST=nom_du_test$(NC)"
	@exit 1
endif
	@echo "$(YELLOW)Mise à jour: $(TEST)$(NC)"
	$(PYTEST) --update-golden -k "$(TEST)" golden_master.py::TestGenericGoldenMaster

list-tests: ## Liste tous les tests
	@echo "$(YELLOW)Tests disponibles:$(NC)"
	@$(PYTHON) -c "\
import json; \
config = json.load(open('$(CONFIG_FILE)')); \
[print(f'  $(GREEN){tc[\"name\"]}$(NC): {tc[\"description\"]}') for tc in config['test_cases']] \
"

show-config: ## Affiche la configuration
	@echo "$(YELLOW)Configuration actuelle:$(NC)"
	@$(PYTHON) -c "\
import json; \
config = json.load(open('$(CONFIG_FILE)')); \
print('$(BLUE)Executables:$(NC)'); \
[print(f'  {name}: {exec_config[\"path\"]} ({exec_config[\"type\"]})') for name, exec_config in config['test_config']['executables'].items()]; \
print('$(BLUE)Tests:$(NC)', len(config['test_cases'])) \
"

clean: ## Nettoie les fichiers temporaires
	@echo "$(YELLOW)Nettoyage...$(NC)"
	@rm -rf __pycache__ .pytest_cache outputs/* work/*
	@find . -name "*.pyc" -delete
	@echo "$(GREEN)Nettoyage terminé$(NC)"

clean-golden: ## Supprime les golden masters (ATTENTION!)
	@echo "$(RED)ATTENTION: Suppression de tous les golden masters$(NC)"
	@read -p "Êtes-vous sûr? [y/N] " confirm && [ "$$confirm" = "y" ] || exit 1
	@rm -rf golden/*
	@echo "$(YELLOW)Golden masters supprimés$(NC)"

report: ## Génère un rapport HTML
	@echo "$(YELLOW)Génération du rapport...$(NC)"
	$(PYTEST) --html=report.html --self-contained-html golden_master.py::TestGenericGoldenMaster
	@echo "$(GREEN)Rapport: report.html$(NC)"

# Tests CLI direct (sans pytest)
cli-test: check-executables ## Test via CLI direct
	@echo "$(YELLOW)Test via CLI direct...$(NC)"
	$(PYTHON) golden_master.py

cli-update: check-executables ## Mise à jour via CLI
	@echo "$(YELLOW)Mise à jour via CLI...$(NC)"
	$(PYTHON) golden_master.py --update-golden

cli-single: check-executables ## Test single via CLI (make cli-single TEST=nom)
ifndef TEST
	@echo "$(RED)ERREUR: Spécifiez TEST=nom_du_test$(NC)"
	@exit 1
endif
	$(PYTHON) golden_master.py --test $(TEST)

stats: ## Statistiques
	@echo "$(YELLOW)Statistiques:$(NC)"
	@echo "  $(BLUE)Tests configurés:$(NC) $$($(PYTHON) -c 'import json; print(len(json.load(open(\"$(CONFIG_FILE)\"))[\"test_cases\"]))')"
	@echo "  $(BLUE)Golden masters:$(NC) $$(ls -1 golden/ 2>/dev/null | wc -l)"
	@echo "  $(BLUE)Fichiers de test:$(NC) $$(ls -1 test_data/ 2>/dev/null | wc -l)"