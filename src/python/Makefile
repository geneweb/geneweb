# Makefile for GeneWeb Python Project
# Professional project management and automation

PYTHON = python3
PROJECT_ROOT = .
GED2GWB_PATH = $(PROJECT_ROOT)/ged2gwb
GEDCOM_PATH = $(PROJECT_ROOT)/gedcom
LIB_PATH = $(PROJECT_ROOT)/lib
VENV_DIR = .venv
VENV_PYTHON = $(VENV_DIR)/bin/python
VENV_PIP = $(VENV_DIR)/bin/pip

# Colors for output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[0;33m
BLUE = \033[0;34m
CYAN = \033[0;36m
NC = \033[0m # No Color

# Default target
.PHONY: all
all: help

# Help target
.PHONY: help
help:
	@echo "$(BLUE)GeneWeb Python Project - Available Commands$(NC)"
	@echo "=================================================="
	@echo ""
	@echo "$(GREEN)Setup & Installation:$(NC)"
	@echo "  venv           Create and activate virtual environment"
	@echo "  install-dev    Install package in development mode"
	@echo "  install        Install package normally"
	@echo "  install-tools  Install development tools (ruff, pytest, etc.)"
	@echo "  uninstall      Uninstall package"
	@echo ""
	@echo "$(GREEN)Testing:$(NC)"
	@echo "  test           Run all tests (unit + integration + concrete)"
	@echo "  test-unit      Run unit tests only"
	@echo "  test-integration Run integration tests only"
	@echo "  test-concrete  Run concrete tests with real files"
	@echo "  test-coverage  Run tests with coverage report"
	@echo ""
	@echo "$(GREEN)Development:$(NC)"
	@echo "  format         Format code with ruff"
	@echo "  lint           Run linting with ruff and mypy"
	@echo "  fix            Auto-fix linting issues with ruff"
	@echo "  fix-all        Fix all issues including unsafe fixes"
	@echo "  clean-code     Format + fix + lint (complete cleanup)"
	@echo "  check-ruff     Run format + ruff check + test (no mypy)"
	@echo "  check          Run all checks (format + lint + test)"
	@echo ""
	@echo "$(GREEN)Documentation:$(NC)"
	@echo "  docs           Generate documentation"
	@echo "  docs-serve     Serve documentation locally"
	@echo ""
	@echo "$(GREEN)Utilities:$(NC)"
	@echo "  clean          Clean temporary files and caches"
	@echo "  clean-all      Clean everything including __pycache__"
	@echo "  clean-venv     Remove virtual environment"
	@echo "  status         Show linting status and remaining issues"
	@echo "  issues         Show only remaining linting issues"
	@echo "  demo           Run demo with sample.ged"
	@echo "  demo-uk        Run demo with uk.ged"
	@echo ""

# Setup & Installation
.PHONY: venv
venv:
	@echo "$(BLUE)Creating virtual environment...$(NC)"
	@if [ ! -d "$(VENV_DIR)" ]; then \
		$(PYTHON) -m venv $(VENV_DIR); \
		echo "$(GREEN)✓ Virtual environment created$(NC)"; \
	else \
		echo "$(YELLOW)Virtual environment already exists$(NC)"; \
	fi
	@echo "$(BLUE)Activating virtual environment...$(NC)"
	@echo "$(YELLOW)To activate manually: source $(VENV_DIR)/bin/activate$(NC)"

.PHONY: install-dev
install-dev: venv
	@echo "$(BLUE)Installing package in development mode...$(NC)"
	$(VENV_PIP) install -e .
	@echo "$(GREEN)✓ Development installation complete$(NC)"

.PHONY: install
install: venv
	@echo "$(BLUE)Installing package...$(NC)"
	$(VENV_PIP) install .
	@echo "$(GREEN)✓ Installation complete$(NC)"

.PHONY: install-tools
install-tools: venv
	@echo "$(BLUE)Installing development tools...$(NC)"
	$(VENV_PIP) install ruff mypy pytest pytest-cov
	@echo "$(GREEN)✓ Development tools installed$(NC)"

.PHONY: uninstall
uninstall:
	@echo "$(BLUE)Uninstalling package...$(NC)"
	$(VENV_PIP) uninstall geneweb-python -y
	@echo "$(GREEN)✓ Uninstallation complete$(NC)"

# Testing
.PHONY: test
test: test-unit test-integration test-concrete
	@echo "$(GREEN)✓ All tests completed$(NC)"

.PHONY: test-unit
test-unit:
	@echo "$(BLUE)Running unit tests...$(NC)"
	cd $(GED2GWB_PATH) && $(MAKE) test-unit

.PHONY: test-integration
test-integration:
	@echo "$(BLUE)Running integration tests...$(NC)"
	cd $(GED2GWB_PATH) && $(MAKE) test-integration

.PHONY: test-concrete
test-concrete:
	@echo "$(BLUE)Running concrete tests...$(NC)"
	cd $(GED2GWB_PATH) && $(MAKE) test-concrete

.PHONY: test-coverage
test-coverage:
	@echo "$(BLUE)Running tests with coverage...$(NC)"
	$(VENV_PYTHON) -m pytest --cov=ged2gwb --cov=gedcom --cov=lib --cov-report=html --cov-report=term
	@echo "$(GREEN)✓ Coverage report generated in htmlcov/$(NC)"

# Development
.PHONY: format
format:
	@echo "$(BLUE)Formatting code with ruff...$(NC)"
	$(VENV_PYTHON) -m ruff format ged2gwb/ gedcom/ lib/
	@echo "$(GREEN)✓ Code formatted$(NC)"

.PHONY: lint
lint:
	@echo "$(BLUE)Running linting with ruff...$(NC)"
	$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	$(VENV_PYTHON) -m mypy ged2gwb/ gedcom/ lib/ --ignore-missing-imports --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ Linting complete$(NC)"

.PHONY: fix
fix:
	@echo "$(BLUE)Auto-fixing linting issues with ruff...$(NC)"
	$(VENV_PYTHON) -m ruff check --fix ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ Auto-fix complete$(NC)"

.PHONY: fix-all
fix-all:
	@echo "$(BLUE)Auto-fixing all linting issues (including unsafe fixes)...$(NC)"
	$(VENV_PYTHON) -m ruff check --fix --unsafe-fixes ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ All fixes complete$(NC)"

.PHONY: clean-code
clean-code: format fix
	@echo "$(BLUE)Running final lint check...$(NC)"
	$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests || true
	@echo "$(GREEN)✓ Code cleanup complete$(NC)"

.PHONY: check-ruff
check-ruff: format
	@echo "$(BLUE)Running ruff check...$(NC)"
	$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ Ruff check passed$(NC)"
	@echo "$(BLUE)Running tests...$(NC)"
	$(VENV_PYTHON) -m pytest ged2gwb/tests/ gedcom/tests/ -v
	@echo "$(GREEN)✓ All checks passed$(NC)"

.PHONY: check
check: format lint test
	@echo "$(GREEN)✓ All checks passed$(NC)"

# Documentation
.PHONY: docs
docs:
	@echo "$(BLUE)Generating documentation...$(NC)"
	$(PYTHON) -m sphinx -b html docs docs/_build/html
	@echo "$(GREEN)✓ Documentation generated in docs/_build/html/$(NC)"

.PHONY: docs-serve
docs-serve: docs
	@echo "$(BLUE)Serving documentation at http://localhost:8000$(NC)"
	cd docs/_build/html && $(PYTHON) -m http.server 8000

# Utilities
.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning temporary files...$(NC)"
	find $(PROJECT_ROOT) -type f -name "*.pyc" -delete
	find $(PROJECT_ROOT) -type d -name "__pycache__" -delete
	find $(PROJECT_ROOT) -type f -name "*.log" -delete
	find $(PROJECT_ROOT) -type f -name "*.tmp" -delete
	find $(PROJECT_ROOT) -type f -name "test_*.pkl*" -delete
	@echo "$(GREEN)✓ Cleanup complete$(NC)"

.PHONY: clean-all
clean-all: clean
	@echo "$(BLUE)Cleaning all caches and build files...$(NC)"
	rm -rf $(PROJECT_ROOT)/build/
	rm -rf $(PROJECT_ROOT)/dist/
	rm -rf $(PROJECT_ROOT)/*.egg-info/
	rm -rf $(PROJECT_ROOT)/.pytest_cache/
	rm -rf $(PROJECT_ROOT)/htmlcov/
	rm -rf $(PROJECT_ROOT)/.coverage
	@echo "$(GREEN)✓ Deep cleanup complete$(NC)"

.PHONY: clean-venv
clean-venv:
	@echo "$(BLUE)Removing virtual environment...$(NC)"
	rm -rf $(VENV_DIR)
	@echo "$(GREEN)✓ Virtual environment removed$(NC)"

.PHONY: status
status:
	@echo "$(BLUE)Checking linting status...$(NC)"
	@echo "$(YELLOW)Running ruff check...$(NC)"
	@$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests --statistics || true
	@echo ""
	@echo "$(YELLOW)Running mypy check...$(NC)"
	@$(VENV_PYTHON) -m mypy ged2gwb/ gedcom/ lib/ --ignore-missing-imports --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests --show-error-codes || true

.PHONY: issues
issues:
	@echo "$(BLUE)Showing remaining linting issues...$(NC)"
	@$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests --output-format=concise || true

.PHONY: demo
demo:
	@echo "$(BLUE)Running demo with sample.ged...$(NC)"
	$(VENV_PYTHON) -m ged2gwb $(GEDCOM_PATH)/ged/sample.ged --output demo-sample.pkl --verbose
	@echo "$(GREEN)✓ Demo complete - check demo-sample.pkl$(NC)"

.PHONY: demo-uk
demo-uk:
	@echo "$(BLUE)Running demo with uk.ged...$(NC)"
	$(VENV_PYTHON) -m ged2gwb $(GEDCOM_PATH)/ged/uk.ged --output demo-uk.pkl --verbose
	@echo "$(GREEN)✓ Demo complete - check demo-uk.pkl$(NC)"

# Quick development setup
.PHONY: dev-setup
dev-setup: venv install-dev
	@echo "$(BLUE)Setting up development environment...$(NC)"
	$(VENV_PIP) install -r requirements.txt
	$(VENV_PIP) install black isort flake8 mypy pytest pytest-cov
	@echo "$(GREEN)✓ Development environment ready$(NC)"
	@echo "$(YELLOW)Run 'make demo' to test with sample.ged$(NC)"

# CI/CD helpers
.PHONY: ci-test
ci-test: clean test-coverage
	@echo "$(GREEN)✓ CI tests completed$(NC)"

.PHONY: ci-check
ci-check: format lint ci-test
	@echo "$(GREEN)✓ CI checks completed$(NC)"
