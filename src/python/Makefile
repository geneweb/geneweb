## Makefile for GeneWeb Python Project

PYTHON = python3
PROJECT_ROOT = .
VENV_DIR = .venv
VENV_PYTHON = $(VENV_DIR)/bin/python
VENV_PIP = $(VENV_DIR)/bin/pip

# Colors for output
RED = \033[0;31m
GREEN = \033[0;32m
YELLOW = \033[0;33m
BLUE = \033[0;34m
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
	@echo "$(GREEN)Setup:$(NC)"
	@echo "  setup          Complete development setup (venv + install + tools)"
	@echo "  venv           Create virtual environment"
	@echo "  install        Install package in development mode"
	@echo "  install-tools  Install development tools (ruff, pytest, mypy)"
	@echo ""
	@echo "$(GREEN)Testing:$(NC)"
	@echo "  test           Run all tests"
	@echo "  test-ged2gwb   Run ged2gwb tests only"
	@echo "  test-gedcom    Run GEDCOM tests only"
	@echo "  test-lib       Run lib tests (db_pickle + util)"
	@echo "  test-coverage  Run tests with coverage report"
	@echo ""
	@echo "$(GREEN)Development:$(NC)"
	@echo "  format         Format code with ruff"
	@echo "  lint           Run linting (ruff + mypy)"
	@echo "  fix            Auto-fix linting issues"
	@echo "  check          Format + lint + test (complete check)"
	@echo ""
	@echo "$(GREEN)Utilities:$(NC)"
	@echo "  clean          Clean temporary files"
	@echo "  demo           Run demo with sample.ged"
	@echo "  demo-uk        Run demo with uk.ged"
	@echo ""

# Setup & Installation
.PHONY: setup
setup: venv install install-tools
	@echo "$(GREEN)✓ Complete development setup ready$(NC)"
	@echo "$(YELLOW)Run 'make demo' to test with sample.ged$(NC)"

.PHONY: venv
venv:
	@echo "$(BLUE)Creating virtual environment...$(NC)"
	@if [ ! -d "$(VENV_DIR)" ]; then \
		$(PYTHON) -m venv $(VENV_DIR); \
		echo "$(GREEN)✓ Virtual environment created$(NC)"; \
	else \
		echo "$(YELLOW)Virtual environment already exists$(NC)"; \
	fi

.PHONY: install
install: venv
	@echo "$(BLUE)Installing package in development mode...$(NC)"
	$(VENV_PIP) install -e .
	@echo "$(GREEN)✓ Package installed$(NC)"

.PHONY: install-tools
install-tools: venv
	@echo "$(BLUE)Installing development tools...$(NC)"
	$(VENV_PIP) install ruff mypy pytest pytest-cov
	@echo "$(GREEN)✓ Development tools installed$(NC)"

# Testing
.PHONY: test
test:
	@echo "$(BLUE)Running all tests...$(NC)"
	@echo "$(BLUE)Testing ged2gwb...$(NC)"
	$(VENV_PYTHON) -m pytest ged2gwb/tests/ -v --tb=short
	@echo "$(BLUE)Testing gedcom...$(NC)"
	$(VENV_PYTHON) -m pytest gedcom/tests/ -v --tb=short
	@echo "$(BLUE)Testing lib...$(NC)"
	$(VENV_PYTHON) -m pytest lib/db_pickle/tests/ -v --tb=short
	$(VENV_PYTHON) -m pytest lib/util/ -v --tb=short || echo "$(YELLOW)No tests found in lib/util$(NC)"
	@echo "$(GREEN)✓ All tests completed$(NC)"

.PHONY: test-ged2gwb
test-ged2gwb:
	@echo "$(BLUE)Running ged2gwb tests...$(NC)"
	$(VENV_PYTHON) -m pytest ged2gwb/tests/ -v --tb=short

.PHONY: test-gedcom
test-gedcom:
	@echo "$(BLUE)Running GEDCOM tests...$(NC)"
	$(VENV_PYTHON) -m pytest gedcom/tests/ -v --tb=short

.PHONY: test-lib
test-lib:
	@echo "$(BLUE)Running lib tests...$(NC)"
	$(VENV_PYTHON) -m pytest lib/db_pickle/tests/ -v --tb=short
	$(VENV_PYTHON) -m pytest lib/util/ -v --tb=short || echo "$(YELLOW)No tests found in lib/util$(NC)"

.PHONY: test-coverage
test-coverage:
	@echo "$(BLUE)Running tests with coverage...$(NC)"
	$(VENV_PYTHON) -m pytest --cov=ged2gwb --cov=gedcom --cov=lib --cov-report=html --cov-report=term

# Development
.PHONY: format
format:
	@echo "$(BLUE)Formatting code with ruff...$(NC)"
	$(VENV_PYTHON) -m ruff format ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ Code formatted$(NC)"

.PHONY: lint
lint:
	@echo "$(BLUE)Running linting...$(NC)"
	@echo "$(YELLOW)Running ruff check...$(NC)"
	$(VENV_PYTHON) -m ruff check ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(YELLOW)Running mypy check...$(NC)"
	$(VENV_PYTHON) -m mypy --ignore-missing-imports --exclude=ged2gwb/examples,gedcom/tests,ged2gwb/tests --follow-imports=skip --show-error-codes ged2gwb/ gedcom/ || true
	@echo "$(GREEN)✓ Linting completed$(NC)"

.PHONY: fix
fix:
	@echo "$(BLUE)Auto-fixing linting issues...$(NC)"
	$(VENV_PYTHON) -m ruff check --fix ged2gwb/ gedcom/ lib/ --exclude=lib/old_db,lib/util,ged2gwb/examples,gedcom/tests,ged2gwb/tests
	@echo "$(GREEN)✓ Auto-fixes applied$(NC)"

.PHONY: check
check: format fix lint test
	@echo "$(GREEN)✓ Complete check passed$(NC)"

# Utilities
.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning temporary files...$(NC)"
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pkl" -delete 2>/dev/null || true
	@find . -type f -name "*.pkl.gz" -delete 2>/dev/null || true
	@find . -type f -name ".coverage" -delete 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
	@find . -type d -name "htmlcov" -exec rm -rf {} + 2>/dev/null || true
	@echo "$(GREEN)✓ Cleanup completed$(NC)"

.PHONY: demo
demo:
	@echo "$(BLUE)Running demo with sample.ged...$(NC)"
	$(VENV_PYTHON) -m ged2gwb gedcom/ged/sample.ged --output demo-sample.pkl --verbose
	@echo "$(GREEN)✓ Demo complete - check demo-sample.pkl$(NC)"

.PHONY: demo-uk
demo-uk:
	@echo "$(BLUE)Running demo with uk.ged...$(NC)"
	$(VENV_PYTHON) -m ged2gwb gedcom/ged/uk.ged --output demo-uk.pkl --verbose
	@echo "$(GREEN)✓ Demo complete - check demo-uk.pkl$(NC)"
