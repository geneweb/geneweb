# Makefile for GED2GWB module
# Professional test organization and automation

PYTHON = ../.venv/bin/python
MODULE_PATH = .
TEST_PATH = $(MODULE_PATH)/tests
EXAMPLES_PATH = $(MODULE_PATH)/examples

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
	@echo "$(BLUE)GED2GWB Module Commands$(NC)"
	@echo "=========================="
	@echo ""
	@echo "$(GREEN)Testing:$(NC)"
	@echo "  test            - Run all test suites"
	@echo "  test-unit       - Run unit tests only"
	@echo "  test-integration- Run integration tests only"
	@echo "  test-concrete   - Run concrete tests with real files"
	@echo "  test-coverage   - Run tests with coverage report"
	@echo ""
	@echo "$(GREEN)Development:$(NC)"
	@echo "  lint            - Run code linting"
	@echo "  format          - Format code with black"
	@echo "  clean           - Clean generated files"
	@echo "  structure       - Show module structure"
	@echo ""
	@echo "$(GREEN)Execution:$(NC)"
	@echo "  run             - Run ged2gwb with arguments"
	@echo "  help-cli        - Show ged2gwb CLI help"
	@echo "  examples        - Run example scripts"
	@echo ""
	@echo "$(GREEN)Examples:$(NC)"
	@echo "  make test"
	@echo "  make test-unit"
	@echo "  make run ARGS=\"family.ged -o family.pkl --compress\""
	@echo "  make examples"

# =============================================================================
# TESTING TARGETS
# =============================================================================

# Run all test suites
.PHONY: test
test:
	@echo "$(BLUE)Running all GED2GWB test suites...$(NC)"
	@$(PYTHON) -m tests.run_tests

# Run unit tests
.PHONY: test-unit
test-unit:
	@echo "$(BLUE)Running unit tests...$(NC)"
	@$(PYTHON) -m tests.unit.run_unit_tests

# Run integration tests
.PHONY: test-integration
test-integration:
	@echo "$(BLUE)Running integration tests...$(NC)"
	@$(PYTHON) -m tests.test_integration

# Run concrete tests with real GEDCOM files
.PHONY: test-concrete
test-concrete:
	@echo "$(BLUE)Running concrete tests with real files...$(NC)"
	@$(PYTHON) -m tests.test_concrete

# Run tests with coverage
.PHONY: test-coverage
test-coverage:
	@echo "$(BLUE)Running tests with coverage...$(NC)"
	@if command -v coverage >/dev/null 2>&1; then \
		coverage run -m tests.run_tests && coverage report; \
	else \
		echo "$(YELLOW)Coverage not installed. Install with: pip install coverage$(NC)"; \
	fi

# =============================================================================
# DEVELOPMENT TARGETS
# =============================================================================

# Run code linting
.PHONY: lint
lint:
	@echo "$(BLUE)Running code linting...$(NC)"
	@if command -v flake8 >/dev/null 2>&1; then \
		flake8 . --max-line-length=100 --ignore=E203,W503; \
	else \
		echo "$(YELLOW)Flake8 not installed. Install with: pip install flake8$(NC)"; \
	fi

# Format code
.PHONY: format
format:
	@echo "$(BLUE)Formatting code...$(NC)"
	@if command -v black >/dev/null 2>&1; then \
		black . --line-length=100; \
	else \
		echo "$(YELLOW)Black not installed. Install with: pip install black$(NC)"; \
	fi

# Clean generated files
.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning generated files...$(NC)"
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	@find . -name "*.pkl" -delete
	@find . -name "*.pkl.gz" -delete
	@find . -name "*.log" -delete
	@find . -name "test_*.pkl" -delete
	@find . -name "test_*.pkl.gz" -delete
	@find . -name ".coverage" -delete
	@find . -name "htmlcov" -type d -exec rm -rf {} + 2>/dev/null || true

# Show module structure
.PHONY: structure
structure:
	@echo "$(BLUE)GED2GWB Module Structure:$(NC)"
	@echo "============================="
	@find . -name "*.py" | grep -v __pycache__ | sort

# =============================================================================
# EXECUTION TARGETS
# =============================================================================

# Run ged2gwb with arguments
.PHONY: run
run:
	@echo "$(BLUE)Running ged2gwb...$(NC)"
	@$(PYTHON) -m ged2gwb $(ARGS)

# Show CLI help
.PHONY: help-cli
help-cli:
	@echo "$(BLUE)Showing ged2gwb CLI help...$(NC)"
	@$(PYTHON) -m ged2gwb --help

# Run example scripts
.PHONY: examples
examples:
	@echo "$(BLUE)Running example scripts...$(NC)"
	@if [ -d "$(EXAMPLES_PATH)" ]; then \
		for script in $(EXAMPLES_PATH)/*.py; do \
			if [ -f "$$script" ]; then \
				echo "$(YELLOW)Running $$script...$(NC)"; \
				$(PYTHON) "$$script"; \
			fi; \
		done; \
	else \
		echo "$(YELLOW)No examples directory found$(NC)"; \
	fi

# =============================================================================
# QUICK TARGETS
# =============================================================================

# Quick test (unit tests only)
.PHONY: quick-test
quick-test: test-unit

# Full test suite
.PHONY: full-test
full-test: clean test

# Development setup
.PHONY: dev-setup
dev-setup:
	@echo "$(BLUE)Setting up development environment...$(NC)"
	@echo "Installing development dependencies..."
	@pip install coverage flake8 black pytest 2>/dev/null || echo "$(YELLOW)Some dependencies may not be installed$(NC)"
	@echo "$(GREEN)Development setup complete!$(NC)"

# Show test results summary
.PHONY: test-summary
test-summary:
	@echo "$(BLUE)Test Results Summary:$(NC)"
	@echo "======================"
	@echo "$(GREEN)Unit Tests:$(NC)"
	@$(PYTHON) -m tests.unit.run_unit_tests >/dev/null 2>&1 && echo "  ✓ Unit tests" || echo "  ✗ Unit tests"
	@echo "$(GREEN)Integration Tests:$(NC)"
	@$(PYTHON) -m tests.test_integration >/dev/null 2>&1 && echo "  ✓ Integration tests" || echo "  ✗ Integration tests"
	@echo "$(GREEN)Concrete Tests:$(NC)"
	@$(PYTHON) -m tests.test_concrete >/dev/null 2>&1 && echo "  ✓ Concrete tests" || echo "  ✗ Concrete tests"
