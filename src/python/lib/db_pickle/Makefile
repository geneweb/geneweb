# Makefile for db_pickle Module
# Simple and focused testing

# Configuration
PYTHON = python3
TEST_DIR = tests

# Colors
BLUE = \033[0;34m
GREEN = \033[0;32m
YELLOW = \033[0;33m
NC = \033[0m

# Default target
.PHONY: all
all: help

# Help
.PHONY: help
help:
	@echo "$(BLUE)db_pickle Module - Simple Makefile$(NC)"
	@echo ""
	@echo "$(GREEN)Available targets:$(NC)"
	@echo "  test          Run all tests (84 tests)"
	@echo "  test-models   Run model tests"
	@echo "  test-database Run database tests"
	@echo "  test-io       Run I/O tests"
	@echo "  test-search   Run search tests"
	@echo "  test-perf     Run performance tests"
	@echo "  test-integ    Run integration tests"
	@echo "  clean         Clean temporary files"

# Main test target
.PHONY: test
test:
	@echo "$(BLUE)Running all db_pickle tests...$(NC)"
	@$(PYTHON) $(TEST_DIR)/run_tests.py

# Individual test categories
.PHONY: test-models
test-models:
	@echo "$(BLUE)Running model tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_models.py -v

.PHONY: test-database
test-database:
	@echo "$(BLUE)Running database tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_database.py -v

.PHONY: test-io
test-io:
	@echo "$(BLUE)Running I/O tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_io.py -v

.PHONY: test-search
test-search:
	@echo "$(BLUE)Running search tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_search.py -v

.PHONY: test-perf
test-perf:
	@echo "$(BLUE)Running performance tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_performance.py -v

.PHONY: test-integ
test-integ:
	@echo "$(BLUE)Running integration tests...$(NC)"
	@$(PYTHON) -m pytest $(TEST_DIR)/test_integration.py -v

# Clean temporary files
.PHONY: clean
clean:
	@echo "$(BLUE)Cleaning temporary files...$(NC)"
	@find . -type f -name "*.pyc" -delete 2>/dev/null || true
	@find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	@find . -type f -name "*.pkl" -delete 2>/dev/null || true
	@find . -type f -name "*.pkl.gz" -delete 2>/dev/null || true
	@find . -type f -name ".coverage" -delete 2>/dev/null || true
	@find . -type d -name ".pytest_cache" -exec rm -rf {} + 2>/dev/null || true
