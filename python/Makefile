# Makefile for GeneWeb Python Implementation
# ===========================================

.PHONY: help install test test-unit test-integration test-compatibility test-hybrid clean dev-setup

# Variables
PYTHON := python
PYTEST := pytest
SCRIPTS_DIR := scripts
TEST_DIR := tests
REPORTS_DIR := $(TEST_DIR)/reports

# Colors for display
GREEN := \033[32m
YELLOW := \033[33m
BLUE := \033[34m
RED := \033[31m
NC := \033[0m # No Color

.DEFAULT_GOAL := help

help: ## Show this help
	@echo "$(BLUE)🧪 GeneWeb Python - Available Commands$(NC)"
	@echo "=============================================="
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | awk 'BEGIN {FS = ":.*?## "}; {printf "$(GREEN)%-20s$(NC) %s\n", $$1, $$2}'

install: ## Install Python consang package
	@echo "$(YELLOW)📦 Installing Python consang package...$(NC)"
	pip install -e .
	@echo "$(GREEN)✅ Installation completed!$(NC)"

dev-setup: ## Setup development environment
	@echo "$(YELLOW)🔧 Setting up development environment...$(NC)"
	@$(MAKE) install
	pip install pytest pytest-cov pytest-html PyYAML
	@echo "$(GREEN)✅ Development environment ready!$(NC)"

test-unit: ## Run Python unit tests
	@echo "$(YELLOW)🧪 Running unit tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/unit/python/ -v --tb=short
	@echo "$(GREEN)✅ Unit tests completed!$(NC)"

test-integration: ## Run Python integration tests
	@echo "$(YELLOW)🔗 Running integration tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/integration/ -v --tb=short
	@echo "$(GREEN)✅ Integration tests completed!$(NC)"

test-compatibility: ## Run OCaml vs Python compatibility tests
	@echo "$(YELLOW)⚖️  Running compatibility tests...$(NC)"
	@chmod +x $(SCRIPTS_DIR)/run_compatibility_tests.sh
	@$(SCRIPTS_DIR)/run_compatibility_tests.sh
	@echo "$(GREEN)✅ Compatibility tests completed!$(NC)"

test-hybrid: ## Run complete hybrid test suite
	@echo "$(YELLOW)🚀 Running complete hybrid suite...$(NC)"
	@chmod +x $(SCRIPTS_DIR)/run_hybrid_tests.sh
	@$(SCRIPTS_DIR)/run_hybrid_tests.sh
	@echo "$(GREEN)✅ Hybrid suite completed!$(NC)"

test-all: ## Run all Python tests
	@echo "$(BLUE)🎯 Running all Python tests...$(NC)"
	@$(MAKE) test-unit
	@$(MAKE) test-integration
	@$(MAKE) test-compatibility

test: test-all ## Alias for test-all

clean: ## Clean Python build artifacts
	@echo "$(YELLOW)🧹 Cleaning Python artifacts...$(NC)"
	@find . -name "*.pyc" -delete
	@find . -name "__pycache__" -type d -exec rm -rf {} + 2>/dev/null || true
	@rm -rf build dist *.egg-info
	@rm -rf $(REPORTS_DIR)
	@echo "$(GREEN)✅ Cleanup completed!$(NC)"

lint: ## Check Python code style (if flake8 is installed)
	@echo "$(YELLOW)🔍 Checking code style...$(NC)"
	@if command -v flake8 >/dev/null 2>&1; then \
		flake8 consang/ --max-line-length=100; \
		echo "$(GREEN)✅ Style checked!$(NC)"; \
	else \
		echo "$(RED)⚠️  flake8 not installed. Install with: pip install flake8$(NC)"; \
	fi

format: ## Format Python code (if black is installed)
	@echo "$(YELLOW)✨ Formatting code...$(NC)"
	@if command -v black >/dev/null 2>&1; then \
		black consang/ --line-length=100; \
		echo "$(GREEN)✅ Code formatted!$(NC)"; \
	else \
		echo "$(RED)⚠️  black not installed. Install with: pip install black$(NC)"; \
	fi

coverage: ## Run tests with code coverage
	@echo "$(YELLOW)📊 Running tests with coverage...$(NC)"
	$(PYTEST) $(TEST_DIR)/unit/python/ --cov=consang --cov-report=html:$(REPORTS_DIR)/coverage --cov-report=term
	@echo "$(GREEN)✅ Coverage report generated in $(REPORTS_DIR)/coverage/$(NC)"

benchmark: ## Run performance tests
	@echo "$(YELLOW)⚡ Running performance tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/performance/ -v --tb=short
	@echo "$(GREEN)✅ Performance tests completed!$(NC)"

status: ## Show Python project status
	@echo "$(BLUE)📋 GeneWeb Python Project Status$(NC)"
	@echo "================================="
	@echo "$(YELLOW)Installed package:$(NC)"
	@pip show geneweb-consang 2>/dev/null || echo "  ❌ Not installed"
	@echo "$(YELLOW)Consang binary:$(NC)"
	@which consang 2>/dev/null || echo "  ❌ Not found in PATH"
	@echo "$(YELLOW)Available tests:$(NC)"
	@find $(TEST_DIR) -name "test_*.py" | wc -l | xargs echo "  📁 Test files:"
	@echo "$(YELLOW)Reports:$(NC)"
	@if [ -d "$(REPORTS_DIR)" ]; then \
		echo "  📊 Reports available in $(REPORTS_DIR)/"; \
	else \
		echo "  📊 No reports generated"; \
	fi

# Advanced development commands
dev-install: ## Full development installation with dev dependencies
	@echo "$(YELLOW)🔧 Complete development installation...$(NC)"
	pip install -e ".[dev]"
	pip install pytest pytest-cov pytest-html pytest-benchmark PyYAML black flake8
	@echo "$(GREEN)✅ Development installation completed!$(NC)"

quick-test: ## Quick tests (unit tests only)
	@echo "$(YELLOW)⚡ Quick tests...$(NC)"
	$(PYTEST) $(TEST_DIR)/unit/python/ -x -v
	@echo "$(GREEN)✅ Quick tests completed!$(NC)"

watch: ## Run tests in watch mode (if pytest-watch is installed)
	@echo "$(YELLOW)👀 Watch mode activated...$(NC)"
	@if command -v ptw >/dev/null 2>&1; then \
		ptw $(TEST_DIR)/unit/python/ -- -v; \
	else \
		echo "$(RED)⚠️  pytest-watch not installed. Install with: pip install pytest-watch$(NC)"; \
	fi

help-detailed:
	@echo "$(BLUE)🧪 GeneWeb Python - Complete Guide$(NC)"
	@echo "==================================="
	@echo ""
	@echo "$(YELLOW)🚀 Quick start:$(NC)"
	@echo "  make dev-setup     # Setup environment"
	@echo "  make test-all      # Run all tests"
	@echo ""
	@echo "$(YELLOW)🧪 Tests:$(NC)"
	@echo "  make test-unit           # Unit tests only"
	@echo "  make test-integration    # Integration tests only"
	@echo "  make test-compatibility  # OCaml vs Python compatibility tests"
	@echo "  make test-hybrid         # Complete suite with reports"
	@echo ""
	@echo "$(YELLOW)🔧 Development:$(NC)"
	@echo "  make lint          # Style checking"
	@echo "  make format        # Automatic formatting"
	@echo "  make coverage      # Tests with coverage"
	@echo "  make clean         # Cleanup"
	@echo ""
	@echo "$(YELLOW)📊 Monitoring:$(NC)"
	@echo "  make status        # Project status"
	@echo "  make benchmark     # Performance tests"
