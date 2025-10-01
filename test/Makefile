# Test targets for consang binary

.PHONY: test-consang test-consang-capture test-consang-behavior test-consang-business test-consang-business-full test-consang-validate test-consang-clean

# Run complete consang test suite
test-consang: ../distrib
	@echo "🧪 Running complete consang test suite..."
	cd .. && python3 test/run_consang_tests.py

# Capture snapshots only
test-consang-capture: ../distrib
	@echo "📸 Capturing consang snapshots..."
	cd .. && python3 test/run_consang_tests.py --capture-only

# Run behavioral tests only
test-consang-behavior: ../distrib
	@echo "🔍 Running consang behavioral tests..."
	cd .. && python3 test/test_consang_behavior.py

# Run business logic tests only
test-consang-business: ../distrib
	@echo "🏢 Running consang business logic tests..."
	cd .. && python3 test/test_consang_business_logic.py

# Run business logic tests via main runner
test-consang-business-full: ../distrib
	@echo "🏢 Running consang business logic tests (full suite)..."
	cd .. && python3 test/run_consang_tests.py --business-only

# Validate existing snapshots
test-consang-validate:
	@echo "✅ Validating consang snapshots..."
	cd .. && python3 test/validate_snapshots.py

# Clean test artifacts
test-consang-clean:
	@echo "🧹 Cleaning consang test artifacts..."
	rm -rf snapshots/consang/
	rm -f fixtures/consang_test.ged
	-rm -rf ../test-consang-db.gwb 2>/dev/null || echo "Could not remove test database (may be in use)"

# Help for test targets
test-help:
	@echo "Consang Test Targets:"
	@echo "  test-consang         - Run complete test suite"
	@echo "  test-consang-capture - Capture reference snapshots"
	@echo "  test-consang-behavior- Run behavioral analysis"
	@echo "  test-consang-business- Run business logic tests"
	@echo "  test-consang-validate- Validate snapshots"
	@echo "  test-consang-clean   - Clean test artifacts"

# Convenience targets that can be run from the project root
../distrib:
	cd .. && $(MAKE) distrib
