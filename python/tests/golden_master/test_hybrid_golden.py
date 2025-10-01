"""Golden Master tests using hybrid configuration."""

import pytest
from tests.common.hybrid_config import hybrid_config
from tests.golden_master.hybrid_runner import HybridGoldenRunner


@pytest.mark.golden
class TestHybridGolden:
    """Hybrid configuration Golden Master tests."""

    def test_config_loading(self):
        """Test configuration loads correctly."""
        errors = hybrid_config.validate_config()
        assert not errors, f"Configuration errors: {errors}"

        enabled_binaries = hybrid_config.get_enabled_binaries()
        assert "consang" in enabled_binaries, "consang should be enabled"

    def test_consang_golden_master(self):
        """Complete Golden Master test for consang."""
        if not hybrid_config.is_binary_enabled("consang"):
            pytest.skip("consang not enabled in configuration")

        runner = HybridGoldenRunner("consang")
        results = runner.run_all_tests()

        runner.print_summary(results)
        runner.save_results(results)

        golden_config = hybrid_config.get_test_suite_config("consang", "golden")
        success_threshold = golden_config.get("success_threshold", 80) if golden_config else 80

        assert results.success_rate >= success_threshold, \
            f"Success rate too low: {results.success_rate:.1f}% < {success_threshold}%"

    @pytest.mark.parametrize("binary_name", hybrid_config.get_enabled_binaries())
    def test_all_enabled_binaries(self, binary_name):
        """Test Golden Master for all enabled binaries."""
        if not hybrid_config.is_test_suite_enabled(binary_name, "golden"):
            pytest.skip(f"Golden Master disabled for {binary_name}")

        runner = HybridGoldenRunner(binary_name)
        results = runner.run_all_tests()

        runner.print_summary(results)

        assert results.success_rate >= 60, \
            f"Success rate too low for {binary_name}: {results.success_rate:.1f}%"

    def test_configuration_summary(self):
        """Test and display configuration summary."""
        summary = hybrid_config.get_summary()

        print(f"\nConfiguration Summary:")
        print(f"   Project: {summary.get('project', {}).get('name', 'N/A')}")
        print(f"   Enabled binaries: {summary.get('enabled_binaries', [])}")
        print(f"   Total test cases: {summary.get('total_test_cases', 0)}")

        for binary, suites in summary.get('test_suites', {}).items():
            print(f"   {binary}: {', '.join(suites)}")

        assert summary.get('enabled_binaries'), "No binaries enabled"
        assert summary.get('total_test_cases', 0) > 0, "No test cases configured"
