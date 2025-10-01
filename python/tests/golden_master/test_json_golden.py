"""Golden Master tests using JSON configuration."""

import pytest
from pathlib import Path
from .json_golden_runner import JsonGoldenRunner


@pytest.mark.golden
@pytest.mark.consang
class TestJsonGolden:
    """JSON-based Golden Master tests."""

    def test_consang_golden_master(self):
        """Complete Golden Master test for consang."""
        config_file = Path(__file__).parent.parent / "config" / "golden_data.json"

        if not config_file.exists():
            pytest.skip("Golden Master configuration not found")

        runner = JsonGoldenRunner(config_file)
        results = runner.run_all_tests()

        runner.print_results(results)

        success_rate = (results["passed"] / results["total_tests"] * 100) if results["total_tests"] > 0 else 0
        assert success_rate >= 50, f"Success rate too low: {success_rate:.1f}%"
