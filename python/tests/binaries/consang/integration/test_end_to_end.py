"""Simple integration tests for consang."""

import pytest
import subprocess
import tempfile
import shutil
from pathlib import Path


class TestConsangIntegration:
    """Integration tests for consang binary."""

    def run_consang(self, args, timeout=30):
        """Run consang with given arguments."""
        result = subprocess.run(["consang"] + args, capture_output=True, text=True, timeout=timeout)
        return result

    def assert_error(self, result, expected_text=None):
        """Assert that result indicates an error."""
        assert result.returncode != 0, "Expected non-zero exit code"

        if expected_text:
            output = (result.stderr + result.stdout).lower()
            expected_variations = [
                expected_text.lower(),
                expected_text.replace(" ", "").lower(),
                expected_text.replace("_", " ").lower(),
                expected_text.replace("-", " ").lower()
            ]

            found = any(exp in output for exp in expected_variations)
            if not found:
                pytest.fail(f"Expected one of {expected_variations} in output: {output}")

    @pytest.mark.consang
    @pytest.mark.integration
    def test_help_command(self):
        """Test help command end-to-end."""
        result = self.run_consang(["--help"])
        assert result.returncode == 0
        assert "usage: consang" in result.stdout

    @pytest.mark.consang
    @pytest.mark.integration
    def test_error_messages(self):
        """Test proper error messages for invalid usage."""
        # Test invalid option
        result = self.run_consang(["-h"])
        expected_error = "unknown option"
        self.assert_error(result, expected_text=expected_error)

    @pytest.mark.consang
    @pytest.mark.integration
    def test_performance_options(self):
        """Test performance options don't crash."""
        options = ["-fast", "-mem", "-nolock", "-scratch"]

        for option in options:
            result = self.run_consang([option, "bad.gwb"])
            # Should fail due to file, not option
            assert result.returncode == 2

    @pytest.mark.consang
    @pytest.mark.integration
    @pytest.mark.slow
    def test_empty_database(self):
        """Test with empty database."""
        temp_dir = tempfile.mkdtemp()
        try:
            db_path = Path(temp_dir) / "empty.gwb"
            db_path.mkdir()

            # Create minimal base file
            base_file = db_path / "base"
            base_file.write_text("# Empty database\n")

            result = self.run_consang([str(db_path)])

            # Should not crash
            assert "Segmentation fault" not in result.stderr
            assert "core dumped" not in result.stderr

        finally:
            shutil.rmtree(temp_dir)

    @pytest.mark.consang
    @pytest.mark.integration
    def test_concurrent_execution(self):
        """Test multiple simultaneous executions."""
        import concurrent.futures

        def run_help():
            result = self.run_consang(["--help"])
            return result.returncode == 0

        with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
            futures = [executor.submit(run_help) for _ in range(3)]
            results = [f.result() for f in futures]

        assert all(results), "All concurrent runs should succeed"
