"""Simple integration tests for consang."""

import pytest
import subprocess
import os


@pytest.mark.integration
@pytest.mark.consang
class TestConsangSimpleIntegration:
    """Basic integration tests for consang."""

    def run_binary(self, args=None, timeout=10):
        """Execute consang binary."""
        cmd = ['consang']
        if args:
            cmd.extend(args)

        try:
            env = os.environ.copy()
            env.pop('CONSANG_OCAML_HELP', None)
            return subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False,
                env=env
            )
        except subprocess.TimeoutExpired:
            return subprocess.CompletedProcess(
                args=cmd,
                returncode=124,
                stdout="",
                stderr="Timeout"
            )

    def test_help_works(self):
        """Test help functionality end-to-end."""
        result = self.run_binary(['--help'])
        assert result.returncode == 0
        assert 'usage: consang' in result.stdout

    def test_error_handling_works(self):
        """Test error handling functionality."""
        result = self.run_binary([])
        assert result.returncode == 2
        assert 'Missing file name' in result.stderr

    def test_basic_validation_works(self):
        """Test basic file validation."""
        result = self.run_binary(['nonexistent.gwb'])
        assert result.returncode == 2
        assert 'No such file or directory' in result.stderr
