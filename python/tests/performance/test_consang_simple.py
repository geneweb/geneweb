"""Simple performance tests for consang."""

import pytest
import subprocess
import time


@pytest.mark.performance
@pytest.mark.consang
class TestConsangSimplePerformance:
    """Basic performance tests for consang."""

    def run_binary_timed(self, args=None, timeout=10):
        """Execute consang with timing measurement."""
        cmd = ['consang']
        if args:
            cmd.extend(args)

        start_time = time.perf_counter()
        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False
            )
            exec_time = time.perf_counter() - start_time
            return result, exec_time
        except subprocess.TimeoutExpired:
            exec_time = time.perf_counter() - start_time
            result = subprocess.CompletedProcess(
                args=cmd,
                returncode=124,
                stdout="",
                stderr="Timeout"
            )
            return result, exec_time

    def test_startup_time(self):
        """Test that startup is fast."""
        times = []
        for _ in range(3):
            _, exec_time = self.run_binary_timed(['--help'])
            times.append(exec_time)

        avg_time = sum(times) / len(times)
        assert avg_time < 3.0, f"Startup too slow: {avg_time:.2f}s"

    def test_error_response_time(self):
        """Test that errors are handled quickly."""
        times = []
        for _ in range(3):
            _, exec_time = self.run_binary_timed([])
            times.append(exec_time)

        avg_time = sum(times) / len(times)
        assert avg_time < 1.0, f"Error handling too slow: {avg_time:.2f}s"

    def test_help_response_time(self):
        """Test that help displays quickly."""
        _, exec_time = self.run_binary_timed(['--help'])
        assert exec_time < 2.0, f"Help too slow: {exec_time:.2f}s"
