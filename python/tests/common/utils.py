"""Common test utilities."""

import subprocess
import time
from typing import List, Dict, Any


class TestUtils:
    """Test execution utilities."""

    @staticmethod
    def run_binary(binary_name: str, args: List[str] = None, timeout: int = 10) -> subprocess.CompletedProcess:
        """Execute binary and capture output."""
        cmd = [binary_name]
        if args:
            cmd.extend(args)

        try:
            return subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                timeout=timeout,
                check=False
            )
        except subprocess.TimeoutExpired:
            return subprocess.CompletedProcess(
                args=cmd,
                returncode=124,
                stdout="",
                stderr="Timeout expired"
            )
        except Exception as e:
            return subprocess.CompletedProcess(
                args=cmd,
                returncode=1,
                stdout="",
                stderr=str(e)
            )

    @staticmethod
    def measure_time(func, *args, **kwargs) -> tuple:
        """Measure function execution time."""
        start_time = time.perf_counter()
        result = func(*args, **kwargs)
        end_time = time.perf_counter()
        return result, end_time - start_time


class PerformanceTester:
    """Performance testing utilities."""

    def __init__(self, binary_name: str):
        self.binary_name = binary_name

    def benchmark_startup_time(self, iterations: int = 5) -> Dict[str, float]:
        """Measure startup time."""
        times = []

        for _ in range(iterations):
            _, exec_time = TestUtils.measure_time(
                TestUtils.run_binary, self.binary_name, ['--help']
            )
            times.append(exec_time)

        return {
            'min': min(times),
            'max': max(times),
            'avg': sum(times) / len(times)
        }

    def benchmark_error_handling(self, iterations: int = 3) -> Dict[str, float]:
        """Measure error handling time."""
        times = []

        for _ in range(iterations):
            _, exec_time = TestUtils.measure_time(
                TestUtils.run_binary, self.binary_name, []
            )
            times.append(exec_time)

        return {
            'min': min(times),
            'max': max(times),
            'avg': sum(times) / len(times)
        }
