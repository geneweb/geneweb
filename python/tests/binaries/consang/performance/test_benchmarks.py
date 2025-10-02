"""Simple performance benchmarks for consang."""

import pytest
import subprocess
import time


@pytest.mark.consang
@pytest.mark.performance
def test_startup_benchmark():
    """Benchmark consang startup time."""
    times = []

    for _ in range(5):
        start_time = time.time()
        result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
        end_time = time.time()

        assert result.returncode == 0
        times.append(end_time - start_time)

    avg_time = sum(times) / len(times)
    max_time = max(times)

    print(f"\nStartup performance:")
    print(f"  Average: {avg_time:.3f}s")
    print(f"  Worst: {max_time:.3f}s")

    assert avg_time < 3.0, f"Average startup too slow: {avg_time:.3f}s"
    assert max_time < 5.0, f"Worst startup too slow: {max_time:.3f}s"


@pytest.mark.consang
@pytest.mark.performance
def test_error_handling_benchmark():
    """Benchmark error handling performance."""
    times = []

    for _ in range(5):
        start_time = time.time()
        result = subprocess.run(["consang", "nonexistent.gwb"], capture_output=True, text=True)
        end_time = time.time()

        assert result.returncode == 2
        times.append(end_time - start_time)

    avg_time = sum(times) / len(times)

    print(f"\nError handling performance:")
    print(f"  Average: {avg_time:.3f}s")

    assert avg_time < 1.0, f"Error handling too slow: {avg_time:.3f}s"


@pytest.mark.consang
@pytest.mark.performance
def test_memory_usage():
    """Test memory usage is reasonable."""
    try:
        import psutil
    except ImportError:
        pytest.skip("psutil not available")

    proc = subprocess.Popen(["consang", "--help"], stdout=subprocess.PIPE, stderr=subprocess.PIPE)

    try:
        process = psutil.Process(proc.pid)
        memory_info = process.memory_info()
        proc.wait(timeout=5)

        memory_mb = memory_info.rss / 1024 / 1024
        print(f"\nMemory usage: {memory_mb:.1f} MB")

        assert memory_mb < 50.0, f"Memory usage too high: {memory_mb:.1f} MB"

    except psutil.NoSuchProcess:
        pass
    except subprocess.TimeoutExpired:
        proc.kill()
        pytest.fail("Help command timed out")


@pytest.mark.consang
@pytest.mark.performance
@pytest.mark.slow
def test_load_test():
    """Load test with multiple rapid invocations."""
    iterations = 10
    times = []

    for i in range(iterations):
        start_time = time.time()
        result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
        end_time = time.time()

        assert result.returncode == 0, f"Iteration {i} failed"
        times.append(end_time - start_time)

    avg_time = sum(times) / len(times)
    max_time = max(times)

    print(f"\nLoad test ({iterations} iterations):")
    print(f"  Average: {avg_time:.3f}s")
    print(f"  Slowest: {max_time:.3f}s")

    assert avg_time < 2.0, f"Performance degraded: {avg_time:.3f}s"
    assert max_time < 5.0, f"Worst case too slow: {max_time:.3f}s"


@pytest.mark.consang
@pytest.mark.performance
def test_concurrent_execution():
    """Test concurrent execution performance."""
    import concurrent.futures

    def run_consang():
        result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
        return result.returncode == 0

    with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
        futures = [executor.submit(run_consang) for _ in range(5)]
        results = [future.result() for future in concurrent.futures.as_completed(futures)]

    assert all(results), "Some concurrent executions failed"


@pytest.mark.consang
@pytest.mark.performance
def test_options_performance_impact():
    """Test performance impact of options."""
    options_to_test = [
        [],
        ["-q"],
        ["-fast"],
        ["-mem"],
        ["-q", "-fast"]
    ]

    results = {}

    for options in options_to_test:
        option_name = "-".join(options) if options else "baseline"

        start_time = time.time()
        result = subprocess.run(["consang"] + options + ["nonexistent.gwb"], capture_output=True, text=True)
        end_time = time.time()

        results[option_name] = {
            "time": end_time - start_time,
            "success": result.returncode == 2
        }

    print(f"\nOptions performance impact:")
    baseline = results.get("baseline", {}).get("time", 1.0)

    for name, data in results.items():
        if data["success"]:
            ratio = data["time"] / baseline
            print(f"  {name}: {data['time']:.3f}s ({ratio:.1f}x)")

    # All options should work
    assert all(data["success"] for data in results.values())
