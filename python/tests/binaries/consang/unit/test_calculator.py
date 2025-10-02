"""Simple unit tests for consang calculator."""

import pytest
import subprocess


@pytest.mark.consang
@pytest.mark.unit
def test_calculator_help_available():
    """Test calculator help is available through binary."""
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)

    assert result.returncode == 0
    assert "usage" in result.stdout.lower()


@pytest.mark.consang
@pytest.mark.unit
def test_calculator_handles_invalid_input():
    """Test calculator handles invalid input gracefully."""
    result = subprocess.run(["consang", "invalid_file.gwb"], capture_output=True, text=True)

    assert result.returncode == 2
    assert "No such file" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
@pytest.mark.parametrize("option", ["-fast", "-mem"])
def test_calculator_performance_options(option):
    """Test calculator accepts performance options."""
    result = subprocess.run(["consang", option, "nonexistent.gwb"], capture_output=True, text=True)

    # Should fail due to file, not option
    assert result.returncode == 2
    assert "No such file" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
def test_calculator_basic_validation():
    """Test basic calculator validation through binary."""
    # Test no arguments
    result = subprocess.run(["consang"], capture_output=True, text=True)
    assert result.returncode == 2

    # Test invalid option
    result = subprocess.run(["consang", "-invalid"], capture_output=True, text=True)
    assert result.returncode == 2
    error_text = (result.stderr + result.stdout).lower()
    assert "unknown option" in error_text or "unrecognized arguments" in error_text


@pytest.mark.consang
@pytest.mark.unit
def test_calculator_startup_performance():
    """Test calculator startup performance."""
    import time

    start_time = time.time()
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    execution_time = time.time() - start_time

    assert result.returncode == 0
    assert execution_time < 3.0, f"Startup too slow: {execution_time:.2f}s"

