"""Simple unit tests for consang CLI."""

import pytest
import subprocess


@pytest.mark.consang
@pytest.mark.unit
def test_help_works():
    """Test help command works."""
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)

    assert result.returncode == 0
    assert "usage: consang" in result.stdout
    assert "-fast" in result.stdout
    assert "-mem" in result.stdout


@pytest.mark.consang
@pytest.mark.unit
def test_no_args_error():
    """Test no arguments shows error."""
    result = subprocess.run(["consang"], capture_output=True, text=True)

    assert result.returncode == 2
    assert "Missing file name" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
def test_invalid_option():
    """Test invalid option shows error."""
    result = subprocess.run(["consang", "-invalid"], capture_output=True, text=True)

    assert result.returncode == 2
    error_text = (result.stderr + result.stdout).lower()
    assert "unknown option" in error_text or "unrecognized arguments" in error_text


@pytest.mark.consang
@pytest.mark.unit
def test_nonexistent_file():
    """Test nonexistent file shows error."""
    result = subprocess.run(["consang", "nonexistent.gwb"], capture_output=True, text=True)

    assert result.returncode == 2
    assert "No such file" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
@pytest.mark.parametrize("option", ["-q", "-fast", "-mem", "-nolock"])
def test_valid_options(option):
    """Test valid options work (even with bad file)."""
    result = subprocess.run(["consang", option, "nonexistent.gwb"], capture_output=True, text=True)

    # Should fail due to file, not option
    assert result.returncode == 2
    assert "No such file" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
def test_combined_options():
    """Test combined options work."""
    result = subprocess.run(["consang", "-q", "-fast", "nonexistent.gwb"], capture_output=True, text=True)

    assert result.returncode == 2
    assert "No such file" in result.stderr


@pytest.mark.consang
@pytest.mark.unit
def test_startup_performance():
    """Test startup is fast."""
    import time

    start_time = time.time()
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    execution_time = time.time() - start_time

    assert result.returncode == 0
    assert execution_time < 2.0, f"Startup too slow: {execution_time:.2f}s"
