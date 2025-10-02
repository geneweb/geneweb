"""Simple integration tests for consang."""

import pytest
import subprocess
import tempfile
import shutil
from pathlib import Path


@pytest.mark.consang
@pytest.mark.integration
def test_help_command():
    """Test help command end-to-end."""
    result = subprocess.run(["consang", "--help"], capture_output=True, text=True)

    assert result.returncode == 0
    assert "usage: consang" in result.stdout


@pytest.mark.consang
@pytest.mark.integration
def test_error_messages():
    """Test error messages are helpful."""
    test_cases = [
        ([], "Missing file name"),
        (["-h"], "unknown option"),
        (["bad.gwb"], "No such file")
    ]

    for args, expected_error in test_cases:
        result = subprocess.run(["consang"] + args, capture_output=True, text=True)

        assert result.returncode != 0
        output = (result.stderr + result.stdout).lower()
        assert expected_error.lower() in output or "unrecognized" in output


@pytest.mark.consang
@pytest.mark.integration
def test_performance_options():
    """Test performance options don't crash."""
    options = ["-fast", "-mem", "-nolock", "-scratch"]

    for option in options:
        result = subprocess.run(["consang", option, "bad.gwb"], capture_output=True, text=True)
        # Should fail due to file, not option
        assert result.returncode == 2


@pytest.mark.consang
@pytest.mark.integration
@pytest.mark.slow
def test_empty_database():
    """Test with empty database."""
    temp_dir = tempfile.mkdtemp()
    try:
        db_path = Path(temp_dir) / "empty.gwb"
        db_path.mkdir()

        # Create minimal base file
        base_file = db_path / "base"
        base_file.write_text("# Empty database\n")

        result = subprocess.run(["consang", str(db_path)], capture_output=True, text=True, timeout=30)

        # Should not crash
        assert "Segmentation fault" not in result.stderr
        assert "core dumped" not in result.stderr

    finally:
        shutil.rmtree(temp_dir)


@pytest.mark.consang
@pytest.mark.integration
def test_concurrent_execution():
    """Test multiple simultaneous executions."""
    import concurrent.futures

    def run_help():
        result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
        return result.returncode == 0

    with concurrent.futures.ThreadPoolExecutor(max_workers=3) as executor:
        futures = [executor.submit(run_help) for _ in range(3)]
        results = [f.result() for f in futures]

    assert all(results), "All concurrent runs should succeed"
