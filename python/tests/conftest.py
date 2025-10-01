"""Simplified pytest configuration."""

import pytest
import tempfile
import shutil
from pathlib import Path
import sys

# Add python to path
python_dir = Path(__file__).parent.parent / "python"
if str(python_dir) not in sys.path:
    sys.path.insert(0, str(python_dir))


@pytest.fixture
def temp_database_dir():
    """Temporary directory for tests."""
    temp_dir = tempfile.mkdtemp(prefix="geneweb_test_")
    temp_path = Path(temp_dir)
    yield temp_path

    try:
        shutil.rmtree(temp_dir)
    except:
        pass


@pytest.fixture
def mock_gwb_database(temp_database_dir: Path) -> Path:
    """Mock GWB database."""
    db_path = temp_database_dir / "test.gwb"
    db_path.mkdir(exist_ok=True)

    base_file = db_path / "base"
    base_file.write_text("# Test database")

    return db_path


def pytest_configure(config):
    """Configure pytest markers."""
    markers = [
        "unit: Unit tests",
        "integration: Integration tests",
        "golden: Golden Master tests",
        "performance: Performance tests",
        "compatibility: Compatibility tests",
        "consang: Tests for consang binary",
        "geneweb_common: Tests for geneweb_common",
        "slow: Slow running tests"
    ]

    for marker in markers:
        config.addinivalue_line("markers", marker)
