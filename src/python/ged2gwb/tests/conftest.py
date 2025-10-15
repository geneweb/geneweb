"""
Pytest configuration and fixtures for GED2GWB tests.

This module provides shared fixtures and configuration for all test modules.
"""

import tempfile
from pathlib import Path
from typing import Generator

import pytest

from ged2gwb.core.converter import Ged2GwbConverter
from ged2gwb.utils.options import ConversionOptions


@pytest.fixture
def sample_gedcom_content() -> str:
    """Sample GEDCOM content for testing."""
    return """0 HEAD
1 SOUR Test
1 CHAR UTF-8
1 GEDC
2 VERS 5.5.1
0 @I1@ INDI
1 NAME Jean /Dupont/
2 GIVN Jean
2 SURN Dupont
1 SEX M
1 BIRT
2 DATE 15 MAR 1980
2 PLAC Paris, France
1 FAMS @F1@
0 @I2@ INDI
1 NAME Marie /Martin/
2 GIVN Marie
2 SURN Martin
1 SEX F
1 BIRT
2 DATE 20 JUN 1985
2 PLAC Lyon, France
1 FAMS @F1@
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 MARR
2 DATE 10 SEP 2010
2 PLAC Paris, France
0 TRLR
"""


@pytest.fixture
def empty_gedcom_content() -> str:
    """Empty GEDCOM content for testing."""
    return """0 HEAD
1 SOUR Test
1 CHAR UTF-8
1 GEDC
2 VERS 5.5.1
0 TRLR
"""


@pytest.fixture
def malformed_gedcom_content() -> str:
    """Malformed GEDCOM content (missing TRLR) for testing."""
    return """0 HEAD
1 SOUR Test
1 CHAR UTF-8
1 GEDC
2 VERS 5.5.1
0 @I1@ INDI
1 NAME Jean /Dupont/
1 SEX M
"""


@pytest.fixture
def sample_gedcom_file(sample_gedcom_content: str) -> Generator[Path, None, None]:
    """Create a temporary sample GEDCOM file."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".ged", delete=False) as f:
        f.write(sample_gedcom_content)
        f.flush()
        temp_path = Path(f.name)

    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()


@pytest.fixture
def empty_gedcom_file(empty_gedcom_content: str) -> Generator[Path, None, None]:
    """Create a temporary empty GEDCOM file."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".ged", delete=False) as f:
        f.write(empty_gedcom_content)
        f.flush()
        temp_path = Path(f.name)

    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()


@pytest.fixture
def malformed_gedcom_file(malformed_gedcom_content: str) -> Generator[Path, None, None]:
    """Create a temporary malformed GEDCOM file."""
    with tempfile.NamedTemporaryFile(mode="w", suffix=".ged", delete=False) as f:
        f.write(malformed_gedcom_content)
        f.flush()
        temp_path = Path(f.name)

    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()


@pytest.fixture
def basic_conversion_options(sample_gedcom_file: Path) -> ConversionOptions:
    """Basic conversion options for testing."""
    return ConversionOptions(
        input_file=sample_gedcom_file,
        output_file=Path("test-basic.pkl"),
        compress=False,
        verbose=False,
    )


@pytest.fixture
def compressed_conversion_options(sample_gedcom_file: Path) -> ConversionOptions:
    """Compressed conversion options for testing."""
    return ConversionOptions(
        input_file=sample_gedcom_file,
        output_file=Path("test-compressed.pkl"),
        compress=True,
        verbose=True,
    )


@pytest.fixture
def converter(basic_conversion_options: ConversionOptions) -> Ged2GwbConverter:
    """GED2GWB converter instance for testing."""
    return Ged2GwbConverter(basic_conversion_options)


@pytest.fixture
def temp_output_file() -> Generator[Path, None, None]:
    """Create a temporary output file path."""
    temp_path = Path(tempfile.mktemp(suffix=".pkl"))
    yield temp_path

    # Cleanup
    if temp_path.exists():
        temp_path.unlink()
    # Also cleanup compressed version
    compressed_path = Path(str(temp_path) + ".gz")
    if compressed_path.exists():
        compressed_path.unlink()


@pytest.fixture
def real_sample_gedcom() -> Path:
    """Path to the real sample.ged file if it exists."""
    sample_path = Path("gedcom/ged/sample.ged")
    if sample_path.exists():
        return sample_path

    # Try alternative paths
    alt_paths = [
        Path("../gedcom/ged/sample.ged"),
        Path("../../gedcom/ged/sample.ged"),
        Path("src/python/gedcom/ged/sample.ged"),
    ]

    for path in alt_paths:
        if path.exists():
            return path

    pytest.skip("sample.ged not found")


@pytest.fixture
def real_uk_gedcom() -> Path:
    """Path to the real uk.ged file if it exists."""
    uk_path = Path("gedcom/ged/uk.ged")
    if uk_path.exists():
        return uk_path

    # Try alternative paths
    alt_paths = [
        Path("../gedcom/ged/uk.ged"),
        Path("../../gedcom/ged/uk.ged"),
        Path("src/python/gedcom/ged/uk.ged"),
    ]

    for path in alt_paths:
        if path.exists():
            return path

    pytest.skip("uk.ged not found")


@pytest.fixture(autouse=True)
def cleanup_test_files():
    """Automatically cleanup test files after each test."""
    yield

    # Cleanup common test files
    test_files = [
        "test_basic.pkl",
        "test_compressed.pkl",
        "test_legacy.pkl",
        "test_structure.pkl",
        "test_perf.pkl",
        "test_empty.pkl",
        "test_malformed.pkl",
        "test_integration.pkl",
        "test_sample_concrete.pkl",
        "test_uk_concrete.pkl",
        "test_cli_concrete.pkl",
        "test_load_concrete.pkl",
        "base.pkl",
    ]

    for file_name in test_files:
        file_path = Path(file_name)
        if file_path.exists():
            file_path.unlink()

        # Also cleanup compressed versions
        compressed_path = Path(str(file_path) + ".gz")
        if compressed_path.exists():
            compressed_path.unlink()
