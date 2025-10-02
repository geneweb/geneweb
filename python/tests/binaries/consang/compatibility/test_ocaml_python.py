"""Simple compatibility tests between OCaml and Python consang."""

import pytest
import subprocess
import os
import re


def find_ocaml_binary():
    """Find OCaml consang binary."""
    possible_paths = [
        "../../../../../distribution/gw/consang",
        "../distribution/gw/consang",
        "./consang",
        "consang.opt"
    ]

    for path in possible_paths:
        if os.path.exists(path) and os.access(path, os.X_OK):
            return path

    return "consang"  # Fallback


def normalize_output(text):
    """Normalize output for comparison."""
    # Remove binary paths
    text = re.sub(r'usage: [^\s]*consang', 'usage: consang', text)
    return '\n'.join(line.rstrip() for line in text.split('\n')).strip()


@pytest.mark.consang
@pytest.mark.compatibility
def test_help_compatibility():
    """Test help output compatibility."""
    ocaml_binary = find_ocaml_binary()

    python_result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    ocaml_result = subprocess.run([ocaml_binary, "--help"], capture_output=True, text=True)

    if not ocaml_result.returncode == 0:
        pytest.skip("OCaml binary not available")

    assert python_result.returncode == 0
    assert ocaml_result.returncode == 0

    # Compare normalized outputs
    python_help = normalize_output(python_result.stdout)
    ocaml_help = normalize_output(ocaml_result.stdout)

    assert "usage: consang" in python_help
    assert "usage: consang" in ocaml_help
    assert "-fast" in python_help and "-fast" in ocaml_help


@pytest.mark.consang
@pytest.mark.compatibility
def test_error_compatibility():
    """Test error handling compatibility."""
    ocaml_binary = find_ocaml_binary()

    test_cases = [
        [],  # No args
        ["-h"],  # Invalid option
        ["nonexistent.gwb"]  # Bad file
    ]

    for args in test_cases:
        python_result = subprocess.run(["consang"] + args, capture_output=True, text=True)
        ocaml_result = subprocess.run([ocaml_binary] + args, capture_output=True, text=True)

        if not ocaml_result.returncode:
            continue  # Skip if OCaml not available

        # Both should fail similarly
        assert python_result.returncode == ocaml_result.returncode, \
            f"Exit codes differ for {args}: Python={python_result.returncode}, OCaml={ocaml_result.returncode}"


@pytest.mark.consang
@pytest.mark.compatibility
@pytest.mark.parametrize("option", ["-q", "-fast", "-mem", "-nolock"])
def test_option_compatibility(option):
    """Test individual option compatibility."""
    ocaml_binary = find_ocaml_binary()
    args = [option, "nonexistent.gwb"]

    python_result = subprocess.run(["consang"] + args, capture_output=True, text=True)
    ocaml_result = subprocess.run([ocaml_binary] + args, capture_output=True, text=True)

    if not ocaml_result.returncode:
        pytest.skip("OCaml binary not available")

    assert python_result.returncode == ocaml_result.returncode, \
        f"Option {option} handled differently"


@pytest.mark.consang
@pytest.mark.compatibility
def test_performance_comparison():
    """Simple performance comparison."""
    import time

    ocaml_binary = find_ocaml_binary()

    # Test Python
    start_time = time.time()
    python_result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    python_time = time.time() - start_time

    # Test OCaml
    start_time = time.time()
    ocaml_result = subprocess.run([ocaml_binary, "--help"], capture_output=True, text=True)
    ocaml_time = time.time() - start_time

    if not ocaml_result.returncode == 0:
        pytest.skip("OCaml binary not available")

    assert python_result.returncode == 0
    assert ocaml_result.returncode == 0

    # Python should not be unreasonably slower
    if ocaml_time > 0:
        ratio = python_time / ocaml_time
        assert ratio < 10.0, f"Python too slow: {ratio:.1f}x slower than OCaml"
