import pytest
import subprocess
import os
import re


def find_ocaml_binary():
    """Find OCaml consang binary."""
    possible_paths = [
        "../distribution/gw/consang",
        "./consang",
        "consang.opt",
    ]

    for path in possible_paths:
        try:
            if os.path.exists(path) and os.access(path, os.X_OK):
                result = subprocess.run([path, "--help"], capture_output=True, text=True, timeout=5)
                if result.returncode == 0:
                    return path
        except (OSError, subprocess.SubprocessError, FileNotFoundError):
            continue

    return None


def check_ocaml_available():
    """Check if OCaml binary is available and working."""
    binary = find_ocaml_binary()
    if not binary:
        return False, "OCaml consang binary not found"

    try:
        result = subprocess.run([binary, "--help"], capture_output=True, text=True, timeout=5)
        return result.returncode == 0, f"OCaml binary failed: {result.stderr}"
    except Exception as e:
        return False, f"OCaml binary error: {e}"


def normalize_output(text):
    """Normalize output for comparison."""
    # Remove binary paths
    text = re.sub(r'usage: [^\s]*consang', 'usage: consang', text)
    return '\n'.join(line.rstrip() for line in text.split('\n')).strip()


@pytest.mark.consang
@pytest.mark.compatibility
def test_help_compatibility():
    """Test help output compatibility."""
    available, reason = check_ocaml_available()
    if not available:
        pytest.skip(f"OCaml binary not available: {reason}")

    ocaml_binary = find_ocaml_binary()

    python_result = subprocess.run(["consang", "--help"], capture_output=True, text=True)
    ocaml_result = subprocess.run([ocaml_binary, "--help"], capture_output=True, text=True)

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
    available, reason = check_ocaml_available()
    if not available:
        pytest.skip(f"OCaml binary not available: {reason}")

    ocaml_binary = find_ocaml_binary()

    test_cases = [
        [],  # No args
        ["-h"],  # Invalid option
        ["nonexistent.gwb"]  # Bad file
    ]

    for args in test_cases:
        python_result = subprocess.run(["consang"] + args, capture_output=True, text=True)
        ocaml_result = subprocess.run([ocaml_binary] + args, capture_output=True, text=True)

        # Both should fail similarly
        assert python_result.returncode == ocaml_result.returncode, \
            f"Exit codes differ for {args}: Python={python_result.returncode}, OCaml={ocaml_result.returncode}"


@pytest.mark.consang
@pytest.mark.compatibility
@pytest.mark.parametrize("option", ["-q", "-fast", "-mem", "-nolock"])
def test_option_compatibility(option):
    """Test individual option compatibility."""
    available, reason = check_ocaml_available()
    if not available:
        pytest.skip(f"OCaml binary not available: {reason}")

    ocaml_binary = find_ocaml_binary()
    args = [option, "nonexistent.gwb"]

    python_result = subprocess.run(["consang"] + args, capture_output=True, text=True)
    ocaml_result = subprocess.run([ocaml_binary] + args, capture_output=True, text=True)

    assert python_result.returncode == ocaml_result.returncode, \
        f"Option {option} handled differently"


@pytest.mark.consang
@pytest.mark.compatibility
def test_performance_comparison():
    """Simple performance comparison."""
    available, reason = check_ocaml_available()
    if not available:
        pytest.skip(f"OCaml binary not available: {reason}")

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

    assert python_result.returncode == 0
    assert ocaml_result.returncode == 0

    # Python should not be unreasonably slower
    if ocaml_time > 0:
        ratio = python_time / ocaml_time
        assert ratio < 10.0, f"Python too slow: {ratio:.1f}x slower than OCaml"
