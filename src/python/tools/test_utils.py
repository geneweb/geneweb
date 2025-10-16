#!/usr/bin/env python3
"""
Simple test utilities for gwb2ged tests
Only essential functions
"""

import subprocess
import os
import tempfile
from pathlib import Path
from typing import Tuple

def get_project_root() -> Path:
    """Get the project root directory"""
    # From tools -> python -> src -> geneweb
    return Path(__file__).parent.parent.parent.parent

def change_to_project_root():
    """Change working directory to project root"""
    project_root = get_project_root()
    os.chdir(project_root)
    print(f"Changed to project root: {os.getcwd()}")

def get_absolute_path(relative_path: str) -> str:
    """Get absolute path from project root"""
    project_root = get_project_root()
    return str(project_root / relative_path)

def run_command(cmd: str, timeout: int = 30) -> Tuple[int, str, str]:
    """Execute a command and return the result"""
    try:
        result = subprocess.run(
            cmd,
            shell=True,
            capture_output=True,
            text=True,
            timeout=timeout
        )
        return result.returncode, result.stdout, result.stderr
    except subprocess.TimeoutExpired:
        return -1, "", "Timeout"
    except Exception as e:
        return -1, "", str(e)

def check_file_exists(file_path: str) -> bool:
    """Check if a file exists"""
    if not os.path.exists(file_path):
        print(f"ERROR: File not found: {file_path}")
        return False
    return True

def read_file(file_path: str) -> str:
    """Read a file and return its content"""
    with open(file_path, 'r', encoding='utf-8') as f:
        return f.read()

def create_temp_file(suffix: str = '.ged') -> str:
    """Create a temporary file and return its path"""
    with tempfile.NamedTemporaryFile(mode='w', suffix=suffix, delete=False) as f:
        return f.name

def cleanup_file(file_path: str):
    """Clean up a temporary file"""
    if os.path.exists(file_path):
        os.unlink(file_path)

def print_file_content(content: str, max_lines: int = 20):
    """Print file content in a readable format"""
    lines = content.splitlines()
    print(f"\nFile content:")
    print("-" * 40)
    for i, line in enumerate(lines[:max_lines]):
        print(f"{i+1:2d}: {line}")

    if len(lines) > max_lines:
        print(f"   ... ({len(lines) - max_lines} additional lines)")
    print("-" * 40)
