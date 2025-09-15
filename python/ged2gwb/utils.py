import re
import sys
from typing import Optional

log_oc = sys.stdout
in_file: str = ""

def set_context(log_stream, filename: str):
    global log_oc, in_file
    log_oc = log_stream
    in_file = filename

def print_location(pos: int):
    print(f'File "{in_file}", line {pos}:', file=log_oc)

def strip_char(c: str, s: Optional[str]) -> str:
    if s is None:
        return ""
    start = 0
    end = len(s) - 1
    while start < len(s) and s[start] == c:
        start += 1
    while end >= 0 and s[end] == c:
        end -= 1
    return "" if start > end else s[start:end+1]

def strip_spaces(s: Optional[str]) -> str:
    return strip_char(' ', s)

def strip_newlines(s: Optional[str]) -> str:
    return strip_char('\n', s)

def preg_match(pattern: str, subject: str) -> bool:
    return re.search(pattern, subject) is not None

def less_greater_escaped(s: str) -> str:
    return s.replace('<', '&lt;').replace('>', '&gt;')

def good_name(name: str) -> bool:
    """Check if database name is valid - match OCaml exactly"""
    if not name:
        return False

    for char in name:
        if not (char.isalnum() or char == '-'):
            return False

    return True

def safe_remove_file(filename: str) -> bool:
    """Safely remove file, return True if successful"""
    try:
        import os
        if os.path.exists(filename):
            os.remove(filename)
        return True
    except OSError:
        return False

def can_write_file(filename: str) -> bool:
    """Check if file can be written"""
    try:
        import os
        dir_path = os.path.dirname(filename) or '.'
        return os.access(dir_path, os.W_OK)
    except:
        return False
