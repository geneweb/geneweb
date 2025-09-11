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
