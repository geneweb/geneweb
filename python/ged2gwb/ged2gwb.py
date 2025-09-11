"""
Squelette de traduction de bin/ged2gwb/ged2gwb.ml -> Python.

Contient:
- définitions d'enums et flags globaux
- utilitaires charset (stubs)
- structures Tab / Gen minimalistes
- stubs pour pass1/pass2/pass3, make_arrays, make_subarrays, finish_base

Objectif: fournir une base propre et exécutable pour traduire la logique OCaml étape par étape.
"""
from dataclasses import dataclass, field
import io
import os
import sys
import logging
from enum import Enum
from typing import Optional, Tuple, List, Any, Dict, auto

# Remplacer les définitions d'enums et dataclasses par imports centralisés
from .types import MonthNumberDates, Charset, Case, Tab, Gen, Record

# Flags globaux (mutable)
lowercase_first_names: bool = False
track_ged2gw_id: bool = False
case_surnames: Case = Case.NoCase
extract_first_names: bool = False
extract_public_names: bool = True
charset_option: Optional[Charset] = None
charset: Charset = Charset.Ascii
alive_years: int = 80
dead_years: int = 120
try_negative_dates: bool = False
no_negative_dates: bool = False
month_number_dates: MonthNumberDates = MonthNumberDates.NoMonthNumberDates
no_public_if_titles: bool = False
first_names_brackets: Optional[Tuple[str, str]] = None
untreated_in_notes: bool = False
default_source: str = ""
relation_status = "Married"
no_picture: bool = False
do_check: bool = True
particles: List[str] = []  # à charger plus tard

log_oc = sys.stdout
line_cnt: int = 1
in_file: str = ""

# -----------------------
# Charset / small utils
# -----------------------
# Minimal translations tables (partial, safe fallback to identity)
_MSDOS_TO_ISO = {
    0x80: 0xE7, 0x81: 0xFC, 0x82: 0xE9, 0x83: 0xE2,
    0x84: 0xE4, 0x85: 0xE0, 0x86: 0xE5, 0x87: 0xE7,
}
_MAC_TO_ISO = {
    0x80: 0xC4, 0x81: 0xC5, 0x82: 0xC7,
}

def print_location(pos: int):
    print(f'File "{in_file}", line {pos}:', file=log_oc)

def ascii_of_msdos(s: str) -> str:
    out = []
    for b in s.encode('latin-1', errors='replace'):
        mapped = _MSDOS_TO_ISO.get(b, b)
        out.append(mapped)
    return bytes(out).decode('latin-1', errors='replace')

def ascii_of_macintosh(s: str) -> str:
    out = []
    for b in s.encode('latin-1', errors='replace'):
        mapped = _MAC_TO_ISO.get(b, b)
        out.append(mapped)
    return bytes(out).decode('latin-1', errors='replace')

def utf8_of_string(s: str) -> str:
    cs = charset_option or charset
    if cs is None:
        return s
    cs = cs.upper()
    if cs == "MSDOS":
        return ascii_of_msdos(s)
    if cs == "MACINTOSH":
        return ascii_of_macintosh(s)
    return s

def strip(c: str, s: str) -> str:
    if s is None:
        return ""
    start = 0
    end = len(s) - 1
    while start < len(s) and s[start] == c:
        start += 1
    while end >= 0 and s[end] == c:
        end -= 1
    if start > end:
        return ""
    return s[start:end+1]

def strip_spaces(s: str) -> str:
    return strip(' ', s)

def strip_newlines(s: str) -> str:
    return strip('\n', s)

# -----------------------
# NAME parsing (simplified)
# -----------------------
def ident_slash(s: str) -> str:
    try:
        i = s.index('/')
        return s[:i]
    except ValueError:
        return s

def parse_name(s: str) -> Tuple[str,str]:
    s = s.strip()
    parts = s.split('/')
    if len(parts) >= 3:
        f = parts[0].strip()
        sname = parts[1].strip()
    else:
        tokens = s.split()
        f = tokens[0] if tokens else "?"
        sname = tokens[-1] if len(tokens) > 1 else "?"
    if f == "":
        f = "x"
    if sname == "":
        sname = "?"
    return f, sname

# -----------------------
# Record utilities
# -----------------------
def find_field(lab: str, rl: List[Record]) -> Optional[Record]:
    for r in rl:
        if r.rlab == lab:
            r.rused = True
            return r
    return None

def find_all_fields(lab: str, rl: List[Record]) -> List[Record]:
    out = []
    for r in rl:
        if r.rlab == lab:
            r.rused = True
            out.append(r)
    return out

def find_field_with_value(lab: str, v: str, rl: List[Record]) -> bool:
    for r in rl:
        if r.rlab == lab and r.rval == v:
            r.rused = True
            return True
    return False

def flatten_notes(rl: List[Record]) -> List[Tuple[str,str]]:
    out = []
    for r in rl:
        if r.rlab in ("CONC", "CONT", "NOTE"):
            out.append((r.rlab, r.rval))
            out.extend(flatten_notes(r.rsons))
        else:
            out.extend(flatten_notes(r.rsons))
    return out

# -----------------------
# make_arrays / make_subarrays / finish_base
# -----------------------
from .pipeline import make_arrays, make_subarrays, finish_base
import logging
import sys

def main(argv=None):
    if argv is None:
        argv = sys.argv[1:]
    if not argv:
        print("Usage: python -m ged2gwb <file.ged>")
        return
    infile = argv[0]
    arrays = make_arrays(infile)
    subarrays = make_subarrays(arrays)
    finish_base(subarrays)
    logging.info("ged2gwb pipeline (minimal) completed for %s", infile)

if __name__ == "__main__":
    main()
