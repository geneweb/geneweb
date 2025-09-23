"""Miscellaneous utilities for GeneWeb database handling."""

from contextlib import contextmanager
import enum
import functools
import io
import logging
import os
import re
from typing import List

from lib.db.unmarshall.v2.stdlib import Ref

VERBOSE = Ref(logging.WARNING)


def get_logger(name: str, *, log_level: int = None) -> logging.Logger:
    """Get a logger for the specified name.

    Args:
        name (str): The name of the logger.

    Returns:
        logging.Logger: Configured logger instance.
    """
    logger = logging.getLogger(name)

    ch = logging.StreamHandler()

    class Formatter(logging.Formatter):
        def get_prefix(self, levelno):
            if levelno >= logging.ERROR:
                return "⛔ "  # Red
            elif levelno >= logging.WARNING:
                return "⚠️ "  # Yellow
            else:
                return "🔵 "  # Blue

        def format(self, record):
            prefix = self.get_prefix(record.levelno)
            record.msg = f"{prefix}{record.msg}"
            return super().format(record)

    ch.setFormatter(
        Formatter("(%(filename)s:%(lineno)d) - %(name)s - %(levelname)s - %(message)s")
    )
    ch.setLevel(logging.DEBUG)
    if ch not in logger.handlers:
        logger.addHandler(ch)
    logger.propagate = False
    if log_level is not None:
        logger.setLevel(log_level)
    else:
        logger.setLevel(VERBOSE.ref * 10)
    return logger


def get_child_logger(logger: logging.Logger, name: str) -> logging.Logger:
    """Get a child logger with the specified name.

    Args:
        logger (logging.Logger): Parent logger.
        name (str): Name of the child logger.

    Returns:
        logging.Logger: Child logger instance.
    """
    child_logger = logger.getChild(name)
    if child_logger.hasHandlers() and len(child_logger.handlers) > 1:
        child_logger.removeHandler(child_logger.handlers[0])
    return child_logger


def get_dataclass_fields(cls: type) -> tuple[List[str], range]:
    """Get fields of a dataclass.

    Args:
        cls: Dataclass type

    Returns:
        Tuple of:
        - List of field names
        - Range of field indices for fields without default values
    """
    field_names_range = list(
        (0, k) if not v.default and not v.default_factory else (1, k)
        for (k, v) in cls.__dataclass_fields__.items()
        if v.init
    )
    field_names = list(f[1] for f in field_names_range)
    field_name_range = range(
        len([f for f in field_names_range if f[0] == 0]), len(field_names_range) + 1
    )
    return field_names, field_name_range


def default_particles():
    upper = [
        "AF ",
        "D'",
        "D’",
        "DAL ",
        "DE ",
        "DES ",
        "DI ",
        "DU ",
        "OF ",
        "VAN ",
        "VON UND ZU ",
        "VON ",
        "Y ",
        "ZU ",
        "ZUR ",
    ]
    # Add lowercase versions of each string, then append the originals
    return [s.lower() for s in upper] + upper


def input_particles(fname):
    try:
        with open(fname, "r", encoding="utf-8") as f:
            result = []
            buff = []
            while True:
                c = f.read(1)
                if not c:
                    break
                if c == "_":
                    buff.append(" ")
                elif c == "\n":
                    if buff:
                        result.append("".join(buff))
                        buff = []
                elif c == "\r":
                    continue
                else:
                    buff.append(c)
            if buff:
                result.append("".join(buff))
            return result
    except OSError:
        return default_particles  # You need to define default_particles elsewhere


def check_magic(magic: bytes, f: io.BufferedReader) -> bool:
    """
    magic: bytes or str (expected magic string)
    f: file object opened in binary mode
    """
    len_magic = len(magic)
    pos = f.tell()
    f.seek(0, os.SEEK_END)  # move to end to get file length
    file_length = f.tell()
    f.seek(pos)  # go back to original position
    if file_length - pos < len_magic:
        print("File too short to contain magic")
        return False
    data = f.read(len_magic)
    if data == magic:
        return True
    else:
        f.seek(pos)
        return False


@contextmanager
def checking_magic(length: int, f: io.BufferedReader):
    """
    Context manager to check magic at the start of a file and restore position afterwards.
    """
    pos = f.tell()
    f.seek(0, os.SEEK_END)  # move to end to get file length
    file_length = f.tell()
    f.seek(pos)  # go back to original position
    if file_length - pos < length:
        logging.warning("File too short to contain magic")
        yield False
    try:
        yield f.read(length)
    except Exception as e:
        f.seek(pos)
        raise e
    finally:
        # The file position only needs to be restored if the magic check failed
        pass


def compile_particles(particles: List[str]) -> re.Pattern:
    """
    Compile list of particles into regex pattern.

    Args:
        particles: List of particle strings

    Returns:
        Compiled regex pattern that matches particles at start of string
    """
    # Convert particles to regex patterns:
    # 1. Replace _ with space in each particle
    # 2. Create regex pattern for each particle
    # 3. Join with | for alternation
    # 4. Add start anchor and greedy match
    parts = [re.escape(p.replace("_", " ")) for p in particles]
    pattern = f"^({'|'.join(parts)}).*"

    return re.compile(pattern)


def colon_to_at_word(s: str, ibeg: int, iend: int):
    iendroot = iend
    for i in range(ibeg, iend - 3):
        # find first occurrence of ':c:' with c any char
        if s[i] == ":" and s[i + 2] == ":":
            iendroot = i
            break
    if iendroot == iend:  # no occurrence found
        return s[ibeg:iend]

    # for i in range(iendroot, iend):
    # def loop(list, maxd, i):
    #     if i >= iend: return (list, maxd)
    #     inext = 0
    #     for j in range(i + 3, iend - 3):
    #         if s[i] == ':' and s[i + 2] == ':': # find next occurrence of ':c:'
    #             inext = j
    #             break
    #     i = i + 3
    #     j = inext
    #     if i < j and s[i] == "+":
    #         e = s[i + 1:j]
    #         return loop([s[i + 1], e] + list)
    # listdcl, maxd = loop([], 0, iendroot)
    def parse_declarations(
        s: str, iendroot: int, ibeg: int, iend: int
    ) -> tuple[list[tuple[str, str]], int]:
        """
        Parse name declarations with special formatting.

        Args:
            s: Input string
            iendroot: End index of root part
            ibeg: Begin index
            iend: End index

        Returns:
            Tuple of:
            - List of (char, string) tuples representing declarations
            - Maximum depth found
        """

        def find_next_delimiter(i: int) -> int:
            """Find next :: delimiter"""
            while i + 3 < iend:
                if s[i] == ":" and s[i + 2] == ":":
                    return i
                i += 1
            return iend

        def parse_entry(i: int, j: int) -> tuple[str, int]:
            """Parse entry and get depth"""
            i += 3  # Skip past delimiter
            if i >= j:
                return "", iendroot - ibeg
            if s[i] == "+":
                return s[i + 1 : j - 1], 0
            elif s[i] == "-":
                depth = 1
                i += 1
                while i < j and s[i] == "-":
                    depth += 1
                    i += 1
                return s[i:j], depth
            else:
                return s[i:j], iendroot - ibeg

        declarations = []
        max_depth = 0
        i = iendroot

        while i < iend:
            inext = find_next_delimiter(i)
            entry, depth = parse_entry(i, inext)
            declarations.append((s[i + 1], entry))
            max_depth = max(depth, max_depth)
            i = inext

        return declarations, max_depth

    listdecl, maxd = parse_declarations(s, iendroot, ibeg, iend)
    length = max(0, iendroot - ibeg - maxd)
    root = s[ibeg : ibeg + length]
    s = functools.reduce(
        lambda acc, decl: ":".join(f"{decl[0]}?{decl[1]}", acc),
        listdecl,
        s[ibeg + length : iendroot],
    )
    return f"{root}@({s})"


def colon_to_at(s: str) -> str:
    if not s:
        return ""
    for i, c in enumerate(s):
        match c:
            case " " | "<" | "/" as sep:
                return colon_to_at_word(s, 0, i) + sep + colon_to_at(s[i + 1 :])
            case ">":
                return s[: i + 1] + colon_to_at(s[i + 1 :])
            case _:
                continue
    raise ValueError(f"Invalid string: {s}")


def decline(case: str, s: str) -> str:
    """
    Decline a string according to case.

    Args:
        case: Case to decline to ('n' for nominative)
        s: String to decline

    Returns:
        Declined string
    """
    if ":" in s:
        s = colon_to_at(s)
    return f"@(@({case}){s})"


def nominative(s: str) -> str:
    """
    Get nominative form of a string.
    If string contains ':', use decline('n'),
    otherwise return string as-is.

    Args:
        s: Input string

    Returns:
        String in nominative form
    """
    try:
        _ = s.rindex(":")
        return decline("n", s)
    except ValueError:
        return s


def enum_to_string(e: enum.Enum) -> str:
    """Convert enum to string representation."""
    s = e.name.split("_")
    return " ".join([s[0].capitalize()] + [w.lower() for w in s[1:]])
