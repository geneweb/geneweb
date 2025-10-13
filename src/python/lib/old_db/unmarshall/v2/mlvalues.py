#!/usr/bin/env python3
"""
OCaml MLValues Library - Python Translation

This file is a Python translation of OCaml's mlvalues.c header file.
It provides constants, type definitions, and utility functions for
working with OCaml values in their internal representation.

Original file:
    Xavier Leroy and Damien Doligez, INRIA Rocquencourt
    Copyright 1996 Institut National de Recherche en Informatique et
    en Automatique.

Python translation provides similar functionality for reading OCaml
values from binary data.
"""

import enum
import struct
import sys
from dataclasses import dataclass
from typing import Optional

# Platform-specific constants
WORD_SIZE = 8 if sys.maxsize > 2**32 else 4
CHAR_BIT = 8

# Type aliases for better readability
value = int
header_t = int
reserved_t = int
mlsize_t = int
tag_t = int
color_t = int
mark_t = int
atomic_value = int
opcode_t = int


@dataclass
class CamlResult:
    """
    A 'result' type for OCaml computations.
    Represents the result of computing an OCaml term -- either a value or an exception.
    """

    is_exception: bool
    data: value

    @classmethod
    def value(cls, v: value) -> "CamlResult":
        """Create a result containing a value."""
        return cls(is_exception=False, data=v)

    @classmethod
    def exception(cls, exn: value) -> "CamlResult":
        """Create a result containing an exception."""
        return cls(is_exception=True, data=exn)


# Basic value checking
def is_long(x: value) -> bool:
    """Check if a value represents a long integer (tagged)."""
    return (x & 1) != 0


def is_block(x: value) -> bool:
    """Check if a value represents a block (untagged pointer)."""
    return (x & 1) == 0


# Long/int conversion
def val_long(x: int) -> value:
    """Convert a long integer to OCaml value representation."""
    return (x << 1) + 1


def long_val(x: value) -> int:
    """Extract long integer from OCaml value."""
    return x >> 1


def val_int(x: int) -> value:
    """Convert an integer to OCaml value representation."""
    return val_long(x)


def int_val(x: value) -> int:
    """Extract integer from OCaml value."""
    return long_val(x)


def unsigned_long_val(x: value) -> int:
    """Extract unsigned long from OCaml value."""
    return x >> 1


def unsigned_int_val(x: value) -> int:
    """Extract unsigned int from OCaml value."""
    return unsigned_long_val(x)


# Maximum and minimum values for tagged integers
MAX_LONG = (1 << (8 * WORD_SIZE - 2)) - 1
MIN_LONG = -(1 << (8 * WORD_SIZE - 2))

# Header structure constants
HEADER_BITS = WORD_SIZE * CHAR_BIT
HEADER_TAG_BITS = 8
HEADER_TAG_MASK = (1 << HEADER_TAG_BITS) - 1
HEADER_COLOR_BITS = 2
HEADER_COLOR_SHIFT = HEADER_TAG_BITS
HEADER_COLOR_MASK = ((1 << HEADER_COLOR_BITS) - 1) << HEADER_COLOR_SHIFT

# Assume no reserved bits for simplicity (can be configured)
HEADER_RESERVED_BITS = 0
HEADER_WOSIZE_BITS = (
    HEADER_BITS - HEADER_TAG_BITS - HEADER_COLOR_BITS - HEADER_RESERVED_BITS
)
HEADER_WOSIZE_SHIFT = HEADER_COLOR_SHIFT + HEADER_COLOR_BITS
HEADER_WOSIZE_MASK = ((1 << HEADER_WOSIZE_BITS) - 1) << HEADER_WOSIZE_SHIFT


def tag_hd(hd: header_t) -> tag_t:
    """Extract tag from header."""
    return hd & HEADER_TAG_MASK


def hd_with_tag(hd: header_t, tag: tag_t) -> header_t:
    """Create header with specified tag."""
    return (hd & ~HEADER_TAG_MASK) | tag


def wosize_hd(hd: header_t) -> mlsize_t:
    """Extract word size from header."""
    return (hd & HEADER_WOSIZE_MASK) >> HEADER_WOSIZE_SHIFT


def cleanhd_hd(hd: header_t) -> header_t:
    """Clean header, removing color and reserved bits."""
    return hd & (HEADER_TAG_MASK | HEADER_WOSIZE_MASK)


def color_hd(hd: header_t) -> color_t:
    """Extract color from header."""
    return hd & HEADER_COLOR_MASK


def hd_with_color(hd: header_t, color: color_t) -> header_t:
    """Create header with specified color."""
    return (hd & ~HEADER_COLOR_MASK) | color


# Constants for various limits
NUM_TAGS = 1 << HEADER_TAG_BITS
MAX_WOSIZE = (1 << HEADER_WOSIZE_BITS) - 1


# Size conversion utilities
def whsize_wosize(sz: mlsize_t) -> mlsize_t:
    """Word size including header from word size of data."""
    return sz + 1


def wosize_whsize(sz: mlsize_t) -> mlsize_t:
    """Word size of data from word size including header."""
    return sz - 1


def bsize_wsize(sz: mlsize_t) -> mlsize_t:
    """Byte size from word size."""
    return sz * WORD_SIZE


def wsize_bsize(sz: mlsize_t) -> mlsize_t:
    """Word size from byte size."""
    return sz // WORD_SIZE


def bhsize_wosize(sz: mlsize_t) -> mlsize_t:
    """Byte size including header from word size of data."""
    return bsize_wsize(whsize_wosize(sz))


def bhsize_bosize(sz: mlsize_t) -> mlsize_t:
    """Byte size including header from byte size of data."""
    return sz + WORD_SIZE


# OCaml tag constants
class Tag(enum.IntEnum):
    NO_SCAN_TAG = 251
    FORWARD_TAG = 250
    INFIX_TAG = 249
    OBJECT_TAG = 248
    CLOSURE_TAG = 247
    LAZY_TAG = 246
    CONT_TAG = 245
    FORCING_TAG = 244
    ABSTRACT_TAG = 251
    STRING_TAG = 252
    DOUBLE_TAG = 253
    DOUBLE_ARRAY_TAG = 254
    CUSTOM_TAG = 255


# Special value constructors
def val_bool(x: bool) -> value:
    """Convert boolean to OCaml value."""
    return val_int(1 if x else 0)


def bool_val(x: value) -> bool:
    """Extract boolean from OCaml value."""
    return int_val(x) != 0


VAL_FALSE = val_int(0)
VAL_TRUE = val_int(1)


def val_not(x: value) -> value:
    """Logical NOT for OCaml boolean values."""
    return VAL_FALSE + VAL_TRUE - x


# Unit value
VAL_UNIT = val_int(0)

# List constructors
VAL_EMPTYLIST = val_int(0)
TAG_CONS = 0

# Option constructors
VAL_NONE = val_int(0)
TAG_SOME = 0


def is_none(v: value) -> bool:
    """Check if value is None option."""
    return v == VAL_NONE


def is_some(v: value) -> bool:
    """Check if value is Some option."""
    return is_block(v)


# Double word size for floating point
DOUBLE_WOSIZE = 8 // WORD_SIZE if WORD_SIZE == 4 else 1


class OCamlValueReader:
    """
    Utility class for reading OCaml values from binary data.
    """

    def __init__(self, data: bytes, offset: int = 0, big_endian: bool = False):
        self.data = data
        self.offset = offset
        self.big_endian = big_endian
        self.endian_prefix = ">" if big_endian else "<"

    def read_header(self) -> header_t:
        """Read a header at current offset."""
        fmt = f"{self.endian_prefix}{'Q' if WORD_SIZE == 8 else 'L'}"
        header = struct.unpack_from(fmt, self.data, self.offset)[0]
        self.offset += WORD_SIZE
        return header

    def read_value(self) -> value:
        """Read a value at current offset."""
        return self.read_header()  # Values and headers have same size

    def read_int32(self) -> int:
        """Read a 32-bit integer."""
        fmt = f"{self.endian_prefix}l"
        result = struct.unpack_from(fmt, self.data, self.offset)[0]
        self.offset += 4
        return result

    def read_int64(self) -> int:
        """Read a 64-bit integer."""
        fmt = f"{self.endian_prefix}q"
        result = struct.unpack_from(fmt, self.data, self.offset)[0]
        self.offset += 8
        return result

    def read_double(self) -> float:
        """Read a double-precision float."""
        fmt = f"{self.endian_prefix}d"
        result = struct.unpack_from(fmt, self.data, self.offset)[0]
        self.offset += 8
        return result

    def read_string(self, length: int) -> bytes:
        """Read a string of specified length."""
        result = self.data[self.offset : self.offset + length]
        self.offset += length
        return result

    def peek_header(self) -> header_t:
        """Peek at header without advancing offset."""
        fmt = f"{self.endian_prefix}{'Q' if WORD_SIZE == 8 else 'L'}"
        return struct.unpack_from(fmt, self.data, self.offset)[0]


class OCamlValue:
    """
    Represents an OCaml value with its type information and data.
    """

    def __init__(self, raw_value: value, tag: Optional[tag_t] = None):
        self.raw_value = raw_value
        self._tag = tag

    @property
    def is_long(self) -> bool:
        return is_long(self.raw_value)

    @property
    def is_block(self) -> bool:
        return is_block(self.raw_value)

    @property
    def as_long(self) -> int:
        """Get value as long integer (if it's a long)."""
        if not self.is_long:
            raise ValueError("Value is not a long")
        return long_val(self.raw_value)

    @property
    def as_int(self) -> int:
        """Get value as integer (if it's a long)."""
        return self.as_long

    @property
    def as_bool(self) -> bool:
        """Get value as boolean (if it's a long)."""
        return bool_val(self.raw_value)

    @property
    def tag(self) -> Optional[tag_t]:
        """Get the tag of this value (if it's a block)."""
        return self._tag

    def __repr__(self) -> str:
        if self.is_long:
            return f"OCamlValue(long={self.as_long})"
        else:
            return f"OCamlValue(block, tag={self.tag}, raw=0x{self.raw_value:x})"


# Exception handling for obsolete encoded exceptions
def make_exception_result(v: value) -> value:
    """Create an encoded exception result (obsolete method)."""
    return v | 2


def is_exception_result(v: value) -> bool:
    """Check if value is an encoded exception (obsolete method)."""
    return (v & 3) == 2


def extract_exception(v: value) -> value:
    """Extract exception from encoded result (obsolete method)."""
    return v & ~3


# Utility functions for creating out-of-heap headers
def caml_out_of_heap_header(wosize: mlsize_t, tag: tag_t) -> header_t:
    """Create header for out-of-heap blocks."""
    return caml_out_of_heap_header_with_reserved(wosize, tag, 0)


def caml_out_of_heap_header_with_reserved(
    wosize: mlsize_t, tag: tag_t, reserved: int
) -> header_t:
    """Create header for out-of-heap blocks with reserved bits."""
    # Simplified version without reserved bits handling
    return (
        (wosize << HEADER_WOSIZE_SHIFT)
        | (3 << HEADER_COLOR_SHIFT)  # NOT_MARKABLE color
        | tag
    )


if __name__ == "__main__":
    # Simple test/demo
    print("OCaml MLValues Python Library")
    print(f"Word size: {WORD_SIZE} bytes")
    print(f"Max tagged long: {MAX_LONG}")
    print(f"Min tagged long: {MIN_LONG}")

    # Test some conversions
    test_int = 42
    test_val = val_int(test_int)
    print(f"val_int({test_int}) = {test_val}")
    print(f"int_val({test_val}) = {int_val(test_val)}")

    # Test boolean
    test_bool = True
    test_bool_val = val_bool(test_bool)
    print(f"val_bool({test_bool}) = {test_bool_val}")
    print(f"bool_val({test_bool_val}) = {bool_val(test_bool_val)}")
