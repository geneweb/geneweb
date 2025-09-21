"""
Python port of OCaml's mlvalues.h header.
Defines the core value representations and tag types for OCaml marshalled values.
"""

import struct
from dataclasses import dataclass
from typing import Optional, List
from enum import IntEnum

# Basic type definitions
Value = int  # OCaml value representation
HeaderT = int  # Block header type
Tag = int  # Block tag type


# /* Structure of the header:

# For 16-bit and 32-bit architectures:
#      +--------+-------+-----+
#      | wosize | color | tag |
#      +--------+-------+-----+
# bits  31    10 9     8 7   0

# For 64-bit architectures:

#      +----------+--------+-------+-----+
#      | reserved | wosize | color | tag |
#      +----------+--------+-------+-----+
# bits  63    64-R 63-R  10 9     8 7   0

# where 0 <= R <= 31 is HEADER_RESERVED_BITS, set with the
# --enable-reserved-header-bits=R argument to configure.

# */

# Header bit definitions
HEADER_TAG_BITS = 8
HEADER_TAG_MASK = (1 << HEADER_TAG_BITS) - 1

HEADER_COLOR_BITS = 2
HEADER_COLOR_SHIFT = HEADER_TAG_BITS
HEADER_COLOR_MASK = ((1 << HEADER_COLOR_BITS) - 1) << HEADER_COLOR_SHIFT

HEADER_RESERVED_BITS = 0  # Configurable in OCaml, we use 0 for simplicity

HEADER_WOSIZE_BITS = (
    (64 if struct.calcsize("P") == 8 else 32)
    - HEADER_TAG_BITS
    - HEADER_COLOR_BITS
    - HEADER_RESERVED_BITS
)
HEADER_WOSIZE_SHIFT = HEADER_COLOR_SHIFT + HEADER_COLOR_BITS
HEADER_WOSIZE_MASK = ((1 << HEADER_WOSIZE_BITS) - 1) << HEADER_WOSIZE_SHIFT


# Utility functions for working with headers
def tag_hd(hd: HeaderT) -> Tag:
    """Get the tag from a block header"""
    return hd & HEADER_TAG_MASK


def hd_with_tag(hd: HeaderT, tag: Tag) -> HeaderT:
    """Set the tag in a block header"""
    return (hd & ~HEADER_TAG_MASK) | tag


def wosize_hd(hd: HeaderT) -> int:
    """Get the word size from a block header"""
    return (hd & HEADER_WOSIZE_MASK) >> HEADER_WOSIZE_SHIFT


def cleanhd_hd(hd: HeaderT) -> HeaderT:
    """Get a clean header without reserved or color bits"""
    return hd & (HEADER_TAG_MASK | HEADER_WOSIZE_MASK)


# Long vs block detection
def is_long(x: Value) -> bool:
    """Test if a value is a long integer"""
    return bool(x & 1)


def is_block(x: Value) -> bool:
    """Test if a value is a block"""
    return not bool(x & 1)


# Conversion between Python and OCaml values
def val_long(x: int) -> Value:
    """Convert a Python int to an OCaml long value"""
    return (x << 1) + 1


def long_val(x: Value) -> int:
    """Convert an OCaml long value to a Python int"""
    return x >> 1


MAX_LONG = (1 << (64 if struct.calcsize("P") == 8 else 32) - 2) - 1
MIN_LONG = -(1 << (64 if struct.calcsize("P") == 8 else 32) - 2)


def val_int(x: int) -> Value:
    """Convert a Python int to an OCaml int value"""
    return val_long(x)


def int_val(x: Value) -> int:
    """Convert an OCaml int value to a Python int"""
    return long_val(x)


def unsigned_long_val(x: Value) -> int:
    """Convert an OCaml value to an unsigned Python int"""
    return x >> 1


def unsigned_int_val(x: Value) -> int:
    """Convert an OCaml value to an unsigned Python int"""
    return unsigned_long_val(x)


# Special tags for block values
class Tag(IntEnum):
    """OCaml block tags"""

    FORWARD_TAG = 250  # Forwarding pointer
    INFIX_TAG = 249  # Infix header inside closure
    OBJECT_TAG = 248  # Object
    CLOSURE_TAG = 247  # Function closure
    LAZY_TAG = 246  # Lazy value
    CONT_TAG = 245  # Continuation
    FORCING_TAG = 244  # Forcing tag for lazy values
    ABSTRACT_TAG = 251  # Abstract value
    STRING_TAG = 252  # String
    DOUBLE_TAG = 253  # Float
    DOUBLE_ARRAY_TAG = 254  # Float array
    CUSTOM_TAG = 255  # Custom block
    NO_SCAN_TAG = 251  # First tag for blocks containing no values
    CONS_TAG = 0  # List cons cell tag


# Size calculations
def whsize_wosize(sz: int) -> int:
    """Convert word size to word header size"""
    return sz + 1


def wosize_whsize(sz: int) -> int:
    """Convert word header size to word size"""
    return sz - 1


def wosize_bhsize(sz: int) -> int:
    """Convert byte header size to word size"""
    return sz // struct.calcsize("P") - 1


def bsize_wsize(sz: int) -> int:
    """Convert word size to byte size"""
    return sz * struct.calcsize("P")


def wsize_bsize(sz: int) -> int:
    """Convert byte size to word size"""
    return sz // struct.calcsize("P")


def bhsize_wosize(sz: int) -> int:
    """Convert word size to byte header size"""
    return bsize_wsize(whsize_wosize(sz))


# Common value constructors
VAL_FALSE = val_int(0)  # OCaml false
VAL_TRUE = val_int(1)  # OCaml true
VAL_NONE = val_int(0)  # OCaml None
VAL_UNIT = val_int(0)  # OCaml unit value
VAL_EMPTY_LIST = val_int(0)  # OCaml empty list


@dataclass
class Block:
    """Represents an OCaml heap block"""

    tag: Tag
    size: int  # Word size
    fields: List[Value]

    def __init__(self, tag: Tag, size: int, fields: Optional[List[Value]] = None):
        self.tag = tag
        self.size = size
        self.fields = fields if fields is not None else []

    @property
    def header(self) -> HeaderT:
        """Get the block header"""
        return (self.size << HEADER_WOSIZE_SHIFT) | self.tag


def make_header(wosize: int, tag: Tag, reserved: int = 0) -> HeaderT:
    """Create a block header"""
    return (
        (
            (reserved & ((1 << HEADER_RESERVED_BITS) - 1))
            << (64 if struct.calcsize("P") == 8 else 32) - HEADER_RESERVED_BITS
        )
        | ((wosize & ((1 << HEADER_WOSIZE_BITS) - 1)) << HEADER_WOSIZE_SHIFT)
        | (3 << HEADER_COLOR_SHIFT)  # Not markable
        | tag
    )
