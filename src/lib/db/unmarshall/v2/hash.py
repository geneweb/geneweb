"""
Python port of OCaml's generic hashing primitive.
Based on MurmurHash 3 implementation from OCaml runtime.
"""

import struct
from typing import Any
from collections import deque

# Constants
HASH_QUEUE_SIZE = 256
MAX_FORWARD_DEREFERENCE = 1000


def rotl32(x: int, n: int) -> int:
    """32-bit left rotation"""
    x = x & 0xFFFFFFFF
    return ((x << n) | (x >> (32 - n))) & 0xFFFFFFFF


def mix(h: int, d: int) -> int:
    """MurmurHash3 mix operation"""
    d = (d * 0xCC9E2D51) & 0xFFFFFFFF
    d = rotl32(d, 15)
    d = (d * 0x1B873593) & 0xFFFFFFFF
    h = (h ^ d) & 0xFFFFFFFF
    h = rotl32(h, 13)
    h = (h * 5 + 0xE6546B64) & 0xFFFFFFFF
    return h


def final_mix(h: int) -> int:
    """MurmurHash3 final mix"""
    h = (h ^ (h >> 16)) & 0xFFFFFFFF
    h = (h * 0x85EBCA6B) & 0xFFFFFFFF
    h = (h ^ (h >> 13)) & 0xFFFFFFFF
    h = (h * 0xC2B2AE35) & 0xFFFFFFFF
    h = (h ^ (h >> 16)) & 0xFFFFFFFF
    return h


def caml_hash_mix_uint32(h: int, d: int) -> int:
    """Mix a 32-bit unsigned integer"""
    return mix(h, d & 0xFFFFFFFF)


def caml_hash_mix_intnat(h: int, d: int) -> int:
    """Mix a platform-native integer"""
    if struct.calcsize("P") == 8:  # 64-bit
        # Mix high and low 32 bits for 32/64 compatibility
        n = ((d >> 32) ^ (d >> 63) ^ d) & 0xFFFFFFFF
    else:  # 32-bit
        n = d & 0xFFFFFFFF
    return mix(h, n)


def caml_hash_mix_int64(h: int, d: int) -> int:
    """Mix a 64-bit integer"""
    hi = (d >> 32) & 0xFFFFFFFF
    lo = d & 0xFFFFFFFF
    h = mix(h, lo)
    h = mix(h, hi)
    return h


def caml_hash_mix_double(h: int, d: float) -> int:
    """Mix a double-precision float"""
    # Convert to bytes and then to two 32-bit integers
    bytes_repr = struct.pack("d", d)

    # Handle endianness
    if struct.pack("=I", 1) == struct.pack("<I", 1):  # Little endian
        lo, hi = struct.unpack("<II", bytes_repr)
    else:  # Big endian
        hi, lo = struct.unpack(">II", bytes_repr)

    # Normalize NaNs
    if (hi & 0x7FF00000) == 0x7FF00000 and (lo | (hi & 0xFFFFF)) != 0:
        hi = 0x7FF00000
        lo = 0x00000001
    # Normalize -0 to +0
    elif hi == 0x80000000 and lo == 0:
        hi = 0

    h = mix(h, lo)
    h = mix(h, hi)
    return h


def caml_hash_mix_float(h: int, d: float) -> int:
    """Mix a single-precision float"""
    # Convert to 32-bit representation
    bytes_repr = struct.pack("f", d)
    n = struct.unpack("I", bytes_repr)[0]

    # Normalize NaNs
    if (n & 0x7F800000) == 0x7F800000 and (n & 0x007FFFFF) != 0:
        n = 0x7F800001
    # Normalize -0 to +0
    elif n == 0x80000000:
        n = 0

    return mix(h, n)


def caml_hash_mix_string(h: int, s: Any) -> int:
    """Mix an OCaml string"""
    # Dummy implementation - replace with actual string handling
    if isinstance(s, str):
        s_bytes = s.encode("utf-8")
    else:
        # Assume it's already bytes or OCaml string value
        s_bytes = bytes(s) if hasattr(s, "__iter__") else str(s).encode("utf-8")

    length = len(s_bytes)

    # Mix by 32-bit blocks (little-endian)
    i = 0
    while i + 4 <= length:
        w = struct.unpack("<I", s_bytes[i : i + 4])[0]
        h = mix(h, w)
        i += 4

    # Handle remaining bytes
    w = 0
    remaining = length & 3
    if remaining >= 3:
        w |= s_bytes[i + 2] << 16
    if remaining >= 2:
        w |= s_bytes[i + 1] << 8
    if remaining >= 1:
        w |= s_bytes[i]
    if remaining > 0:
        h = mix(h, w)

    # Mix in the length
    h = (h ^ length) & 0xFFFFFFFF
    return h


def caml_hash(count: int, limit: int, seed: int, obj: Any) -> int:
    """
    Generic OCaml hash function.

    Args:
        count: Maximum number of meaningful values to examine
        limit: Maximum size of traversal queue
        seed: Initial hash seed
        obj: Object to hash

    Returns:
        Hash value as OCaml integer
    """
    # Initialize queue for breadth-first traversal
    queue = deque([obj])
    sz = min(max(0, limit), HASH_QUEUE_SIZE)
    num = count
    h = seed & 0xFFFFFFFF

    while queue and num > 0:
        v = queue.popleft()

        # Handle the value based on its type
        if isinstance(v, int):
            h = caml_hash_mix_intnat(h, v)
            num -= 1
        else:
            # tag = Tag_val(v)

            if isinstance(v, str):
                h = caml_hash_mix_string(h, v)
                num -= 1

            elif isinstance(v, float):
                h = caml_hash_mix_double(h, v)
                num -= 1

            # elif tag == Double_array_tag:
            #     wosize = Wosize_val(v)
            #     array_len = wosize // Double_wosize
            #     for i in range(array_len):
            #         if num <= 0:
            #             break
            #         h = caml_hash_mix_double(h, Double_flat_field(v, i))
            #         num -= 1

            # elif tag == Abstract_tag:
            #     # Block contents unknown, do nothing
            #     pass

            # elif tag == Infix_tag:
            #     # Mix in offset and continue with adjusted value
            #     h = caml_hash_mix_uint32(h, Infix_offset_val(v))
            #     # Simulate v = v - Infix_offset_val(v) and goto again
            #     adjusted_v = v  # Dummy implementation
            #     queue.appendleft(adjusted_v)

            # elif tag == Forward_tag:
            #     # Follow forward links with limit
            #     current_v = v
            #     for _ in range(MAX_FORWARD_DEREFERENCE):
            #         current_v = Forward_val(current_v)
            #         if isinstance(current_v, int) or Tag_val(current_v) != Forward_tag:
            #             queue.appendleft(current_v)
            #             break

            # elif tag == Object_tag:
            #     h = caml_hash_mix_intnat(h, Oid_val(v))
            #     num -= 1

            # elif tag == Custom_tag:
            #     # Use custom hash function if available
            #     # Dummy implementation - assume no custom hash
            #     pass

            # elif tag == Closure_tag:
            #     wosize = Wosize_val(v)
            #     startenv = Start_env_closinfo(Closinfo_val(v))

            #     # Mix in tag and size
            #     h = caml_hash_mix_uint32(h, Cleanhd_hd(Hd_val(v)))

            #     # Mix code pointers and closure info
            #     for i in range(min(startenv, wosize)):
            #         if num <= 0:
            #             break
            #         h = caml_hash_mix_intnat(h, Field(v, i))
            #         num -= 1

            #     # Add environment fields to queue
            #     for i in range(startenv, wosize):
            #         if len(queue) >= sz:
            #             break
            #         queue.append(Field(v, i))

            # elif tag == Cont_tag:
            #     # All continuations hash to same value
            #     pass

            else:
                raise TypeError("Unsupported type for hashing")
                # Default case: mix tag/size and add fields to queue
                # h = caml_hash_mix_uint32(h, cleanhd_hd(v))
                # wosize = Wosize(v)
                # for i in range(wosize):
                #     if len(queue) >= sz:
                #         break
                #     queue.append(v[i])

    # Final mixing
    h = final_mix(h)

    # Fold to range [0, 2^30-1] for OCaml integer compatibility
    return h & 0x3FFFFFFF


def caml_string_hash(seed: int, string: Any) -> int:
    """Hash a string with given seed"""
    h = int(seed)
    h = caml_hash_mix_string(h, string)
    h = final_mix(h)
    return h & 0x3FFFFFFF


def caml_hash_variant(tag: str) -> int:
    """Hash a variant tag string"""
    accu = 0  # Val_int(0)

    for char in tag:
        accu = (223 * accu + ord(char)) & 0xFFFFFFFF

    if struct.calcsize("P") == 8:  # 64-bit
        accu = accu & 0x7FFFFFFF

    # Force sign extension of bit 31 for 32/64 compatibility
    return struct.unpack("i", struct.pack("I", accu))[0]


# Dummy implementations for missing functions - implement these later
def custom_ops_val(v: Any) -> Any:
    """Dummy: Get custom operations for value"""
    return None


def hash(x):
    """Hash function for general use, mimicking OCaml's behavior."""
    return caml_hash(10, 100, 0, x)


def hash_param(n1, n2, x):
    """Hash function with parameters, mimicking OCaml's behavior."""
    return caml_hash(n1, n2, 0, x)


def seeded_hash(seed, x):
    """Hash function with a seed, mimicking OCaml's behavior."""
    return caml_hash(10, 100, seed, x)


# Export main functions
__all__ = [
    "hash",
    "hash_param",
    "seeded_hash",
    "caml_hash",
    "caml_string_hash",
    "caml_hash_variant",
    "caml_hash_mix_uint32",
    "caml_hash_mix_intnat",
    "caml_hash_mix_int64",
    "caml_hash_mix_double",
    "caml_hash_mix_float",
    "caml_hash_mix_string",
]
