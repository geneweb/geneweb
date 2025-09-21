import sys
from typing import Any

MASK_32_BIT = 0xFFFFFFFF


# define ROTL32(x,n) ((x) << n | (x) >> (32-n))
def ROTL32(x: int, n: int) -> int:
    """Rotate left a 32-bit integer x by n bits."""
    return ((x << n) | (x >> (32 - n))) & MASK_32_BIT


def FINAL_MIX(h: int) -> int:
    h ^= h >> 16
    h *= 0x85EBCA6B
    h ^= h >> 13
    h *= 0xC2B2AE35
    h ^= h >> 16
    return h & MASK_32_BIT


# #define MIX(h,d) \
#   d *= 0xcc9e2d51; \
#   d = ROTL32(d, 15); \
#   d *= 0x1b873593; \
#   h ^= d; \
#   h = ROTL32(h, 13); \
#   h = h * 5 + 0xe6546b64;
def MIX(h: int, d: int) -> int:
    """
    Mix function used in hash computation.

    Args:
        h: Current hash value.
        d: Data to mix into the hash.

    Returns:
        Updated hash value.
    """
    d *= 0xCC9E2D51
    d = ROTL32(d, 15)
    d *= 0x1B873593
    h ^= d
    h = ROTL32(h, 13)
    h = (h * 5 + 0xE6546B64) & MASK_32_BIT
    return h


# CAMLexport uint32_t caml_hash_mix_intnat(uint32_t h, intnat d)
# {
#   uint32_t n;
# #ifdef ARCH_SIXTYFOUR
#   /* Mix the low 32 bits and the high 32 bits, in a way that preserves
#      32/64 compatibility: we want n = (uint32_t) d
#      if d is in the range [-2^31, 2^31-1]. */
#   n = (d >> 32) ^ (d >> 63) ^ d;
#   /* If 0 <= d < 2^31:   d >> 32 = 0     d >> 63 = 0
#      If -2^31 <= d < 0:  d >> 32 = -1    d >> 63 = -1
#      In both cases, n = (uint32_t) d.  */
# #else
#   n = d;
# #endif
#   MIX(h, n);
#   return h;
# }
def caml_hash_mix_intnat(h: int, d: int) -> int:
    """
    Mix an integer into the hash value.

    Args:
        h: Current hash value.
        d: Integer to mix into the hash.

    Returns:
        Updated hash value.
    """
    n: int
    if sys.maxsize == 2**63 - 1:  # Check for 64-bit architecture
        # Mix the low 32 bits and the high 32 bits, in a way that preserves
        # 32/64 compatibility: we want n = (uint32_t) d
        # if d is in the range [-2^31, 2^31-1].
        n = (d >> 32) ^ (d >> 63) ^ d
        # If 0 <= d < 2^31:   d >> 32 = 0     d >> 63 = 0
        # If -2^31 <= d < 0:  d >> 32 = -1    d >> 63 = -1
        # In both cases, n = (uint32_t) d.
    else:
        n = d
    h = MIX(h, n)
    return h & 0xFFFFFFFF  # Ensure it stays within 32 bits


# CAMLexport uint32_t caml_hash_mix_string(uint32_t h, value s)
# {
#   mlsize_t len = caml_string_length(s);
#   mlsize_t i;
#   uint32_t w;


#   /* Mix by 32-bit blocks (little-endian) */
#   for (i = 0; i + 4 <= len; i += 4) {
# #ifdef ARCH_BIG_ENDIAN
#     w = Byte_u(s, i)
#         | (Byte_u(s, i+1) << 8)
#         | (Byte_u(s, i+2) << 16)
#         | (Byte_u(s, i+3) << 24);
# #else
#     w = *((uint32_t *) &Byte_u(s, i));
# #endif
#     MIX(h, w);
#   }
#   /* Finish with up to 3 bytes */
#   w = 0;
#   switch (len & 3) {
#   case 3: w  = Byte_u(s, i+2) << 16; fallthrough;
#   case 2: w |= Byte_u(s, i+1) << 8;  fallthrough;
#   case 1: w |= Byte_u(s, i);
#           MIX(h, w);                 fallthrough;
#   default: /*skip*/;     /* len & 3 == 0, no extra bytes, do nothing */
#   }
#   /* Finally, mix in the length.  Ignore the upper 32 bits, generally 0. */
#   h ^= (uint32_t) len;
#   return h;
# }
def caml_hash_mix_string(h: int, s: str) -> int:
    """
    Mix a string into the hash value.

    Args:
        h: Current hash value.
        s: String to mix into the hash.

    Returns:
        Updated hash value.
    """
    length = len(s)
    i = 0
    w = 0

    # Mix by 32-bit blocks (little-endian)
    while i + 4 <= length:
        if sys.byteorder == "big":
            w = (
                ord(s[i])
                | (ord(s[i + 1]) << 8)
                | (ord(s[i + 2]) << 16)
                | (ord(s[i + 3]) << 24)
            )
        else:
            w = int.from_bytes(s[i : i + 4], byteorder="little", signed=False)
        h = MIX(h, w)
        i += 4

    # Finish with up to 3 bytes
    w = 0
    if length & 3 == 3:
        w = ord(s[i + 2]) << 16
    if length & 3 >= 2:
        w |= ord(s[i + 1]) << 8
    if length & 3 >= 1:
        w |= ord(s[i])
        h = MIX(h, w)

    # Finally, mix in the length. Ignore the upper 32 bits, generally 0.
    h ^= length
    return h & MASK_32_BIT  # Ensure it stays within 32 bits


# /* Mix a double-precision float.
#    Treats +0.0 and -0.0 identically.
#    Treats all NaNs identically.
# */


# CAMLexport uint32_t caml_hash_mix_double(uint32_t hash, double d)
# {
#   union {
#     double d;
# #if defined(ARCH_BIG_ENDIAN) || (defined(__arm__) && !defined(__ARM_EABI__))
#     struct { uint32_t h; uint32_t l; } i;
# #else
#     struct { uint32_t l; uint32_t h; } i;
# #endif
#   } u;
#   uint32_t h, l;
#   /* Convert to two 32-bit halves */
#   u.d = d;
#   h = u.i.h; l = u.i.l;
#   /* Normalize NaNs */
#   if ((h & 0x7FF00000) == 0x7FF00000 && (l | (h & 0xFFFFF)) != 0) {
#     h = 0x7FF00000;
#     l = 0x00000001;
#   }
#   /* Normalize -0 into +0 */
#   else if (h == 0x80000000 && l == 0) {
#     h = 0;
#   }
#   MIX(hash, l);
#   MIX(hash, h);
#   return hash;
# }
def caml_hash_mix_double(hash: int, d: float) -> int:
    """
    Mix a double-precision float into the hash value.

    Args:
        hash: Current hash value.
        d: Double-precision float to mix into the hash.

    Returns:
        Updated hash value.
    """
    import struct

    # Convert to two 32-bit halves
    packed = struct.pack(">d", d)  # Big-endian double
    if sys.byteorder == "big":
        h, l = struct.unpack(">II", packed)  # Unpack into two unsigned ints
    else:
        l, h = struct.unpack("<II", packed)  # Unpack into two unsigned ints

    # Normalize NaNs
    if (h & 0x7FF00000) == 0x7FF00000 and (l | (h & 0xFFFFF)) != 0:
        h = 0x7FF00000
        l = 0x00000001
    # Normalize -0 into +0
    elif h == 0x80000000 and l == 0:
        h = 0

    hash = MIX(hash, l)
    hash = MIX(hash, h)
    return hash & MASK_32_BIT  # Ensure it stays within 32 bits


HASH_QUEUE_SIZE = 256
MAX_FORWARD_DEREFERENCE = 1000


def caml_hash(count: int, limit: int, seed: int, obj: Any) -> int:
    """
    Compute a hash value similar to OCaml's `caml_hash`.

    Args:
        count: Number of characters to consider from the string.
        limit: Maximum hash value (table size).
        seed: Initial seed value for the hash computation.
        obj: The object to hash (expected to be a string).

    Returns:
        An integer hash value in the range [0, limit).
    """

    sz: int = HASH_QUEUE_SIZE if limit < 0 or limit > HASH_QUEUE_SIZE else limit
    num: int = count
    h: int = seed
    queue = [obj]
    rd: int = 0
    wr: int = 1
    v: Any = None
    length: int = 0

    while rd < wr and num > 0:
        v = queue[rd]
        rd += 1

        # again jumppoint

        if isinstance(v, int):
            h = caml_hash_mix_intnat(h, v)
            num -= 1
        elif isinstance(v, str):
            h = caml_hash_mix_string(h, v)
            num -= 1
        elif isinstance(v, float):
            h = caml_hash_mix_double(h, v)
            num -= 1
        else:
            raise TypeError("Unsupported type for hashing")
    h = FINAL_MIX(h)
    return h & 0x3FFFFFFF


# CAMLprim value caml_string_hash(value seed, value string)
# {
#   uint32_t h;
#   h = Int_val(seed);
#   h = caml_hash_mix_string (h, string);
#   FINAL_MIX(h);
#   return Val_int(h & 0x3FFFFFFFU);
# }
def caml_string_hash(seed: int, string: str) -> int:
    """
    Compute a hash value for a string similar to OCaml's `caml_string_hash`.

    Args:
        seed: Initial seed value for the hash computation.
        string: The string to hash.

    Returns:
        An integer hash value in the range [0, 0x3FFFFFFF).
    """
    h = seed
    h = caml_hash_mix_string(h, string)
    h = FINAL_MIX(h)
    return h & 0x3FFFFFFF


def hash(x):
    """Hash function for general use, mimicking OCaml's behavior."""
    return caml_hash(10, 100, 0, x)


def hash_param(n1, n2, x):
    """Hash function with parameters, mimicking OCaml's behavior."""
    return caml_hash(n1, n2, 0, x)


def seeded_hash(seed, x):
    """Hash function with a seed, mimicking OCaml's behavior."""
    return caml_hash(10, 100, seed, x)
