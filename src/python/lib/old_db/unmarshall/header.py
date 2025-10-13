import struct
from dataclasses import dataclass
from typing import BinaryIO, Callable, List, Tuple

from lib.old_db.unmarshall.basic import read8u, read32u, read64u
from lib.old_db.unmarshall.v2 import intext as Intext

from .v2.ocaml_input import OCamlInput


@dataclass
class MarshalHeader:
    """Represents the header of a marshalled OCaml data structure.

    Header format for the "small" model: 20 bytes
    0   "small" magic number
    4   length of marshaled data, in bytes
    8   number of shared blocks
    12  size in words when read on a 32-bit platform
    16  size in words when read on a 64-bit platform
    The 4 numbers are 32 bits each, in big endian.

    Header format for the "big" model: 32 bytes
    0   "big" magic number
    4   four reserved bytes, currently set to 0
    8   length of marshaled data, in bytes
    16  number of shared blocks
    24  size in words when read on a 64-bit platform
    The 3 numbers are 64 bits each, in big endian.

    Header format for the "compressed" model: 10 to 55 bytes
    0   "compressed" magic number
    4   low 6 bits: total size of the header
        high 2 bits: reserved, currently 0
    5 and following
        5 variable-length integers, in VLQ format (1 to 10 bytes each)
        - length of compressed marshaled data, in bytes
        - length of uncompressed marshaled data, in bytes
        - number of shared blocks
        - size in words when read on a 32-bit platform
        - size in words when read on a 64-bit platform

    VLQ format is one or several bytes like 1xxxxxxx 1yyyyyyy 0zzzzzzz.
    First bytes have top bit 1, last byte has top bit 0.
    Each byte carries 7 bits of the number.
    Bytes come in big-endian order: xxxxxxx are the 7 high-order bits,
    zzzzzzzz the 7 low-order bits.
    """

    magic: int = 0
    header_len: int = 0
    data_len: int = 0
    uncompressed_data_len: int = 0
    num_objects: int = 0
    whsize: int = 0
    compressed: int = 0


# static int readvlq(struct caml_intern_state* s, /*out*/ uintnat * res)
# {
#   unsigned char c = read8u(s);
#   uintnat n = c & 0x7F;
#   int retcode = 0;
#   while ((c & 0x80) != 0) {
#     c = read8u(s);
#     uintnat n7 = n << 7;
#     if (n != n7 >> 7) retcode = -1;
#     n = n7 | (c & 0x7F);
#   }
#   if (res) *res = n;
#   return retcode;
# }
def readvlq(oi: OCamlInput, res: Callable[[int], None]) -> int:
    """Read a variable-length quantity (VLQ) from the data.

    Args:
        oi (OCamlInput): The OCamlInput object to read from
        res (Callable[[int], None]): A callback to store the result

    Raises:
        ValueError: If the VLQ is too large to be represented

    Returns:
        Tuple[int, int]: The decoded VLQ and the new offset
    """
    c = oi.read_uint8()
    n = c & 0x7F
    retcode = 0
    while (c & 0x80) != 0:
        c = oi.read_uint8()
        n7 = n << 7
        if n != n7 >> 7:
            retcode = -1
        n = n7 | (c & 0x7F)
    if retcode == -1:
        raise ValueError("VLQ overflow")
    if res is not None:
        res(n)
    return retcode


def caml_parse_header_f(f: BinaryIO) -> Tuple["MarshalHeader", int]:
    """Parses the header of a marshalled OCaml data structure.

    Raises:
        ValueError: If the magic number is unrecognized.
        ValueError: If the object is too large to be read back on this platform.

    Returns:
        Tuple[MarshalHeader, int]: The parsed header and the new offset.
    """
    oi = OCamlInput(f)
    h = MarshalHeader()
    # h.magic, offset = read32u(data, offset)
    h.magic = oi.read_uint32()
    match Intext.MagicNumbers(h.magic):
        case Intext.MagicNumbers.SMALL:
            h.header_len = 20
            h.compressed = 0
            h.data_len = oi.read_int32()
            h.uncompressed_data_len = h.data_len
            h.num_objects = oi.read_int32()

            ## vv for 64-bit systems vv
            _ = oi.read_int32()
            h.whsize = oi.read_int32()
            ## ^^ for 64-bit systems ^^
            ##
            ## vv for 32-bit systems vv
            # h.whsize, offset = read32u(data, offset)
            # _, offset = read32u(data, offset)
            ## ^^ for 32-bit systems ^^

        case Intext.MagicNumbers.BIG:
            # Only for 64-bit systems
            h.header_len = 32
            h.compressed = 0
            _ = oi.read_int32()  # Skip the next value
            h.data_len = oi.read_int64()
            h.uncompressed_data_len = h.data_len
            h.num_objects = oi.read_int64()
            h.whsize = oi.read_int64()
        case Intext.MagicNumbers.COMPRESSED:
            h.header_len = oi.read_int8()
            h.header_len &= 0x3F
            h.compressed = 1
            overflow = 0
            overflow |= readvlq(oi, lambda x: setattr(h, "data_len", x))
            overflow |= readvlq(oi, lambda x: setattr(h, "uncompressed_data_len", x))
            overflow |= readvlq(oi, lambda x: setattr(h, "num_objects", x))
            readvlq(oi, None)  # Skip for 64-bit systems
            overflow |= readvlq(oi, lambda x: setattr(h, "whsize", x))
            # readvlq(oi, None) # Skip for 32-bit systems
            if overflow:
                raise ValueError("Object too large to be read back on this platform")
        case _:
            raise ValueError("Bad object")

    return h


def caml_parse_header(data: bytes, offset: int) -> Tuple["MarshalHeader", int]:
    """Parses the header of a marshalled OCaml data structure.

    Raises:
        ValueError: If the magic number is unrecognized.
        ValueError: If the object is too large to be read back on this platform.

    Returns:
        Tuple[MarshalHeader, int]: The parsed header and the new offset.
    """
    h = MarshalHeader()
    h.magic, offset = read32u(data, offset)

    if h.magic == Intext.MagicNumbers.SMALL:
        print("Small magic number detected")
        h.header_len = 20
        h.compressed = 0
        h.data_len, offset = read32u(data, offset)
        h.uncompressed_data_len = h.data_len
        h.num_objects, offset = read32u(data, offset)

        ## vv for 64-bit systems vv
        _, offset = read32u(data, offset)
        h.whsize, offset = read32u(data, offset)
        ## ^^ for 64-bit systems ^^
        ##
        ## vv for 32-bit systems vv
        # h.whsize, offset = read32u(data, offset)
        # _, offset = read32u(data, offset)
        ## ^^ for 32-bit systems ^^

    elif h.magic == Intext.MagicNumbers.BIG:
        # Only for 64-bit systems
        h.header_len = 32
        h.compressed = 0
        _, offset = read32u(data, offset)  # Skip the next value
        h.data_len, offset = read64u(data, offset)
        h.uncompressed_data_len = h.data_len
        h.num_objects, offset = read64u(data, offset)
        h.whsize, offset = read64u(data, offset)
    elif h.magic == Intext.MagicNumbers.COMPRESSED:
        h.header_len, offset = read8u(data, offset)
        h.header_len &= 0x3F
        h.compressed = 1
        overflow = 0
        loffset = [offset]
        overflow |= readvlq(data, loffset, lambda x: setattr(h, "data_len", x))
        overflow |= readvlq(
            data, loffset, lambda x: setattr(h, "uncompressed_data_len", x)
        )
        overflow |= readvlq(data, loffset, lambda x: setattr(h, "num_objects", x))
        readvlq(data, loffset, None)  # Skip for 64-bit systems
        overflow |= readvlq(data, loffset, lambda x: setattr(h, "whsize", x))
        # readvlq(data, loffset, None) # Skip for 32-bit systems
        if overflow:
            raise ValueError("Object too large to be read back on this platform")
    else:
        raise ValueError("Bad object")

    return h, offset


# Example usage
# with open('data.bin', 'rb') as f:
#     header, _ = caml_parse_header(f.read(), 0)
#     print(header.__dict__)
