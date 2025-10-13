import struct

from . import intext

SIZEOF_LONG = 4
WORD_SIZE = SIZEOF_LONG


class InChannelFuns:
    def __init__(self, f):
        self.f = f

    def input_byte(self):
        """Same as Stdlib.input_char, but return the 8-bit integer representing the character."""
        b = self.f.read(1)
        if not b:
            raise EOFError
        return b[0]

    def input_binary_int(self):
        """Read an integer encoded in binary format (4 bytes, big-endian) from the given input channel. See Stdlib.output_binary_int"""
        b = self.f.read(4)
        if len(b) < 4:
            raise EOFError
        return struct.unpack(">i", b)[0]

    def input(self, buf: bytes, offset: int, length: int):
        """Read 'length' bytes. Store them in 'buf' starting at 'offset'."""
        data = self.f.read(length)
        buf[offset : offset + length] = data


def input_loop(ifuns: InChannelFuns):
    raise NotImplementedError(
        "OCaml input_loop not implemented. Use intern_rec.read_bin_caml_input_rec instead."
    )
    # code = ifuns.input_byte()
    # # This is a stub: OCaml's format is complex!
    # # You would need to implement all the cases for small ints, blocks, strings, etc.
    # # Here, we just read a 4-byte int for demonstration.
    # if code >= intext.PREFIX_SMALL_INT:


def input(f):
    ifuns = InChannelFuns(f)
    return input_loop(ifuns)


# Usage:
# with open("some_ocaml_marshal_file", "rb") as f:
#     value = input(f)
