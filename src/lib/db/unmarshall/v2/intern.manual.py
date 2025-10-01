import struct

# Import OCaml marshaling codes from intext.py
from intext import (
    PREFIX_SMALL_BLOCK,
    PREFIX_SMALL_INT,
    PREFIX_SMALL_STRING,
    CODE_INT8,
    CODE_INT16,
    CODE_INT32,
    CODE_INT64,
    CODE_BLOCK32,
    CODE_BLOCK64,
    CODE_STRING8,
    CODE_STRING32,
    CODE_STRING64,
    CODE_DOUBLE_BIG,
    CODE_DOUBLE_LITTLE,
)


class OCamlUnmarshalError(Exception):
    pass


class OCamlInput:
    def __init__(self, data):
        self.data = data
        self.pos = 0

    def read_byte(self):
        if self.pos >= len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        b = self.data[self.pos]
        self.pos += 1
        return b

    def read_int16(self):
        if self.pos + 2 > len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        val = struct.unpack(">h", self.data[self.pos : self.pos + 2])[0]
        self.pos += 2
        return val

    def read_int32(self):
        if self.pos + 4 > len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        val = struct.unpack(">i", self.data[self.pos : self.pos + 4])[0]
        self.pos += 4
        return val

    def read_int64(self):
        if self.pos + 8 > len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        val = struct.unpack(">q", self.data[self.pos : self.pos + 8])[0]
        self.pos += 8
        return val

    def read_float64(self):
        if self.pos + 8 > len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        val = struct.unpack(">d", self.data[self.pos : self.pos + 8])[0]
        self.pos += 8
        return val

    def read_bytes(self, length):
        if self.pos + length > len(self.data):
            raise OCamlUnmarshalError("Unexpected end of data")
        val = self.data[self.pos : self.pos + length]
        self.pos += length
        return val


def intern_rec(ocaml_input):
    code = ocaml_input.read_byte()
    # Small int and small block
    if code >= PREFIX_SMALL_INT:
        if code >= PREFIX_SMALL_BLOCK:
            tag = code & 0xF
            size = (code >> 4) & 0x7
            # Block: array of values
            return [intern_rec(ocaml_input) for _ in range(size)]
        else:
            # Small int
            return code & 0x3F
    elif code >= PREFIX_SMALL_STRING:
        # Small string
        length = code & 0x1F
        return ocaml_input.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_INT8:
        return struct.unpack("b", bytes([ocaml_input.read_byte()]))[0]
    elif code == CODE_INT16:
        return ocaml_input.read_int16()
    elif code == CODE_INT32:
        return ocaml_input.read_int32()
    elif code == CODE_INT64:
        return ocaml_input.read_int64()
    elif code == CODE_BLOCK32:
        header = ocaml_input.read_int32()
        tag = header & 0xFF
        size = header >> 10
        return [intern_rec(ocaml_input) for _ in range(size)]
    elif code == CODE_BLOCK64:
        header = ocaml_input.read_int64()
        tag = header & 0xFF
        size = header >> 10
        return [intern_rec(ocaml_input) for _ in range(size)]
    elif code == CODE_STRING8:
        length = ocaml_input.read_byte()
        return ocaml_input.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_STRING32:
        length = ocaml_input.read_int32()
        return ocaml_input.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_STRING64:
        length = ocaml_input.read_int64()
        return ocaml_input.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_DOUBLE_BIG or code == CODE_DOUBLE_LITTLE:
        return ocaml_input.read_float64()
    else:
        raise OCamlUnmarshalError(f"Unsupported code: {code:#x}")


# Example usage:
# with open("ocaml_marshal_file", "rb") as f:
#     data = f.read()
#     ocaml_input = OCamlInput(data)
#     value = intern_rec(ocaml_input)
