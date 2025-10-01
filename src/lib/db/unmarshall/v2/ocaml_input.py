import io
import struct
from typing import Any, List

from lib.db.unmarshall.intern import InternItem


class OCamlInput:
    def __init__(self, f: io.BufferedReader):
        # self.data = data
        self.f = f
        # self.pos = 0
        self.obj_table: List[Any] = []
        self.obj_counter = 0
        self.stack: List[InternItem] = []

    def read_byte(self) -> int:
        """Read a single byte from the input."""
        # val = self.data[self.pos]
        # self.pos += 1
        # return val
        return self.f.read(1)[0]

    def read_int16(self) -> int:
        """Read a 16-bit signed integer from the input (2 bytes)."""
        # val = struct.unpack(">h", self.data[self.pos : self.pos + 2])[0]
        # self.pos += 2
        val = struct.unpack(">h", self.f.read(2))[0]
        return val

    def read_uint16(self) -> int:
        """Read a 16-bit unsigned integer from the input (2 bytes)."""
        # val = struct.unpack(">H", self.data[self.pos : self.pos + 2])[0]
        # self.pos += 2
        val = struct.unpack(">H", self.f.read(2))[0]
        return val

    def read_int32(self) -> int:
        """Read a 32-bit signed integer from the input (4 bytes)."""
        # val = struct.unpack(">i", self.data[self.pos : self.pos + 4])[0]
        # self.pos += 4
        val = struct.unpack(">i", self.f.read(4))[0]
        return val

    def read_int(self):
        """Same as read_int32 for compatibility."""
        return self.read_int32()

    def read_uint32(self) -> int:
        """Read a 32-bit unsigned integer from the input (4 bytes)."""
        # val = struct.unpack(">I", self.data[self.pos : self.pos + 4])[0]
        # self.pos += 4
        val = struct.unpack(">I", self.f.read(4))[0]
        return val

    def read_int64(self) -> int:
        """Read a 64-bit signed integer from the input (8 bytes)."""
        # val = struct.unpack(">q", self.data[self.pos : self.pos + 8])[0]
        # self.pos += 8
        val = struct.unpack(">q", self.f.read(8))[0]
        return val

    def read_uint64(self) -> int:
        """Read a 64-bit unsigned integer from the input (8 bytes)."""
        # val = struct.unpack(">Q", self.data[self.pos : self.pos + 8])[0]
        # self.pos += 8
        val = struct.unpack(">Q", self.f.read(8))[0]
        return val

    def read_float64(self) -> float:
        """Read a 64-bit float (double) from the input (8 bytes)."""
        # val = struct.unpack(">d", self.data[self.pos : self.pos + 8])[0]
        # self.pos += 8
        val = struct.unpack(">d", self.f.read(8))[0]
        return val

    def read_bytes(self, length: int) -> bytes:
        """Read a sequence of bytes from the input."""
        # val = self.data[self.pos : self.pos + length]
        # self.pos += length
        val = self.f.read(length)
        return val
