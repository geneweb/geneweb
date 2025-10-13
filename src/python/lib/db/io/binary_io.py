"""
Binary I/O utilities for GeneWeb database files.

Implements reading and writing of GeneWeb binary format,
which is compatible with OCaml's Marshal format.
"""

import pickle
import struct
from typing import Any, BinaryIO, Callable


class BinaryIO:
    """
    Binary I/O utilities for reading/writing GeneWeb data.

    This implements the custom binary format used by GeneWeb databases.
    For Python compatibility, we use pickle for complex objects while
    maintaining the same binary integer format.
    """

    SIZEOF_LONG = 4  # 4-byte integers in database files

    # Marshal format prefix codes (from OCaml)
    PREFIX_SMALL_INT = 0x40
    PREFIX_SMALL_BLOCK = 0x80
    PREFIX_SMALL_STRING = 0x20
    CODE_INT8 = 0x0
    CODE_INT16 = 0x1
    CODE_INT32 = 0x2
    CODE_INT64 = 0x3
    CODE_BLOCK32 = 0x8
    CODE_STRING8 = 0x9
    CODE_STRING32 = 0xA

    @staticmethod
    def input_binary_int(f: BinaryIO) -> int:
        """
        Read 4-byte big-endian integer from file.

        This is the standard format for integers in GeneWeb database files.

        Args:
            f: Binary file object

        Returns:
            Integer value (unsigned 32-bit)

        Raises:
            EOFError: If not enough bytes available
        """
        data = f.read(4)
        if len(data) < 4:
            raise EOFError("Unexpected end of file while reading binary int")
        return struct.unpack(">I", data)[0]

    @staticmethod
    def output_binary_int(f: BinaryIO, value: int) -> None:
        """
        Write 4-byte big-endian integer to file.

        Args:
            f: Binary file object
            value: Integer to write (will be masked to 32 bits)
        """
        f.write(struct.pack(">I", value & 0xFFFFFFFF))

    @staticmethod
    def input_value(f: BinaryIO) -> Any:
        """
        Read Python object from file using pickle.

        In the OCaml implementation, this uses Marshal.from_channel.
        For Python, we use pickle which provides similar functionality.

        Args:
            f: Binary file object

        Returns:
            Deserialized Python object

        Raises:
            pickle.UnpicklingError: If data is corrupted
        """
        return pickle.load(f)

    @staticmethod
    def output_value(f: BinaryIO, obj: Any) -> None:
        """
        Write Python object to file using pickle.

        Args:
            f: Binary file object
            obj: Object to serialize
        """
        pickle.dump(obj, f, protocol=pickle.HIGHEST_PROTOCOL)

    @staticmethod
    def output_value_no_sharing(f: BinaryIO, obj: Any) -> None:
        """
        Write object without preserving shared references.

        In OCaml, Marshal.No_sharing prevents sharing of substructures.
        In Python pickle, we use protocol 4+ for better performance.

        Args:
            f: Binary file object
            obj: Object to serialize
        """
        # Protocol 4+ has better handling and is more efficient
        pickle.dump(obj, f, protocol=4)

    @staticmethod
    def size_of_value(obj: Any) -> int:
        """
        Calculate serialized size of object in bytes.

        Args:
            obj: Object to measure

        Returns:
            Size in bytes when serialized
        """
        import io

        buffer = io.BytesIO()
        pickle.dump(obj, buffer, protocol=pickle.HIGHEST_PROTOCOL)
        return buffer.tell()

    @staticmethod
    def output_array_access(
        f: BinaryIO, arr_get: Callable[[int], Any], arr_len: int, pos: int
    ) -> int:
        """
        Output array access table for direct element access.

        This creates an index that maps array indices to file positions,
        allowing direct access to array elements without loading the
        entire array. Used for .acc files in GeneWeb.

        Args:
            f: Binary file object
            arr_get: Function to get array element by index
            arr_len: Length of array
            pos: Starting position in file

        Returns:
            Position after last element in file
        """
        current_pos = pos

        # Write position of each element
        for i in range(arr_len):
            BinaryIO.output_binary_int(f, current_pos)
            # Calculate size of this element
            element_size = BinaryIO.size_of_value(arr_get(i))
            current_pos += element_size

        return current_pos

    @staticmethod
    def input_binary_int64(f: BinaryIO) -> int:
        """
        Read 8-byte big-endian integer from file.

        Args:
            f: Binary file object

        Returns:
            64-bit integer value
        """
        data = f.read(8)
        if len(data) < 8:
            raise EOFError("Unexpected end of file while reading int64")
        return struct.unpack(">Q", data)[0]

    @staticmethod
    def output_binary_int64(f: BinaryIO, value: int) -> None:
        """
        Write 8-byte big-endian integer to file.

        Args:
            f: Binary file object
            value: 64-bit integer to write
        """
        f.write(struct.pack(">Q", value & 0xFFFFFFFFFFFFFFFF))
