"""
Binary I/O utilities for Geneweb database files.
Equivalent to OCaml's iovalue.ml using Python's pickle with optimizations.
"""

import pickle
import struct
from typing import Any, Callable, BinaryIO

# Constants
SIZEOF_LONG = 4
PICKLE_PROTOCOL = 5  # Latest protocol for best performance


def input_value(file: BinaryIO) -> Any:
    """
    Input a value from the given binary file.
    Equivalent to Marshal.from_channel in OCaml.

    Args:
        file: Binary file opened for reading

    Returns:
        Deserialized Python object
    """
    return pickle.load(file)


def output_value(file: BinaryIO, obj: Any) -> None:
    """
    Output a value to the given binary file.
    Uses pickle protocol 5 for best performance.

    Args:
        file: Binary file opened for writing
        obj: Python object to serialize
    """
    pickle.dump(obj, file, protocol=PICKLE_PROTOCOL)


def output_binary_int(file: BinaryIO, value: int) -> None:
    """
    Output a 32-bit integer in big-endian format.

    Args:
        file: Binary file opened for writing
        value: Integer to write (must fit in 32 bits)
    """
    file.write(struct.pack('>i', value))


def input_binary_int(file: BinaryIO) -> int:
    """
    Input a 32-bit integer in big-endian format.

    Args:
        file: Binary file opened for reading

    Returns:
        32-bit signed integer
    """
    data = file.read(4)
    return struct.unpack('>i', data)[0]


def output_array_access(
    file: BinaryIO,
    acc_file: BinaryIO,
    arr_get: Callable[[int], Any],
    arr_len: int,
    start_pos: int
) -> int:
    """
    Output an array and write access positions for each element.

    Args:
        file: Binary file for array data
        acc_file: Binary file for access positions
        arr_get: Function to get array element at index
        arr_len: Length of the array
        start_pos: Starting position in file

    Returns:
        Position after the array in file
    """
    # Calculate header size
    header_size = _calculate_pickle_header_size(arr_len)
    pos = start_pos + header_size

    positions = []
    serialized_elements = []

    # Serialize each element and track positions
    for i in range(arr_len):
        element = arr_get(i)
        serialized = pickle.dumps(element, protocol=PICKLE_PROTOCOL)
        positions.append(pos)
        serialized_elements.append(serialized)
        pos += len(serialized)

    # Write access positions
    for position in positions:
        output_binary_int(acc_file, position)

    # Write array data
    output_value(file, [pickle.loads(s) for s in serialized_elements])

    return pos


def _calculate_pickle_header_size(arr_len: int) -> int:
    """
    Estimate pickle header size for an array.
    This is an approximation based on pickle protocol 5.

    Args:
        arr_len: Length of the array

    Returns:
        Estimated header size in bytes
    """
    # Pickle protocol overhead varies, use conservative estimate
    if arr_len < 256:
        return 20  # Small arrays
    elif arr_len < 65536:
        return 30  # Medium arrays
    else:
        return 50  # Large arrays


def size_of(obj: Any) -> int:
    """
    Calculate the serialized size of an object.

    Args:
        obj: Object to measure

    Returns:
        Size in bytes when serialized
    """
    return len(pickle.dumps(obj, protocol=PICKLE_PROTOCOL))


# Alternative with msgpack for better performance
class OptimizedSerializer:
    """
    Alternative serializer using msgpack for better performance.
    Install with: pip install msgpack
    """

    def __init__(self, use_msgpack: bool = False):
        self.use_msgpack = use_msgpack
        if use_msgpack:
            try:
                import msgpack
                self.msgpack = msgpack
            except ImportError:
                raise ImportError(
                    "msgpack not installed. Install with: pip install msgpack"
                )

    def dump(self, obj: Any, file: BinaryIO) -> None:
        """Serialize object to file."""
        if self.use_msgpack:
            packed = self.msgpack.packb(obj, use_bin_type=True)
            file.write(packed)
        else:
            pickle.dump(obj, file, protocol=PICKLE_PROTOCOL)

    def load(self, file: BinaryIO) -> Any:
        """Deserialize object from file."""
        if self.use_msgpack:
            return self.msgpack.unpackb(file.read(), raw=False)
        else:
            return pickle.load(file)

    def size_of(self, obj: Any) -> int:
        """Calculate serialized size."""
        if self.use_msgpack:
            return len(self.msgpack.packb(obj, use_bin_type=True))
        else:
            return len(pickle.dumps(obj, protocol=PICKLE_PROTOCOL))


# Factory function
def create_serializer(optimized: bool = False) -> OptimizedSerializer:
    """
    Create a serializer instance.

    Args:
        optimized: If True, use msgpack for better performance

    Returns:
        Serializer instance
    """
    return OptimizedSerializer(use_msgpack=optimized)
