import logging
from dataclasses import dataclass
from typing import BinaryIO, Callable, Generic, Optional, TypeVar

T = TypeVar("T")  # Generic type for record items


@dataclass
class DataArray(Generic[T]):
    """Represents either read-only or read-write data arrays"""

    data: list[T]
    read_only: bool


@dataclass
class ImmutableRecord(Generic[T]):
    """Immutable record access with lazy loading"""

    _array: Callable[[], DataArray[T]]  # Lazy array loader
    _get: Callable[[int], T]  # Item getter
    _clear_array: Callable[[], None]  # Array clearer

    def get_array(self) -> DataArray[T]:
        return self._array()

    def get(self, idx: int, *, safe: bool = False) -> T:
        try:
            return self._get(idx)
        except IndexError as e:
            if safe:
                return None
            raise e

    def clear_array(self) -> None:
        self._clear_array()


def make_immutable_record_access(
    read_only: bool,
    input_channel: BinaryIO,  # File-like object for main database
    index_channel: BinaryIO,  # File-like object for index
    shift: int,  # Offset in index file
    array_pos: int,  # Position in main file
    length: int,  # Length of array
    name: str,  # Name for error messages
    input_array: Callable[[BinaryIO], list[T]],  # Function to read full array
    input_item: Callable[[BinaryIO], T],  # Function to read single item
    logger: logging.Logger,
) -> ImmutableRecord[T]:
    """
    Create immutable record access for database arrays.

    Args:
        read_only: If True, data is read-only
        input_channel: Main database file handle
        index_channel: Index file handle
        shift: Offset in index file
        array_pos: Position of array in main file
        length: Length of array
        name: Name for error messages
        input_array: Function to read entire array
        input_item: Function to read single item

    Returns:
        ImmutableRecord with lazy loading and access functions
    """
    data_ref: Optional[DataArray[T]] = None
    input_path = getattr(input_channel, "name", None)
    index_path = getattr(index_channel, "name", None)
    logger.debug(
        f"Setting up ImmutableRecord for {name}: "
        f"{'read-only' if read_only else 'read-write'}, "
        f"array at {input_path}:{array_pos}, length {length}, "
        f"index at {index_path} with shift {shift}"
    )

    def load_array() -> DataArray[T]:
        """Lazy load the array data"""
        nonlocal data_ref
        if data_ref is None:
            with open(input_path, "rb") as f:
                try:
                    f.seek(array_pos)
                    data = input_array(f)
                    data_ref = DataArray(data, read_only)
                except Exception as e:
                    logger.error(f"Error loading {name} array: {e}")
                    raise ValueError(f"Error loading {name} array: {e}")
        return data_ref

    def get_item(idx: int) -> T:
        """Get single item with bounds checking"""
        if not (0 <= idx < length):
            logger.error(f"Invalid {name} index: {idx}")
            raise IndexError(f"Invalid {name} index: {idx}")

        # Try using index for direct access
        if index_path is not None:
            try:
                word_size = 4  # Assuming 64-bit offsets
                with (
                    open(index_path, "rb") as findex,
                    open(input_path, "rb") as finput,
                ):
                    findex.seek(shift + (idx * word_size))  # 8 bytes per index entry
                    item_pos = int.from_bytes(findex.read(word_size), "big")
                    logger.debug(
                        f"Accessing {name} at index {idx}, position {item_pos}; {shift + (idx * word_size)=}"
                    )
                    finput.seek(item_pos)
                    return input_item(finput)
            except Exception as e:
                logger.warning(f"Index access failed for {name} at {idx}: {e}")
                raise e

        # Fall back to array access
        return load_array().data[idx]

    def clear_array() -> None:
        """Clear cached array data"""
        nonlocal data_ref
        data_ref = None

    return ImmutableRecord(load_array, get_item, clear_array)
