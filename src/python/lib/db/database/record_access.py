"""Record access layer for database arrays."""

from dataclasses import dataclass
from typing import Any, Callable, Generic, TypeVar

T = TypeVar("T")


@dataclass
class RecordAccess(Generic[T]):
    """
    Access layer for database arrays with patching support.
    Handles both committed and pending modifications.
    """

    load_array: Callable[[], None]
    get: Callable[[int], T]
    get_nopending: Callable[[int], T]  # Get without pending patches
    output_array: Callable[[Any], None]
    length: int
    clear_array: Callable[[], None]
