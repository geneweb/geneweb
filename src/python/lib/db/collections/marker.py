"""
Marker class for annotating collection elements.

Markers allow attaching temporary data to collection elements,
useful for algorithms that need to mark visited nodes, store
intermediate results, etc.
"""

from typing import Any, Callable, Generic, List, TypeVar

from .collection import Collection

K = TypeVar("K")
V = TypeVar("V")


class Marker(Generic[K, V]):
    """
    Markers annotate collection elements with extra information.
    Used for marking visited nodes, storing temporary data, etc.
    """

    def __init__(
        self, key_to_int: Callable[[K], int], collection: Collection, initial: V
    ):
        """
        Create marker for collection.

        Args:
            key_to_int: Function to convert key to array index
            collection: Collection to mark
            initial: Initial value for all markers
        """
        self._key_to_int = key_to_int
        self._storage: List[V] = [initial] * collection.length

    @classmethod
    def dummy(cls, initial_value: V) -> "Marker[Any, V]":
        """Create dummy marker for placeholders."""
        marker = cls.__new__(cls)
        marker._key_to_int = lambda _: 0
        marker._storage = []
        marker._dummy_value = initial_value
        return marker

    def get(self, key: K) -> V:
        """Get annotation for key."""
        if not self._storage:
            return self._dummy_value
        idx = self._key_to_int(key)
        return self._storage[idx]

    def set(self, key: K, value: V) -> None:
        """Set annotation for key."""
        if self._storage:
            idx = self._key_to_int(key)
            self._storage[idx] = value
