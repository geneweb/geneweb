"""
Collection class for functional data traversal.

Provides a functional interface for working with indexed collections
with operations like map, fold, filter, etc.
"""

from typing import Any, Callable, Generic, Optional, TypeVar

T = TypeVar("T")


class Collection(Generic[T]):
    """
    A collection is a set of elements you want to traverse.
    Provides functional operations like map, fold, filter.
    """

    def __init__(self, length: int, get_func: Callable[[int], Optional[T]]):
        """
        Create a collection with accessor function.

        Args:
            length: Number of elements in collection
            get_func: Function to get element at index (returns None if invalid)
        """
        self._length = length
        self._get_func = get_func

    @classmethod
    def empty(cls) -> "Collection[T]":
        """Create an empty collection."""
        return cls(-1, lambda _: None)

    @property
    def length(self) -> int:
        """Return the number of elements in collection."""
        return self._length

    def get(self, index: int) -> Optional[T]:
        """Get element at index."""
        return self._get_func(index)

    def map(self, fn: Callable[[T], Any]) -> "Collection":
        """
        Apply transformation to each element.

        Args:
            fn: Transformation function

        Returns:
            New collection with transformed elements
        """

        def mapped_get(i: int) -> Optional[Any]:
            value = self._get_func(i)
            return fn(value) if value is not None else None

        return Collection(self._length, mapped_get)

    def iter(self, fn: Callable[[T], None]) -> None:
        """Apply side-effect function to each element."""
        for i in range(self._length):
            value = self.get(i)
            if value is not None:
                fn(value)

    def iteri(self, fn: Callable[[int, T], None]) -> None:
        """Apply side-effect function to each element with its index."""
        for i in range(self._length):
            value = self.get(i)
            if value is not None:
                fn(i, value)

    def fold(
        self,
        fn: Callable[[Any, T], Any],
        acc: Any,
        from_idx: Optional[int] = None,
        until_idx: Optional[int] = None,
    ) -> Any:
        """
        Fold collection into single value.

        Args:
            fn: Reduction function (accumulator, element) -> new_accumulator
            acc: Initial accumulator value
            from_idx: Start index (inclusive)
            until_idx: End index (inclusive)

        Returns:
            Final accumulated value
        """
        start = from_idx if from_idx is not None else 0
        end = (until_idx + 1) if until_idx is not None else self._length

        result = acc
        for i in range(start, end):
            value = self.get(i)
            if value is not None:
                result = fn(result, value)

        return result

    def fold_until(
        self, continue_fn: Callable[[Any], bool], fn: Callable[[Any, T], Any], acc: Any
    ) -> Any:
        """
        Fold until predicate fails.

        Args:
            continue_fn: Predicate to check if folding should continue
            fn: Reduction function
            acc: Initial value

        Returns:
            Final accumulated value
        """
        result = acc
        for i in range(self._length):
            if not continue_fn(result):
                break
            value = self.get(i)
            if value is not None:
                result = fn(result, value)

        return result

    def iterator(self) -> Callable[[], Optional[T]]:
        """
        Create iterator function.

        Returns:
            Function that returns next element or None when exhausted
        """
        cursor = 0

        def next_func() -> Optional[T]:
            nonlocal cursor
            while cursor < self._length:
                value = self.get(cursor)
                cursor += 1
                if value is not None:
                    return value
            return None

        return next_func
