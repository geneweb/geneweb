"""Binary search utilities for sorted collections."""

from typing import TypeVar, List, Callable, Optional

T = TypeVar('T')


class BinarySearch:
    """Binary search operations on sorted data."""

    @staticmethod
    def find_first(
        items: List[T],
        key: T,
        compare: Callable[[T, T], int]
    ) -> Optional[int]:
        """
        Find first occurrence of key in sorted list.

        Args:
            items: Sorted list to search
            key: Item to find
            compare: Comparison function (-1, 0, 1)

        Returns:
            Index of first occurrence or None if not found
        """
        left, right = 0, len(items) - 1
        result = None

        while left <= right:
            mid = (left + right) // 2
            cmp = compare(items[mid], key)

            if cmp == 0:
                result = mid
                right = mid - 1  # Continue searching left for first occurrence
            elif cmp < 0:
                left = mid + 1
            else:
                right = mid - 1

        return result

    @staticmethod
    def find_range(
        items: List[T],
        key: T,
        compare: Callable[[T, T], int]
    ) -> List[int]:
        """
        Find all occurrences of key in sorted list.

        Args:
            items: Sorted list to search
            key: Item to find
            compare: Comparison function

        Returns:
            List of all matching indices
        """
        first = BinarySearch.find_first(items, key, compare)
        if first is None:
            return []

        # Find all consecutive matches
        indices = [first]

        # Search left
        i = first - 1
        while i >= 0 and compare(items[i], key) == 0:
            indices.insert(0, i)
            i -= 1

        # Search right
        i = first + 1
        while i < len(items) and compare(items[i], key) == 0:
            indices.append(i)
            i += 1

        return indices

    @staticmethod
    def lower_bound(
        items: List[T],
        key: T,
        compare: Callable[[T, T], int]
    ) -> int:
        """
        Find first position where key could be inserted.

        Args:
            items: Sorted list
            key: Item to find position for
            compare: Comparison function

        Returns:
            Insertion position
        """
        left, right = 0, len(items)

        while left < right:
            mid = (left + right) // 2
            if compare(items[mid], key) < 0:
                left = mid + 1
            else:
                right = mid

        return left
