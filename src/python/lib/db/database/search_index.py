"""Search index for efficient name searching."""

from dataclasses import dataclass
from typing import Callable, List


@dataclass
class SearchIndex:
    """Index for efficient name searching."""

    find: Callable[[int], List[int]]  # Find persons by string ID
    cursor: Callable[[str], int]  # Find first string ID >= key
    next: Callable[[int], int]  # Get next string ID
