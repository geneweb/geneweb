"""Protocol for indexed database types."""

from typing import Protocol


class Indexed(Protocol):
    """Protocol for indexed database types (person ID, family ID, string ID)."""

    @staticmethod
    def dummy() -> int:
        """Return a dummy/invalid ID value."""
        ...

    @staticmethod
    def is_dummy(value: int) -> bool:
        """Check if value is a dummy ID."""
        ...

    @staticmethod
    def hash(value: int) -> int:
        """Compute hash (identity for integer IDs)."""
        ...

    @staticmethod
    def equal(a: int, b: int) -> bool:
        """Check equality."""
        ...

    @staticmethod
    def compare(a: int, b: int) -> int:
        """Compare two values (-1, 0, 1)."""
        ...

    @staticmethod
    def to_string(value: int) -> str:
        """Convert to string."""
        ...

    @staticmethod
    def of_string(s: str) -> int:
        """Parse from string."""
        ...
