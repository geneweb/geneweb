"""
Custom exception classes.

Defines all exception types used by the library:
- DatabaseError: General database errors
- NotFoundError: Entity not found
- InvalidOperationError: Operation not allowed
- etc.
"""


class DatabaseError(Exception):
    """Base exception for database errors."""

    pass


class NotFoundError(DatabaseError):
    """Raised when entity is not found."""

    pass


class InvalidOperationError(DatabaseError):
    """Raised when operation is not allowed."""

    pass


class CorruptedDataError(DatabaseError):
    """Raised when data is corrupted."""

    pass


class PermissionError(DatabaseError):
    """Raised when operation requires higher permissions."""

    pass
