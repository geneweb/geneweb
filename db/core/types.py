"""
Core indexed types for the database.

This module defines the three fundamental ID types:
- Iper: Person ID
- Ifam: Family ID
- Istr: String ID
"""

class Iper:
    """Person ID implementation."""

    DUMMY = -1

    @staticmethod
    def dummy() -> int:
        return Iper.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Iper.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)


class Ifam:
    """Family ID implementation."""

    DUMMY = -1

    @staticmethod
    def dummy() -> int:
        return Ifam.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Ifam.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)


class Istr:
    """String ID implementation with special values for empty and quest."""

    EMPTY = 0  # ID for empty string ""
    QUEST = 1  # ID for question mark "?"
    DUMMY = -1

    @staticmethod
    def empty() -> int:
        return Istr.EMPTY

    @staticmethod
    def quest() -> int:
        return Istr.QUEST

    @staticmethod
    def is_empty(value: int) -> bool:
        return value == Istr.EMPTY

    @staticmethod
    def is_quest(value: int) -> bool:
        return value == Istr.QUEST

    @staticmethod
    def dummy() -> int:
        return Istr.DUMMY

    @staticmethod
    def is_dummy(value: int) -> bool:
        return value == Istr.DUMMY

    @staticmethod
    def hash(value: int) -> int:
        return value

    @staticmethod
    def equal(a: int, b: int) -> bool:
        return a == b

    @staticmethod
    def compare(a: int, b: int) -> int:
        return -1 if a < b else (1 if a > b else 0)

    @staticmethod
    def to_string(value: int) -> str:
        return str(value)

    @staticmethod
    def of_string(s: str) -> int:
        return int(s)
