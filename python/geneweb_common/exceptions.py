"""Exception classes for geneweb_common package."""


class GenewebError(Exception):
    """Base exception for all Geneweb errors."""
    pass


class ValidationError(GenewebError):
    """Exception raised for data validation errors."""
    pass


class DatabaseError(GenewebError):
    """Exception raised for database-related errors."""
    pass


class ArgumentError(GenewebError):
    """Exception raised for command-line argument errors."""
    pass


class CalculationError(GenewebError):
    """Exception raised during genealogical calculations."""
    pass


class ParsingError(GenewebError):
    """Exception raised during file parsing."""
    pass


class FileFormatError(ParsingError):
    """Exception raised for unsupported or invalid file formats."""
    pass


class NetworkError(GenewebError):
    """Exception raised for network-related operations."""
    pass
