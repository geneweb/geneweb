"""Test suite for GED2GWB module."""

from .test_options import main as test_options
from .test_conversion import main as test_conversion
from .test_integration import main as test_integration

__all__ = ['test_options', 'test_conversion', 'test_integration']
