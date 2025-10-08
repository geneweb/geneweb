"""
GEDCOM 5.5.1 Exporter - Clean Code Architecture

This module provides a complete GEDCOM 5.5.1 exporter following clean code principles:
- Single Responsibility Principle
- Dependency Inversion
- Clear separation of concerns
- Comprehensive error handling
"""

from .exporters import GedcomExporter, create_exporter

# Re-export main classes for backward compatibility
__all__ = ['GedcomExporter', 'create_exporter']
