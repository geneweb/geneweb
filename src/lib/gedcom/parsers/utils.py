"""
Parser Utilities

This module contains shared utility methods for GEDCOM parsing.
"""

from typing import Optional
import re
from ..models import GedcomDate, GedcomPlace, GedcomName

class ParserUtils:
    """Utility class with shared parsing methods."""

    @staticmethod
    def parse_date(date_str: str) -> GedcomDate:
        """Parse GEDCOM date with full 5.5.1 support."""
        date = GedcomDate(raw=date_str)

        upper = date_str.upper().strip()

        if upper.startswith('ABT '):
            date.is_approximate = True
            date_str = date_str[4:].strip()
        elif upper.startswith('CAL '):
            date.is_calculated = True
            date_str = date_str[4:].strip()
        elif upper.startswith('EST '):
            date.is_estimated = True
            date_str = date_str[4:].strip()
        elif upper.startswith('BEF '):
            date.is_before = True
            date_str = date_str[4:].strip()
        elif upper.startswith('AFT '):
            date.is_after = True
            date_str = date_str[4:].strip()

        if upper.startswith('BET ') and ' AND ' in upper:
            date.is_range = True
            date.range_type = 'BET'
            bet_parts = upper[4:].split(' AND ')
            if len(bet_parts) == 2:
                date.start_date = ParserUtils.parse_simple_date(bet_parts[0].strip())
                date.end_date = ParserUtils.parse_simple_date(bet_parts[1].strip())
            return date
        elif upper.startswith('FROM ') and ' TO ' in upper:
            date.is_range = True
            date.range_type = 'FROM...TO'
            from_parts = upper[5:].split(' TO ')
            if len(from_parts) == 2:
                date.start_date = ParserUtils.parse_simple_date(from_parts[0].strip())
                date.end_date = ParserUtils.parse_simple_date(from_parts[1].strip())
            return date
        elif upper.startswith('FROM '):
            date.is_range = True
            date.range_type = 'FROM'
            date.start_date = ParserUtils.parse_simple_date(upper[5:].strip())
            return date
        elif upper.startswith('TO '):
            date.is_range = True
            date.range_type = 'TO'
            date.end_date = ParserUtils.parse_simple_date(upper[3:].strip())
            return date

        # Parse simple date
        return ParserUtils.parse_simple_date(date_str, date)

    @staticmethod
    def parse_simple_date(date_str: str, date: Optional[GedcomDate] = None) -> GedcomDate:
        """Parse a simple date without qualifiers."""
        if date is None:
            date = GedcomDate(raw=date_str)

        months = {
            'JAN': 1, 'FEB': 2, 'MAR': 3, 'APR': 4, 'MAY': 5, 'JUN': 6,
            'JUL': 7, 'AUG': 8, 'SEP': 9, 'OCT': 10, 'NOV': 11, 'DEC': 12
        }

        parts = date_str.strip().split()

        try:
            if len(parts) == 3:
                # Full date: DD MMM YYYY
                date.day = int(parts[0])
                date.month = months.get(parts[1].upper())
                date.year = int(parts[2])
                date.has_day = True
                date.has_month = True
                date.has_year = True
            elif len(parts) == 2:
                # Month and year: MMM YYYY
                date.month = months.get(parts[0].upper())
                date.year = int(parts[1])
                date.has_month = True
                date.has_year = True
            elif len(parts) == 1:
                # Year only: YYYY
                date.year = int(parts[0])
                date.has_year = True
        except (ValueError, IndexError):
            pass

        return date

    @staticmethod
    def parse_place(place_str: str) -> GedcomPlace:
        """Parse GEDCOM place."""
        parts = [p.strip() for p in place_str.split(',')]
        return GedcomPlace(name=place_str, parts=parts)

    @staticmethod
    def parse_name(name_value: str) -> GedcomName:
        """Parse a GEDCOM name."""
        match = re.match(r'^([^/]*)\s*/([^/]*)/\s*(.*)$', name_value)

        if match:
            given = match.group(1).strip() or None
            surname_raw = match.group(2).strip()
            suffix = match.group(3).strip() or None
            if surname_raw == '':
                surname = ''
            else:
                surname = surname_raw or None

            return GedcomName(
                full=name_value,
                given=given,
                surname=surname,
                suffix=suffix
            )
        else:
            return GedcomName(full=name_value)
