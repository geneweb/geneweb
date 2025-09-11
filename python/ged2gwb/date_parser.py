from dataclasses import dataclass
from typing import Optional, List, Tuple
import re

from .types import Calendar, Precision, Date

def is_roman_int(s: str) -> Optional[int]:
    """Convert roman numeral to int, return None if not roman"""
    roman_map = {
        'M': 1000, 'CM': 900, 'D': 500, 'CD': 400,
        'C': 100, 'XC': 90, 'L': 50, 'XL': 40,
        'X': 10, 'IX': 9, 'V': 5, 'IV': 4, 'I': 1
    }
    s = s.upper()
    if not re.match(r'^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$', s):
        return None

    result = 0
    index = 0
    while index < len(s):
        if index + 1 < len(s) and s[index:index+2] in roman_map:
            result += roman_map[s[index:index+2]]
            index += 2
        else:
            result += roman_map[s[index]]
            index += 1
    return result

def parse_number(s: str) -> Optional[int]:
    """Parse string as number (arabic or roman)"""
    try:
        return int(s)
    except ValueError:
        return is_roman_int(s)

def parse_month(s: str) -> Optional[int]:
    """Parse month name to number (1-12)"""
    months = {
        'JAN': 1, 'FEB': 2, 'MAR': 3, 'APR': 4, 'MAY': 5, 'JUN': 6,
        'JUL': 7, 'AUG': 8, 'SEP': 9, 'OCT': 10, 'NOV': 11, 'DEC': 12
    }
    return months.get(s[:3].upper())

def parse_date_tokens(tokens: List[str]) -> Optional[Date]:
    """Parse list of tokens into Date structure"""
    if not tokens:
        return None

    # Handle calendar prefix
    calendar = Calendar.GREGORIAN
    if tokens[0] in ['@#DFRENCH@', '@#DJULIAN@', '@#DHEBREW@']:
        calendar = {
            '@#DFRENCH@': Calendar.FRENCH,
            '@#DJULIAN@': Calendar.JULIAN,
            '@#DHEBREW@': Calendar.HEBREW
        }[tokens[0]]
        tokens = tokens[1:]

    # Handle precision keywords
    precision = Precision.SURE
    if tokens[0] in ['ABT', 'EST', 'CAL']:
        precision = Precision.ABOUT
        tokens = tokens[1:]
    elif tokens[0] in ['BEF', 'TO']:
        precision = Precision.BEFORE
        tokens = tokens[1:]
    elif tokens[0] in ['AFT', 'FROM']:
        precision = Precision.AFTER
        tokens = tokens[1:]
    elif tokens[0] == 'BET':
        precision = Precision.BETWEEN
        tokens = tokens[1:]

    # Parse date components
    date = Date(calendar=calendar, precision=precision)

    # Handle day/month/year
    if len(tokens) >= 1:
        num = parse_number(tokens[0])
        if num is not None:
            if 1 <= num <= 31:
                date.day = num
            else:
                date.year = num

    if len(tokens) >= 2:
        month = parse_month(tokens[1])
        if month is not None:
            date.month = month
            if len(tokens) >= 3:
                year = parse_number(tokens[2])
                if year is not None:
                    date.year = year
        else:
            # Second token might be year
            year = parse_number(tokens[1])
            if year is not None:
                date.year = year

    return date

def date_of_field(s: str) -> Optional[Date]:
    """Top-level date parser: string -> Date"""
    if not s:
        return None

    # Split into tokens
    tokens = s.split()

    # Try parsing as structured date
    date = parse_date_tokens(tokens)
    if date:
        return date

    # Fallback: store as text
    return Date(
        calendar=Calendar.GREGORIAN,
        precision=Precision.SURE,
        text=s
    )
