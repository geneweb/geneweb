from typing import Optional
from models import DateValue

GREGORIAN_MONTHS = {
    "JAN": 1, "FEB": 2, "MAR": 3, "APR": 4, "MAY": 5, "JUN": 6,
    "JUL": 7, "AUG": 8, "SEP": 9, "OCT": 10, "NOV": 11, "DEC": 12
}

FRENCH_MONTHS = {
    "VEND": 1, "BRUM": 2, "FRIM": 3, "NIVO": 4, "PLUV": 5, "VENT": 6,
    "GERM": 7, "FLOR": 8, "PRAI": 9, "MESS": 10, "THER": 11, "FRUC": 12, "COMP": 13
}

HEBREW_MONTHS = {
    "TSH": 1, "CSH": 2, "KSL": 3, "TVT": 4, "SHV": 5, "ADR": 6, "ADS": 7,
    "NSN": 8, "IYR": 9, "SVN": 10, "TMZ": 11, "AAV": 12, "ELL": 13
}

month_number_dates = "NoMonthNumberDates"
no_negative_dates = False
try_negative_dates = False
bad_dates_warned = False

def date_of_field(date_str: str) -> Optional[DateValue]:
    """Parse date from string - placeholder"""
    if not date_str or date_str.strip() == "":
        return None

    return DateValue(text=date_str)

def cdate_of_od(date_val: Optional[DateValue]) -> Optional[str]:
    """Convert DateValue to string - placeholder"""
    if date_val is None:
        return None
    return getattr(date_val, 'text', str(date_val))

def compare_date(d1: Optional[DateValue], d2: Optional[DateValue]) -> int:
    """Compare two dates (-1, 0, 1)"""
    if d1 is None and d2 is None:
        return 0
    if d1 is None:
        return -1
    if d2 is None:
        return 1

    y1 = d1.year or 0
    y2 = d2.year or 0
    if y1 != y2:
        return -1 if y1 < y2 else 1

    m1 = d1.month or 0
    m2 = d2.month or 0
    if m1 != m2:
        return -1 if m1 < m2 else 1

    d1_day = d1.day or 0
    d2_day = d2.day or 0
    if d1_day != d2_day:
        return -1 if d1_day < d2_day else 1

    return 0

def cdate_to_dmy_opt(date_str: str) -> Optional[DateValue]:
    """Parse compact date string back to DateValue"""
    if not date_str:
        return None
    return date_of_field(date_str)
