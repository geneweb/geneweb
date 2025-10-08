import unittest
import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from gedcom.parsers.utils import ParserUtils

class TestParserUtils(unittest.TestCase):
    def test_parse_date_simple(self):
        date = ParserUtils.parse_date('1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.has_year)
        self.assertTrue(date.has_month)
        self.assertTrue(date.has_day)

    def test_parse_date_year_only(self):
        date = ParserUtils.parse_date('1900')
        self.assertEqual(date.year, 1900)
        self.assertTrue(date.has_year)
        self.assertFalse(date.has_month)
        self.assertFalse(date.has_day)

    def test_parse_date_month_year(self):
        date = ParserUtils.parse_date('JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertTrue(date.has_year)
        self.assertTrue(date.has_month)
        self.assertFalse(date.has_day)

    def test_parse_date_approximate(self):
        date = ParserUtils.parse_date('ABT 1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.is_approximate)

    def test_parse_date_calculated(self):
        date = ParserUtils.parse_date('CAL 1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.is_calculated)

    def test_parse_date_estimated(self):
        date = ParserUtils.parse_date('EST 1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.is_estimated)

    def test_parse_date_before(self):
        date = ParserUtils.parse_date('BEF 1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.is_before)

    def test_parse_date_after(self):
        date = ParserUtils.parse_date('AFT 1 JAN 1900')
        self.assertEqual(date.year, 1900)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertTrue(date.is_after)

    def test_parse_date_range_bet_and(self):
        date = ParserUtils.parse_date('BET 1 JAN 1900 AND 31 DEC 1900')
        self.assertTrue(date.is_range)
        self.assertEqual(date.range_type, 'BET')
        self.assertEqual(date.start_date.year, 1900)
        self.assertEqual(date.start_date.month, 1)
        self.assertEqual(date.start_date.day, 1)
        self.assertEqual(date.end_date.year, 1900)
        self.assertEqual(date.end_date.month, 12)
        self.assertEqual(date.end_date.day, 31)

    def test_parse_date_range_from_to(self):
        date = ParserUtils.parse_date('FROM 1 JAN 1900 TO 31 DEC 1900')
        self.assertTrue(date.is_range)
        self.assertEqual(date.range_type, 'FROM...TO')
        self.assertEqual(date.start_date.year, 1900)
        self.assertEqual(date.end_date.year, 1900)

    def test_parse_date_range_from(self):
        date = ParserUtils.parse_date('FROM 1 JAN 1900')
        self.assertTrue(date.is_range)
        self.assertEqual(date.range_type, 'FROM')
        self.assertEqual(date.start_date.year, 1900)

    def test_parse_date_range_to(self):
        date = ParserUtils.parse_date('TO 31 DEC 1900')
        self.assertTrue(date.is_range)
        self.assertEqual(date.range_type, 'TO')
        self.assertEqual(date.end_date.year, 1900)

    def test_parse_place_simple(self):
        place = ParserUtils.parse_place('New York, NY, USA')
        self.assertEqual(place.name, 'New York, NY, USA')
        self.assertEqual(place.parts, ['New York', 'NY', 'USA'])

    def test_parse_place_single(self):
        place = ParserUtils.parse_place('New York')
        self.assertEqual(place.name, 'New York')
        self.assertEqual(place.parts, ['New York'])

    def test_parse_name_full(self):
        name = ParserUtils.parse_name('John /Doe/')
        self.assertEqual(name.full, 'John /Doe/')
        self.assertEqual(name.given, 'John')
        self.assertEqual(name.surname, 'Doe')
        self.assertIsNone(name.suffix)

    def test_parse_name_with_suffix(self):
        name = ParserUtils.parse_name('John /Doe/ Jr.')
        self.assertEqual(name.full, 'John /Doe/ Jr.')
        self.assertEqual(name.given, 'John')
        self.assertEqual(name.surname, 'Doe')
        self.assertEqual(name.suffix, 'Jr.')

    def test_parse_name_no_surname(self):
        name = ParserUtils.parse_name('John')
        self.assertEqual(name.full, 'John')
        self.assertIsNone(name.given)
        self.assertIsNone(name.surname)
        self.assertIsNone(name.suffix)

    def test_parse_name_empty_surname(self):
        name = ParserUtils.parse_name('John //')
        self.assertEqual(name.full, 'John //')
        self.assertEqual(name.given, 'John')
        self.assertEqual(name.surname, '')
        self.assertIsNone(name.suffix)

if __name__ == '__main__':
    unittest.main()
