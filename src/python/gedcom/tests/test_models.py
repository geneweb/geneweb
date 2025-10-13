"""
Unit tests for GEDCOM models.
"""

import unittest
from datetime import datetime

from ..models import (
    GedcomDatabase,
    GedcomDate,
    GedcomDateType,
    GedcomEvent,
    GedcomFamily,
    GedcomHeader,
    GedcomIndividual,
    GedcomName,
    GedcomPlace,
    GedcomSex,
)


class TestGedcomDate(unittest.TestCase):
    """Test cases for GedcomDate."""

    def test_create_basic_date(self):
        """Test creating basic date."""
        date = GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1)

        self.assertEqual(date.raw, "1 JAN 1980")
        self.assertEqual(date.year, 1980)
        self.assertEqual(date.month, 1)
        self.assertEqual(date.day, 1)
        self.assertFalse(date.is_approximate)
        self.assertTrue(date.is_valid)

    def test_approximate_date(self):
        """Test approximate date."""
        date = GedcomDate(raw="ABT 1980", year=1980, is_approximate=True)

        self.assertTrue(date.is_approximate)
        self.assertTrue(date.is_valid)

    def test_invalid_date(self):
        """Test invalid date."""
        date = GedcomDate(raw="Unknown")

        self.assertFalse(date.is_valid)
        self.assertEqual(date.sort_key, 0)

    def test_sort_key(self):
        """Test date sorting."""
        date1 = GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1)
        date2 = GedcomDate(raw="15 MAR 1980", year=1980, month=3, day=15)
        date3 = GedcomDate(raw="1981", year=1981)

        self.assertLess(date1.sort_key, date2.sort_key)
        self.assertLess(date2.sort_key, date3.sort_key)

    def test_string_representation(self):
        """Test string representation."""
        date = GedcomDate(raw="1 JAN 1980")
        self.assertEqual(str(date), "1 JAN 1980")


class TestGedcomPlace(unittest.TestCase):
    """Test cases for GedcomPlace."""

    def test_create_place(self):
        """Test creating place."""
        place = GedcomPlace(name="New York, NY, USA", parts=["New York", "NY", "USA"])

        self.assertEqual(place.name, "New York, NY, USA")
        self.assertEqual(len(place.parts), 3)
        self.assertEqual(place.city, "New York")
        self.assertEqual(place.country, "USA")

    def test_empty_place(self):
        """Test empty place."""
        place = GedcomPlace(name="Unknown")

        self.assertIsNone(place.city)
        self.assertIsNone(place.country)

    def test_string_representation(self):
        """Test string representation."""
        place = GedcomPlace(name="Paris, France")
        self.assertEqual(str(place), "Paris, France")


class TestGedcomEvent(unittest.TestCase):
    """Test cases for GedcomEvent."""

    def test_create_event(self):
        """Test creating event."""
        date = GedcomDate(raw="1 JAN 1980", year=1980, month=1, day=1)
        place = GedcomPlace(name="Hospital, City")
        event = GedcomEvent(tag="BIRT", date=date, place=place)

        self.assertEqual(event.tag, "BIRT")
        self.assertEqual(event.date, date)
        self.assertEqual(event.place, place)
        self.assertTrue(event.is_birth)
        self.assertFalse(event.is_death)

    def test_death_event(self):
        """Test death event detection."""
        event = GedcomEvent(tag="DEAT")
        self.assertTrue(event.is_death)
        self.assertFalse(event.is_birth)

    def test_string_representation(self):
        """Test string representation."""
        date = GedcomDate(raw="1980")
        place = GedcomPlace(name="City")
        event = GedcomEvent(tag="BIRT", date=date, place=place)

        self.assertIn("BIRT", str(event))
        self.assertIn("1980", str(event))
        self.assertIn("City", str(event))


class TestGedcomName(unittest.TestCase):
    """Test cases for GedcomName."""

    def test_create_name(self):
        """Test creating name."""
        name = GedcomName(
            full="John /Doe/ Jr.", given="John", surname="Doe", suffix="Jr."
        )

        self.assertEqual(name.full, "John /Doe/ Jr.")
        self.assertEqual(name.given, "John")
        self.assertEqual(name.surname, "Doe")
        self.assertEqual(name.suffix, "Jr.")
        self.assertEqual(name.display_name, "John Doe")
        self.assertEqual(name.sort_name, "Doe, John")

    def test_name_without_components(self):
        """Test name without parsed components."""
        name = GedcomName(full="John Doe")

        self.assertEqual(name.display_name, "John Doe")
        self.assertEqual(name.sort_name, "John Doe")

    def test_string_representation(self):
        """Test string representation."""
        name = GedcomName(full="John Doe")
        self.assertEqual(str(name), "John Doe")


class TestGedcomIndividual(unittest.TestCase):
    """Test cases for GedcomIndividual."""

    def test_create_individual(self):
        """Test creating individual."""
        name = GedcomName(full="John Doe", given="John", surname="Doe")
        birth = GedcomEvent(tag="BIRT", date=GedcomDate(raw="1980", year=1980))
        individual = GedcomIndividual(xref="@I1@", names=[name], sex="M", birth=birth)

        self.assertEqual(individual.xref, "@I1@")
        self.assertEqual(individual.primary_name, name)
        self.assertEqual(individual.sex, "M")
        self.assertEqual(individual.birth_year, 1980)
        self.assertIsNone(individual.death_year)
        self.assertTrue(individual.is_alive)
        self.assertEqual(individual.lifespan, "1980-")

    def test_deceased_individual(self):
        """Test deceased individual."""
        birth = GedcomEvent(tag="BIRT", date=GedcomDate(raw="1920", year=1920))
        death = GedcomEvent(tag="DEAT", date=GedcomDate(raw="2000", year=2000))
        individual = GedcomIndividual(xref="@I1@", birth=birth, death=death)

        self.assertEqual(individual.birth_year, 1920)
        self.assertEqual(individual.death_year, 2000)
        self.assertFalse(individual.is_alive)
        self.assertEqual(individual.lifespan, "1920-2000")

    def test_individual_without_name(self):
        """Test individual without name."""
        individual = GedcomIndividual(xref="@I1@")

        self.assertIsNone(individual.primary_name)
        self.assertIn("Unknown", str(individual))

    def test_string_representation(self):
        """Test string representation."""
        name = GedcomName(full="John Doe")
        individual = GedcomIndividual(xref="@I1@", names=[name])

        self.assertIn("John Doe", str(individual))
        self.assertIn("@I1@", str(individual))


class TestGedcomFamily(unittest.TestCase):
    """Test cases for GedcomFamily."""

    def test_create_family(self):
        """Test creating family."""
        marriage = GedcomEvent(tag="MARR", date=GedcomDate(raw="2000", year=2000))
        family = GedcomFamily(
            xref="@F1@",
            husband="@I1@",
            wife="@I2@",
            children=["@I3@", "@I4@"],
            marriage=marriage,
        )

        self.assertEqual(family.xref, "@F1@")
        self.assertEqual(family.husband, "@I1@")
        self.assertEqual(family.wife, "@I2@")
        self.assertEqual(len(family.children), 2)
        self.assertEqual(family.marriage_year, 2000)

        parents = family.parents
        self.assertEqual(len(parents), 2)
        self.assertIn("@I1@", parents)
        self.assertIn("@I2@", parents)

    def test_single_parent_family(self):
        """Test single parent family."""
        family = GedcomFamily(xref="@F1@", wife="@I1@", children=["@I2@"])

        parents = family.parents
        self.assertEqual(len(parents), 1)
        self.assertEqual(parents[0], "@I1@")

    def test_string_representation(self):
        """Test string representation."""
        family = GedcomFamily(xref="@F1@")
        self.assertEqual(str(family), "Family @F1@")


class TestGedcomDatabase(unittest.TestCase):
    """Test cases for GedcomDatabase."""

    def setUp(self):
        """Set up test fixtures."""
        self.header = GedcomHeader(source="Test")
        self.database = GedcomDatabase(header=self.header)

        # Add test individuals
        john = GedcomIndividual(
            xref="@I1@", names=[GedcomName(full="John Doe")], sex="M"
        )
        jane = GedcomIndividual(
            xref="@I2@", names=[GedcomName(full="Jane Smith")], sex="F"
        )
        baby = GedcomIndividual(
            xref="@I3@", names=[GedcomName(full="Baby Doe")], sex="M"
        )

        john.fams = ["F1"]
        jane.fams = ["F1"]
        baby.famc = ["F1"]

        self.database.individuals = {"@I1@": john, "@I2@": jane, "@I3@": baby}

        # Add test family
        family = GedcomFamily(xref="@F1@", husband="I1", wife="I2", children=["I3"])
        self.database.families = {"@F1@": family}

    def test_get_individual(self):
        """Test getting individual."""
        john = self.database.get_individual("@I1@")
        self.assertIsNotNone(john)
        self.assertEqual(john.xref, "@I1@")

        # Test with and without @ symbols
        john2 = self.database.get_individual("I1")
        self.assertEqual(john, john2)

        # Test non-existent
        nobody = self.database.get_individual("@I999@")
        self.assertIsNone(nobody)

    def test_get_family(self):
        """Test getting family."""
        family = self.database.get_family("@F1@")
        self.assertIsNotNone(family)
        self.assertEqual(family.xref, "@F1@")

        # Test with and without @ symbols
        family2 = self.database.get_family("F1")
        self.assertEqual(family, family2)

    def test_get_children(self):
        """Test getting children."""
        children = self.database.get_children("@F1@")
        self.assertEqual(len(children), 1)
        self.assertEqual(children[0].xref, "@I3@")

        # Test non-existent family
        no_children = self.database.get_children("@F999@")
        self.assertEqual(len(no_children), 0)

    def test_get_parents(self):
        """Test getting parents."""
        parents = self.database.get_parents("@I3@")
        self.assertEqual(len(parents), 2)

        parent_xrefs = {p.xref for p in parents}
        self.assertEqual(parent_xrefs, {"@I1@", "@I2@"})

        # Test individual without parents
        no_parents = self.database.get_parents("@I1@")
        self.assertEqual(len(no_parents), 0)

    def test_get_spouses(self):
        """Test getting spouses."""
        john_spouses = self.database.get_spouses("@I1@")
        self.assertEqual(len(john_spouses), 1)
        self.assertEqual(john_spouses[0].xref, "@I2@")

        jane_spouses = self.database.get_spouses("@I2@")
        self.assertEqual(len(jane_spouses), 1)
        self.assertEqual(jane_spouses[0].xref, "@I1@")

        # Test child (no spouses)
        baby_spouses = self.database.get_spouses("@I3@")
        self.assertEqual(len(baby_spouses), 0)

    def test_statistics(self):
        """Test database statistics."""
        stats = self.database.statistics

        self.assertEqual(stats["individuals"], 3)
        self.assertEqual(stats["families"], 1)
        self.assertEqual(stats["notes"], 0)
        self.assertEqual(stats["sources"], 0)

    def test_string_representation(self):
        """Test string representation."""
        db_str = str(self.database)
        self.assertIn("3 individuals", db_str)
        self.assertIn("1 families", db_str)


if __name__ == "__main__":
    unittest.main()
