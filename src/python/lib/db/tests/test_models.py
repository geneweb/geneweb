"""
Tests for MessagePack models.
"""

from lib.db.models.person import GenPerson
from lib.db.models.family import GenFamily
from lib.db.models.relations import GenCouple, GenDescend
from lib.db.core.enums import Sex, RelationKind
from lib.db.models.events import Date


class TestGenPerson:
    """Test GenPerson model."""

    def test_person_creation(self):
        """Test basic person creation."""
        person = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=3, day=15),
        )

        assert person.first_name == "John"
        assert person.surname == "Smith"
        assert person.sex == Sex.MALE
        assert person.birth.year == 1980
        assert person.birth.month == 3
        assert person.birth.day == 15
        assert person.death is None
        assert person.burial is None

    def test_person_with_death(self):
        """Test person with death date."""
        person = GenPerson(
            first_name="Jane",
            surname="Doe",
            sex=Sex.FEMALE,
            birth=Date(year=1950, month=1, day=1),
            death=Date(year=2020, month=12, day=31),
        )

        assert person.death is not None
        assert person.death.year == 2020
        assert person.death.month == 12
        assert person.death.day == 31

    def test_person_with_burial(self):
        """Test person with burial date."""
        person = GenPerson(
            first_name="Bob",
            surname="Johnson",
            sex=Sex.MALE,
            birth=Date(year=1940, month=6, day=15),
            burial=Date(year=2010, month=7, day=20),
        )

        assert person.burial is not None
        assert person.burial.year == 2010
        assert person.burial.month == 7
        assert person.burial.day == 20

    def test_person_equality(self):
        """Test person equality."""
        person1 = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=3, day=15),
        )

        person2 = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=3, day=15),
        )

        assert person1 == person2

    def test_person_inequality(self):
        """Test person inequality."""
        person1 = GenPerson(
            first_name="John",
            surname="Smith",
            sex=Sex.MALE,
            birth=Date(year=1980, month=3, day=15),
        )

        person2 = GenPerson(
            first_name="Jane",
            surname="Smith",
            sex=Sex.FEMALE,
            birth=Date(year=1980, month=3, day=15),
        )

        assert person1 != person2


class TestGenFamily:
    """Test GenFamily model."""

    def test_family_creation(self):
        """Test basic family creation."""
        family = GenFamily(relation=RelationKind.MARRIED)

        assert family.relation == RelationKind.MARRIED

    def test_family_different_relations(self):
        """Test family with different relation types."""
        married = GenFamily(relation=RelationKind.MARRIED)
        divorced = GenFamily(relation=RelationKind.DIVORCED)
        separated = GenFamily(relation=RelationKind.SEPARATED)

        assert married.relation == RelationKind.MARRIED
        assert divorced.relation == RelationKind.DIVORCED
        assert separated.relation == RelationKind.SEPARATED


class TestGenCouple:
    """Test GenCouple model."""

    def test_couple_creation(self):
        """Test basic couple creation."""
        couple = GenCouple(father=1, mother=2)

        assert couple.father == 1
        assert couple.mother == 2

    def test_couple_equality(self):
        """Test couple equality."""
        couple1 = GenCouple(father=1, mother=2)
        couple2 = GenCouple(father=1, mother=2)

        assert couple1 == couple2

    def test_couple_inequality(self):
        """Test couple inequality."""
        couple1 = GenCouple(father=1, mother=2)
        couple2 = GenCouple(father=2, mother=1)

        assert couple1 != couple2


class TestGenDescend:
    """Test GenDescend model."""

    def test_descend_creation(self):
        """Test basic descend creation."""
        descend = GenDescend(children=[1, 2, 3])

        assert descend.children == [1, 2, 3]

    def test_descend_empty_children(self):
        """Test descend with empty children list."""
        descend = GenDescend(children=[])

        assert descend.children == []

    def test_descend_single_child(self):
        """Test descend with single child."""
        descend = GenDescend(children=[5])

        assert descend.children == [5]

    def test_descend_equality(self):
        """Test descend equality."""
        descend1 = GenDescend(children=[1, 2, 3])
        descend2 = GenDescend(children=[1, 2, 3])

        assert descend1 == descend2

    def test_descend_inequality(self):
        """Test descend inequality."""
        descend1 = GenDescend(children=[1, 2, 3])
        descend2 = GenDescend(children=[1, 2, 4])

        assert descend1 != descend2


class TestDate:
    """Test Date type."""

    def test_date_creation(self):
        """Test basic date creation."""
        date = Date(year=1980, month=3, day=15)

        assert date.year == 1980
        assert date.month == 3
        assert date.day == 15

    def test_date_partial(self):
        """Test date with partial information."""
        date_year_only = Date(year=1980)
        date_year_month = Date(year=1980, month=3)

        assert date_year_only.year == 1980
        assert date_year_only.month == 0
        assert date_year_only.day == 0

        assert date_year_month.year == 1980
        assert date_year_month.month == 3
        assert date_year_month.day == 0

    def test_date_equality(self):
        """Test date equality."""
        date1 = Date(year=1980, month=3, day=15)
        date2 = Date(year=1980, month=3, day=15)

        assert date1 == date2

    def test_date_inequality(self):
        """Test date inequality."""
        date1 = Date(year=1980, month=3, day=15)
        date2 = Date(year=1980, month=3, day=16)

        assert date1 != date2

    def test_date_string_representation(self):
        """Test date string representation."""
        date = Date(year=1980, month=3, day=15)
        date_str = str(date)

        assert "1980" in date_str
        assert "3" in date_str
        assert "15" in date_str
