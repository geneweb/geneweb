"""Tests pour les modèles geneweb_common."""

import pytest
from common.models import Name, Person, Database


@pytest.mark.unit
@pytest.mark.geneweb_common
class TestGenewebCommonModels:
    """Tests pour les modèles de base."""

    def test_name_creation(self):
        """Test de création d'un nom."""
        name = Name(first_name="John", surname="Doe")
        assert name.first_name == "John"
        assert name.surname == "Doe"

    def test_person_creation(self):
        """Test de création d'une personne."""
        name = Name(first_name="John", surname="Doe")
        person = Person(id=1, name=name, sex="M")

        assert person.id == 1
        assert person.name.first_name == "John"
        assert person.sex == "M"

    def test_database_operations(self):
        """Test des opérations de base de données."""
        db = Database()

        name = Name(first_name="John", surname="Doe")
        person = Person(id=1, name=name, sex="M")

        db.add_person(person)

        assert len(db.get_all_persons()) == 1
        retrieved = db.get_person(1)
        assert retrieved.id == 1
        assert retrieved.name.first_name == "John"

    def test_person_full_name_method(self):
        """Test de la méthode get_full_name."""
        name = Name(first_name="John", surname="Doe")
        person = Person(id=1, name=name, sex="M")

        full_name = person.get_full_name()
        assert "John" in full_name
        assert "Doe" in full_name
