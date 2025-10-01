"""Fixtures communes pour tous les tests."""

import pytest
import tempfile
import shutil
from pathlib import Path
from typing import Dict, Any, List
import subprocess

from python.common import Person, Name, Database, Family, Sex


@pytest.fixture(scope="session")
def test_data_dir() -> Path:
    """Répertoire des données de test."""
    return Path(__file__).parent / "test_data"


@pytest.fixture(scope="session")
def sample_databases_dir(test_data_dir) -> Path:
    """Répertoire des bases de données d'exemple."""
    return test_data_dir / "sample_databases"


@pytest.fixture
def sample_person() -> Person:
    """Crée une personne d'exemple pour les tests."""
    return Person(
        id=1,
        name=Name(first_name="John", surname="Doe"),
        sex=Sex.MALE,
        parents=None
    )


@pytest.fixture
def sample_family_data() -> List[Dict[str, Any]]:
    """Données d'une famille d'exemple."""
    return [
        {"id": 1, "first_name": "John", "surname": "Smith", "sex": "M", "parents": None},
        {"id": 2, "first_name": "Jane", "surname": "Doe", "sex": "F", "parents": None},
        {"id": 3, "first_name": "Bob", "surname": "Smith", "sex": "M", "parents": (1, 2)},
        {"id": 4, "first_name": "Alice", "surname": "Johnson", "sex": "F", "parents": None},
        {"id": 5, "first_name": "Charlie", "surname": "Smith", "sex": "M", "parents": (3, 4)},
    ]


@pytest.fixture
def sample_database(sample_family_data: List[Dict[str, Any]]) -> Database:
    """Base de données d'exemple pour les tests."""
    database = Database()

    # Ajouter les personnes
    for person_data in sample_family_data:
        name = Name(
            first_name=person_data["first_name"],
            surname=person_data["surname"]
        )
        person = Person(
            id=person_data["id"],
            name=name,
            sex=person_data["sex"],
            parents=person_data.get("parents")
        )
        database.add_person(person)

    # Créer les familles
    family_id = 1

    # Famille 1: John + Jane -> Bob
    family1 = Family(id=family_id, father_id=1, mother_id=2, children=[3])
    database.add_family(family1)
    family_id += 1

    # Famille 2: Bob + Alice -> Charlie
    family2 = Family(id=family_id, father_id=3, mother_id=4, children=[5])
    database.add_family(family2)

    return database


@pytest.fixture
def temp_database_dir():
    """Répertoire temporaire pour les bases de données de test."""
    temp_dir = tempfile.mkdtemp(prefix="geneweb_test_")
    yield Path(temp_dir)
    shutil.rmtree(temp_dir)


@pytest.fixture
def mock_gwb_database(temp_database_dir: Path) -> Path:
    """Crée une fausse base de données .gwb pour les tests."""
    db_path = temp_database_dir / "test.gwb"
    db_path.mkdir()

    # Créer un fichier base factice
    base_file = db_path / "base"
    base_file.write_text("# Test database")

    return db_path


@pytest.fixture
def cli_runner():
    """Runner pour tester les interfaces CLI."""
    def run_command(cmd: List[str], timeout: int = 10) -> subprocess.CompletedProcess:
        """Exécute une commande et retourne le résultat."""
        return subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout
        )

    return run_command


@pytest.fixture
def binary_tester(cli_runner):
    """Testeur générique pour binaires."""
    def test_binary(binary_name: str, args: List[str] = None) -> subprocess.CompletedProcess:
        """Test un binaire avec des arguments."""
        cmd = [binary_name]
        if args:
            cmd.extend(args)
        return cli_runner(cmd)

    return test_binary


@pytest.fixture
def gedcom_sample() -> str:
    """Exemple de fichier GEDCOM pour les tests."""
    return '''0 HEAD
1 SOUR GeneWeb
1 GEDC
2 VERS 5.5
2 FORM LINEAGE-LINKED
1 CHAR UTF-8
0 @I1@ INDI
1 NAME John /Smith/
1 SEX M
1 BIRT
2 DATE 1 JAN 1970
0 @I2@ INDI
1 NAME Jane /Doe/
1 SEX F
1 BIRT
2 DATE 1 JAN 1972
0 @F1@ FAM
1 HUSB @I1@
1 WIFE @I2@
1 CHIL @I3@
0 @I3@ INDI
1 NAME Bob /Smith/
1 SEX M
1 BIRT
2 DATE 1 JAN 1995
0 TRLR
'''


@pytest.fixture
def performance_config() -> Dict[str, Any]:
    """Configuration pour les tests de performance."""
    return {
        "max_startup_time": 2.0,
        "max_error_time": 1.0,
        "benchmark_iterations": 5,
        "timeout_seconds": 10
    }
