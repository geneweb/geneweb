#!/usr/bin/env python3


import argparse
import os

import pytest

import lib.old_db.v2.database as db
from lib.old_db.unmarshall.v2 import dbdisk
from lib.old_db.v2 import defs, mutil


def print_witness(witness, database: dbdisk.DskBase, indent: str = ""):
    wid, role = witness
    person_wit = database.data.persons.get(wid.ref, safe=True)
    if person_wit is not None:
        print(f"{indent}- {person_wit.first_name} {person_wit.surname} ({role})")
    else:
        print(f"{indent}- [ID {wid} not found] ({role})")


def print_person(person: db.Person, iper, database: dbdisk.DskBase):
    if not database.func.iper_exists(iper):
        print(f"Person ID {iper} does not exist.")
        return
    print(f"Found Person #{iper}: {person.first_name} {person.surname}")

    # print all attributes of the person instance
    print("Person attributes:")
    for attr, value in vars(person).items():
        print(f"  {attr}: ", end="")
        match attr:
            case "related":
                print()
                for rel in person.related:
                    rper = database.data.persons.get(rel.ref, safe=True)
                    if rper is not None:
                        print(f"  - {rper.first_name} {rper.surname}")
                    else:
                        print(f"  - [ID {rel.ref} not found]")

            case "aliases" | "qualifiers":
                print()
                for alias in value:
                    print(f"  - {alias}")
                print("  ===")
            case "pevents":
                print()
                for event in person.pevents:
                    print(
                        f"  - {event.epers_name} at {event.epers_place} the {event.epers_date}"
                    )
                    if event.epers_reason:
                        print(f"    reason: {event.epers_reason}")
                    if event.epers_witnesses:
                        print("    witnesses:")
                        for witness in event.epers_witnesses:
                            print_witness(witness, database, "      ")
                        print("    ---")
                    if event.epers_note:
                        print("    note:")
                        print(event.epers_note)
                        print("    ---")
                    print("  ---")
                print("  ===")
            case "death":
                if isinstance(value, tuple):
                    print(*value)
                else:
                    print("<None>")
            case "burial":
                if isinstance(value, tuple):
                    print(*value)
                else:
                    print(value)
            case _:
                print(f"{value}")


def test_db_open():
    """Test whether or not we can open a database"""
    db_path = os.path.join(os.path.dirname(__file__), "test_data", "base")
    with db.Database.open(db_path, read_only=True) as database:
        assert database is not None
        assert isinstance(database, dbdisk.DskBase)


def test_nb_persons():
    """Test whether or not we can count persons in a database"""
    db_path = os.path.join(os.path.dirname(__file__), "test_data", "base")
    with db.Database.open(db_path, read_only=True) as database:
        assert database.data.persons.len == 4


@pytest.fixture
def base_db():
    db_path = os.path.join(os.path.dirname(__file__), "test_data", "base")
    with db.Database.open(db_path, read_only=True) as database:
        yield database


@pytest.fixture
def base_persons(base_db):
    yield list(base_db.data.persons.get(i) for i in range(4))


def test_load_person_name(base_persons):
    """Test loading of person names"""

    def check_person(p: db.Person, fname: str, sname: str):
        assert p is not None
        assert str(p.first_name) == fname
        assert str(p.surname) == sname

    check_person(base_persons[0], "monsieur", "monsieur")
    check_person(base_persons[1], "madame", "monsieur")
    check_person(base_persons[2], "garçon", "monsieur")
    check_person(base_persons[3], "fille", "monsieur")


def test_load_person_birth(base_persons):
    """Test loading of person birth dates"""

    def check_person(p: db.Person, year: int, month: int, day: int):
        assert p is not None
        assert p.birth.year == year
        assert p.birth.month == month
        assert p.birth.day == day

    check_person(base_persons[0], 1901, 1, 1)
    check_person(base_persons[1], 1902, 2, 1)
    check_person(base_persons[2], 1920, 1, 3)
    check_person(base_persons[3], 1925, 1, 4)


def test_load_death(base_persons):
    """Test loading of person death dates"""

    def check_person(p: db.Person, year: int, month: int = 0, day: int = 0):
        assert p is not None
        if isinstance(p.death, tuple):
            assert p.death[1].year == year
            assert p.death[1].month == month
            assert p.death[1].day == day
        if year is None:
            assert p.death == defs.Death.NOT_DEAD
        if year == "?":
            assert p.death == defs.Death.DEAD_DONT_KNOW_WHEN

    check_person(base_persons[0], "?")
    check_person(base_persons[1], "?")
    check_person(base_persons[2], "?")
    check_person(base_persons[3], 1977, 9, 10)


def test_load_burial(base_persons):
    """Test loading of person burial dates"""

    def check_person(p: db.Person, year: int, month: int = 0, day: int = 0):
        assert p is not None
        if isinstance(p.burial, tuple):
            assert p.burial[1].year == year
            assert p.burial[1].month == month
            assert p.burial[1].day == day
        if year is None:
            assert p.burial == defs.Burial.UnknownBurial

    check_person(base_persons[0], None)
    check_person(base_persons[1], None)
    check_person(base_persons[2], None)
    check_person(base_persons[3], 1977, 9, 11)


def test_load_ascendants(base_persons: list[db.Person], base_db: dbdisk.DskBase):
    """Test loading of person ascendants"""
    base_db
    p3 = base_persons[3]

    ascends = base_db.data.ascends.get(p3.key_index.ref)
    assert ascends is not None
    assert base_db.func.ifam_exists(ascends.parents.ref)


def test_load_descendants(base_persons: list[db.Person], base_db: dbdisk.DskBase):
    """Test loading of person descendants"""
    p3 = base_persons[0]

    descendants = base_db.data.descends.get(p3.key_index.ref)
    assert descendants is not None
    assert len(descendants.children) == 2
    for child_ref in descendants.children:
        assert base_db.func.iper_exists(child_ref.ref)


def test_person_exists(base_db: dbdisk.DskBase):
    """Test person existence check"""
    assert base_db.func.iper_exists(0) is True
    assert base_db.func.iper_exists(1) is True
    assert base_db.func.iper_exists(2) is True
    assert base_db.func.iper_exists(3) is True
    assert base_db.func.iper_exists(4) is False
    assert base_db.func.iper_exists(100) is False
    assert base_db.func.iper_exists(-1) is False


def test_family_exists(base_db: dbdisk.DskBase):
    """Test family existence check"""
    assert base_db.func.ifam_exists(0) is True
    assert base_db.func.ifam_exists(1) is False
    assert base_db.func.ifam_exists(2) is False
    assert base_db.func.ifam_exists(100) is False
    assert base_db.func.ifam_exists(-1) is False


if __name__ == "__main__":

    parser = argparse.ArgumentParser(description="Geneweb Database Utility")
    parser.add_argument("database", type=str, help="Path to the Geneweb database")
    parser.add_argument(
        "-v",
        "--verbose",
        action="count",
        default=0,
        help="Increase verbosity level (can be used multiple times)",
    )
    args = parser.parse_args()

    if args.verbose > 0:
        mutil.VERBOSE.ref = args.verbose
    with db.Database.open(args.database, read_only=True) as database:
        print(f"Database '{database.data.bdir}' opened successfully.")
        print(f"Version: {database.version.name}")
        print(f"Number of persons: {database.data.persons.len}")
        print(f"Number of notes: {len(database.data.bnotes.efiles())}")

        print("Enter a person ID to look up (or 'exit' to quit):")
        # for i in range(database.data.strings.len):
        #     print(f"{i}:{database.data.strings.get(i)=}")
        while (i := input("> ")) not in ("exit", "quit", ""):
            try:
                try:
                    ipersons = [int(i)]
                except ValueError:
                    print(f"Looking for persons with exact name '{i}':")
                    ipersons = database.func.persons_of_name(i)

                # iper = database.func.persons_of_surname.cursor(i)
                for iper in ipersons:
                    person = database.data.persons.get(iper)
                    print_person(person, iper, database)

                # print(f"Next person with surname starting '{i}':")
                # iper = database.func.persons_of_surname.next(person.surname.ref)
                # person = database.data.persons.get(iper)
                # print_person(person, iper, database)

            except (KeyError, IndexError) as e:
                print("Person was not found.")
                raise e
