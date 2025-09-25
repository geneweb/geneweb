#!/usr/bin/env python3

from lib.db.unmarshall.v2 import dbdisk
from lib.db.v2 import mutil
import lib.db.v2.database as db
import argparse


def print_witness(witness, database: dbdisk.DskBase, indent: str = ""):
    wid, role = witness
    person_wit = database.data.persons.get(wid.ref, safe=True)
    if person_wit is not None:
        print(f"{indent}- {person_wit.first_name} {person_wit.surname} ({role})")
    else:
        print(f"{indent}- [ID {wid} not found] ({role})")


def print_person(person: db.Person, iper, database: dbdisk.DskBase):
    print(f"Found Person #{iper or "?"}: {person.first_name} {person.surname}")

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
