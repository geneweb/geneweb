#!/usr/bin/env python3

from pprint import pprint
from lib.db.v2 import mutil
import lib.db.v2.database as db
import argparse


def print_person(person: db.Person, iper, database: db.Database):
    print(f"Found Person #{iper or "?"}: {person.first_name} {person.surname}")
    print(
        f"  Born: {person.birth} at {person.birth_place}, Died: {person.death} at {person.death_place}"
    )
    for parent in person.rparents:
        if (father := parent.r_fath) is not None:
            try:
                f = database.data.persons.get(father.ref)
                print(f"  Father: {f.first_name} {f.surname}")
            except IndexError:
                print("  Father: [ID not found]")
        if (mother := parent.r_moth) is not None:
            try:
                m = database.data.persons.get(mother.ref)
                print(f"  Mother: {m.first_name} {m.surname}")
            except IndexError:
                print("  Mother: [ID not found]")
        if (rtype := parent.r_type) is not None:
            print(f"  Parent type: {rtype}")
    for related in person.related:
        try:
            r = database.data.persons.get(related.ref)
            print(f"  Related: {r.first_name} {r.surname}")
        except IndexError:
            print("  Related: [ID not found]")
    descendants = database.data.descends.get(iper, safe=True)
    if descendants is not None and descendants.children:
        for child in descendants.children:
            try:
                c = database.data.persons.get(child.ref)
                print(f"  Child: {c.first_name} {c.surname}")
            except IndexError:
                print("  Child: [ID not found]")


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
                    iper = int(i)
                except ValueError:
                    continue
                # iper = database.func.persons_of_surname.cursor(i)
                person = database.data.persons.get(iper)
                print_person(person, iper, database)

                # print(f"Next person with surname starting '{i}':")
                # iper = database.func.persons_of_surname.next(person.surname.ref)
                # person = database.data.persons.get(iper)
                # print_person(person, iper, database)

            except (KeyError, IndexError) as e:
                print("Person was not found.")
                raise e
