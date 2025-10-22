"""
MessagePack I/O operations.
Reuses existing pickle structure with MessagePack serialization.
"""

import os
import msgpack
from pathlib import Path
from typing import Optional, Dict, Any
from datetime import datetime

from ..models.person import GenPerson
from ..models.family import GenFamily
from ..models.relations import GenAscend, GenUnion, GenCouple, GenDescend
from ..models.events import Date, Event, Title
from ..database.base_data import BaseData


class MessagePackWriter:
    """Writer for MessagePack database directory structure (like .gwb)."""

    def __init__(self, output_dir: str = "bases"):
        """Initialize writer with output directory."""
        self.output_dir = output_dir
        os.makedirs(self.output_dir, exist_ok=True)

    def write_database(self, data: BaseData, db_name: str) -> str:
        """
        Write database to MessagePack directory structure.

        Args:
            data: BaseData instance
            db_name: Name of the database (without extension)

        Returns:
            Path to the created directory
        """
        # Create database directory
        db_dir = Path(self.output_dir) / f"{db_name}.msgpack"
        db_dir.mkdir(parents=True, exist_ok=True)

        # Create subdirectories
        (db_dir / "notes_d").mkdir(exist_ok=True)
        (db_dir / "wiznotes").mkdir(exist_ok=True)

        # Write main database file
        self._write_base_file(data, db_dir)

        # Write access files
        self._write_access_files(data, db_dir)

        # Write index files
        self._write_index_files(data, db_dir)

        # Write metadata files
        self._write_metadata_files(data, db_dir)

        return str(db_dir)

    def _write_base_file(self, data: BaseData, db_dir: Path) -> None:
        """Write main database file (equivalent to 'base' in .gwb)."""
        base_file = db_dir / "base"

        # Create header with magic number and counts
        header = {
            "magic": "GWB_MSG",  # 8-byte magic number
            "version": "1.0",
            "persons_count": data.persons_count,
            "families_count": data.families_count,
            "strings_count": data.strings_count,
            "created_at": datetime.now().isoformat(),
            "last_modified": datetime.now().isoformat(),
        }

        # Serialize base data
        base_data = self._serialize_base_data(data)

        # Write header + data
        with open(base_file, "wb") as f:
            # Write header
            f.write(msgpack.packb(header, use_bin_type=True))
            # Write separator
            f.write(b"\n---\n")
            # Write data
            f.write(msgpack.packb(base_data, use_bin_type=True))

    def _serialize_base_data(self, data: BaseData) -> Dict[str, Any]:
        """Serialize BaseData to dictionary for MessagePack."""
        return {
            "persons": {
                str(k): self._serialize_person(v) for k, v in data.persons.items()
            },
            "families": {
                str(k): self._serialize_family(v) for k, v in data.families.items()
            },
            "ascends": {
                str(k): self._serialize_ascend(v) for k, v in data.ascends.items()
            },
            "unions": {
                str(k): self._serialize_union(v) for k, v in data.unions.items()
            },
            "couples": {
                str(k): self._serialize_couple(v) for k, v in data.couples.items()
            },
            "descends": {
                str(k): self._serialize_descend(v) for k, v in data.descends.items()
            },
            "strings": {str(int(k)): v for k, v in data.strings.items()},
            "first_name_index": {
                k: [str(pid) for pid in v] for k, v in data.first_name_index.items()
            },
            "surname_index": {
                k: [str(pid) for pid in v] for k, v in data.surname_index.items()
            },
            "full_name_index": {
                k: [str(pid) for pid in v] for k, v in data.full_name_index.items()
            },
            "string_content_index": {
                k: [str(sid) for sid in v] for k, v in data.string_content_index.items()
            },
            "bdir": data.bdir,
            "particles": data.particles,
        }

    def _serialize_person(self, person) -> Dict[str, Any]:
        """Serialize person to dictionary."""
        return {
            "first_name": person.first_name,
            "surname": person.surname,
            "occ": person.occ,
            "sex": person.sex.value
            if hasattr(person.sex, "value")
            else str(person.sex),
            "birth": self._serialize_date(person.birth) if person.birth else None,
            "death": self._serialize_date(person.death) if person.death else None,
            "baptism": self._serialize_date(person.baptism) if person.baptism else None,
            "burial": self._serialize_date(person.burial) if person.burial else None,
            "titles": [self._serialize_title(t) for t in person.titles],
            "events": [self._serialize_event(e) for e in person.events],
            "notes": person.notes,
            "sources": person.sources,
            "key_index": int(person.key_index) if person.key_index else 0,
        }

    def _serialize_family(self, family) -> Dict[str, Any]:
        """Serialize family to dictionary."""
        return {
            "marriage": self._serialize_date(family.marriage)
            if family.marriage
            else None,
            "marriage_place": family.marriage_place,
            "marriage_src": family.marriage_src,
            "marriage_note": family.marriage_note,
            "relation": family.relation.value
            if hasattr(family.relation, "value")
            else str(family.relation),
            "divorce": family.divorce.value
            if hasattr(family.divorce, "value")
            else str(family.divorce),
            "divorce_date": self._serialize_date(family.divorce_date)
            if family.divorce_date
            else None,
            "divorce_place": family.divorce_place,
            "divorce_src": family.divorce_src,
            "divorce_note": family.divorce_note,
            "events": [self._serialize_event(e) for e in family.events],
            "notes": family.notes,
            "sources": family.sources,
            "fam_index": int(family.fam_index) if family.fam_index else 0,
        }

    def _serialize_ascend(self, ascend) -> Dict[str, Any]:
        """Serialize ascend to dictionary."""
        # Handle both list and single int cases
        if isinstance(ascend.parents, list):
            parents = [str(p) for p in ascend.parents]
        elif isinstance(ascend.parents, int):
            parents = [str(ascend.parents)]
        else:
            parents = []

        return {
            "parents": parents,
            "consang": ascend.consang,
        }

    def _serialize_union(self, union) -> Dict[str, Any]:
        """Serialize union to dictionary."""
        return {
            "family": [str(f) for f in union.family],
        }

    def _serialize_couple(self, couple) -> Dict[str, Any]:
        """Serialize couple to dictionary."""
        return {
            "father": str(couple.father) if couple.father else None,
            "mother": str(couple.mother) if couple.mother else None,
        }

    def _serialize_descend(self, descend) -> Dict[str, Any]:
        """Serialize descend to dictionary."""
        return {
            "children": [str(c) for c in descend.children],
        }

    def _serialize_date(self, date) -> Dict[str, Any]:
        """Serialize date to dictionary."""
        return {
            "year": date.year,
            "month": date.month,
            "day": date.day,
            "prec": date.prec,
            "delta": date.delta,
        }

    def _serialize_event(self, event) -> Dict[str, Any]:
        """Serialize event to dictionary."""
        return {
            "name": event.name,
            "date": self._serialize_date(event.date) if event.date else None,
            "place": event.place,
            "note": event.note,
            "src": event.src,
            "witness": event.witness,
        }

    def _serialize_title(self, title) -> Dict[str, Any]:
        """Serialize title to dictionary."""
        return {
            "name": title.name,
            "title": title.title,
            "date_begin": self._serialize_date(title.date_begin)
            if title.date_begin
            else None,
            "date_end": self._serialize_date(title.date_end)
            if title.date_end
            else None,
            "nth": title.nth,
        }

    def _write_access_files(self, data: BaseData, db_dir: Path) -> None:
        """Write access files (equivalent to 'base.acc' in .gwb)."""
        acc_file = db_dir / "base.acc"

        # Create access data for fast lookups
        access_data = {
            "persons_offsets": [str(k) for k in data.persons.keys()],
            "families_offsets": [str(k) for k in data.families.keys()],
            "strings_offsets": [str(k) for k in data.strings.keys()],
        }

        with open(acc_file, "wb") as f:
            f.write(msgpack.packb(access_data, use_bin_type=True))

    def _write_index_files(self, data: BaseData, db_dir: Path) -> None:
        """Write index files for fast searching."""
        # Names index (mixed first names and surnames)
        names_data = {
            "first_names": {
                k: [str(pid) for pid in v] for k, v in data.first_name_index.items()
            },
            "surnames": {
                k: [str(pid) for pid in v] for k, v in data.surname_index.items()
            },
            "full_names": {
                k: [str(pid) for pid in v] for k, v in data.full_name_index.items()
            },
        }

        with open(db_dir / "names.inx", "wb") as f:
            f.write(msgpack.packb(names_data, use_bin_type=True))

        # Surnames index
        with open(db_dir / "snames.inx", "wb") as f:
            f.write(
                msgpack.packb(
                    {k: [str(pid) for pid in v] for k, v in data.surname_index.items()},
                    use_bin_type=True,
                )
            )

        with open(db_dir / "snames.dat", "wb") as f:
            f.write(
                msgpack.packb(
                    {k: [str(pid) for pid in v] for k, v in data.surname_index.items()},
                    use_bin_type=True,
                )
            )

        # First names index
        with open(db_dir / "fnames.inx", "wb") as f:
            f.write(
                msgpack.packb(
                    {
                        k: [str(pid) for pid in v]
                        for k, v in data.first_name_index.items()
                    },
                    use_bin_type=True,
                )
            )

        with open(db_dir / "fnames.dat", "wb") as f:
            f.write(
                msgpack.packb(
                    {
                        k: [str(pid) for pid in v]
                        for k, v in data.first_name_index.items()
                    },
                    use_bin_type=True,
                )
            )

        # Strings index
        with open(db_dir / "strings.inx", "wb") as f:
            f.write(
                msgpack.packb(
                    {
                        k: [str(sid) for sid in v]
                        for k, v in data.string_content_index.items()
                    },
                    use_bin_type=True,
                )
            )

    def _write_metadata_files(self, data: BaseData, db_dir: Path) -> None:
        """Write metadata and auxiliary files."""
        # Notes file
        with open(db_dir / "notes", "w", encoding="utf-8") as f:
            f.write("")  # Empty notes file

        # Particles file
        with open(db_dir / "particles.txt", "w", encoding="utf-8") as f:
            for particle in data.particles:
                f.write(f"{particle}\n")

        # Number of persons
        with open(db_dir / "nb_persons", "w") as f:
            f.write(str(data.persons_count))

        # Patches file (empty initially)
        with open(db_dir / "patches", "w") as f:
            f.write("")

        # Sync patches file (empty initially)
        with open(db_dir / "synchro_patches", "w") as f:
            f.write("")


class MessagePackReader:
    """Reader for MessagePack database directory structure."""

    def __init__(self, data_dir: str = "bases"):
        """Initialize reader with data directory."""
        self.data_dir = data_dir

    def load_database(self, db_name: str) -> Optional[BaseData]:
        """
        Load database from MessagePack directory structure.

        Args:
            db_name: Name of the database (without extension)

        Returns:
            PickleBaseData instance or None if error
        """
        db_dir = os.path.join(self.data_dir, f"{db_name}.msgpack")

        if not os.path.exists(db_dir):
            return None

        try:
            # Read main base file
            base_file = os.path.join(db_dir, "base")
            if not os.path.exists(base_file):
                return None

            with open(base_file, "rb") as f:
                data = f.read()

            # Split header and data
            parts = data.split(b"\n---\n", 1)
            if len(parts) != 2:
                return None

            msgpack.unpackb(parts[0], raw=False)
            base_data = msgpack.unpackb(parts[1], raw=False)

            # Reconstruct PickleBaseData
            return self._deserialize_base_data(base_data)

        except Exception as e:
            print(f"Error loading database {db_name}: {e}")
            return None

    def _deserialize_base_data(self, data: Dict[str, Any]) -> BaseData:
        """Deserialize dictionary to PickleBaseData."""
        from ..core.types import Iper, Ifam, Istr

        base_data = BaseData()

        # Reconstruct persons
        for k, v in data.get("persons", {}).items():
            base_data.persons[Iper(int(k))] = self._deserialize_person(v)

        # Reconstruct families
        for k, v in data.get("families", {}).items():
            base_data.families[Ifam(int(k))] = self._deserialize_family(v)

        # Reconstruct relations
        for k, v in data.get("ascends", {}).items():
            base_data.ascends[Iper(int(k))] = self._deserialize_ascend(v)

        for k, v in data.get("unions", {}).items():
            base_data.unions[Iper(int(k))] = self._deserialize_union(v)

        for k, v in data.get("couples", {}).items():
            base_data.couples[Ifam(int(k))] = self._deserialize_couple(v)

        for k, v in data.get("descends", {}).items():
            base_data.descends[Ifam(int(k))] = self._deserialize_descend(v)

        # Reconstruct strings
        for k, v in data.get("strings", {}).items():
            base_data.strings[Istr(int(k))] = v

        # Reconstruct indexes
        for k, v in data.get("first_name_index", {}).items():
            base_data.first_name_index[k] = [Iper(int(pid)) for pid in v]

        for k, v in data.get("surname_index", {}).items():
            base_data.surname_index[k] = [Iper(int(pid)) for pid in v]

        for k, v in data.get("full_name_index", {}).items():
            base_data.full_name_index[k] = [Iper(int(pid)) for pid in v]

        for k, v in data.get("string_content_index", {}).items():
            base_data.string_content_index[k] = [Istr(sid) for sid in v]

        base_data.bdir = data.get("bdir", "")
        base_data.particles = data.get("particles", [])

        return base_data

    def _deserialize_person(self, data: Dict[str, Any]) -> GenPerson:
        """Deserialize person from dictionary."""
        from ..core.types import Iper
        from ..core.enums import Sex

        return GenPerson(
            first_name=data.get("first_name", ""),
            surname=data.get("surname", ""),
            occ=data.get("occ", 0),
            sex=Sex(data.get("sex", "NEUTER"))
            if isinstance(data.get("sex"), str)
            else data.get("sex", Sex.NEUTER),
            birth=self._deserialize_date(data.get("birth"))
            if data.get("birth")
            else None,
            death=self._deserialize_date(data.get("death"))
            if data.get("death")
            else None,
            baptism=self._deserialize_date(data.get("baptism"))
            if data.get("baptism")
            else None,
            burial=self._deserialize_date(data.get("burial"))
            if data.get("burial")
            else None,
            titles=[self._deserialize_title(t) for t in data.get("titles", [])],
            events=[self._deserialize_event(e) for e in data.get("events", [])],
            notes=data.get("notes", ""),
            sources=data.get("sources", ""),
            key_index=Iper(data.get("key_index", 0)),
        )

    def _deserialize_family(self, data: Dict[str, Any]) -> GenFamily:
        """Deserialize family from dictionary."""
        from ..core.types import Ifam
        from ..core.enums import RelationKind, DivorceStatus

        return GenFamily(
            marriage=self._deserialize_date(data.get("marriage"))
            if data.get("marriage")
            else None,
            marriage_place=data.get("marriage_place", ""),
            marriage_src=data.get("marriage_src", ""),
            marriage_note=data.get("marriage_note", ""),
            relation=RelationKind(data.get("relation", "MARRIED"))
            if isinstance(data.get("relation"), str)
            else data.get("relation", RelationKind.MARRIED),
            divorce=DivorceStatus(data.get("divorce", "NOT_DIVORCED"))
            if isinstance(data.get("divorce"), str)
            else data.get("divorce", DivorceStatus.NOT_DIVORCED),
            divorce_date=self._deserialize_date(data.get("divorce_date"))
            if data.get("divorce_date")
            else None,
            divorce_place=data.get("divorce_place", ""),
            divorce_src=data.get("divorce_src", ""),
            divorce_note=data.get("divorce_note", ""),
            events=[self._deserialize_event(e) for e in data.get("events", [])],
            notes=data.get("notes", ""),
            sources=data.get("sources", ""),
            fam_index=Ifam(data.get("fam_index", 0)),
        )

    def _deserialize_ascend(self, data: Dict[str, Any]) -> GenAscend:
        """Deserialize ascend from dictionary."""
        from ..core.types import Iper
        from ..models.relations import GenAscend

        return GenAscend(
            parents=[Iper(int(p)) for p in data.get("parents", [])]
            if data.get("parents")
            else None,
            consang=data.get("consang", 0.0),
        )

    def _deserialize_union(self, data: Dict[str, Any]) -> GenUnion:
        """Deserialize union from dictionary."""
        from ..core.types import Ifam
        from ..models.relations import GenUnion

        return GenUnion(
            family=[Ifam(int(f)) for f in data.get("family", [])],
        )

    def _deserialize_couple(self, data: Dict[str, Any]) -> GenCouple:
        """Deserialize couple from dictionary."""
        from ..core.types import Iper
        from ..models.relations import GenCouple

        return GenCouple(
            father=Iper(int(data["father"])) if data.get("father") else None,
            mother=Iper(int(data["mother"])) if data.get("mother") else None,
        )

    def _deserialize_descend(self, data: Dict[str, Any]) -> GenDescend:
        """Deserialize descend from dictionary."""
        from ..core.types import Iper
        from ..models.relations import GenDescend

        return GenDescend(
            children=[Iper(int(c)) for c in data.get("children", [])],
        )

    def _deserialize_date(self, data: Dict[str, Any]) -> Date:
        """Deserialize date from dictionary."""
        from ..models.events import Date

        return Date(
            year=data.get("year", 0),
            month=data.get("month", 0),
            day=data.get("day", 0),
            prec=data.get("prec", 0),
            delta=data.get("delta", 0),
        )

    def _deserialize_event(self, data: Dict[str, Any]) -> Event:
        """Deserialize event from dictionary."""
        from ..models.events import Event

        return Event(
            name=data.get("name", ""),
            date=self._deserialize_date(data["date"]) if data.get("date") else None,
            place=data.get("place", ""),
            note=data.get("note", ""),
            src=data.get("src", ""),
            witness=data.get("witness", ""),
        )

    def _deserialize_title(self, data: Dict[str, Any]) -> Title:
        """Deserialize title from dictionary."""
        from ..models.events import Title

        return Title(
            name=data.get("name", ""),
            title=data.get("title", ""),
            date_begin=self._deserialize_date(data["date_begin"])
            if data.get("date_begin")
            else None,
            date_end=self._deserialize_date(data["date_end"])
            if data.get("date_end")
            else None,
            nth=data.get("nth", 0),
        )

    def list_available_databases(self) -> list[str]:
        """List all available MessagePack databases."""
        databases = []

        if not os.path.exists(self.data_dir):
            return databases

        for item in os.listdir(self.data_dir):
            if item.endswith(".msgpack") and os.path.isdir(
                os.path.join(self.data_dir, item)
            ):
                db_name = item[:-8]  # Remove .msgpack extension
                databases.append(db_name)

        return databases

    # ========================================================================
    # Search Functions
    # ========================================================================

    def search_persons_by_name(
        self, db_name: str, first_name: str = "", surname: str = ""
    ) -> list:
        """Search persons by first name and/or surname."""
        db = self.load_database(db_name)
        if not db:
            return []

        results = []
        for person in db.persons.values():
            if first_name and first_name.lower() not in person.first_name.lower():
                continue
            if surname and surname.lower() not in person.surname.lower():
                continue
            results.append(person)

        return results

    def search_persons_by_full_name(self, db_name: str, full_name: str) -> list:
        """Search persons by full name (case-insensitive, partial match)."""
        db = self.load_database(db_name)
        if not db:
            return []

        # Use the built-in search from BaseData
        person_ids = db.search_persons_by_full_name(full_name)
        return [db.persons[pid] for pid in person_ids if pid in db.persons]

    def search_strings_by_content(self, db_name: str, content: str) -> list:
        """Search strings by content (case-insensitive, partial match)."""
        db = self.load_database(db_name)
        if not db:
            return []

        # Use the built-in search from BaseData
        string_ids = db.search_strings_by_content(content)
        return [(sid, db.strings[sid]) for sid in string_ids if sid in db.strings]

    def search_persons_by_surname(self, db_name: str, surname: str) -> list:
        """Search persons by surname (case-insensitive, partial match)."""
        db = self.load_database(db_name)
        if not db:
            return []

        # Use the built-in search from BaseData
        person_ids = db.search_persons_by_surname(surname)
        return [db.persons[pid] for pid in person_ids if pid in db.persons]

    def search_persons_by_first_name(self, db_name: str, first_name: str) -> list:
        """Search persons by first name (case-insensitive, partial match)."""
        db = self.load_database(db_name)
        if not db:
            return []

        # Use the built-in search from BaseData
        person_ids = db.search_persons_by_first_name(first_name)
        return [db.persons[pid] for pid in person_ids if pid in db.persons]

    def search_persons_by_place(self, db_name: str, place: str) -> list:
        """Search persons by place (birth, death, baptism, burial)."""
        db = self.load_database(db_name)
        if not db:
            return []

        results = []
        place_lower = place.lower()

        for person in db.persons.values():
            # Check birth place
            if (
                person.birth
                and hasattr(person.birth, "place")
                and place_lower in person.birth.place.lower()
            ):
                results.append(person)
                continue

            # Check death place
            if (
                person.death
                and hasattr(person.death, "place")
                and place_lower in person.death.place.lower()
            ):
                results.append(person)
                continue

            # Check baptism place
            if (
                person.baptism
                and hasattr(person.baptism, "place")
                and place_lower in person.baptism.place.lower()
            ):
                results.append(person)
                continue

            # Check burial place
            if (
                person.burial
                and hasattr(person.burial, "place")
                and place_lower in person.burial.place.lower()
            ):
                results.append(person)
                continue

            # Check events
            for event in person.events:
                if event.place and place_lower in event.place.lower():
                    results.append(person)
                    break

        return results

    def get_person_by_id(self, db_name: str, person_id: int) -> Optional[Any]:
        """Get person by ID (key_index)."""
        db = self.load_database(db_name)
        if not db:
            return None

        from ..core.types import Iper

        for person in db.persons.values():
            if person.key_index == Iper(person_id):
                return person

        return None

    def get_family_by_id(self, db_name: str, family_id: int) -> Optional[Any]:
        """Get family by ID (fam_index)."""
        db = self.load_database(db_name)
        if not db:
            return None

        from ..core.types import Ifam

        for family in db.families.values():
            if family.fam_index == Ifam(family_id):
                return family

        return None

    def get_database_statistics(self, db_name: str) -> Optional[Dict[str, Any]]:
        """Get database statistics."""
        db = self.load_database(db_name)
        if not db:
            return None

        return {
            "name": db_name,
            "total_persons": db.persons_count,
            "total_families": db.families_count,
            "total_strings": db.strings_count,
            "bdir": db.bdir,
            "particles_count": len(db.particles),
        }

    def get_all_persons(self, db_name: str) -> list:
        """Get all persons from database."""
        db = self.load_database(db_name)
        if not db:
            return []

        return list(db.persons.values())

    def get_all_families(self, db_name: str) -> list:
        """Get all families from database."""
        db = self.load_database(db_name)
        if not db:
            return []

        return list(db.families.values())

    def search_families_by_marriage_date(self, db_name: str, year: int) -> list:
        """Search families by marriage year."""
        db = self.load_database(db_name)
        if not db:
            return []

        # Use the BaseData method
        family_ids = db.search_families_by_marriage_date(year)
        return [db.families[fid] for fid in family_ids if fid in db.families]

    def get_database_metadata(self, db_name: str) -> Optional[Dict[str, Any]]:
        """Get database metadata."""
        db = self.load_database(db_name)
        if not db:
            return None

        return {
            "bdir": db.bdir,
            "particles": db.particles,
            "persons_count": db.persons_count,
            "families_count": db.families_count,
            "strings_count": db.strings_count,
        }
