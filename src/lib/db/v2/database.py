"""Database module for Geneweb database access.

This module provides the core database functionality for Geneweb,
including access to persons, families, and other genealogical data.
"""

import logging
import pathlib
import time
import os
from http import HTTPStatus
from typing import (
    Dict,
    Generator,
    Iterable,
    List,
    Tuple,
    Any,
    Type,
    TypeVar,
    Union,
)
from contextlib import contextmanager
from lib.db.unmarshall.v2 import dbdisk
from lib.db.unmarshall.v2.caml_input_val import input_value
from lib.db.unmarshall.v2.intern_rec import (
    read_bin_caml_input,
    read_bin_caml_input_rec,
    unmarshall_ocaml_data,
)
from lib.db.unmarshall.v2.make_immutable_record_access import (
    ImmutableRecord,
    make_immutable_record_access,
)
from lib.db.unmarshall.v2.ocaml_input import OCamlInput
from lib.db.unmarshall.v2.stdlib import Ref, StringRef
from lib.db.v2.defs import (
    Ascend,
    BaseNotes,
    CDate,
    Couple,
    Descend,
    Family,
    PatchEntry,
    DskUnion,
    PatchesHT,
    Person,
    SynchroPatch,
    NoteReadMode,
    HttpError,
)
from lib.db.v2 import dutil as Dutil, mutil as Mutil, lookup
from lib.db.v2.inverted_index import InvertedIndex, StringIndex

T = TypeVar("T")


class Database:
    """Main database class providing access to Geneweb data."""

    MAGIC_PATCH = b"GnPa0001"

    class InvertedIndex:
        hash: Dict[str, List[int]]

    @property
    def bname(self) -> str:
        """Get the full path to the database directory."""
        return self._name

    @property
    def bpath(self) -> pathlib.Path:
        """Get the Path object for the database directory."""
        return pathlib.Path(self._name)

    def __init__(self, name: str, read_only: bool = False):
        """Initialize database access.

        Args:
            name: Path to database (with or without .gwb extension)
            read_only: If True, loads database in memory cache
        """
        self._name = (
            name if name.endswith(".gwb") else name + ".gwb"
        )  # Path to database
        self.I: InvertedIndex = None  # Inverted index for strings

        self.read_only: dbdisk.Permission = read_only
        self.version: dbdisk.BaseVersion = None

        self.patches: PatchesHT = PatchesHT()
        self.pending: PatchesHT = PatchesHT()
        self.sync_patches: SynchroPatch = SynchroPatch(synch_list=[])
        self.synchro_person: List[int] = []
        self.synchro_family: List[int] = []
        self.record_access: Dict[str, dbdisk.RecordAccess[Any]] = {}
        self.data: dbdisk.BaseData = None
        self.inv_idx: InvertedIndex = None

        # LOGGER
        self.logger = Mutil.get_logger("Database")

    ####### STATIC #######

    @staticmethod
    @contextmanager
    def open(
        name: str, *, read_only: bool = False
    ) -> Generator[dbdisk.DskBase, None, None]:
        """Open a database and provide access through context manager.

        Args:
            name: Path to database
            read_only: If True, loads database in memory cache

        Yields:
            Database instance

        Raises:
            FileNotFoundError: If database doesn't exist
            IOError: If database can't be opened
        """
        # Accept both with and without .gwb extension
        bname = name if name.endswith(".gwb") else name + ".gwb"
        bpath = pathlib.Path(bname)
        if not bpath.exists():
            raise FileNotFoundError(f"Database file not found: {bname}")
        db = Database(bname, read_only=read_only or Database.is_read_only(bname))
        try:
            # Load patches
            # print("\033[33m⚠️  db.load_patches has been disabled\033[0m")
            patches = db.load_patches()
            db._reset_pending()
            # Load synchronization history
            synchro = db.load_synchro()
            # Read particles
            particles = db.load_particles()
            # db.load_database_headers()
            (
                persons,
                ascends,
                unions,
                families,
                couples,
                descends,
                strings,
                norigin_file,
            ) = db.load_data()
            if not db.read_only:
                db.commit_patches(persons)
            db._dereference_strings_in_patches(patches, strings)

            class StringIndexImpl(StringIndex):
                def hash(self, s: str) -> int:
                    return hash(s)

                def equal(self, s1: str, s2: str) -> bool:
                    return s1 == s2

                def string_of_id(self, id: int) -> str:
                    return strings.get(id, safe=True)

            db.I = InvertedIndex(StringIndexImpl(), logger=db.logger)
            db.inv_idx = db.I.load(
                db.version,
                inx=bpath / "strings.inx",
                indexes=[patches.h_string[1], db.pending.h_string[1]],
            )
            bnotes = BaseNotes(norigin_file)
            bnotes.nread = db.read_notes
            bnotes.efiles = db.ext_files
            base_data = dbdisk.BaseData(
                persons=persons,
                ascends=ascends,
                unions=unions,
                visible=dbdisk.VisibleRecordAccess(db.read_only, bname, persons),
                families=families,
                couples=couples,
                descends=descends,
                strings=strings,
                particles_txt=particles,
                particles=Mutil.compile_particles(particles),
                bnotes=bnotes,
                bdir=bname,
                perm=db.read_only,
            )
            persons_of_name = lookup.person.ByName(bpath, patches.h_name)
            person_of_key = lookup.person.ByKey(persons, strings, persons_of_name)
            base_func = dbdisk.BaseFunc.build(
                person_of_key=person_of_key,
                persons_of_name=persons_of_name,
                strings_of_sname=lookup.strings.BySurname(
                    db.version, db.bname, strings, patches.h_person
                ),
                strings_of_fname=lookup.strings.ByFirstname(
                    db.version, db.bname, strings, patches.h_person
                ),
                persons_of_surname=...,
                persons_of_first_name=...,
                patch_person=db.patch_person,
                patch_ascend=db.patch_ascend,
                patch_union=db.patch_union,
                patch_family=db.patch_family,
                patch_couple=db.patch_couple,
                patch_descend=db.patch_descend,
                patch_name=db.patch_name,
                insert_string=db.insert_string,
                commit_patches=db.commit_patches,
                commit_notes=db.commit_notes,
                commit_wiznotes=db.commit_wiznotes,
                nb_of_real_persons=db.read_nbp_count,
                iper_exists=db.iper_exists,
                ifam_exists=db.ifam_exists,
            )
            yield dbdisk.DskBase(base_data, base_func, db.version)
        finally:
            # If any cleanup is needed, do it here
            pass

    @staticmethod
    def is_empty_name(p: Person) -> bool:
        """Check if person has empty name fields"""
        return p.surname in (None, 0, 1) and p.first_name in (None, 0, 1)

    @staticmethod
    def move_with_backup(src: str, dst: str) -> None:
        """
        Move a file with backup:
        1. Remove old backup if exists (dst~)
        2. Move current file to backup (dst -> dst~)
        3. Move new file to destination (src -> dst)

        Args:
            src: Source file path
            dst: Destination file path
        """
        logging.warning(f"Tried to move with backup {src} to {dst}")
        return
        backup = f"{dst}~"

        # Remove old backup if exists
        try:
            os.remove(backup)
        except OSError:
            pass

        # Move current file to backup
        try:
            os.rename(dst, backup)
        except OSError:
            pass

        # Move new file to destination
        os.rename(src, dst)

    @staticmethod
    def default_particles() -> list[str]:
        """
        Returns list of default name particles in both uppercase and lowercase.
        Used for name parsing and sorting.
        """
        upper = [
            "AF ",
            "D'",
            "D'",
            "DAL ",
            "DE ",
            "DES ",
            "DI ",
            "DU ",
            "OF ",
            "VAN ",
            "VON UND ZU ",
            "VON ",
            "Y ",
            "ZU ",
            "ZUR ",
        ]

        # Create list with lowercase versions followed by original uppercase versions
        return [p.lower() for p in upper] + upper

    @staticmethod
    def is_read_only(bname: str):
        """Check if the database is read-only."""
        tm_name = os.path.join(bname, "commit_timestamp")
        return os.path.exists(tm_name)

    @staticmethod
    def _make_record_exists(
        patches: PatchesHT, pending: PatchesHT, length: int, i: int
    ) -> bool:
        """
        Check if a record exists in patches, pending, or within valid array bounds.

        Args:
            patches: Dictionary of patches
            pending: Dictionary of pending changes
            length: Length of the base array
            i: Index to check

        Returns:
            bool: True if record exists, False otherwise
        """
        return i in pending or i in patches or (0 <= i < length)

    ############## PRIVATE ##############

    def _make_record_access(
        self,
        immut_record: ImmutableRecord[T],
        patches: Tuple[Ref[int], Dict[int, T]],
        pending: Tuple[Ref[int], Dict[int, T]],
        length: int,
    ) -> dbdisk.RecordAccess[T]:
        """
        Create record access with patches and pending changes.

        Args:
            immut_record: Base immutable record access
            patches: Tuple of (max_len, patches_dict)
            pending: Tuple of (ignored, pending_dict)
            length: Base array length
        """
        plen_ref, patches_dict = patches
        _, pending_dict = pending

        def get_nopending(i: int, *, safe: bool = False) -> T:
            """Get value with only patches applied"""
            try:
                return patches_dict[i]
            except (IndexError, KeyError):
                try:
                    self.logger.debug(f"get_nopending: not in patches {i=}, try base")
                    return immut_record.get(i, safe=safe)
                except (IndexError, KeyError) as e:
                    self.logger.debug(f"get_nopending: not in base {i=}, fail")
                    if safe:
                        return None
                    raise e

        def get(i: int, *, safe: bool = False) -> T:
            """Get value with both patches and pending changes"""
            try:
                self.logger.debug(f"get: try pending {i=}")
                return pending_dict[i]
            except (IndexError, KeyError):
                self.logger.debug(f"get: not in pending {i=}, try patches")
                return get_nopending(i, safe=safe)

        # Calculate max length considering patches
        max_len = max(length, plen_ref.ref)

        def output_array(out_file):
            """Write array to file with patches applied"""
            array = immut_record.get_array()
            if array.read_only:
                raise ValueError("cannot modify read-only data")
            # Apply patches to array
            for idx, value in patches_dict.items():
                array.data[idx] = value
            # Write to file
            # write_value(out_file, array.data)
            raise NotImplementedError("Writing OCaml data is not yet supported")

        return dbdisk.RecordAccess.create(
            load_array=lambda: immut_record.get_array(),
            get=get,
            get_nopending=get_nopending,
            len=max_len,
            output_array=output_array,
            clear_array=immut_record.clear_array,
        )

    def _init_count(self, persons: dbdisk.RecordAccess[Person]) -> int:
        """Initialize person count by scanning database"""
        count = 0
        for i in range(persons.len):
            if not Database.is_empty_name(persons.get(i)):
                count += 1

        # Write count to file
        with open(os.path.join(self.bname, "nb_persons"), "wb") as f:
            # pickle.dump(count, f)
            self.logger.warning("Writing to db not implemented yet")

        return count

    def _reset_pending(self):
        """Reset pending changes to empty."""
        self.pending = PatchesHT()
        self.logger.info(f"{self.pending.h_person[0]=}, ")
        self.pending.h_person[0].ref = self.patches.h_person[0].ref
        self.pending.h_ascend[0].ref = self.patches.h_ascend[0].ref
        self.pending.h_union[0].ref = self.patches.h_union[0].ref
        self.pending.h_family[0].ref = self.patches.h_family[0].ref
        self.pending.h_couple[0].ref = self.patches.h_couple[0].ref
        self.pending.h_descend[0].ref = self.patches.h_descend[0].ref
        self.pending.h_string[0].ref = self.patches.h_string[0].ref

    def _dereference_strings_in_patches(
        self, patches: PatchesHT, strings: dbdisk.RecordAccess[str]
    ) -> PatchesHT:
        """Dereference Ref fields in patches to actual values."""
        logger = self.logger.getChild("dereference_patches")
        logger.debug("dereference patches")

        def deref(d: Any, structure: Type) -> Dict[int, T]:
            nonlocal patches, logger
            logger.debug(f"deref {structure=} {type(d)=}")
            if hasattr(structure, "__origin__"):
                origin = structure.__origin__
                args = structure.__args__
                if origin is Union:
                    if not isinstance(d, Ref):
                        return d
                    for arg in args:
                        if arg is type(None):
                            continue
                        try:
                            return deref(d, arg)
                        except Exception as e:
                            logger.debug(f"Union deref failed for {arg}: {e}")
                    raise ValueError(f"Cannot deref union for {d} with args {args}")
                elif origin is dict:
                    key_type, value_type = args
                    if key_type is not int:
                        raise NotImplementedError(
                            "Only int keys are supported in deref"
                        )
                    return {k: deref(v, value_type) for k, v in d.items()}
                elif issubclass(origin, Tuple):
                    logger.debug(
                        f"origin is tuple {d=} {list((i for (i, item) in enumerate(d)))=}"
                    )
                    return origin(deref(item, args[i]) for (i, item) in enumerate(d))
                elif issubclass(origin, Iterable):
                    logger.debug(
                        f"origin is iterable {d=} {list((i for (i, item) in enumerate(d)))=}"
                    )
                    return origin(deref(item, args[0]) for (i, item) in enumerate(d))

                elif issubclass(origin, Ref):
                    logger.debug(f"Dereferencing Ref: {origin=} {d=}")
                    return d
            elif structure is StringRef:
                logger.debug(f"Dereferencing StringRef: {d} {patches.h_string=}")
                if d.ref in (None, 0, 1):
                    return ""
                res = patches.h_string[1].get(d.ref, None) or strings.get(
                    d.ref, safe=True
                )
                logger.debug(f"{res=}")
                if res is None:
                    return d
                return res
            elif hasattr(structure, "__dataclass_fields__"):
                self.logger.debug(f"Dereferencing dataclass {structure} {d=}")
                if d is None:
                    return d
                fields_name, _ = Mutil.get_dataclass_fields(structure)
                for key in fields_name:
                    attr = getattr(d, key)
                    field_type = structure.__dataclass_fields__[key].type
                    logger.debug(f"{key=} {field_type=}")
                    setattr(d, key, deref(attr, field_type))
                return d
            return d

        return deref(patches, PatchesHT)

    ############## PUBLIC ##############

    def read_nbp_count(self) -> int:
        """Read person count from file or recalculate"""
        nbp_fname = os.path.join(self.bname, "nb_persons")
        if os.path.exists(nbp_fname):
            with open(nbp_fname, "rb") as f:
                return unmarshall_ocaml_data(f, magic=None)
        return self._init_count()

    def insert_string(self, s: str) -> int:
        """Insert string into database and return its ID."""
        try:
            return self.I.find(s)
        except KeyError:
            i = self.data.strings.len
            self.data.strings.len += 1
            self.pending.h_string[0].ref = self.data.strings.len
            self.pending.h_string[1][i] = s
            self.inv_idx.insert(s, i)
            return i

    def read_notes(self, fnotes: str, rn_mode: NoteReadMode):
        """Read notes from a file.

        Args:
            fnotes (str): File name (None for main notes)
            rn_mode (NoteReadMode): Read mode

        Returns:
            str: The read notes
        """
        fname = "notes" if not fnotes else os.path.join("notes_d", f"{fnotes}.txt")
        try:
            with open(os.path.join(self.bname, fname), "r") as f:
                match NoteReadMode(rn_mode):
                    case NoteReadMode.ALL:
                        return f.read()
                    case NoteReadMode.ONE_LINE:
                        return f.readline()
                    case NoteReadMode.DEG:
                        pos = f.tell()
                        f.seek(0, os.SEEK_END)
                        if f.tell() > pos:
                            return " "
                        return ""
        except OSError:
            return ""

    def ext_files(self):
        """List external files in the database directory."""
        notes_dir = self.bpath / "nodes_d"
        files = []

        try:
            if notes_dir.exists() and notes_dir.is_dir():
                # Recursively find all .txt files
                return [f.stem for f in notes_dir.rglob("*.txt")]
        except (OSError, PermissionError):
            # Handle errors silently like original
            pass

        return files

    def iper_exists(self, i: int) -> bool:
        """Check if person record exists."""
        return Database._make_record_exists(
            self.patches.h_person[1], self.pending.h_person[1], self.data.persons.len, i
        )

    def ifam_exists(self, i: int) -> bool:
        """Check if family record exists."""
        return Database._make_record_exists(
            self.patches.h_family[1],
            self.pending.h_family[1],
            self.data.families.len,
            i,
        )

    ####### LOAD #######

    def load_data(self) -> Tuple[
        dbdisk.RecordAccess[Person],
        dbdisk.RecordAccess[Ascend],
        dbdisk.RecordAccess[DskUnion],
        dbdisk.RecordAccess[Family],
        dbdisk.RecordAccess[Couple],
        dbdisk.RecordAccess[Descend],
        dbdisk.RecordAccess[str],
    ]:
        """Load core data arrays from the database."""
        fname = os.path.join(self.bname, "base")
        with open(fname, "rb") as ic:
            # Detect version
            with Mutil.checking_magic(len(Dutil.magic_GnWb0024), ic) as magic_number:
                if magic_number == Dutil.magic_GnWb0024:
                    self.version = dbdisk.BaseVersion.GnWb0024
                elif magic_number == Dutil.magic_GnWb0023:
                    self.version = dbdisk.BaseVersion.GnWb0023
                elif magic_number == Dutil.magic_GnWb0022:
                    self.version = dbdisk.BaseVersion.GnWb0022
                elif magic_number == Dutil.magic_GnWb0021:
                    self.version = dbdisk.BaseVersion.GnWb0021
                elif magic_number == Dutil.magic_GnWb0020:
                    self.version = dbdisk.BaseVersion.GnWb0020
                elif magic_number[:4] == b"GnWb":
                    self.logger.error(
                        f"this is a GeneWeb base, but not compatible: {magic_number=}"
                    )
                else:
                    raise RuntimeError(
                        "this is not a GeneWeb base, or it is a very old version"
                    )
            self.logger.info(f"Database version detected: {self.version.name}")
            oi = OCamlInput(ic)
            # Read header
            persons_len = oi.read_uint32()
            families_len = oi.read_uint32()
            strings_len = oi.read_uint32()
            persons_array_pos = oi.read_uint32()
            ascends_array_pos = oi.read_uint32()
            unions_array_pos = oi.read_uint32()
            families_array_pos = oi.read_uint32()
            couples_array_pos = oi.read_uint32()
            descends_array_pos = oi.read_uint32()
            strings_array_pos = oi.read_uint32()
            norigin_file = input_value(oi)
            # Try to open base.acc
            # ic_acc = try_with_open_bin(os.path.join(bname, "base.acc"), lambda x: x)
            with open(os.path.join(self.bname, "base.acc"), "rb") as ic_acc:
                data = ic_acc.read()
                ic_acc.seek(0)
                shift = 0
                stat = os.stat(self.bname)
                bid = (stat.st_dev, stat.st_ino)
                #     # Create immut_record objects
                im_persons: ImmutableRecord[Person] = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    persons_array_pos,
                    persons_len,
                    "persons",
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Person
                    ),
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Person
                    ),
                    self.logger,
                )
                # shift += persons_len * Iovalue.sizeof_long
                shift += persons_len * 8  # Assuming 64-bit longs
                im_ascends = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    ascends_array_pos,
                    persons_len,
                    "ascends",
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Ascend
                    ),
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Ascend
                    ),
                    self.logger,
                )
                #     shift += persons_len * Iovalue.sizeof_long
                shift += persons_len * 8  # Assuming 64-bit longs
                im_unions = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    unions_array_pos,
                    persons_len,
                    "unions",
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=DskUnion
                    ),
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=DskUnion
                    ),
                    self.logger,
                )
                #     shift += persons_len * Iovalue.sizeof_long
                shift += persons_len * 8  # Assuming 64-bit longs
                im_families = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    families_array_pos,
                    families_len,
                    "families",
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Family
                    ),
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Family
                    ),
                    self.logger,
                )
                #     shift += families_len * Iovalue.sizeof_long
                shift += families_len * 8  # Assuming 64-bit longs
                im_couples = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    couples_array_pos,
                    families_len,
                    "couples",
                    lambda x: read_bin_caml_input_rec(
                        OCamlInput(x), logger=self.logger, structure=Couple
                    ),
                    lambda x: read_bin_caml_input_rec(
                        OCamlInput(x), logger=self.logger, structure=Couple
                    ),
                    self.logger,
                )
                #     shift += families_len * Iovalue.sizeof_long
                shift += families_len * 8  # Assuming 64-bit longs
                im_descends = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    descends_array_pos,
                    families_len,
                    "descends",
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Descend
                    ),
                    lambda f: read_bin_caml_input_rec(
                        OCamlInput(f), logger=self.logger, structure=Descend
                    ),
                    self.logger,
                )
                #     shift += families_len * Iovalue.sizeof_long
                shift += families_len * 8  # Assuming 64-bit longs
                im_strings = make_immutable_record_access(
                    True,
                    ic,
                    ic_acc,
                    shift,
                    strings_array_pos,
                    strings_len,
                    "strings",
                    lambda x: read_bin_caml_input(OCamlInput(x), self.logger),
                    lambda x: read_bin_caml_input(OCamlInput(x), self.logger),
                    self.logger,
                )
                # Wrap records
                persons = self._make_record_access(
                    im_persons,
                    self.patches.h_person,
                    self.pending.h_person,
                    persons_len,
                )
                ascends = self._make_record_access(
                    im_ascends,
                    self.patches.h_ascend,
                    self.pending.h_ascend,
                    persons_len,
                )
                unions = self._make_record_access(
                    im_unions, self.patches.h_union, self.pending.h_union, persons_len
                )
                families = self._make_record_access(
                    im_families,
                    self.patches.h_family,
                    self.pending.h_family,
                    families_len,
                )
                couples = self._make_record_access(
                    im_couples,
                    self.patches.h_couple,
                    self.pending.h_couple,
                    families_len,
                )
                descends = self._make_record_access(
                    im_descends,
                    self.patches.h_descend,
                    self.pending.h_descend,
                    families_len,
                )
                strings = self._make_record_access(
                    im_strings,
                    self.patches.h_string,
                    self.pending.h_string,
                    strings_len,
                )
                self.record_access = {
                    "persons": persons,
                    "ascends": ascends,
                    "unions": unions,
                    "families": families,
                    "couples": couples,
                    "descends": descends,
                    "strings": strings,
                }
                self.logger.info("Database loaded successfully")
                return (
                    persons,
                    ascends,
                    unions,
                    families,
                    couples,
                    descends,
                    strings,
                    norigin_file,
                )

    def load_patches(self) -> PatchesHT:
        """Load patches from the database directory."""
        fname = os.path.join(self.bname, "patches")
        if not os.path.exists(fname):
            self.logger.warning("no patches file")
            self.sync_patches = SynchroPatch(synch_list=[])
            return PatchesHT()
        try:
            with open(fname, "rb") as f:

                self.logger.debug(f"loading patches from {fname}")
                self.patches: PatchesHT = unmarshall_ocaml_data(
                    f, log_level=logging.INFO, structure=PatchesHT
                )
                return self.patches

        except FileExistsError as e:
            print(f"{fname}: corrupted file ({e})")
            return PatchesHT()

    def load_particles(self) -> List[str]:
        """
        Read particles from a file, handling special formatting.

        Args:
            fname: Path to particles file

        Returns:
            List of particles, falling back to default_particles on error

        Format:
            - '_' is converted to space
            - '\n' separates particles
            - '\r' is ignored
        """
        fname = os.path.join(self.bname, "particles.txt")
        try:
            with open(fname, "r") as f:
                result: list[str] = []
                buffer = []

                while True:
                    c = f.read(1)
                    if not c:  # EOF
                        if buffer:
                            result.append("".join(buffer))
                        break

                    if c == "_":
                        buffer.append(" ")
                    elif c == "\n":
                        if buffer:
                            result.append("".join(buffer))
                            buffer = []
                    elif c == "\r":
                        continue
                    else:
                        buffer.append(c)

                return result

        except OSError:
            return Database.default_particles()

    def load_synchro(self):
        """Load synchronization patches from the database."""
        try:
            with open(os.path.join(self.bname, "synchro_patches"), "rb") as ic:

                r: SynchroPatch = unmarshall_ocaml_data(
                    ic, log_level=logging.INFO, structure=SynchroPatch, magic=None
                )
                self.logger.info(f"Loaded {len(r.synch_list)} synchro patches")
                self.logger.debug(f"{r=}")
                self.sync_patches = r
        except FileNotFoundError:
            self.logger.warning("No synchro_patches file found, starting fresh.")
            self.sync_patches = SynchroPatch(synch_list=[])
        return self.sync_patches

    ####### COMMIT #######
    def commit_synchro(self):
        tmp_fname = os.path.join(self.bname, "1synchro_patches")
        fname = os.path.join(self.bname, "synchro_patches")
        try:
            with open(tmp_fname, "wb") as oc9:
                self.sync_patches.insert_patch(self.synchro_person, self.synchro_family)

                self.logger.warning("writing to db not implemented yet")  # TODO
        except OSError:
            raise RuntimeError("the database is not writable")
        Database.move_with_backup(tmp_fname, fname)

    def commit_patches(self, persons: dbdisk.RecordAccess[Person]) -> None:
        """
        Commit pending patches to database.

        Args:
            tm_fname: Timestamp file path
            pending: Pending changes
            patches: Current patches
            persons: Persons database
            nbp_fname: Number of persons file path

        Raises:
            HttpExn: If database is read-only
        """
        tm_fname = os.path.join(self.bname, "commit_timestamp")
        nbp_fname = os.path.join(self.bname, "nb_persons")
        if self.read_only:
            # raise HttpExn("Forbidden", __file__)
            # raise RuntimeError("the database is read-only")
            self.logger.warning("Tried to write into a read-only database")
            raise HttpError(HTTPStatus.FORBIDDEN)

        # Get formatted timestamp
        tm = time.strftime("%Y-%m-%d", time.gmtime(time.time()))

        # Calculate real number of persons including pending patches

        nbp = self.read_nbp_count()

        # Adjust count based on pending person patches
        for ip, p in self.pending.h_person[1].items():
            try:
                old = persons.get_nopending(ip)
                if old is not None:
                    if Database.is_empty_name(old) and not Database.is_empty_name(p):
                        nbp += 1
                    elif not Database.is_empty_name(old) and Database.is_empty_name(p):
                        nbp -= 1
                elif not Database.is_empty_name(p):
                    nbp += 1
            except Exception:
                if not Database.is_empty_name(p):
                    nbp += 1

        # Write updated person count
        tmp_nbp_fname = f"{nbp_fname}_tmp"
        with open(tmp_nbp_fname, "wb") as f:
            # pickle.dump(nbp, f)
            self.logger.warning("writing to db not implemented yet")

        # Helper to merge patches
        def merge_patches(patches_tuple: PatchEntry, pending_tuple: PatchEntry):
            count_ref, patches_dict = patches_tuple
            _, pending_dict = pending_tuple
            count_ref.ref = len(pending_dict)
            patches_dict.update(pending_dict)
            pending_dict.clear()

        # Commit all pending patches
        merge_patches(self.patches.h_person, self.pending.h_person)
        merge_patches(self.patches.h_ascend, self.pending.h_ascend)
        merge_patches(self.patches.h_union, self.pending.h_union)
        merge_patches(self.patches.h_family, self.pending.h_family)
        merge_patches(self.patches.h_couple, self.pending.h_couple)
        merge_patches(self.patches.h_descend, self.pending.h_descend)
        merge_patches(self.patches.h_string, self.pending.h_string)

        # Update patches file
        tmp_fname = os.path.join(self.bname, "1patches")
        fname = os.path.join(self.bname, "patches")

        # Write timestamp
        with open(tm_fname, "w") as f:
            # f.write(tm)
            self.logger.warning("writing to db not implemented yet")

        # Write patches
        with open(tmp_fname, "wb") as f:
            # f.write(MAGIC_PATCH.encode())
            # pickle.dump(patches, f)
            self.logger.warning("writing to db not implemented yet")

        # Move files with backup
        Database.move_with_backup(tmp_nbp_fname, nbp_fname)
        Database.move_with_backup(tmp_fname, fname)

        # Commit synchronization info
        self.commit_synchro()

        # Remove timestamp file
        os.remove(tm_fname)

    def commit_notes(self, fnotes: str, content: str):
        """
        Commit notes with backup handling.

        Args:
            fnotes: Notes filename (empty for main notes)
            content: Note content to write

        Raises:
            HttpExn: If database is read-only
        """
        if self.read_only:
            raise HttpError(HTTPStatus.FORBIDDEN)
        fname = "notes"
        if os.path.exists(fnotes):
            try:
                os.mkdir(os.path.join(self.bname, "notes_d"), 0o755)
            except FileExistsError:
                pass
            fname = os.path.join("notes_d", f"{fnotes}.txt")
        fname = os.path.join(self.bname, fname)
        try:
            os.remove(f"{fname}~")
        except OSError:
            pass
        try:
            os.rename(fname, f"{fname}~")
        except OSError:
            pass
        if content:
            with open(fname, "w") as f:
                # f.write(content)
                self.logger.warning("writing to db not implemented yet")

    def commit_wiznotes(self, fnotes: str, content: str):
        """
        Commit wizard notes with backup handling.

        Args:
            fnotes: Notes filename
            content: Note content to write

        Raises:
            HttpExn: If database is read-only
        """
        if self.read_only:
            raise HttpError(HTTPStatus.FORBIDDEN)
        if not fnotes:
            return
        wiznotes_dir = os.path.join(self.bname, "wiznotes")
        try:
            if not os.path.exists(wiznotes_dir):
                os.mkdir(wiznotes_dir, 0o755)
        except FileExistsError:
            pass
        fname = os.path.join(wiznotes_dir, f"{fnotes}.txt")
        try:
            os.remove(f"{fname}~")
        except OSError:
            pass
        try:
            os.rename(fname, f"{fname}~")
        except OSError:
            pass
        if content:
            with open(fname, "w") as f:
                # f.write(content)
                self.logger.warning("writing to db not implemented yet")

    ####### PATCH #######

    def patch_person(self, id_: int, p: Person) -> None:
        """
        Add or update a person record in pending changes.

        Args:
            i: Person ID/index
            p: Person record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid person ID"
        # Update length if needed
        self.data.persons.len = max(self.data.persons.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_person[0].ref = self.data.persons.len
        # Add/update person in pending changes
        self.pending.h_person[1][id_] = p
        # Track modified person for synchronization
        self.synchro_person.append(id_)
        self.logger.debug(f"Patched person {id_}: {p}")

    def patch_ascend(self, id_: int, a: Ascend) -> None:
        """
        Add or update an ascend record in pending changes.

        Args:
            i: Person ID/index
            a: Ascend record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid person ID"
        # Update length if needed
        self.data.ascends.len = max(self.data.ascends.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_ascend[0].ref = self.data.ascends.len
        # Add/update person in pending changes
        self.pending.h_ascend[1][id_] = a
        # Track modified person for synchronization
        self.synchro_person.append(id_)
        self.logger.debug(f"Patched person {id_}: {a}")

    def patch_union(self, id_: int, u: DskUnion):
        """
        Add or update a union record in pending changes.

        Args:
            i: Person ID/index
            u: Union record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid person ID"
        # Update length if needed
        self.data.unions.len = max(self.data.unions.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_union[0].ref = self.data.unions.len
        # Add/update person in pending changes
        self.pending.h_union[1][id_] = u
        # Track modified person for synchronization
        self.synchro_person.append(id_)
        self.logger.debug(f"Patched union {id_}: {u}")

    def patch_family(self, id_: int, f: Family):
        """
        Add or update a family record in pending changes.

        Args:
            i: Family ID/index
            f: Family record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid family ID"
        # Update length if needed
        self.data.families.len = max(self.data.families.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_family[0].ref = self.data.families.len
        # Add/update person in pending changes
        self.pending.h_family[1][id_] = f
        # Track modified family for synchronization
        self.synchro_family.append(id_)
        self.logger.debug(f"Patched family {id_}: {f}")

    def patch_couple(self, id_: int, c: Couple):
        """
        Add or update a couple record in pending changes.

        Args:
            i: Couple ID/index
            c: Couple record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid couple ID"
        # Update length if needed
        self.data.couples.len = max(self.data.couples.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_couple[0].ref = self.data.couples.len
        # Add/update couple in pending changes
        self.pending.h_couple[1][id_] = c
        # Track modified couple for synchronization
        self.synchro_family.append(id_)
        self.logger.debug(f"Patched couple {id_}: {c}")

    def patch_descend(self, id_: int, d: Descend):
        """
        Add or update a descend record in pending changes.

        Args:
            i: Family ID/index
            d: Descend record to patch

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        assert id_ != -1, "Invalid descend ID"
        # Update length if needed
        self.data.descends.len = max(self.data.descends.len, id_ + 1)
        # Update pending changes counter
        self.pending.h_descend[0].ref = self.data.descends.len
        # Add/update descend in pending changes
        self.pending.h_descend[1][id_] = d
        # Track modified descend for synchronization
        self.synchro_family.append(id_)
        self.logger.debug(f"Patched descend {id_}: {d}")

    def patch_name(self, name: str, person_id: int) -> int:
        """
        Add or update a name record in pending changes.

        Args:
            name: Name to patch
            person_id: Person ID/index

        Raises:
            AssertionError: If invalid ID (-1) provided
        """
        # FIXME pending patches?
        name_index = Dutil.name_index(name)
        try:
            person_list = self.patches.h_name[name_index]
            if person_id not in person_list:
                self.patches[name_index] = [person_id] + person_list
        except KeyError:
            self.patches.h_name[name_index] = [person_id]
        self.logger.debug(f"Patched name '{name}' for person ID {person_id}")
        return name_index
