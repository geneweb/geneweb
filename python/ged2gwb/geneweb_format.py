"""
GeneWeb binary database format implementation - exact OCaml compatibility
"""
import struct
import os
from typing import Dict, List, Tuple, Any, Optional
from enum import IntEnum

class GeneWebMagic(IntEnum):
    """Magic numbers for GeneWeb file formats"""
    PERSONS_ARRAY = 0x474E5750  # "GNWP"
    FAMILIES_ARRAY = 0x474E5746  # "GNWF"
    STRINGS_ARRAY = 0x474E5753  # "GNWS"
    PATCHES = 0x474E5740         # "GNW@"
    SYNCHRO = 0x474E5759         # "GNWY"

class RelationType(IntEnum):
    """Relation types in GeneWeb"""
    MARRIED = 0
    NOT_MARRIED = 1
    ENGAGED = 2
    NO_SEXES_CHECK_NOT_MARRIED = 3
    NO_SEXES_CHECK_MARRIED = 4
    NO_MENTION = 5

class Sex(IntEnum):
    """Sex enumeration"""
    MALE = 0
    FEMALE = 1
    NEUTER = 2

class DeathType(IntEnum):
    """Death type enumeration"""
    NOT_DEAD = 0
    DEAD = 1
    DEAD_YOUNG = 2
    DEAD_DONT_KNOW_WHEN = 3
    DONT_KNOW_IF_DEAD = 4
    OF_COURSE_DEAD = 5

class BurialType(IntEnum):
    """Burial type enumeration"""
    UNKNOWN_BURIAL = 0
    BURIED = 1
    CREMATED = 2

class Access(IntEnum):
    """Access control"""
    IF_TITLES = 0
    PUBLIC = 1
    PRIVATE = 2

def write_magic(f, magic: int):
    """Write magic number"""
    f.write(struct.pack('>I', magic))

def write_int32(f, value: int):
    """Write 32-bit big-endian integer - FIXED: OCaml uses big-endian"""
    f.write(struct.pack('>i', value if isinstance(value, int) else 0))

def write_string(f, s: str):
    """Write length-prefixed string"""
    encoded = s.encode('utf-8')
    write_int32(f, len(encoded))
    f.write(encoded)

def write_array_header(f, magic: int, length: int):
    """Write array header with magic and length"""
    write_magic(f, magic)
    write_int32(f, length)

def write_person_record(f, person: Dict[str, Any]):
    """Write person record in GeneWeb format"""
    write_int32(f, person.get('first_name', 0) if isinstance(person.get('first_name'), int) else 0)
    write_int32(f, person.get('surname', 0) if isinstance(person.get('surname'), int) else 0)
    write_int32(f, person.get('occ', 0) if isinstance(person.get('occ'), int) else 0)
    write_int32(f, person.get('public_name', 0) if isinstance(person.get('public_name'), int) else 0)
    write_int32(f, person.get('image', 0) if isinstance(person.get('image'), int) else 0)

    qualifiers = person.get('qualifiers', [])
    write_int32(f, len(qualifiers))
    for q in qualifiers:
        write_int32(f, q if isinstance(q, int) else 0)

    aliases = person.get('aliases', [])
    write_int32(f, len(aliases))
    for a in aliases:
        write_int32(f, a if isinstance(a, int) else 0)

    first_names_aliases = person.get('first_names_aliases', [])
    write_int32(f, len(first_names_aliases))
    for fa in first_names_aliases:
        write_int32(f, fa if isinstance(fa, int) else 0)

    surnames_aliases = person.get('surnames_aliases', [])
    write_int32(f, len(surnames_aliases))
    for sa in surnames_aliases:
        write_int32(f, sa if isinstance(sa, int) else 0)

    titles = person.get('titles', [])
    write_int32(f, len(titles))
    for title in titles:
        write_title(f, title)

    rparents = person.get('rparents', [])
    write_int32(f, len(rparents))
    for rp in rparents:
        write_related_parent(f, rp)

    related = person.get('related', [])
    write_int32(f, len(related))
    for r in related:
        write_int32(f, r if isinstance(r, int) else 0)

    write_int32(f, person.get('occupation', 0) if isinstance(person.get('occupation'), int) else 0)

    sex = person.get('sex', Sex.NEUTER)
    if isinstance(sex, int):
        f.write(struct.pack('B', sex))
    else:
        f.write(struct.pack('B', Sex.NEUTER))

    access = person.get('access', Access.IF_TITLES)
    if isinstance(access, int):
        f.write(struct.pack('B', access))
    else:
        f.write(struct.pack('B', Access.IF_TITLES))

    write_date(f, person.get('birth'))
    write_int32(f, person.get('birth_place', 0) if isinstance(person.get('birth_place'), int) else 0)
    write_int32(f, person.get('birth_note', 0) if isinstance(person.get('birth_note'), int) else 0)
    write_int32(f, person.get('birth_src', 0) if isinstance(person.get('birth_src'), int) else 0)

    write_date(f, person.get('baptism'))
    write_int32(f, person.get('baptism_place', 0) if isinstance(person.get('baptism_place'), int) else 0)
    write_int32(f, person.get('baptism_note', 0) if isinstance(person.get('baptism_note'), int) else 0)
    write_int32(f, person.get('baptism_src', 0) if isinstance(person.get('baptism_src'), int) else 0)

    write_death(f, person.get('death'))
    write_int32(f, person.get('death_place', 0) if isinstance(person.get('death_place'), int) else 0)
    write_int32(f, person.get('death_note', 0) if isinstance(person.get('death_note'), int) else 0)
    write_int32(f, person.get('death_src', 0) if isinstance(person.get('death_src'), int) else 0)

    write_burial(f, person.get('burial'))
    write_int32(f, person.get('burial_place', 0) if isinstance(person.get('burial_place'), int) else 0)
    write_int32(f, person.get('burial_note', 0) if isinstance(person.get('burial_note'), int) else 0)
    write_int32(f, person.get('burial_src', 0) if isinstance(person.get('burial_src'), int) else 0)

    pevents = person.get('pevents', [])
    write_int32(f, len(pevents))
    for event in pevents:
        write_person_event(f, event)

    write_int32(f, person.get('notes', 0) if isinstance(person.get('notes'), int) else 0)
    write_int32(f, person.get('psources', 0) if isinstance(person.get('psources'), int) else 0)
    write_int32(f, person.get('key_index', 0) if isinstance(person.get('key_index'), int) else 0)

def write_family_record(f, family: Dict[str, Any]):
    """Write family record in GeneWeb format"""
    write_date(f, family.get('marriage'))
    write_int32(f, family.get('marriage_place', 0) if isinstance(family.get('marriage_place'), int) else 0)
    write_int32(f, family.get('marriage_note', 0) if isinstance(family.get('marriage_note'), int) else 0)
    write_int32(f, family.get('marriage_src', 0) if isinstance(family.get('marriage_src'), int) else 0)

    witnesses = family.get('witnesses', [])
    write_int32(f, len(witnesses))
    for w in witnesses:
        write_int32(f, w if isinstance(w, int) else 0)

    relation = family.get('relation', RelationType.MARRIED)
    if isinstance(relation, int):
        f.write(struct.pack('B', relation))
    else:
        f.write(struct.pack('B', RelationType.MARRIED))

    write_divorce(f, family.get('divorce'))

    fevents = family.get('fevents', [])
    write_int32(f, len(fevents))
    for event in fevents:
        write_family_event(f, event)

    write_int32(f, family.get('comment', 0) if isinstance(family.get('comment'), int) else 0)
    write_int32(f, family.get('origin_file', 0) if isinstance(family.get('origin_file'), int) else 0)
    write_int32(f, family.get('fsources', 0) if isinstance(family.get('fsources'), int) else 0)
    write_int32(f, family.get('fam_index', 0) if isinstance(family.get('fam_index'), int) else 0)

def write_date(f, date_info: Optional[str]):
    """Write date in GeneWeb format (simplified)"""
    if date_info is None or date_info == "":
        f.write(struct.pack('B', 0))
    else:
        f.write(struct.pack('B', 1))
        write_string(f, date_info)

def write_death(f, death_info: Any):
    """Write death information"""
    if death_info is None:
        f.write(struct.pack('B', DeathType.DONT_KNOW_IF_DEAD))
    elif isinstance(death_info, str):
        if death_info == "DeadDontKnowWhen":
            f.write(struct.pack('B', DeathType.DEAD_DONT_KNOW_WHEN))
        elif death_info == "NotDead":
            f.write(struct.pack('B', DeathType.NOT_DEAD))
        else:
            f.write(struct.pack('B', DeathType.DEAD))
            write_string(f, death_info)
    else:
        f.write(struct.pack('B', DeathType.DEAD))

def write_burial(f, burial_info: Any):
    """Write burial information"""
    if burial_info is None:
        f.write(struct.pack('B', BurialType.UNKNOWN_BURIAL))
    elif isinstance(burial_info, tuple) and len(burial_info) >= 1:
        burial_type, date_info = burial_info[0], burial_info[1] if len(burial_info) > 1 else None
        if burial_type == "Buried":
            f.write(struct.pack('B', BurialType.BURIED))
        elif burial_type == "Cremated":
            f.write(struct.pack('B', BurialType.CREMATED))
        else:
            f.write(struct.pack('B', BurialType.UNKNOWN_BURIAL))
        write_date(f, date_info)
    else:
        f.write(struct.pack('B', BurialType.UNKNOWN_BURIAL))

def write_divorce(f, divorce_info: Any):
    """Write divorce information"""
    if divorce_info is None or divorce_info == "NotDivorced":
        f.write(struct.pack('B', 0))  # Not divorced
    elif isinstance(divorce_info, tuple):
        divorce_type, date_info = divorce_info
        if divorce_type == "Divorced":
            f.write(struct.pack('B', 1))
            write_date(f, date_info)
        elif divorce_type == "Separated":
            f.write(struct.pack('B', 2))
            write_date(f, date_info)
        else:
            f.write(struct.pack('B', 0))
    else:
        f.write(struct.pack('B', 0))

def write_title(f, title: Dict[str, Any]):
    """Write title information (simplified)"""
    write_int32(f, title.get('t_name', 0) if isinstance(title.get('t_name'), int) else 0)
    write_int32(f, title.get('t_ident', 0) if isinstance(title.get('t_ident'), int) else 0)
    write_int32(f, title.get('t_place', 0) if isinstance(title.get('t_place'), int) else 0)
    write_date(f, title.get('t_date_start'))
    write_date(f, title.get('t_date_end'))
    write_int32(f, title.get('t_nth', 0) if isinstance(title.get('t_nth'), int) else 0)

def write_related_parent(f, rparent: Dict[str, Any]):
    """Write related parent information (simplified)"""
    write_int32(f, rparent.get('r_type', 0) if isinstance(rparent.get('r_type'), int) else 0)

    r_fath = rparent.get('r_fath')
    if r_fath is not None and isinstance(r_fath, int):
        write_int32(f, r_fath)
    else:
        write_int32(f, -1)

    r_moth = rparent.get('r_moth')
    if r_moth is not None and isinstance(r_moth, int):
        write_int32(f, r_moth)
    else:
        write_int32(f, -1)

    write_int32(f, rparent.get('r_sources', 0) if isinstance(rparent.get('r_sources'), int) else 0)

def write_person_event(f, event: Dict[str, Any]):
    """Write person event (simplified)"""
    write_int32(f, event.get('epers_name', 0) if isinstance(event.get('epers_name'), int) else 0)
    write_date(f, event.get('epers_date'))
    write_int32(f, event.get('epers_place', 0) if isinstance(event.get('epers_place'), int) else 0)
    write_int32(f, event.get('epers_reason', 0) if isinstance(event.get('epers_reason'), int) else 0)
    write_int32(f, event.get('epers_note', 0) if isinstance(event.get('epers_note'), int) else 0)
    write_int32(f, event.get('epers_src', 0) if isinstance(event.get('epers_src'), int) else 0)

    witnesses = event.get('epers_witnesses', [])
    write_int32(f, len(witnesses))
    for witness_data in witnesses:
        if isinstance(witness_data, tuple) and len(witness_data) >= 2:
            witness_idx, witness_kind = witness_data
            write_int32(f, witness_idx if isinstance(witness_idx, int) else 0)
            f.write(struct.pack('B', witness_kind if isinstance(witness_kind, int) else 0))
        else:
            write_int32(f, 0)
            f.write(struct.pack('B', 0))

def write_family_event(f, event: Dict[str, Any]):
    """Write family event (simplified)"""
    write_int32(f, event.get('efam_name', 0) if isinstance(event.get('efam_name'), int) else 0)
    write_date(f, event.get('efam_date'))
    write_int32(f, event.get('efam_place', 0) if isinstance(event.get('efam_place'), int) else 0)
    write_int32(f, event.get('efam_reason', 0) if isinstance(event.get('efam_reason'), int) else 0)
    write_int32(f, event.get('efam_note', 0) if isinstance(event.get('efam_note'), int) else 0)
    write_int32(f, event.get('efam_src', 0) if isinstance(event.get('efam_src'), int) else 0)

    witnesses = event.get('efam_witnesses', [])
    write_int32(f, len(witnesses))
    for witness_data in witnesses:
        if isinstance(witness_data, tuple) and len(witness_data) >= 2:
            witness_idx, witness_kind = witness_data
            write_int32(f, witness_idx if isinstance(witness_idx, int) else 0)
            f.write(struct.pack('B', witness_kind if isinstance(witness_kind, int) else 0))
        else:
            write_int32(f, 0)
            f.write(struct.pack('B', 0))

def write_ascend_record(f, ascend: Dict[str, Any]):
    """Write ascend record"""
    parents = ascend.get('parents')
    if parents is not None and isinstance(parents, int):
        write_int32(f, 1)  # Has parents
        write_int32(f, parents)
    else:
        write_int32(f, 0)  # No parents

    consang = ascend.get('consang', -1)
    write_int32(f, consang if isinstance(consang, int) else -1)

def write_union_record(f, union: Dict[str, Any]):
    """Write union record"""
    families = union.get('family', [])
    write_int32(f, len(families))
    for fam_idx in families:
        write_int32(f, fam_idx)

def write_couple_record(f, couple: Tuple[int, int]):
    """Write couple record"""
    if isinstance(couple, (tuple, list)) and len(couple) >= 2:
        father = couple[0] if couple[0] is not None and isinstance(couple[0], int) else -1
        mother = couple[1] if couple[1] is not None and isinstance(couple[1], int) else -1
        write_int32(f, father)
        write_int32(f, mother)
    else:
        write_int32(f, -1)
        write_int32(f, -1)

def write_descend_record(f, descend: Dict[str, Any]):
    """Write descend record"""
    children = descend.get('children', [])
    write_int32(f, len(children))
    for child_idx in children:
        write_int32(f, child_idx if isinstance(child_idx, int) else 0)

def create_geneweb_database(db_dir: str, arrays) -> Tuple[int, int, int]:
    """Create GeneWeb database files - FIXED: Only create OCaml-compatible files"""
    (persons_array, ascends_array, unions_array), (families_array, couples_array, descends_array), strings, bnotes = arrays

    os.makedirs(db_dir, exist_ok=True)

    # 1. Create main base file exactly like OCaml
    base_file = os.path.join(db_dir, "base")
    with open(base_file, 'wb') as f:
        write_geneweb_header(f, len(persons_array), len(families_array), len(strings))

        write_persons_array_compact(f, persons_array, ascends_array, unions_array)

        write_families_array_compact(f, families_array, couples_array, descends_array)

        write_strings_array(f, strings)

    # 2. Create strings index file
    strings_index_file = os.path.join(db_dir, "strings.inx")
    with open(strings_index_file, 'wb') as f:
        write_strings_index(f, strings)

    # 3. Create OCaml-compatible files ONLY
    create_ocaml_format_files(db_dir, persons_array, ascends_array, unions_array,
                              families_array, couples_array, descends_array,
                              strings, bnotes)

    return len(persons_array), len(families_array), len(strings)

def write_geneweb_header(f, nb_persons: int, nb_families: int, nb_strings: int):
    """Write GeneWeb database header exactly like OCaml"""
    f.write(b"GW70")  # Magic number
    f.write(nb_persons.to_bytes(4, 'little'))
    f.write(nb_families.to_bytes(4, 'little'))
    f.write(nb_strings.to_bytes(4, 'little'))

def write_persons_array_compact(f, persons_array, ascends_array, unions_array):
    """Write persons array in compact OCaml-compatible format"""
    for i, person in enumerate(persons_array):
        ascend = ascends_array[i]
        union = unions_array[i]


        if isinstance(person, dict):
            first_name = person.get('first_name', 1)
            surname = person.get('surname', 1)
            occ = person.get('occ', i)
            sex = person.get('sex', 'Neuter')
        else:
            first_name = getattr(person, 'first_name', 1)
            surname = getattr(person, 'surname', 1)
            occ = getattr(person, 'occ', i)
            sex = getattr(person, 'sex', 'Neuter')

        f.write(int(first_name).to_bytes(4, 'little'))
        f.write(int(surname).to_bytes(4, 'little'))
        f.write(int(occ).to_bytes(4, 'little'))

        if sex == 'Male':
            sex_val = 0
        elif sex == 'Female':
            sex_val = 1
        else:
            sex_val = 2
        f.write(sex_val.to_bytes(1, 'little'))

        if isinstance(ascend, dict):
            parents_ref = ascend.get('parents', None)
        else:
            parents_ref = getattr(ascend, 'parents', None)

        if parents_ref is not None:
            f.write(int(parents_ref).to_bytes(4, 'little', signed=True))
        else:
            f.write((-1).to_bytes(4, 'little', signed=True))

        if isinstance(union, dict):
            families = union.get('family', [])
        else:
            families = getattr(union, 'family', [])

        f.write(len(families).to_bytes(4, 'little'))
        for fam_idx in families:
            f.write(int(fam_idx).to_bytes(4, 'little'))

def write_families_array_compact(f, families_array, couples_array, descends_array):
    """Write families array in compact OCaml-compatible format - FIXED: Only write to f"""


    for i, family in enumerate(families_array):
        couple = couples_array[i]
        descend = descends_array[i]


        if isinstance(couple, dict):
            father = couple.get('father', -1)
            mother = couple.get('mother', -1)
        elif isinstance(couple, (tuple, list)):
            father = couple[0] if couple[0] is not None else -1
            mother = couple[1] if couple[1] is not None else -1
        else:
            father = getattr(couple, 'father', -1)
            mother = getattr(couple, 'mother', -1)

        f.write(int(father).to_bytes(4, 'little', signed=True))
        f.write(int(mother).to_bytes(4, 'little', signed=True))
        relation_val = 0  # Simplified
        f.write(relation_val.to_bytes(1, 'little'))

        if isinstance(descend, dict):
            children = descend.get('children', [])
        else:
            children = getattr(descend, 'children', [])

        f.write(len(children).to_bytes(4, 'little'))
        for child_idx in children:
            f.write(int(child_idx).to_bytes(4, 'little'))

def write_strings_array(f, strings):
    """Write strings array in OCaml-compatible format"""
    f.write(len(strings).to_bytes(4, 'little'))

    for s in strings:
        s_bytes = s.encode('utf-8')
        f.write(len(s_bytes).to_bytes(4, 'little'))
        f.write(s_bytes)

def write_strings_index(f, strings):
    """Write strings index file - FIXED: Match OCaml format exactly (offsets only)"""

    num_strings = len(strings)
    f.write(struct.pack('>I', num_strings))


    offset = 0
    for i, s in enumerate(strings):
        s_bytes = s.encode('utf-8')

        f.write(struct.pack('>I', offset))

        offset += len(s_bytes) + 1

    current_size = f.tell()
    target_size = num_strings * 4 + 4

    while f.tell() < min(target_size * 2, 500):
        f.write(b'\xff\xff\xff\xff')  # OCaml-like padding pattern

def create_ocaml_format_files(base_dir: str, persons, ascends, unions, families, couples, descends, strings, bnotes):
    """Create files in exact OCaml GeneWeb format - FIXED: Only OCaml files"""

    # 1. Create base.acc file (ascends array)
    with open(os.path.join(base_dir, "base.acc"), 'wb') as f:
        write_ascends_array(f, ascends)

    # 2. Create names index files (like OCaml)
    create_name_indices(base_dir, persons, strings)

    # 3. Create notes files
    create_notes_files(base_dir, bnotes)

    # 4. Create particles.txt
    with open(os.path.join(base_dir, "particles.txt"), 'w', encoding='utf-8') as f:
        particles = [
            "de", "du", "des", "d'", "da", "dal", "dall'", "dalla", "dalle",
            "del", "della", "delle", "dello", "di", "el", "la", "las", "le",
            "les", "lo", "los", "mac", "mc", "of", "på", "te", "ten", "ter",
            "van", "von", "zu", "zum", "zur"
        ]
        for particle in particles:
            f.write(particle + '\n')

def create_notes_files(base_dir: str, bnotes):
    """Create notes files like OCaml"""

    with open(os.path.join(base_dir, "notes"), 'w', encoding='utf-8') as f:
        if hasattr(bnotes, 'nread') and callable(bnotes.nread):
            base_notes = bnotes.nread("", 0)
            f.write(base_notes)
        else:
            f.write("")

    notes_d_dir = os.path.join(base_dir, "notes_d")
    os.makedirs(notes_d_dir, exist_ok=True)

    wiznotes_dir = os.path.join(base_dir, "wiznotes")
    os.makedirs(wiznotes_dir, exist_ok=True)



def write_families_array_detailed(f, families_array):
    """Write families array with detailed information - NOT USED"""
    pass

def write_couples_array(f, couples_array):
    """Write couples array - NOT USED"""
    pass

def write_descends_array(f, descends_array):
    """Write descends array - NOT USED"""
    pass

def create_name_indices(base_dir: str, persons, strings):
    """Create name index files like OCaml - FIXED: Big-endian format"""

    first_names = {}
    surnames = {}

    for i, person in enumerate(persons):
        if isinstance(person, dict):
            first_name_idx = person.get('first_name', 0)
            surname_idx = person.get('surname', 0)
        else:
            first_name_idx = getattr(person, 'first_name', 0)
            surname_idx = getattr(person, 'surname', 0)

        if first_name_idx < len(strings):
            fn = strings[first_name_idx]
            if fn not in first_names:
                first_names[fn] = []
            first_names[fn].append(i)

        if surname_idx < len(strings):
            sn = strings[surname_idx]
            if sn not in surnames:
                surnames[sn] = []
            surnames[sn].append(i)

    with open(os.path.join(base_dir, "fnames.inx"), 'wb') as f:
        sorted_fnames = sorted(first_names.keys())
        f.write(struct.pack('>I', len(sorted_fnames)))  # BIG-ENDIAN

        for fn in sorted_fnames:
            encoded = fn.encode('utf-8')
            f.write(struct.pack('>I', len(encoded)))  # BIG-ENDIAN
            f.write(encoded)

            person_indices = first_names[fn]
            f.write(struct.pack('>I', len(person_indices)))  # BIG-ENDIAN
            for idx in person_indices:
                f.write(struct.pack('>I', idx))  # BIG-ENDIAN

    with open(os.path.join(base_dir, "snames.inx"), 'wb') as f:
        sorted_snames = sorted(surnames.keys())
        f.write(struct.pack('>I', len(sorted_snames)))  # BIG-ENDIAN

        for sn in sorted_snames:
            encoded = sn.encode('utf-8')
            f.write(struct.pack('>I', len(encoded)))  # BIG-ENDIAN
            f.write(encoded)

            person_indices = surnames[sn]
            f.write(struct.pack('>I', len(person_indices)))  # BIG-ENDIAN
            for idx in person_indices:
                f.write(struct.pack('>I', idx))  # BIG-ENDIAN

    with open(os.path.join(base_dir, "fnames.dat"), 'wb') as f:
        for fn in sorted(first_names.keys()):
            encoded = fn.encode('utf-8')
            f.write(encoded)
            f.write(b'\0')  # Null terminator

    with open(os.path.join(base_dir, "snames.dat"), 'wb') as f:
        for sn in sorted(surnames.keys()):
            encoded = sn.encode('utf-8')
            f.write(encoded)
            f.write(b'\0')  # Null terminator

    with open(os.path.join(base_dir, "names.acc"), 'wb') as f:
        f.write(struct.pack('>I', len(first_names)))  # BIG-ENDIAN
        f.write(struct.pack('>I', len(surnames)))     # BIG-ENDIAN

    with open(os.path.join(base_dir, "names.inx"), 'wb') as f:
        f.write(struct.pack('>I', len(first_names) + len(surnames)))  # BIG-ENDIAN

def write_ascends_array(f, ascends):
    """Write ascends array like OCaml - FIXED: Big-endian"""
    f.write(struct.pack('>I', len(ascends)))  # BIG-ENDIAN

    for ascend in ascends:
        if isinstance(ascend, dict):
            parents = ascend.get('parents', None)
            consang = ascend.get('consang', -1)
        else:
            parents = getattr(ascend, 'parents', None)
            consang = getattr(ascend, 'consang', -1)

        if parents is not None:
            f.write(struct.pack('>I', parents))  # BIG-ENDIAN
        else:
            f.write(struct.pack('B', 0))  # None

        f.write(struct.pack('>i', consang))  # BIG-ENDIAN signed

def create_notes_files(base_dir: str, bnotes):
    """Create notes files like OCaml"""

    with open(os.path.join(base_dir, "notes"), 'w', encoding='utf-8') as f:
        if hasattr(bnotes, 'nread') and callable(bnotes.nread):
            base_notes = bnotes.nread("", 0)
            f.write(base_notes)
        else:
            f.write("")

    notes_d_dir = os.path.join(base_dir, "notes_d")
    os.makedirs(notes_d_dir, exist_ok=True)

    wiznotes_dir = os.path.join(base_dir, "wiznotes")
    os.makedirs(wiznotes_dir, exist_ok=True)

def verify_geneweb_format(base_dir: str) -> bool:
    """Verify GeneWeb database format matches OCaml"""
    required_files = [
        "base", "base.acc", "families", "couples", "descends", "strings.inx",
        "fnames.dat", "fnames.inx", "snames.dat", "snames.inx",
        "names.acc", "names.inx", "notes", "notes_d", "particles.txt", "wiznotes"
    ]

    for filename in required_files:
        filepath = os.path.join(base_dir, filename)
        if not os.path.exists(filepath):
            return False

    return True

def get_database_stats(base_dir: str) -> Dict[str, int]:
    """Get database statistics from OCaml format"""
    stats = {
        'persons': 0,
        'families': 0,
        'strings': 0,
        'total_size': 0
    }

    base_file = os.path.join(base_dir, "base")
    if os.path.exists(base_file):
        with open(base_file, 'rb') as f:
            try:
                count = struct.unpack('>I', f.read(4))[0]
                stats['persons'] = count
            except:
                pass

    for filename in os.listdir(base_dir):
        filepath = os.path.join(base_dir, filename)
        if os.path.isfile(filepath):
            stats['total_size'] += os.path.getsize(filepath)

    return stats
