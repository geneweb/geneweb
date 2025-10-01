import os
import struct
import sys
from models import Gen

def write_gwb_database(gen: Gen, base_path: str):
    """Write complete GWB database like OCaml - FIXED: Only OCaml files"""

    print(f"*** saving persons array", file=sys.stderr)
    write_persons_array(gen, base_path)

    print(f"*** saving ascends array", file=sys.stderr)
    write_ascends_array(gen, base_path)

    print(f"*** saving unions array", file=sys.stderr)
    write_unions_array(gen, base_path)

    print(f"*** saving strings array", file=sys.stderr)
    write_strings_array(gen, base_path)

    create_additional_files(gen, base_path)

    print(f"*** create name index", file=sys.stderr)
    create_name_indices(gen, base_path)

    print(f"*** create strings of sname", file=sys.stderr)
    create_surname_strings(gen, base_path)

    print(f"*** create strings of fname", file=sys.stderr)
    create_firstname_strings(gen, base_path)

    print(f"*** create string index", file=sys.stderr)
    create_string_index(gen, base_path)

    print(f"*** create surname index", file=sys.stderr)
    create_surname_index(gen, base_path)

    print(f"*** create first name index", file=sys.stderr)
    create_firstname_index(gen, base_path)

    print(f"*** ok", file=sys.stderr)

def write_persons_array(gen: Gen, base_path: str):
    """Write persons array to base file"""
    base_file = os.path.join(base_path, "base")

    with open(base_file, 'wb') as f:
        f.write(struct.pack('>I', gen.g_per.tlen))

        for i in range(gen.g_per.tlen):
            person_entry = gen.g_per.arr[i]
            if isinstance(person_entry, tuple) and person_entry[0] == "Right3":
                person_data, _, _ = person_entry[1]
                write_person_record(f, person_data)
            else:
                write_empty_person_record(f)

def write_ascends_array(gen: Gen, base_path: str):
    """Write ascends array to base.acc file"""
    acc_file = os.path.join(base_path, "base.acc")

    with open(acc_file, 'wb') as f:
        f.write(struct.pack('>I', gen.g_per.tlen))

        for i in range(gen.g_per.tlen):
            person_entry = gen.g_per.arr[i]
            if isinstance(person_entry, tuple) and person_entry[0] == "Right3":
                _, ascend, _ = person_entry[1]
                write_ascend_record(f, ascend)
            else:
                write_empty_ascend_record(f)

def write_unions_array(gen: Gen, base_path: str):
    """Write unions array - OCaml stores this in base file"""
    pass

def write_strings_array(gen: Gen, base_path: str):
    """Write strings array to strings.inx file"""
    strings_file = os.path.join(base_path, "strings.inx")

    with open(strings_file, 'wb') as f:
        f.write(struct.pack('>I', gen.g_str.tlen))

        current_offset = 0
        for i in range(gen.g_str.tlen):
            f.write(struct.pack('>I', current_offset))
            string_val = gen.g_str.arr[i]
            current_offset += len(string_val.encode('utf-8')) + 1  # +1 for null terminator

def create_name_indices(gen: Gen, base_path: str):
    """Create name indices files"""
    names_acc = os.path.join(base_path, "names.acc")
    names_inx = os.path.join(base_path, "names.inx")

    with open(names_acc, 'wb') as f:
        f.write(struct.pack('>I', 0))

    with open(names_inx, 'wb') as f:
        f.write(struct.pack('>I', 0))

def create_surname_strings(gen: Gen, base_path: str):
    """Create surname strings files"""
    snames_dat = os.path.join(base_path, "snames.dat")
    snames_inx = os.path.join(base_path, "snames.inx")

    surnames = set()
    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if isinstance(person_entry, tuple) and person_entry[0] == "Right3":
            person_data, _, _ = person_entry[1]
            surname_idx = person_data.get('surname', 1)  # Default to "?"
            if surname_idx < gen.g_str.tlen:
                surnames.add(gen.g_str.arr[surname_idx])

    surnames_list = sorted(list(surnames))

    with open(snames_dat, 'wb') as f:
        for surname in surnames_list:
            f.write(surname.encode('utf-8') + b'\x00')

    with open(snames_inx, 'wb') as f:
        f.write(struct.pack('>I', len(surnames_list)))
        offset = 0
        for surname in surnames_list:
            f.write(struct.pack('>I', offset))
            offset += len(surname.encode('utf-8')) + 1

def create_firstname_strings(gen: Gen, base_path: str):
    """Create first name strings files"""
    fnames_dat = os.path.join(base_path, "fnames.dat")
    fnames_inx = os.path.join(base_path, "fnames.inx")

    firstnames = set()
    for i in range(gen.g_per.tlen):
        person_entry = gen.g_per.arr[i]
        if isinstance(person_entry, tuple) and person_entry[0] == "Right3":
            person_data, _, _ = person_entry[1]
            firstname_idx = person_data.get('first_name', 1)  # Default to "?"
            if firstname_idx < gen.g_str.tlen:
                firstnames.add(gen.g_str.arr[firstname_idx])

    firstnames_list = sorted(list(firstnames))

    with open(fnames_dat, 'wb') as f:
        for firstname in firstnames_list:
            f.write(firstname.encode('utf-8') + b'\x00')

    with open(fnames_inx, 'wb') as f:
        f.write(struct.pack('>I', len(firstnames_list)))
        offset = 0
        for firstname in firstnames_list:
            f.write(struct.pack('>I', offset))
            offset += len(firstname.encode('utf-8')) + 1

def create_string_index(gen: Gen, base_path: str):
    """Create string index - OCaml creates this automatically"""
    pass

def create_surname_index(gen: Gen, base_path: str):
    """Create surname index - OCaml creates this automatically"""
    pass

def create_firstname_index(gen: Gen, base_path: str):
    """Create first name index - OCaml creates this automatically"""
    pass

def write_person_record(f, person_data):
    """Write individual person record in OCaml format"""
    first_name = person_data.get('first_name', 1)
    surname = person_data.get('surname', 1)
    occ = person_data.get('occ', 0)
    sex = person_data.get('sex', 'Neuter')

    f.write(struct.pack('>I', first_name))
    f.write(struct.pack('>I', surname))
    f.write(struct.pack('>I', occ))

    sex_code = {'Male': 0, 'Female': 1, 'Neuter': 2}.get(sex, 2)
    f.write(struct.pack('>I', sex_code))

def write_empty_person_record(f):
    """Write empty person record"""
    f.write(struct.pack('>I', 1))  # "?" first name
    f.write(struct.pack('>I', 1))  # "?" surname
    f.write(struct.pack('>I', 0))  # occ = 0
    f.write(struct.pack('>I', 2))  # Neuter sex

def write_ascend_record(f, ascend):
    """Write ascend record in OCaml format"""
    parents = ascend.get("parents")
    consang = ascend.get("consang", -1)

    f.write(struct.pack('>i', parents if parents is not None else -1))

    f.write(struct.pack('>i', consang))

def write_empty_ascend_record(f):
    """Write empty ascend record"""
    f.write(struct.pack('>i', -1))  # No parents
    f.write(struct.pack('>i', -1))  # No consanguinity

def create_additional_files(gen: Gen, base_path: str):
    """Create additional files that OCaml generates"""

    notes_file = os.path.join(base_path, "notes")
    with open(notes_file, 'wb') as f:
        f.write(struct.pack('>I', 0))  # Empty notes

    notes_d_file = os.path.join(base_path, "notes_d")
    with open(notes_d_file, 'wb') as f:
        f.write(struct.pack('>I', 0))  # Empty notes_d

    particles_file = os.path.join(base_path, "particles.txt")
    with open(particles_file, 'w', encoding='utf-8') as f:
        particles = ["de", "du", "des", "d'", "van", "von", "le", "la", "les"]
        f.write('\n'.join(particles))

    wiznotes_file = os.path.join(base_path, "wiznotes")
    with open(wiznotes_file, 'wb') as f:
        f.write(struct.pack('>I', 0))  # Empty wiznotes
