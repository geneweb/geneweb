from typing import List, Dict, Tuple, Optional, Any, Union
from models import Gen, Record, Choice3Tab, StringTab
from charset import convert_gedcom_string
from utils import strip_spaces

def create_gen() -> Gen:
    """Create new Gen structure"""
    return Gen()

def make_arrays(gen: Gen) -> Tuple:
    """Convert Gen structure to arrays - FIXED: Handle StringTab properly"""

    strings = []
    for i in range(gen.g_str.tlen):
        strings.append(gen.g_str.arr[i])

    persons_list = []
    ascends_list = []
    unions_list = []

    for i in range(gen.g_per.tlen):
        entry = gen.g_per.arr[i]
        if isinstance(entry, tuple) and entry[0] == "Right3":
            person, ascend, union = entry[1]
            persons_list.append(person)
            ascends_list.append(ascend)
            unions_list.append(union)
        else:
            person = {
                'first_name': 1,  # "?"
                'surname': 1,     # "?"
                'occ': i,
                'sex': 'Neuter',
                'key_index': i
            }
            ascend = {"parents": None, "consang": -1}
            union = {"family": []}
            persons_list.append(person)
            ascends_list.append(ascend)
            unions_list.append(union)

    families_list = []
    couples_list = []
    descends_list = []

    for i in range(gen.g_fam.tlen):
        entry = gen.g_fam.arr[i]
        if isinstance(entry, tuple) and entry[0] == "Right3":
            family, couple, descend = entry[1]
            families_list.append(family)
            couples_list.append(couple)
            descends_list.append(descend)
        else:
            family = {
                'fam_index': i,
                'relation': 'Married'
            }
            couple = (-1, -1)
            descend = {"children": []}
            families_list.append(family)
            couples_list.append(couple)
            descends_list.append(descend)

    persons = (persons_list, ascends_list, unions_list)
    families = (families_list, couples_list, descends_list)

    class SimpleNotes:
        def __init__(self, base_notes: str):
            self.base_notes = base_notes

        def nread(self, s: str, pos: int) -> str:
            return self.base_notes if s == "" else ""

    bnotes = SimpleNotes(gen.g_bnot)

    return persons, families, strings, bnotes

def make_subarrays(arrays: Tuple) -> Tuple:
    """Convert arrays to subarrays format - FIXED: Handle tuple format correctly"""

    persons_data, families_data, strings, bnotes = arrays

    persons_list, ascends_list, unions_list = persons_data

    families_list, couples_list, descends_list = families_data

    persons_array = persons_list
    ascends_array = ascends_list
    unions_array = unions_list

    families_array = families_list
    couples_array = couples_list
    descends_array = descends_list

    strings_array = strings

    persons = (persons_array, ascends_array, unions_array)
    families = (families_array, couples_array, descends_array)

    return persons, families, strings_array, bnotes

def _process_persons_array(persons_list):
    """Process persons array - FIXED: Handle list format"""
    processed_persons = []

    for person in persons_list:
        processed_persons.append(person)

    return processed_persons

def _process_families_array(families_list):
    """Process families array - FIXED: Handle list format"""
    processed_families = []

    for family in families_list:
        processed_families.append(family)

    return processed_families

def return_persons_families_strings(persons, families, strings):
    """Return the main arrays - exact OCaml signature"""
    return persons, families, strings

def finish_base(arrays: Tuple):
    """Finish base processing - exact OCaml logic"""
    if len(arrays) == 4:
        persons, families, strings, bnotes = arrays
    else:
        persons, families = arrays[:2]
        strings = []
        bnotes = {}

    persons_arr, ascends_arr, unions_arr = persons
    families_arr, couples_arr, descends_arr = families

    for i, descend in enumerate(descends_arr):
        children = descend.get("children", [])
        if children:
            children.sort(key=lambda child_idx: _get_birth_sort_key(persons_arr, child_idx))
            descends_arr[i] = {**descend, "children": children}

    for i, union in enumerate(unions_arr):
        families = union.get("family", [])
        if families:
            families.sort(key=lambda fam_idx: _get_marriage_sort_key(families_arr, fam_idx))
            unions_arr[i] = {**union, "family": families}

    for i, person in enumerate(persons_arr):
        ascend = ascends_arr[i]
        union = unions_arr[i]

        if (ascend.get("parents") is not None or
            len(union.get("family", [])) > 0 or
            person.get("notes", 0) != 0):  # string_empty = 0

            first_name_idx = person.get("first_name", 0)
            surname_idx = person.get("surname", 0)
            occ = person.get("occ", 0)

            if first_name_idx < len(strings) and strings[first_name_idx] == "?":
                first_name_idx = 2  # string_x
                occ = i

            if surname_idx < len(strings) and strings[surname_idx] == "?":
                surname_idx = 2  # string_x
                occ = i

            persons_arr[i] = {
                **person,
                "first_name": first_name_idx,
                "surname": surname_idx,
                "occ": occ
            }

def _get_birth_sort_key(persons_arr, child_idx):
    """Get sort key for birth date (simplified)"""
    if child_idx < len(persons_arr):
        person = persons_arr[child_idx]
        return person.get("birth", 0)
    return 0

def _get_marriage_sort_key(families_arr, fam_idx):
    """Get sort key for marriage date (simplified)"""
    if fam_idx < len(families_arr):
        family = families_arr[fam_idx]
        return family.get("marriage", 0)
    return 0

def assume_tab(tab: Choice3Tab, none_value):
    """Expand array if needed - exact OCaml logic"""
    if tab.tlen >= len(tab.arr):
        new_len = 2 * len(tab.arr) + 1
        new_arr = [none_value] * new_len
        new_arr[:len(tab.arr)] = tab.arr
        tab.arr = new_arr

def per_index(gen: Gen, lab: str) -> int:
    """Get person index, creating if needed - exact OCaml logic"""
    lab = extract_addr(lab)

    if lab in gen.g_hper:
        return gen.g_hper[lab]

    i = gen.g_per.tlen
    assume_tab(gen.g_per, ("Left3", ""))
    gen.g_per.arr[i] = ("Left3", lab)
    gen.g_per.tlen += 1
    gen.g_hper[lab] = i

    output_pindex(i, lab)

    return i

def fam_index(gen: Gen, lab: str) -> int:
    """Get family index, creating if needed - exact OCaml logic"""
    lab = extract_addr(lab)

    if lab in gen.g_hfam:
        return gen.g_hfam[lab]

    i = gen.g_fam.tlen
    assume_tab(gen.g_fam, ("Left3", ""))
    gen.g_fam.arr[i] = ("Left3", lab)
    gen.g_fam.tlen += 1
    gen.g_hfam[lab] = i

    return i

def add_string(gen: Gen, s: str) -> int:
    """Add string to string table - exact OCaml logic"""
    if s in gen.g_hstr:
        return gen.g_hstr[s]

    i = gen.g_str.tlen
    assume_tab_str(gen.g_str, "")
    gen.g_str.arr[i] = s
    gen.g_str.tlen += 1
    gen.g_hstr[s] = i

    return i

def assume_tab_str(tab: StringTab, none_value: str):
    """Expand string array if needed"""
    if tab.tlen >= len(tab.arr):
        new_len = 2 * len(tab.arr) + 1
        new_arr = [none_value] * new_len
        new_arr[:len(tab.arr)] = tab.arr
        tab.arr = new_arr

def extract_addr(addr: str) -> str:
    """Extract address from GEDCOM identifier"""
    if addr and addr.startswith('@'):
        try:
            r = addr.index('@', 1)
            return addr[:r+1]
        except ValueError:
            return addr
    return addr

def output_pindex(i: int, lab: str):
    """Output person index tracking"""
    try:
        from ged2gwb import track_ged2gw_id
        if track_ged2gw_id:
            print(f"IDGED2IDPERS {i} {lab}")
    except ImportError:
        pass

def unknown_per(i: int, sex: str) -> Tuple[Dict, Dict, Dict]:
    """Create unknown person - exact OCaml logic"""
    person = {
        "first_name": 1,  # string_quest
        "surname": 1,     # string_quest
        "occ": i,
        "public_name": 0, # string_empty
        "image": 0,       # string_empty
        "qualifiers": [],
        "aliases": [],
        "first_names_aliases": [],
        "surnames_aliases": [],
        "titles": [],
        "rparents": [],
        "related": [],
        "occupation": 0,  # string_empty
        "sex": sex,
        "access": "IfTitles",
        "birth": None,
        "birth_place": 0, # string_empty
        "birth_note": 0,  # string_empty
        "birth_src": 0,   # string_empty
        "baptism": None,
        "baptism_place": 0, # string_empty
        "baptism_note": 0,  # string_empty
        "baptism_src": 0,   # string_empty
        "death": "DontKnowIfDead",
        "death_place": 0,   # string_empty
        "death_note": 0,    # string_empty
        "death_src": 0,     # string_empty
        "burial": "UnknownBurial",
        "burial_place": 0,  # string_empty
        "burial_note": 0,   # string_empty
        "burial_src": 0,    # string_empty
        "pevents": [],
        "notes": 0,         # string_empty
        "psources": 0,      # string_empty
        "key_index": i
    }

    ascend = {
        "parents": None,
        "consang": -1
    }

    union = {
        "family": []
    }

    return person, ascend, union

def unknown_fam(gen: Gen, i: int) -> Tuple[Dict, Tuple, Dict]:
    """Create unknown family - exact OCaml logic"""
    father_idx = phony_per(gen, "Male")
    mother_idx = phony_per(gen, "Female")

    family = {
        "marriage": None,
        "marriage_place": 0,  # string_empty
        "marriage_note": 0,   # string_empty
        "marriage_src": 0,    # string_empty
        "witnesses": [],
        "relation": "Married",
        "divorce": "NotDivorced",
        "fevents": [],
        "comment": 0,         # string_empty
        "origin_file": 0,     # string_empty
        "fsources": 0,        # string_empty
        "fam_index": i
    }

    couple = (father_idx, mother_idx)

    descend = {
        "children": []
    }

    return family, couple, descend

def phony_per(gen: Gen, sex: str) -> int:
    """Create phony person - exact OCaml logic"""
    i = gen.g_per.tlen
    person, ascend, union = unknown_per(i, sex)
    assume_tab(gen.g_per, ("Left3", ""))
    gen.g_per.tlen += 1
    gen.g_per.arr[i] = ("Right3", (person, ascend, union))
    return i
