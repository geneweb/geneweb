import struct
from typing import List, Dict, Any, Tuple

from lib.db.unmarshall.basic import (
    read_cdate,
    read_ocaml_int,
    read_ocaml_string,
    read_ocaml_list,
)


# def read_ocaml_string(data: bytes, offset: int) -> Tuple[str, int]:
#     length = struct.unpack_from("!I", data, offset)[0]
#     offset += 4
#     value = data[offset : offset + length].decode("utf-8")
#     return value, offset + length


# def read_ocaml_int(data: bytes, offset: int) -> Tuple[int, int]:
#     return struct.unpack_from("!I", data, offset)[0], offset + 4


def read_ocaml_string_list(data: bytes, offset: int) -> Tuple[List[str], int]:
    # size, offset = read_ocaml_int(data, offset)
    # string_list = []
    # for _ in range(size):
    #     string_value, offset = read_ocaml_string(data, offset)
    #     string_list.append(string_value)
    # return string_list, offset
    return read_ocaml_list(data, offset, read_ocaml_string)


# type 'string gen_title = {
#   t_name : 'string gen_title_name;
#   t_ident : 'string;
#   t_place : 'string;
#   t_date_start : cdate;
#   t_date_end : cdate;
#   t_nth : int;
# }
def read_gen_title(data: bytes, offset: int) -> Tuple[Any, int]:
    title = {}
    title["t_name"], offset = read_ocaml_string(data, offset)
    title["t_ident"], offset = read_ocaml_string(data, offset)
    title["t_place"], offset = read_ocaml_string(data, offset)
    title["t_date_start"], offset = read_cdate(data, offset)
    title["t_date_end"], offset = read_cdate(data, offset)
    title["t_nth"], offset = read_ocaml_int(data, offset)
    return title, offset


# type ('person, 'string) gen_relation = {
#   r_type : relation_type;
#   r_fath : 'person option;
#   r_moth : 'person option;
#   r_sources : 'string;
# }
def read_gen_relation(data: bytes, offset: int) -> Tuple[Any, int]:
    relation = {}
    relation["r_type"], offset = read_ocaml_int(data, offset)
    relation["r_fath"], offset = read_person_option(data, offset)
    relation["r_moth"], offset = read_person_option(data, offset)
    relation["r_sources"], offset = read_ocaml_string(data, offset)
    return relation, offset


# type 'a option = None | Some of 'a
def read_person_option(data: bytes, offset: int) -> Tuple[Any, int]:
    is_some, offset = read_ocaml_int(data, offset)
    if is_some:
        person, offset = read_person(data, offset)
        return person, offset
    else:
        return None, offset


# type death =
#   | NotDead
#   | Death of death_reason * cdate
#   | DeadYoung
#   | DeadDontKnowWhen
#   | DontKnowIfDead
#   | OfCourseDead
def read_death(data: bytes, offset: int) -> Tuple[Any, int]:
    # Implement reading logic for death type
    death_type, offset = read_ocaml_int(data, offset)
    match death_type:
        case 0:  # NotDead
            return {"NotDead": None}, offset
        case 1:  # Death
            reason, offset = read_death_reason(data, offset)
            date, offset = read_cdate(data, offset)
            return {"Death": {"reason": reason, "date": date}}, offset
        case 2:  # DeadYoung
            return {"DeadYoung": None}, offset
        case 3:  # DeadDontKnowWhen
            return {"DeadDontKnowWhen": None}, offset
        case 4:  # DontKnowIfDead
            return {"DontKnowIfDead": None}, offset
        case 5:  # OfCourseDead
            return {"OfCourseDead": None}, offset
        case _:
            raise ValueError(f"Unknown death type: {death_type}")


# type death_reason = Killed | Murdered | Executed | Disappeared | Unspecified
def read_death_reason(data: bytes, offset: int) -> Tuple[str, int]:
    reason_type, offset = read_ocaml_int(data, offset)
    match reason_type:
        case 0:
            return "Killed", offset
        case 1:
            return "Murdered", offset
        case 2:
            return "Executed", offset
        case 3:
            return "Disappeared", offset
        case 4:
            return "Unspecified", offset
        case _:
            raise ValueError(f"Unknown death reason type: {reason_type}")


# type burial =
#   | UnknownBurial
#   | Buried of cdate
#   | Cremated of cdate
def read_burial(data: bytes, offset: int) -> Tuple[Any, int]:
    # Implement reading logic for burial type
    burial_type, offset = read_ocaml_int(data, offset)
    match burial_type:
        case 0:  # UnknownBurial
            return {"UnknownBurial": None}, offset
        case 1:  # Buried
            date, offset = read_cdate(data, offset)
            return {"Buried": date}, offset
        case 2:  # Cremated
            date, offset = read_cdate(data, offset)
            return {"Cremated": date}, offset
        case _:
            raise ValueError(f"Unknown burial type: {burial_type}")


# type ('person, 'string) gen_pers_event = {
#   epers_name : 'string gen_pers_event_name;
#   epers_date : cdate;
#   epers_place : 'string;
#   epers_reason : 'string;
#   epers_note : 'string;
#   epers_src : 'string;
#   epers_witnesses : ('person * witness_kind) array;
# }
def read_gen_pers_event(data: bytes, offset: int) -> Tuple[Any, int]:
    # Implement reading logic for gen_pers_event type
    event = {}
    event["epers_name"], offset = read_ocaml_string(data, offset)
    event["epers_date"], offset = read_cdate(data, offset)
    event["epers_place"], offset = read_ocaml_string(data, offset)
    event["epers_reason"], offset = read_ocaml_string(data, offset)
    event["epers_note"], offset = read_ocaml_string(data, offset)
    event["epers_src"], offset = read_ocaml_string(data, offset)
    event["epers_witnesses"], offset = read_witnesses(data, offset)
    return event, offset


# epers_witnesses : ('person * witness_kind) array;


# type witness_kind =
#   | Witness
#   | Witness_GodParent
#   | Witness_CivilOfficer
#   | Witness_ReligiousOfficer
#   | Witness_Informant
#   | Witness_Attending
#   | Witness_Mentioned
#   | Witness_Other
def read_witnesses(data: bytes, offset: int) -> Tuple[List[Any], int]:
    size, offset = read_ocaml_int(data, offset)
    witnesses = []
    for _ in range(size):
        person, offset = read_person(data, offset)
        kind, offset = read_ocaml_int(data, offset)
        witnesses.append({"person": person, "kind": kind})
    return witnesses, offset


def read_person(data: bytes, offset: int) -> Tuple[Dict[str, Any], int]:
    person = {}
    person["first_name"], offset = read_ocaml_string(data, offset)
    person["surname"], offset = read_ocaml_string(data, offset)
    person["occ"], offset = read_ocaml_int(data, offset)
    person["image"], offset = read_ocaml_string(data, offset)
    person["public_name"], offset = read_ocaml_string(data, offset)
    person["qualifiers"], offset = read_ocaml_list(data, offset, read_ocaml_string)
    person["aliases"], offset = read_ocaml_list(data, offset, read_ocaml_string)
    person["first_names_aliases"], offset = read_ocaml_list(
        data, offset, read_ocaml_string
    )
    person["surnames_aliases"], offset = read_ocaml_list(
        data, offset, read_ocaml_string
    )

    # Read titles (assuming gen_title is a custom type)
    size, offset = read_ocaml_int(data, offset)
    person["titles"] = []
    for _ in range(size):
        title, offset = read_gen_title(data, offset)
        person["titles"].append(title)

    # Read rparents (assuming gen_relation is a custom type)
    size, offset = read_ocaml_int(data, offset)
    person["rparents"] = []
    for _ in range(size):
        relation, offset = read_gen_relation(data, offset)
        person["rparents"].append(relation)

    # Read related persons
    size, offset = read_ocaml_int(data, offset)
    person["related"] = []
    for _ in range(size):
        related_person, offset = read_person(
            data, offset
        )  # Recursive call to read person
        person["related"].append(related_person)

    person["occupation"], offset = read_ocaml_string(data, offset)
    person["sex"], offset = read_ocaml_int(
        data, offset
    )  # Assuming sex is an integer representation
    person["access"], offset = read_ocaml_int(
        data, offset
    )  # Assuming access is an integer representation
    person["birth"], offset = read_cdate(data, offset)
    person["birth_place"], offset = read_ocaml_string(data, offset)
    person["birth_note"], offset = read_ocaml_string(data, offset)
    person["birth_src"], offset = read_ocaml_string(data, offset)
    person["baptism"], offset = read_cdate(data, offset)
    person["baptism_place"], offset = read_ocaml_string(data, offset)
    person["baptism_note"], offset = read_ocaml_string(data, offset)
    person["baptism_src"], offset = read_ocaml_string(data, offset)
    person["death"], offset = read_death(data, offset)
    person["death_place"], offset = read_ocaml_string(data, offset)
    person["death_note"], offset = read_ocaml_string(data, offset)
    person["death_src"], offset = read_ocaml_string(data, offset)
    person["burial"], offset = read_burial(data, offset)
    person["burial_place"], offset = read_ocaml_string(data, offset)
    person["burial_note"], offset = read_ocaml_string(data, offset)
    person["burial_src"], offset = read_ocaml_string(data, offset)
    person["pevents"], offset = read_ocaml_list(data, offset, read_gen_pers_event)
    person["notes"], offset = read_ocaml_string(data, offset)
    person["psources"], offset = read_ocaml_string(data, offset)
    person["key_index"], offset = read_ocaml_int(
        data, offset
    )  # Assuming key_index is an integer

    return person, offset
