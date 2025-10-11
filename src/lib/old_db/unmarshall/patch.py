import struct
from collections import defaultdict
from typing import Any, Callable, Dict, Tuple

from lib.old_db.unmarshall.header import MarshalHeader, caml_parse_header
from .person import read_person
import logging
from .basic import (
    read32u,
    read_ocaml_int,
    read_ocaml_float,
    read_ocaml_string,
    read_ocaml_list,
    read_ocaml_hashmap,
    read_ocaml_optional,
    read_ocaml_record,
)


# type 'family gen_ascend = { parents : 'family option; consang : Adef.fix }
def read_ascend(data, offset):
    """Reads an OCaml 'ascend' type from the given data."""
    parents, offset = read_ocaml_optional(
        data, offset, read_ocaml_int
    )  # Assuming 'family' is represented as int
    consang, offset = read_ocaml_float(data, offset)
    return {"parents": parents, "consang": consang}, offset


# type 'family gen_union = { family : 'family array }
def read_union(data, offset):
    family, offset = read_ocaml_list(data, offset, read_family)
    return {"family": family}, offset


# type ('person, 'ifam, 'string) gen_family = {
#   marriage : cdate;
#   marriage_place : 'string;
#   marriage_note : 'string;
#   marriage_src : 'string;
#   witnesses : 'person array;
#   relation : relation_kind;
#   divorce : divorce;
#   fevents : ('person, 'string) gen_fam_event list;
#   comment : 'string;
#   origin_file : 'string; (* .gw filename where family is defined *)
#   fsources : 'string;
#   fam_index : 'ifam;
# }
def read_family(data, offset):
    marriage, offset = read_ocaml_int(data, offset)  # Assuming cdate is int
    marriage_place, offset = read_ocaml_string(data, offset)
    marriage_note, offset = read_ocaml_string(data, offset)
    marriage_src, offset = read_ocaml_string(data, offset)
    witnesses, offset = read_ocaml_list(data, offset, read_person)
    relation, offset = read_ocaml_int(data, offset)  # Assuming relation_kind is int
    divorce, offset = read_ocaml_int(data, offset)  # Assuming divorce is int
    # Skipping fevents for simplicity
    comment, offset = read_ocaml_string(data, offset)
    origin_file, offset = read_ocaml_string(data, offset)
    fsources, offset = read_ocaml_string(data, offset)
    fam_index, offset = read_ocaml_int(data, offset)  # Assuming 'ifam' is int
    return {
        "marriage": marriage,
        "marriage_place": marriage_place,
        "marriage_note": marriage_note,
        "marriage_src": marriage_src,
        "witnesses": witnesses,
        "relation": relation,
        "divorce": divorce,
        "comment": comment,
        "origin_file": origin_file,
        "fsources": fsources,
        "fam_index": fam_index,
    }, offset


# type 'person gen_couple = { father : 'person; mother : 'person }
def read_couple(data, offset):
    print(f"couple {offset=}")
    father, offset = read_person(data, offset)
    mother, offset = read_person(data, offset)
    return {"father": father, "mother": mother}, offset


# type 'person gen_descend = { children : 'person array }
def read_descend(data, offset):
    children, offset = read_ocaml_list(data, offset, read_person)
    return {"children": children}, offset


def read_ocaml_marshalled_list(
    data: bytes,
    offset: int,
    item_unmarshal: Callable[[bytes, int], Tuple[Any, int]],
) -> Tuple[list, int]:
    """
    Traverses an OCaml-marshalled list in bytes,
    using the provided item unmarshal function.
    Prints each item.

    Args:
        data (bytes): The marshalled OCaml list.
        offset (int): Starting offset in the data.
        item_unmarshal (function): Function to unmarshal an item from bytes.
    """
    logging.info("Reading OCaml marshalled list")
    logger = logging.getLogger("ocaml_unmarshal")
    logger.setLevel(logging.DEBUG)
    res = []
    while offset < len(data):
        # Check for OCaml empty list tag (0x00)
        if data[offset] == 0x00:
            logger.debug("End of OCaml list reached")
            break  # End of list
        # OCaml cons cell tag is usually 0xFF (for blocks), but actual format may vary
        # Advance offset past the tag (assume 1 byte for simplicity)
        offset += 1
        # Unmarshal item
        logger.debug(f"Unmarshalling item at offset {offset}")
        item, item_len = item_unmarshal(data[offset:], 0)
        logger.debug(f"Item unmarshalled: {item}")
        offset += item_len
        res.append(item)
        logger.debug(f"List so far: {res}")
    return res, offset  # Return empty list and final offset for compatibility


def read_ocaml_marshalled_hashtable(
    data: bytes,
    offset: int,
    key_unmarshal: Callable[[bytes, int], Tuple[Any, int]],
    value_unmarshal: Callable[[bytes, int], Tuple[Any, int]],
) -> Tuple[Dict, int]:
    """
    Traverses an OCaml-marshalled list of key-value pairs in bytes,
    using provided key and value unmarshal functions.
    Prints each key-value pair.

    Args:
        data (bytes): The marshalled OCaml list.
        offset (int): Starting offset in the data.
        key_unmarshal (function): Function to unmarshal a key from bytes.
        value_unmarshal (function): Function to unmarshal a value from bytes.
    """
    logging.info("Reading OCaml marshalled hashtable")
    logger = logging.getLogger("ocaml_unmarshal")
    res = {}
    while offset < len(data):
        # Check for OCaml empty list tag (0x00)
        if data[offset] == 0x00:
            logger.debug("End of OCaml list reached")
            break  # End of list
        # OCaml cons cell tag is usually 0xFF (for blocks), but actual format may vary
        # Advance offset past the tag (assume 1 byte for simplicity)
        offset += 1
        # Unmarshal key
        logger.debug(f"Unmarshalling key at offset {offset}")
        key, key_len = key_unmarshal(data, offset)
        logger.debug(f"Key unmarshalled: {key}")
        offset += key_len
        # Unmarshal value
        value, value_len = value_unmarshal(data, offset)
        offset += value_len
        logger.debug(f"Key: {key}, Value: {value}")
        res[key] = value
    return res, offset  # Return empty dict and final offset for compatibility


# type patches_ht = {
#   h_person : int ref * (int, person) Hashtbl.t;
#   h_ascend : int ref * (int, ascend) Hashtbl.t;
#   h_union : int ref * (int, union) Hashtbl.t;
#   h_family : int ref * (int, family) Hashtbl.t;
#   h_couple : int ref * (int, couple) Hashtbl.t;
#   h_descend : int ref * (int, descend) Hashtbl.t;
#   h_string : int ref * (int, string) Hashtbl.t;
#   h_name : (int, int list) Hashtbl.t;
# }
def read_patches_ht(data, loglevel=logging.DEBUG) -> Dict[str, Any]:
    offset = 0
    patches = {}

    logging.basicConfig(
        level=loglevel,
        format=f"\033[34;2m%(name)s\t\033[0;34;1m%(levelname)s\033[0m\t\033[36m%(message)s\033[0m",
    )
    logger = logging.getLogger("patches_ht")
    logger.info("Starting to read patches_ht")

    read_ocaml_record(data, offset, None)  # Read and skip record header

    patches_keys = [
        "h_person",
        "h_ascend",
        "h_union",
        "h_family",
        "h_couple",
        "h_descend",
        "h_string",
    ]
    # Read each field
    for key, reader in zip(
        patches_keys,
        [
            read_person,
            read_ascend,
            read_union,
            read_family,
            read_couple,
            read_descend,
            read_ocaml_string,
        ],
    ):
        logger.info(f"Reading {key}")
        ref, offset = read_ocaml_int(data, offset)
        patches[key], offset = read_ocaml_marshalled_hashtable(
            data, offset, read_ocaml_int, reader
        )
        logger.debug(f"{key}: ref={ref}, entries={len(patches[key])}")
    logger.info("Reading h_name")
    patches["h_name"], offset = read_ocaml_marshalled_hashtable(
        data,
        offset,
        read_ocaml_int,
        lambda d, o: read_ocaml_marshalled_list(d, o, read_ocaml_int),
    )

    return patches


def parse_patches_ht_header(data: bytes, offset: int = 0, word_size: int = 4):
    """
    Parse the marshalled OCaml patches_ht record header and iterate over its fields.
    Skips over actual field data, just prints field index and type (tuple or hashtable).
    Assumes the magic header "GnPa0001" is present at the start.
    """
    MAGIC = b"GnPa0001"
    assert data.startswith(MAGIC), "Missing magic header"
    offset = len(MAGIC)

    file_header, offset = caml_parse_header(data, offset)
    print(file_header)

    def parse_block_header(data, offset, word_size):
        header = int.from_bytes(data[offset : offset + word_size], "big")
        size = header >> 10
        tag = header & 0x3FF
        print(f"  block header: {header:08x} size={size} tag={tag} {bin(header)}")
        return size, tag, offset + word_size

    # Parse record block header (patches_ht)
    size, tag, offset = parse_block_header(data, offset, word_size)
    assert size == 9, f"Expected 9 fields, got {size}"
    print(f"patches_ht record: size={size}, tag={tag}")

    for i in range(size):
        print(f"Field {i+1}: ", end="")
        # For fields 1-7: tuple (int ref, hashtable)
        if i < 7:
            tuple_size, tuple_tag, offset = parse_block_header(data, offset, word_size)
            print(f"tuple (int ref, hashtable), size={tuple_size}, tag={tuple_tag}")
            # Skip int ref block
            intref_size, intref_tag, offset = parse_block_header(
                data, offset, word_size
            )
            offset += word_size  # skip int value
            # Skip hashtable block
            ht_size, ht_tag, offset = parse_block_header(data, offset, word_size)
            # Skipping actual hashtable/list data
            print(
                f"  int ref: size={intref_size}, tag={intref_tag}; hashtable: size={ht_size}, tag={ht_tag}"
            )
        else:
            # Field 8 and 9: just hashtable
            ht_size, ht_tag, offset = parse_block_header(data, offset, word_size)
            print(f"hashtable, size={ht_size}, tag={ht_tag}")
            # Skipping actual hashtable/list data
