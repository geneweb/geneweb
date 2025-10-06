import struct
from typing import Callable, Tuple, Any, List
import os


def read32u(data: bytes, offset: int = 0) -> Tuple[int, int]:
    return struct.unpack(">I", data[offset : offset + 4])[0], offset + 4


def read64u(data: bytes, offset: int = 0) -> Tuple[int, int]:
    return struct.unpack(">Q", data[offset : offset + 8])[0], offset + 8


def read8u(data: bytes, offset: int = 0) -> Tuple[int, int]:
    return struct.unpack(">B", data[offset : offset + 1])[0], offset + 1


def read_ocaml_auto(data: bytes, offset: int) -> Tuple[Any, int]:
    """
    Reads an OCaml marshalled value from the given data.
    Determines the type automatically based on the OCaml tag.
    """
    tag = data[offset]
    match tag:
        case 0x00:  # String
            return read_ocaml_string(data, offset + 1)
        case 0xFD:  # Float
            return read_ocaml_float(data, offset + 1)
        case _:  # Assume int for simplicity
            return read_ocaml_int(data, offset)


def read_ocaml_int(data: bytes, offset: int) -> Tuple[int, int]:
    # OCaml marshals ints as 32-bit signed (little-endian)
    value = struct.unpack_from(">i", data, offset)[0]
    return value, offset + 4


def read_ocaml_float(data: bytes, offset: int) -> Tuple[float, int]:
    # OCaml marshals floats as 64-bit double (little-endian)
    value = struct.unpack_from("<d", data, offset)[0]
    return value, offset + 8


def read_ocaml_bool(data: bytes, offset: int) -> Tuple[bool, int]:
    # OCaml marshals bool as int: 0 = False, 1 = True
    value = struct.unpack_from("<i", data, offset)[0]
    return bool(value), offset + 4


def read_ocaml_string(data: bytes, offset: int) -> Tuple[str, int]:
    # OCaml marshals strings as: [length (int)][bytes]
    string_header, length = struct.unpack_from("<II", data, offset)
    # assert string_header == 0x00
    offset += 8
    value = data[offset : offset + length]
    for i in range(len(value)):
        print(f"{chr(value[i])=} {value[i]:02x}")
    return value, offset + length


def read_ocaml_list(data: bytes, offset: int, item_unmarshal) -> Tuple[list, int]:
    # OCaml marshals lists as a chain of cons cells ending with 0x00 (empty list tag)
    result = []
    while offset < len(data):
        tag = data[offset]
        if tag == 0x00:
            return result, offset + 1  # End of list
        offset += 1  # Skip cons cell tag
        item, offset = item_unmarshal(data, offset)
        result.append(item)
    return result, offset


def read_ocaml_hashmap(
    data: bytes, offset: int, key_unmarshal, value_unmarshal
) -> Tuple[dict, int]:
    # OCaml marshals hashmaps as a list of (key, value) pairs
    hashmap = {}
    pairs, offset = read_ocaml_list(
        data,
        offset,
        lambda d, o: (
            (key_unmarshal(d, o)[0], value_unmarshal(d, key_unmarshal(d, o)[1])[0]),
            value_unmarshal(d, key_unmarshal(d, o)[1])[1],
        ),
    )
    for k, v in pairs:
        hashmap[k] = v
    return hashmap, offset


def read_ocaml_optional(data, offset, item_unmarshal):
    """Reads an OCaml 'option' type from the given data."""
    if data[offset] == 0x00:  # None
        return None, offset + 1
    elif data[offset] == 0x01:  # Some
        item, new_offset = item_unmarshal(data, offset + 1)
        return item, new_offset
    else:
        raise ValueError("Invalid option tag")


def read_ocaml_variant(
    data: bytes,
    offset: int,
    constructors: List[Tuple[str, Callable[[bytes, int], Tuple[Any, int]]]],
) -> Tuple[Any, int]:
    """
    Unmarshal an OCaml variant from bytes.
    - data: bytes containing the marshalled variant.
    - offset: starting offset in data.
    - constructors: list of (name, arg_unmarshal) tuples.
      - name: constructor name (str)
      - arg_unmarshal: function (data, offset) -> (value, new_offset), or None for no argument.
    Returns: (variant_value, new_offset)

    Example data:
    type cdate =
        | Cgregorian of int
        | Cjulian of int
        | Cfrench of int
        | Chebrew of int
        | Ctext of string
        | Cdate of date
        | Cnone
    Example constructors:
    constructors = [
        ("Cgregorian", read_ocaml_int),
        ("Cjulian", read_ocaml_int),
        ("Cfrench", read_ocaml_int),
        ("Chebrew", read_ocaml_int),
        ("Ctext", read_ocaml_string),
        ("Cdate", read_date),  # Assume read_date is defined elsewhere
        ("Cnone", None),
    ]
    """
    # Read block header (assume 4 bytes: tag and size)
    tag = data[offset]
    size = data[offset + 1]
    offset += 4  # Skip header (simplified, real OCaml header may differ)

    name, arg_unmarshal = constructors[tag]
    if size == 0 or arg_unmarshal is None:
        return (name, None), offset
    else:
        value, offset = arg_unmarshal(data, offset)
        return (name, value), offset


# type cdate =
#   | Cgregorian of int
#   | Cjulian of int
#   | Cfrench of int
#   | Chebrew of int
#   | Ctext of string
#   | Cdate of date
#   | Cnone
def read_cdate(data: bytes, offset: int) -> Tuple[Any, int]:
    # Implement reading logic for cdate type
    cdate_type, offset = read_ocaml_int(data, offset)
    match cdate_type:
        case 0:  # Cgregorian
            value, offset = read_ocaml_int(data, offset)
            return {"Cgregorian": value}, offset
        case 1:  # Cjulian
            value, offset = read_ocaml_int(data, offset)
            return {"Cjulian": value}, offset
        case 2:  # Cfrench
            value, offset = read_ocaml_int(data, offset)
            return {"Cfrench": value}, offset
        case 3:  # Chebrew
            value, offset = read_ocaml_int(data, offset)
            return {"Chebrew": value}, offset
        case 4:  # Ctext
            value, offset = read_ocaml_string(data, offset)
            return {"Ctext": value}, offset
        case 5:  # Cdate
            value, offset = read_date(data, offset)
            return {"Cdate": value}, offset
        case 6:  # Cnone
            return {"Cnone": None}, offset
        case _:
            raise ValueError(f"Unknown cdate type: {cdate_type}")


# type date = Dgreg of dmy * calendar | Dtext of string
def read_date(data: bytes, offset: int) -> Tuple[Any, int]:
    date_type, offset = read_ocaml_int(data, offset)
    match date_type:
        case 0:  # Dgreg
            dmy, offset = read_dmy(data, offset)
            calendar, offset = read_ocaml_int(data, offset)
            return {"Dgreg": {"dmy": dmy, "calendar": calendar}}, offset
        case 1:  # Dtext
            value, offset = read_ocaml_string(data, offset)
            return {"Dtext": value}, offset
        case _:
            raise ValueError(f"Unknown date type: {date_type}")


# and dmy = { day : int; month : int; year : int; prec : precision; delta : int }
def read_dmy(data: bytes, offset: int) -> Tuple[Any, int]:
    dmy = {}
    dmy["day"], offset = read_ocaml_int(data, offset)
    dmy["month"], offset = read_ocaml_int(data, offset)
    dmy["year"], offset = read_ocaml_int(data, offset)
    dmy["prec"], offset = read_ocaml_int(data, offset)
    dmy["delta"], offset = read_ocaml_int(data, offset)
    return dmy, offset


def read_ocaml_record(
    data: bytes,
    offset: int,
    field_unmarshals: List[Tuple[str, Callable[[bytes, int], Tuple[Any, int]]]],
) -> Tuple[dict, int]:
    (tag, size), offset = read_ocaml_header(data, offset)
    assert tag == 0xF8, f"Invalid record header: {0xF8} {tag=} {size=}"
    offset += 8
    print(f"record {tag:08x} {size=}")


# On 32-bit systems, the header is 4 bytes; on 64-bit systems, it is 8 bytes.
# header = (size << 10) | tag
def read_ocaml_header(data: bytes, offset: int) -> Tuple[int, int]:
    # help(struct)
    # header: int = struct.unpack_from("<Q", data, offset)[0]
    # print(header.to_bytes(8, "little").hex())
    # print(f"{header >> 10=:x} {header & 0x3FF:x}")
    # tag = header & 0x3FF  # Lower 10 bits
    # size = header >> 10  # Upper bits
    # return (tag, size), offset + 4
    res = parse_ocaml_block_header(data, offset, word_size=4)
    size = struct.unpack_from("<I", res["size"].to_bytes(4, "little"))[0]
    print(f"header {res=} {size=}")
    return (res["tag"], res["size"]), res["next_offset"]


def parse_ocaml_block_header(data: bytes, offset: int = 0, word_size: int = 4) -> dict:
    """
    Parses an OCaml block header from marshalled data.

    Args:
        data (bytes): The marshalled OCaml data.
        offset (int): The offset to start reading from.
        word_size (int): Size of the OCaml word (4 for 32-bit, 8 for 64-bit).

    Returns:
        dict: {'size': int, 'tag': int, 'header': int, 'next_offset': int}
    """
    if word_size == 4:
        header = int.from_bytes(data[offset : offset + 4], "little")
        next_offset = offset + 4
    elif word_size == 8:
        header = int.from_bytes(data[offset : offset + 8], "little")
        next_offset = offset + 8
    else:
        raise ValueError("word_size must be 4 or 8")

    size = header >> 10
    tag = header & 0x3FF  # lower 10 bits

    return {"size": size, "tag": tag, "header": header, "next_offset": next_offset}
