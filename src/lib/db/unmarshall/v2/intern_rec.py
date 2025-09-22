import enum
import logging
from typing import (
    BinaryIO,
    Callable,
    Dict,
    Iterable,
    Literal,
    Optional,
    List,
    Any,
    Tuple,
    Type,
    Union,
)
import struct
from dataclasses import dataclass
from enum import Enum

from lib.db.unmarshall.intern import InternItem
from lib.db.unmarshall.v2.mlvalues import Tag, tag_hd, wosize_hd
from lib.db.unmarshall.v2.stdlib import Ref, TypeEnum
import lib.db.v2.mutil as Mutil

from ..header import caml_parse_header_f
from .intext import (
    PREFIX_SMALL_BLOCK,
    PREFIX_SMALL_INT,
    CODE_INT8,
    CODE_INT16,
    CODE_INT32,
    CODE_INT64,
    CODE_SHARED8,
    CODE_SHARED16,
    CODE_SHARED32,
    CODE_SHARED64,
    CODE_BLOCK32,
    CODE_BLOCK64,
    CODE_STRING8,
    CODE_STRING32,
    CODE_STRING64,
    CODE_DOUBLE_BIG,
    CODE_DOUBLE_LITTLE,
    CODE_DOUBLE_ARRAY8_BIG,
    CODE_DOUBLE_ARRAY8_LITTLE,
    CODE_DOUBLE_ARRAY32_BIG,
    CODE_DOUBLE_ARRAY32_LITTLE,
    CODE_DOUBLE_ARRAY64_BIG,
    CODE_DOUBLE_ARRAY64_LITTLE,
    CODE_CODEPOINTER,
    CODE_INFIXPOINTER,
    CODE_CUSTOM_LEN,
    CODE_CUSTOM_FIXED,
    PREFIX_SMALL_STRING,
)

from .ocaml_input import OCamlInput

OCAML_NONE = 0
OCAML_EMPTY = OCAML_NONE


class OCamlUnmarshalError(Exception):
    pass


class OCamlInput2:
    def __init__(self, data: bytes):
        self.data = data
        self.pos = 0
        self.obj_table: List[Any] = []
        self.obj_counter = 0
        self.stack: List[InternItem] = []

    def read_byte(self) -> int:
        val = self.data[self.pos]
        self.pos += 1
        return val

    def read_int16(self) -> int:
        val = struct.unpack(">h", self.data[self.pos : self.pos + 2])[0]
        self.pos += 2
        return val

    def read_uint16(self) -> int:
        val = struct.unpack(">H", self.data[self.pos : self.pos + 2])[0]
        self.pos += 2
        return val

    def read_int32(self) -> int:
        val = struct.unpack(">i", self.data[self.pos : self.pos + 4])[0]
        self.pos += 4
        return val

    def read_uint32(self) -> int:
        val = struct.unpack(">I", self.data[self.pos : self.pos + 4])[0]
        self.pos += 4
        return val

    def read_int64(self) -> int:
        val = struct.unpack(">q", self.data[self.pos : self.pos + 8])[0]
        self.pos += 8
        return val

    def read_uint64(self) -> int:
        val = struct.unpack(">Q", self.data[self.pos : self.pos + 8])[0]
        self.pos += 8
        return val

    def read_float64(self) -> float:
        val = struct.unpack(">d", self.data[self.pos : self.pos + 8])[0]
        self.pos += 8
        return val

    def read_bytes(self, length: int) -> bytes:
        val = self.data[self.pos : self.pos + length]
        self.pos += length
        return val


class Val:
    @property
    def v(self):
        return self.__v[0]

    @v.setter
    def v(self, value):
        self.__v[0] = value

    def __init__(self, v):
        self.__v = [v]

    def __repr__(self):
        return f"Val({self.v})"

    def __str__(self):
        return str(self.v)


@enum.unique
class StackOp(Enum):
    OFreshOID = 0
    OShift = 1
    OReadItems = 2


# /* Convenience macros for requesting operation on the stack */
# #define PushItem(s)                                                     \
#   do {                                                                  \
#     sp++;                                                               \
#     if (sp >= s->intern_stack_limit) sp = intern_resize_stack(s, sp);   \
#   } while(0)


# #define ReadItems(s,_dest,_n)                                           \
#   do {                                                                  \
#     if (_n > 0) {                                                       \
#       PushItem(s);                                                      \
#       sp->op = OReadItems;                                              \
#       sp->dest = _dest;                                                 \
#       sp->arg = _n;                                                     \
#     }                                                                   \
#   } while(0)
class Stack:
    SetDest = Callable[[any], None]
    GetDest = Callable[[], any]

    @dataclass
    class InternItem:
        op: StackOp
        dest: "Stack.SetDest"  # Will set the value at dest
        arg: int  # how many items remain
        read_dest: Optional["Stack.GetDest"] = (
            None  # for OReadItems, where to put read value
        )

    @property
    def top(self):
        if self.sp >= 0:
            return self.stack[self.sp]
        return None

    def __init__(self, logger: logging.Logger = None):
        self.stack: List["Stack.InternItem"] = []
        self.sp = -1  # Stack pointer
        self.logger = logger if logger is not None else logging.getLogger(__name__)

    def read_items(
        self, dest: "Stack.SetDest", n: int, read_dest: "Stack.GetDest" = None
    ):
        """Request to read n items into dest"""
        if n > 0:
            self.sp += 1  # push item
            self.stack.append(self.InternItem(StackOp.OReadItems, dest, n, read_dest))
        return self.sp

    # if (size == 0) {
    # v = Atom(tag);
    # } else {
    # v = intern_alloc_obj (s, d, size, tag);
    # if (s->intern_obj_table != NULL)
    #     s->intern_obj_table[s->obj_counter++] = v;
    # /* For objects, we need to freshen the oid */
    # if (tag == Object_tag) {
    #     CAMLassert(size >= 2);
    #     /* Request to read rest of the elements of the block */
    #     ReadItems(s, &Field(v, 2), size - 2);
    #     /* Request freshing OID */
    #     PushItem(s);
    #     sp->op = OFreshOID;
    #     sp->dest = (value*) v;
    #     sp->arg = 1;
    #     /* Finally read first two block elements: method table and old OID */
    #     ReadItems(s, &Field(v, 0), 2);
    # } else
    #     /* If it's not an object then read the contents of the block */
    #     ReadItems(s, &Field(v, 0), size);
    def read_block(self, tag, size, set_dest: "Stack.SetDest"):
        if size == 0:  # return 0-tuple
            self.logger.debug(f"{size=} 0-tuple")
            set_dest(())
            return
        if tag == Tag.OBJECT_TAG:
            if size < 2:
                raise OCamlUnmarshalError("Object block size must be at least 2")
            # Read method table and old OID
            self.logger.debug(f"object block tag {tag=} {size=} ")
            raise NotImplementedError("Object blocks not implemented yet")
            # Request to read rest of the elements of the block
            self.read_items(set_dest, size - 2)
            # Request freshing OID
            self.sp += 1  # push item
            self.stack.append(self.InternItem(StackOp.OFreshOID, set_dest, 1))
            # Finally read first two block elements: method table and old OID
            self.read_items(set_dest, 2)
        else:
            self.logger.debug(f"{size=} ")

            v = Val([])
            set_dest(v)

            def set_block(x):
                nonlocal v
                v.v.append(x)

            def get_block():
                return v.v

            self.read_items(set_block, size, get_block)

    # size = (len + sizeof(value)) / sizeof(value);
    # v = intern_alloc_obj (s, d, size, String_tag);
    # if (s->intern_obj_table != NULL)
    # s->intern_obj_table[s->obj_counter++] = v;
    # Field(v, size - 1) = 0;
    # ofs_ind = Bsize_wsize(size) - 1;
    # Byte(v, ofs_ind) = ofs_ind - len;
    # readblock(s, (char *)String_val(v), len);
    def read_string(self, length: int, set_dest: "Stack.SetDest", oi: OCamlInput):
        if length == 0:
            set_dest("")
            return
        # v = Val("")
        # set_dest(v)
        v = ""

        v = oi.read_bytes(length)
        for i in range(length):
            self.logger.debug(f"{v[i]:#04x} {chr(v[i])}")
        v = v.decode("utf-8", errors="replace")
        set_dest(v)
        return v

    def pop(self) -> Optional[InternItem]:
        """Pop the top item from the stack"""
        if self.sp >= 0:
            item = self.stack[-1]
            item.arg -= 1
            if item.arg == 0:
                self.sp -= 1
                self.stack.pop()
                # item = self.stack[-1] ?? do we want this
            return item
        return None


class CustomFormatter(logging.Formatter):

    # grey = "\033[38;20m"
    # yellow = "\033[33;20m ⚠️"
    # red = "\033[31;20m❗ "
    # bold_red = "\033[31;1m‼️ "
    # dim = "\033[2m"
    # reset = "\033[0m"
    grey = ""
    yellow = "⚠️ "
    red = "❗ "
    bold_red = "‼️ "
    dim = ""
    reset = ""
    format = (
        ""
        "%(name)s [%(levelname)s] - " + reset + "%(message)s (%(filename)s:%(lineno)d)"
    )

    FORMATS = {
        logging.DEBUG: dim + grey + format + reset,
        logging.INFO: grey + format + reset,
        logging.WARNING: yellow + format + reset,
        logging.ERROR: red + format + reset,
        logging.CRITICAL: bold_red + format + reset,
    }

    def format(self, record):
        log_fmt = self.FORMATS.get(record.levelno)
        formatter = logging.Formatter(log_fmt)
        return formatter.format(record)


def unmarshall_ocaml_data(
    f: BinaryIO,
    *,
    log_level=logging.INFO,
    structure: Type = None,
    magic: bytes = b"GnPa0001",
) -> Any:

    if magic:
        inp_magic = f.read(len(magic))
        assert inp_magic == magic, f"Missing magic header {inp_magic}"
    header = caml_parse_header_f(f)
    oi = OCamlInput(f)

    logger = Mutil.get_logger(__name__, log_level=log_level)

    s = Stack(logger)
    v: Val = Val(None)
    result: Any = None

    def set_result(x):
        nonlocal result
        result = x

    def get_result():
        return result

    s.read_items(set_result, 1, get_result)

    while s.sp >= 0:
        # interpret next item on the stack
        top_item = s.pop()
        # print(f"stack op {top_item.op}, arg {top_item.arg}")
        match top_item.op:
            case StackOp.OFreshOID:
                # refresh the object ID
                raise NotImplementedError("OFreshOID not implemented yet")
            case StackOp.OShift:
                # Shift value by an offset
                raise NotImplementedError("OShift not implemented yet")
            case StackOp.OReadItems:
                # read next item
                # read_item(s, oi, top_item, logger)
                data = {}
                item = read_bin_caml_input(oi, logger, data)
                if isinstance(item, list):
                    s.read_block(data["tag"], data["size"], top_item.dest)
                else:
                    top_item.dest(item)
            case _:
                raise OCamlUnmarshalError(f"Invalid stack operation {top_item.op}")

    if structure is not None and result is not None:
        result = convert_structure(result, structure, logger)
        logger.debug(f"final structure {result=}")
    return result


def read_bin_caml_input(
    oi: OCamlInput, logger: logging.Logger = None, data: Dict = None
):
    # read a value and set it to top_item.dest
    if not logger:
        logger = Mutil.get_logger(__name__)
    if data is None:
        data = {}
    code = oi.read_byte()
    logger.debug(f"{code=:#04x}")  # 0x08 block
    if code >= PREFIX_SMALL_INT:
        if code >= PREFIX_SMALL_BLOCK:
            data["tag"] = code & 0xF
            data["size"] = (code >> 4) & 0x7
            logger.debug(f"small block {data["tag"]=} {data["size"]=}")
            # s.read_block(tag, size, top_item.dest)
            # raise NotImplementedError("read_bin_caml_input not implemented yet")
            return []
        else:  # small int
            # v = val_int(code & 0x3F)
            v = (
                code & 0x3F
            )  # do not use val_int because python directly uses long values
            logger.debug(f"small int {v=}")
            return v
    elif code >= PREFIX_SMALL_STRING:
        data["length"] = code & 0x1F
        logger.debug(f"small string {data["length"]=}")
        return oi.read_bytes(data["length"]).decode("utf-8", errors="replace")
    elif code == CODE_INT8:
        # data["v"] = val_long(oi.read_byte())
        data["v"] = (
            oi.read_byte()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int8 + {data['v']=}")
        return data["v"]
    elif code == CODE_INT16:
        # data["v"] = val_long(oi.read_uint16())
        data["v"] = (
            oi.read_uint16()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int16 + {data['v']=}")
        return data["v"]
    elif code == CODE_INT32:
        # data["v"] = val_long(oi.read_uint32())
        data["v"] = (
            oi.read_uint32()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int32 + {data['v']=}")
        return data["v"]
    elif code == CODE_INT64:
        # data["v"] = val_long(oi.read_uint64())
        data["v"] = (
            oi.read_uint64()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int64 + {data['v']=}")
        return data["v"]
    elif code == CODE_SHARED8:
        logger.error("shared8")
    elif code == CODE_SHARED16:
        logger.error("shared16")
    elif code == CODE_SHARED32:
        logger.error("shared32")
    elif code == CODE_SHARED64:
        logger.error("shared64")
    elif code == CODE_BLOCK32:
        header = oi.read_uint32()
        data["tag"] = tag_hd(header)
        data["size"] = wosize_hd(header)
        logger.debug(f"block32 + header({data["tag"]=} {data["size"]=})")
        # s.read_block(data["tag"], data["size"], top_item.dest)
        # raise NotImplementedError("read_bin_caml_input not implemented yet")
        return []
    elif code == CODE_BLOCK64:
        logger.error("block64")
    elif code == CODE_STRING8:
        data["length"] = oi.read_byte()
        logger.debug(f"string8 + {data["length"]=}")
        return oi.read_bytes(data["length"]).decode("utf-8", errors="replace")
    elif code == CODE_STRING32:
        data["length"] = oi.read_uint32()
        logger.debug(f"string32 + {data["length"]=}")
        return oi.read_bytes(data["length"]).decode("utf-8", errors="replace")
    elif code == CODE_STRING64:
        data["length"] = oi.read_uint64()
        logger.debug(f"string64 + {data["length"]=}")
        return oi.read_bytes(data["length"]).decode("utf-8", errors="replace")
    elif code == CODE_DOUBLE_LITTLE:
        logger.error("double_little")
    elif code == CODE_DOUBLE_BIG:
        logger.error("double_big")
    elif code == CODE_DOUBLE_ARRAY8_LITTLE:
        logger.error("double_array8_little")
    elif code == CODE_DOUBLE_ARRAY8_BIG:
        logger.error("double_array8_big")
    elif code == CODE_DOUBLE_ARRAY32_LITTLE:
        logger.error("double_array32_little")
    elif code == CODE_DOUBLE_ARRAY32_BIG:
        logger.error("double_array32_big")
    elif code == CODE_DOUBLE_ARRAY64_LITTLE:
        logger.error("double_array64_little")
    elif code == CODE_DOUBLE_ARRAY64_BIG:
        logger.error("double_array64_big")
    elif code == CODE_CODEPOINTER:
        logger.error("codepointer")
    elif code == CODE_INFIXPOINTER:
        logger.error("infixpointer")
    elif code == CODE_CUSTOM_LEN:
        logger.error("custom_len")
    elif code == CODE_CUSTOM_FIXED:
        logger.error("custom_fixed")
    else:
        logger.error("unknown")


def is_linked_list(value: Iterable):
    if isinstance(value, Val):
        value = value.v
    if not isinstance(value, Iterable) or len(value) != 2:
        return False
    if value[1] == OCAML_EMPTY:  # end of list
        return True
    return is_linked_list(value[1])


def linked_list_to_list(value: Iterable):
    result = []
    while value != OCAML_EMPTY:
        if isinstance(value, Val):
            value = value.v
        result.append(value[0])
        value = value[1]
    return result


def convert_structure(value: Any, structure: Type, logger: logging.Logger) -> Any:
    """Convert the unmarshalled value into the desired structure"""
    if structure is None:
        return value
    logger.debug(f"struct {structure} {value=}")
    if (
        (isinstance(value, Iterable) and len(value) == 1 and isinstance(value[0], int))
        or isinstance(value, int)
    ) and (
        getattr(structure, "__origin__", None) == Ref
        or (isinstance(structure, type) and issubclass(structure, Ref))
    ):
        logger.debug(f"Handling Ref {structure}")
        if isinstance(value, Iterable):
            value = value[0]
        return structure(value)
    if getattr(structure, "__origin__", None) == Optional or (
        getattr(structure, "__origin__", None) == Union
        and len(structure.__args__) == 2
        and type(None) in structure.__args__
    ):
        logger.debug(f"Handling Optional {structure}")
        if value == OCAML_NONE:
            return None
        return convert_structure(value, structure.__args__[0], logger)
    if isinstance(value, Val):
        return convert_structure(value.v, structure, logger)
    if structure in (int, str, float, bool):
        if not isinstance(value, structure):
            raise OCamlUnmarshalError(f"Expected {structure}, got {type(value)}")
        return value
    if hasattr(structure, "__origin__"):
        logger.debug(f"convert generic {structure} {value}")
        origin = structure.__origin__
        args = structure.__args__
        if origin is list or origin is List:
            if not isinstance(value, list):
                if value == OCAML_EMPTY or value == tuple():  # Empty list in OCaml
                    return []
                raise OCamlUnmarshalError(f"Expected list, got {type(value)}")
            if is_linked_list(value):
                logger.debug("Converting linked list to list")
                value = linked_list_to_list(value)
                logger.debug(f"Converted linked list to list: {value}")
            return [convert_structure(v, args[0], logger) for v in value]
        elif origin is tuple or origin is Tuple:
            if not isinstance(value, (list, tuple)):
                raise OCamlUnmarshalError(f"Expected tuple, got {type(value)}")
            if len(value) != len(args):
                raise OCamlUnmarshalError(
                    f"Expected tuple of length {len(args)}, got {len(value)}"
                )
            return tuple(
                convert_structure(v, arg, logger) for v, arg in zip(value, args)
            )
        elif origin is dict or origin is Dict:
            if not isinstance(
                value, (List, list)
            ):  # OCaml unmarshals dict as list of pairs
                raise OCamlUnmarshalError(f"Expected list for dict, got {type(value)}")
            assert len(value) == 4
            # return value
            (nb_of_entries, buckets, seed, initial_size) = value
            logger.debug(f"dict {nb_of_entries=} {seed=} {initial_size=}")
            logger.debug(f"{buckets=}")
            value = convert_dict_from_buckets(
                buckets, nb_of_entries, (args[0], args[1]), logger
            )
            logger.debug(f"converted dict {value=}")
            return value
        elif origin is Union:  # Enum
            if (
                isinstance(value, int)
                and 0 <= value < len(args)
                and type(args[value]) is type(Literal[Any])
            ):
                # Enum represented as int
                return args[value].__args__[0]
            for t in args:
                if isinstance(value, t):
                    return convert_structure(value, t, logger)

            if value >= len(args):
                raise OCamlUnmarshalError(f"Enum value {value} out of range")
            t = args[value]
            return convert_structure(value, t, logger)
        elif origin is Literal:
            return args[0]
        else:
            raise OCamlUnmarshalError(f"Unsupported generic type: {origin}")
    elif isinstance(structure, type) and issubclass(structure, TypeEnum):
        if isinstance(value, int) and 0 <= value < len(structure):
            logger.debug(f"Enum value {value} is {list(structure)[value].name}")
            return structure(list(structure)[value].value)
        for t in structure:
            logger.debug(f"Checking enum value {t.value}")
            if type(t.value) == type(Literal[Any]):
                continue
            logger.debug(f"Continuing enum check with enum value {t.name} {t.value}")
            try:
                return convert_structure(value, t.value, logger)
            except Exception as e:
                logger.warning(
                    f"Enum check failed with {value=} {t.name} {t.value} {e}"
                )
                continue
        logger.error(f"Enum value {value} not in {structure} {list(structure)=}")
        raise OCamlUnmarshalError(f"Enum value {value} not in {structure}")
    elif isinstance(structure, type) and issubclass(structure, Enum):
        if isinstance(value, int):
            if 0 <= value < len(structure):
                return list(structure)[value].name
        raise OCamlUnmarshalError(f"Enum value {value} not in {structure}")
    elif hasattr(structure, "__dataclass_fields__"):
        if not isinstance(value, Iterable):
            if value == OCAML_EMPTY:  # Empty list/None in OCaml
                return None
            raise OCamlUnmarshalError(
                f"Expected list for dataclass {structure}, got {type(value)}"
            )
        field_names, field_range = Mutil.get_dataclass_fields(structure)
        if len(value) not in field_range:
            raise OCamlUnmarshalError(
                f"Expected {field_range.start} <= n < {field_range.stop} fields for dataclass {structure}, got {len(value)}"
            )
        field_values = {
            name: convert_structure(
                val, structure.__dataclass_fields__[name].type, logger
            )
            for name, val in zip(field_names, value)
        }
        logger.debug(f"dataclass {structure} {field_values=}")
        return structure(**field_values)


# type ('a, 'b) t =
#   { mutable size: int;                        (* number of entries *)
#     mutable data: ('a, 'b) bucketlist array;  (* the buckets *)
#     seed: int;                        (* for randomization *)
#     mutable initial_size: int;                (* initial array size *)
#   }
# and ('a, 'b) bucketlist =
#     Empty
#   | Cons of { mutable key: 'a;
#               mutable data: 'b;
#               mutable next: ('a, 'b) bucketlist }
def convert_dict_from_buckets(
    buckets: Union[List, Val],
    nb_of_entries: int,
    dict_type: Tuple[Type],
    logger: logging.Logger,
) -> Dict:
    result = {}
    logger.debug(f"{type(buckets)=}")
    if isinstance(buckets, Val):
        buckets = buckets.v
    logger.debug(f"convert dict from buckets {buckets=}")
    if isinstance(buckets, Iterable):
        while buckets:
            bucket = buckets.pop(0)
            if bucket == OCAML_EMPTY:  # Empty
                continue
            if not isinstance(bucket, Val):
                raise OCamlUnmarshalError(
                    f"Expected Val for bucket, got {type(bucket)}"
                )
            key, value, next = bucket.v
            if key in result:
                logger.warning(f"Duplicate key {key} in dict, overwriting")
            logger.debug(f"{dict_type=}")
            key = convert_structure(key, dict_type[0], logger)
            value = convert_structure(value, dict_type[1], logger)
            result[key] = value
            if next != OCAML_EMPTY:
                buckets.insert(0, next)
    elif buckets is None:
        pass
    else:
        raise OCamlUnmarshalError(
            f"Expected list or None in dict buckets, got {type(buckets)}"
        )
    if len(result) != nb_of_entries:
        logger.warning(
            f"Expected {nb_of_entries} entries in dict, got {len(result)} after conversion"
        )
    return result


def read_bin_caml_input_block_rec(
    oi: OCamlInput, tag: int, size: int, logger: logging.Logger
):
    if tag == 0:
        return list(read_bin_caml_input_rec(oi, logger=logger) for _ in range(size))
    logger.error("non-list blocks not implemented")
    raise NotImplementedError("Reading objects not implemented")


def read_bin_caml_input_rec(
    oi: OCamlInput, *, logger: logging.Logger = None, structure: Type = None
) -> Union[list, int, str]:
    """Read a binary OCaml input value.

    Args:
        oi (OCamlInput): object to read from
        logger (logging.Logger, optional): logger to use. Defaults to None.

    Returns:
        Any: the read value
    """
    # read a value and set it to top_item.dest
    if not logger:
        logger = Mutil.get_logger(__name__)
    code = oi.read_byte()
    logger.debug(f"{code=:#04x}")  # 0x08 block
    if code >= PREFIX_SMALL_INT:
        if code >= PREFIX_SMALL_BLOCK:
            tag = code & 0xF
            size = (code >> 4) & 0x7
            logger.debug(f"small block {tag=} {size=}")
            # s.read_block(tag, size, top_item.dest)
            # raise NotImplementedError("read_bin_caml_input not implemented yet")
            block = read_bin_caml_input_block_rec(oi, tag, size, logger)
            if structure is not None:
                block = convert_structure(block, structure, logger)
            return block
        else:  # small int
            # v = val_int(code & 0x3F)
            v = (
                code & 0x3F
            )  # do not use val_int because python directly uses long values
            logger.debug(f"small int {v=}")
            return v
    elif code >= PREFIX_SMALL_STRING:
        length = code & 0x1F
        logger.debug(f"small string {length=}")
        return oi.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_INT8:
        # data["v"] = val_long(oi.read_byte())
        v = (
            oi.read_byte()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int8 + {v=}")
        return v
    elif code == CODE_INT16:
        # v = val_long(oi.read_uint16())
        v = (
            oi.read_uint16()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int16 + {v=}")
        return v
    elif code == CODE_INT32:
        # v = val_long(oi.read_uint32())
        v = (
            oi.read_uint32()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int32 + {v=}")
        return v
    elif code == CODE_INT64:
        # v = val_long(oi.read_uint64())
        v = (
            oi.read_uint64()
        )  # do not use val_long because python directly uses long values
        logger.debug(f"int64 + {v=}")
        return v
    elif code == CODE_SHARED8:
        logger.error("shared8")
    elif code == CODE_SHARED16:
        logger.error("shared16")
    elif code == CODE_SHARED32:
        logger.error("shared32")
    elif code == CODE_SHARED64:
        logger.error("shared64")
    elif code == CODE_BLOCK32:
        header = oi.read_uint32()
        tag = tag_hd(header)
        size = wosize_hd(header)
        logger.debug(f"block32 + header({tag=} {size=})")
        block = read_bin_caml_input_block_rec(oi, tag, size, logger)
        if structure is not None:
            block = convert_structure(block, structure, logger)
        return block
    elif code == CODE_BLOCK64:
        logger.error("block64")
    elif code == CODE_STRING8:
        length = oi.read_byte()
        logger.debug(f"string8 + {length=}")
        return oi.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_STRING32:
        length = oi.read_uint32()
        logger.debug(f"string32 + {length=}")
        return oi.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_STRING64:
        length = oi.read_uint64()
        logger.debug(f"string64 + {length=}")
        return oi.read_bytes(length).decode("utf-8", errors="replace")
    elif code == CODE_DOUBLE_LITTLE:
        logger.error("double_little")
    elif code == CODE_DOUBLE_BIG:
        logger.error("double_big")
    elif code == CODE_DOUBLE_ARRAY8_LITTLE:
        logger.error("double_array8_little")
    elif code == CODE_DOUBLE_ARRAY8_BIG:
        logger.error("double_array8_big")
    elif code == CODE_DOUBLE_ARRAY32_LITTLE:
        logger.error("double_array32_little")
    elif code == CODE_DOUBLE_ARRAY32_BIG:
        logger.error("double_array32_big")
    elif code == CODE_DOUBLE_ARRAY64_LITTLE:
        logger.error("double_array64_little")
    elif code == CODE_DOUBLE_ARRAY64_BIG:
        logger.error("double_array64_big")
    elif code == CODE_CODEPOINTER:
        logger.error("codepointer")
    elif code == CODE_INFIXPOINTER:
        logger.error("infixpointer")
    elif code == CODE_CUSTOM_LEN:
        logger.error("custom_len")
    elif code == CODE_CUSTOM_FIXED:
        logger.error("custom_fixed")
    else:
        logger.error("unknown")
    raise NotImplementedError("Could not determine value")


if __name__ == "__main__":

    @dataclass
    class Test:
        a: int
        b: str
        c: List[int]

    convert_structure({}, Dict[str, int])
    convert_structure({Val("hello"): Val(3)}, Dict[str, int])
    convert_structure((Val(1), Val("two"), [Val(3), Val(4)]), Test)
