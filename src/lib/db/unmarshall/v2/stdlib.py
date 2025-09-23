import dataclasses
import enum
import struct
from typing import Any, Generic, Optional, TypeVar, TYPE_CHECKING

if TYPE_CHECKING:
    from lib.db.unmarshall.v2.dbdisk import RecordAccess


# def input_value(f: io.BufferedReader, data_class: Type):
#     # Placeholder for actual OCaml unmarshalling logic
#     # For now, just return an empty dict or appropriate structure
#     logging.warning("input_value is a placeholder and needs implementation")
#     return data_class()


def input_binary_int(f):
    # Reads 4 bytes and interprets as a big-endian signed integer
    return struct.unpack(">i", f.read(4))[0]


# Ref = Tuple[int]
@dataclasses.dataclass
class Ref(Generic[T := TypeVar("T")]):
    ref: int


class StringRef(Ref[str]):
    _string_registry: Any = None

    def __init__(self, ref: int):
        super().__init__(ref)
        self._str: Optional[str] = None

    @staticmethod
    def set_record_access(strings: "RecordAccess[str]") -> None:
        StringRef._string_registry = strings

    @staticmethod
    def get_record_access() -> Any:
        # print(f"Getting strings: {StringRef.strings=}")
        return StringRef._string_registry

    def get_str(self):
        if not self._str:
            if StringRef.get_record_access() is None:
                return f"<uninitialized>"
            if self.ref is None:
                return ""
            tmp: str = StringRef.get_record_access().get(self.ref)
            if tmp is None:
                return f"<missing {self.ref}>"
            self._str = str(tmp)
        return self._str

    def __str__(self):
        return self.get_str() or "<None>"

    def __repr__(self):
        return f'StringRef(ref={self.ref}, value="{self.get_str()}")'

    def __hash__(self):
        return self.ref.__hash__()


class TypeEnum(enum.Enum):
    pass
    # def __init__(self, *args):
    #     v, *oth = args
    #     opts = list(self.__class__)
    #     if isinstance(v, int) and 0 <= v < len(opts) and type(opts[v]) is type(Literal[Any]):
    #         self._name_ = opts[v].name
    #         self._value_ = opts[v].value
    #     else:
