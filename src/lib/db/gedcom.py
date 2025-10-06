from dataclasses import dataclass, field
from enum import Enum
import sys
from typing import Generator, Literal, TextIO, List, Optional, Dict, Any, Union
import re


class GedcomTag(Enum):
    INDIVIDUAL = "INDI"
    FAMILY = "FAM"
    NAME = "NAME"
    SEX = "SEX"
    BIRTH = "BIRT"
    DEATH = "DEAT"
    MARRIAGE = "MARR"
    DATE = "DATE"
    PLACE = "PLAC"
    NOTE = "NOTE"
    UNKNOWN = "UNKNOWN"


@dataclass
class GedcomNode:
    level: int
    tag: GedcomTag
    value: Optional[str] = None
    children: List["GedcomNode"] = field(default_factory=list)


@dataclass
class GedcomIndividual:
    id: str
    name: Optional[str] = None
    sex: Optional[str] = None
    birth: Optional[str] = None
    death: Optional[str] = None
    notes: List[str] = field(default_factory=list)


@dataclass
class GedcomFamily:
    id: str
    husband: Optional[str] = None
    wife: Optional[str] = None
    children: List[str] = field(default_factory=list)
    marriage: Optional[str] = None
    notes: List[str] = field(default_factory=list)


class HeaderTag(Enum):
    SOURCE = "SOUR"
    VERSION = "VERS"
    NAME = "NAME"
    CORPORATION = "CORP"
    ADDRESS = "ADDR"
    CONTACT = "CONT"
    PHONE = "PHON"
    DESTINATION = "DEST"
    DATE = "DATE"
    CHARSET = "CHAR"
    FILE = "FILE"
    SUBM = "SUBM"


@dataclass
class GEDCOM:
    version: Optional[str] = None
    format: Optional[str] = None


@dataclass
class Header(Dict[HeaderTag, Optional[str]]):
    # source: Optional[str] = None
    # version: Optional[str] = None
    # name: Optional[str] = None
    # corporation: Optional[str] = None
    # address: Optional[str] = None
    # contact: Optional[str] = None
    # phone: Optional[str] = None
    # file: Optional[str] = None
    # encoding: Optional[str] = None
    # submitter: Optional[str] = None

    # def __setattr__(self, name: str, value: Any) -> None:
    #     return self.__setitem__(HeaderTag[name.upper()], value)

    def __setitem__(self, key: Union[HeaderTag, str], value: str | None) -> None:
        try:
            key = HeaderTag(key)
            super().__setitem__(key, value)
            return
        except ValueError:
            super().__setitem__(key, value)


@dataclass
class Block:
    level: int
    tag: str
    pointer: Optional[str] = None
    value: Optional[str] = None
    children: List["Block"] = field(default_factory=list)
    _children_idx: dict[str, int] = field(default_factory=dict)
    _children_tag_idx: dict[str, List[int]] = field(default_factory=dict)
    # children: Dict[str, "Block"] = field(default_factory=dict)

    def add_child(self, child: "Block"):
        self.children.append(child)
        self._children_idx[child.pointer or child.tag] = len(self.children) - 1
        if child.tag not in self._children_tag_idx:
            self._children_tag_idx[child.tag] = []
        self._children_tag_idx[child.tag].append(len(self.children) - 1)
        # self.children[child.pointer or child.tag] = child

    def __iter__(self):
        return iter(self.children)

    def __getitem__(self, key: str) -> List["Block"]:
        if key in self._children_idx:
            return [self.children[self._children_idx[key]]]
        if key in self._children_tag_idx:
            idxs = self._children_tag_idx[key]
            return [self.children[i] for i in idxs]
        raise KeyError(f"No child with key {key}")

    @classmethod
    def from_block(cls, block: "Block") -> "Block":
        new_block = cls(block.level, block.tag, block.pointer, block.value)
        for child in block.children:
            new_block.add_child(child)
        return new_block


@dataclass
class GEDIndividual(Block):
    @property
    def sex(self) -> Optional[Literal["M", "F", "U", "X"]]:
        try:
            return self["SEX"][0].value  # type: ignore
        except KeyError:
            return None

    @property
    def name(self) -> str:
        try:
            return self["NAME"][0].value  # type: ignore
        except KeyError:
            return None

    @property
    def spouse(self) -> list[str]:
        try:
            return self["FAMS"]
        except KeyError:
            return []

    # TODO: continue adding properties

    @classmethod
    def from_block(cls, block: Block) -> "GEDIndividual":
        return cls(
            block.level,
            block.tag,
            block.pointer,
            block.value,
            block.children,
            block._children_idx,
        )

    def __str__(self):
        return f"Individual {self.name}"


@dataclass
class GEDFamily(Block):
    pass


@dataclass
class GEDNote(Block):
    def __str__(self):
        val = self.value or ""
        for child in self.children:
            if child.tag == "CONT":
                val += "\n" + (child.value or "")
        return val

    @classmethod
    def from_block(cls, block: Block) -> "GEDNote":
        super().from_block(block)
        # return cls(
        #     block.level,
        #     block.tag,
        #     block.pointer,
        #     block.value,
        #     block.children,
        #     block._children_idx,
        # )


@dataclass
class GEDSource(Block):
    pass


@dataclass
class GEDRepository(Block):
    pass


@dataclass
class GEDMultimedia(Block):
    pass


@dataclass
class GedcomData:
    header: Block = field(default_factory=lambda: Block(0, "HEAD"))
    submitter: Block = field(default_factory=lambda: Block(0, "SUBM"))
    individuals: Dict[str, Block] = field(default_factory=dict)  # indi
    families: Dict[str, Block] = field(default_factory=dict)  # fam
    notes: Dict[str, Block] = field(default_factory=dict)  # note
    sources: Dict[str, Block] = field(default_factory=dict)  # sour
    repository: Optional[Block] = None  # repo
    multimedia: Dict[str, Block] = field(default_factory=dict)  # obje


class InvalidDepth(RuntimeError): ...


class GedcomParser:
    def __init__(self):
        self.individuals: Dict[str, GedcomIndividual] = {}
        self.families: Dict[str, GedcomFamily] = {}
        self.header: Header = Header()
        self.nodes: List[GedcomNode] = []

    @staticmethod
    def read_line(
        io: TextIO,
    ) -> Optional[tuple[Optional[int], Optional[str], Optional[str], Optional[str]]]:
        line = io.readline()
        if not line:
            return None
        match = re.match(r"^(\d+)\s+(@[^@]+@)?\s*(\w+)(?:\s+(.*))?", line)
        if not match:
            return None
        level = int(match.group(1))
        pointer = match.group(2)
        tag = match.group(3)
        value = match.group(4) if match.group(4) else None
        return level, pointer, tag, value

    def parsed(self, io: TextIO):
        res = GedcomData()
        while block := self.parse_block(io, -1):
            # res[block.pointer or block.tag] = block
            match block.tag:
                case "HEAD":
                    res.header = GedcomParser.specialize_block(block)
                case "SUBM":
                    res.submitter = GedcomParser.specialize_block(block)
                case "INDI":
                    if block.pointer:
                        res.individuals[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "FAM":
                    if block.pointer:
                        res.families[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "NOTE":
                    if block.pointer:
                        res.notes[block.pointer] = GedcomParser.specialize_block(block)
                case "SOUR":
                    if block.pointer:
                        res.sources[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "REPO":
                    res.repository = GedcomParser.specialize_block(block)
                case "OBJE":
                    if block.pointer:
                        res.multimedia[block.pointer] = GedcomParser.specialize_block(
                            block
                        )
                case "TRLR":
                    break
                case _:
                    print(f"Unknown block: {block.tag}", file=sys.stderr)
        return res

    def parse_block(
        self,
        io: TextIO,
        root_level: int,
    ):
        level = ""
        i = 0
        while c := io.read(1):
            if c == " ":
                break
            level += c
            i += 1
        io.seek(io.tell() - i - 1)
        if not level.isdigit():
            return None
        level = int(level)
        if level <= root_level:
            # print("no go back")
            return None
        level, pointer, tag, value = GedcomParser.read_line(io)
        block = Block(level, tag, pointer, value)
        while child := self.parse_block(io, level):
            block.add_child(child)
        return GedcomParser.specialize_block(block)

    @staticmethod
    def specialize_block(block: Block) -> Block:
        match block.tag:
            case "NOTE":
                return GEDNote.from_block(block)
            case "INDI":
                return GEDIndividual.from_block(block)
            case _:
                return block

    @staticmethod
    def from_file(filepath: str) -> GedcomData:
        with open(filepath, "r", encoding="utf-8-sig") as f:
            parser = GedcomParser()
            return parser.parsed(f)


class GedcomExporter:
    @staticmethod
    def export_summary(data: GedcomData) -> Generator[str, None, None]:
        # head
        yield from GedcomExporter._export_block(data.header)
        # subm
        yield from GedcomExporter._export_block(data.submitter)
        # indi
        for indi in data.individuals.values():
            yield from GedcomExporter._export_block(indi)
        # fam
        for fam in data.families.values():
            yield from GedcomExporter._export_block(fam)
        # note
        for note in data.notes.values():
            yield from GedcomExporter._export_block(note)
        # sour
        for sour in data.sources.values():
            yield from GedcomExporter._export_block(sour)
        # repo
        if data.repository:
            yield from GedcomExporter._export_block(data.repository)
        # obje
        for obje in data.multimedia.values():
            yield from GedcomExporter._export_block(obje)
        # trlr
        yield "0 TRLR"

    @staticmethod
    def _export_block(block: Block) -> Generator[str, None, None]:
        line = f"{block.level} "
        if block.pointer:
            line += f"{block.pointer} "
        line += block.tag
        if block.value:
            line += f" {block.value}"
        yield line
        for child in block:
            yield from GedcomExporter._export_block(child)


if __name__ == "__main__":
    path = "/home/nico/Dev/Legacy/geneweb/examples/uk.ged"
    import pickle

    r = GedcomParser.from_file(path)
    print(r.individuals["@I399@"])
    # print(r.submitter["FAM"])
    # for l in GedcomExporter.export_summary(r):
    #     print(l)

    # print(r.header["SOUR"]["VERS"])
    # p = pickle.dumps(r, protocol=4)
    # print(p)
    # dp = pickle.loads(p)
    # print(dp)
    # print(dp.header["SOUR"]["VERS"])
    # print(list((b.tag, b.value) for b in r["@SUBM@"]))
    # print(parser.export())
    # for i in range(max(0, len(parser.families) - 1)):
    #     print(list(parser.families.items())[i])
