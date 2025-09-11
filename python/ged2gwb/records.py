from typing import List, Optional, Tuple
from .utils import strip_spaces, strip_newlines, print_location
from .charset import utf8_of_string
from .types import Record

def ident_slash(s: str) -> str:
    try:
        i = s.index('/')
        return s[:i]
    except ValueError:
        return s

def parse_name(s: str) -> Tuple[str, str]:
    s = s.strip()
    parts = s.split('/')
    if len(parts) >= 3:
        first = parts[0].strip()
        surname = parts[1].strip()
    else:
        toks = s.split()
        first = toks[0] if toks else "?"
        surname = toks[-1] if len(toks) > 1 else "?"
    if first == "":
        first = "x"
    if surname == "":
        surname = "?"
    return first, surname

def find_field(lab: str, rl: List[Record]) -> Optional[Record]:
    for r in rl:
        if r.rlab == lab:
            r.rused = True
            return r
    return None

def find_all_fields(lab: str, rl: List[Record]) -> List[Record]:
    out = []
    for r in rl:
        if r.rlab == lab:
            r.rused = True
            out.append(r)
    return out

def find_field_with_value(lab: str, v: str, rl: List[Record]) -> bool:
    for r in rl:
        if r.rlab == lab and r.rval == v:
            r.rused = True
            return True
    return False

def rebuild_text(r: Record) -> str:
    s = strip_spaces(r.rval)
    for e in r.rsons:
        e.rused = True
        n = strip_spaces(e.rval)
        if e.rlab == "CONC":
            s = s + n
        elif e.rlab == "CONT":
            s = s + "<br>\n" + n
        else:
            s = s + n
    return s

def flatten_notes(rl: List[Record]) -> List[Tuple[str,str]]:
    out = []
    for r in rl:
        if r.rlab in ("CONC", "CONT", "NOTE"):
            out.append((r.rlab, r.rval))
            out.extend(flatten_notes(r.rsons))
        else:
            out.extend(flatten_notes(r.rsons))
    return out

# treat_notes/note/source expect access to gen to resolve external NOTE/SOUR refs;
# higher-level code (io_parser/pipeline) will call find_notes_record/find_sources_record.
def treat_notes(gen, rl: List[Record]) -> str:
    # simple flatten implementation using extract of NOTE/CONT/CONC
    lines = []
    for r in rl:
        if r.rlab == "NOTE":
            lines.append(("NOTE", r.rval))
        elif r.rlab in ("CONT", "CONC"):
            lines.append((r.rlab, r.rval))
        else:
            lines.extend(flatten_notes(r.rsons))
    buf = []
    for lab, n in lines:
        n = strip_spaces(n)
        if not buf:
            buf.append(n)
        elif lab in ("CONT", "NOTE"):
            buf.append("<br>\n" + n)
        else:
            buf.append(n)
    return strip_newlines("".join(buf))

def note(gen, r: Record):
    nf = find_field("NOTE", r.rsons)
    if nf:
        if nf.rval.startswith('@'):
            rec = gen.find_notes_record(nf.rval)
            if rec:
                return strip_spaces(rec.rcont), rec.rsons
            else:
                print_location(nf.rpos)
                return "", []
        return strip_spaces(nf.rval), nf.rsons
    return "", []

def source(gen, r: Record):
    sf = find_field("SOUR", r.rsons)
    if sf:
        if sf.rval.startswith('@'):
            rec = gen.find_sources_record(sf.rval)
            if rec:
                return strip_spaces(rec.rcont), rec.rsons
            else:
                print_location(sf.rpos)
                return "", []
        return strip_spaces(sf.rval), sf.rsons
    return "", []
