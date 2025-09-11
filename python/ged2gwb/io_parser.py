import io
from typing import Optional
from .charset import utf8_of_string, charset_option
from .types import Record

def open_in_bin_with_bom_check(fname: str) -> io.BufferedReader:
    ic = open(fname, "rb")
    start = ic.read(3)
    if start == b'\xef\xbb\xbf':
        # set global charset option if needed
        try:
            charset_option = "UTF-8"  # caller's module-level var shadowing possible
        except Exception:
            pass
        return ic
    ic.seek(0)
    return ic

def find_lev0(binfile: io.BufferedReader):
    while True:
        bp = binfile.tell()
        line = binfile.readline()
        if not line:
            return None
        try:
            s = line.decode("latin-1")
        except Exception:
            s = line.decode("utf-8", errors="replace")
        s = s.rstrip("\r\n")
        if s == "":
            continue
        parts = s.split(None, 2)
        if parts and parts[0] == '0':
            r1 = parts[1] if len(parts) >= 2 else ""
            r2 = parts[2] if len(parts) >= 3 else ""
            return bp, r1, r2

def get_lev0(binfile: io.BufferedReader) -> Optional[Record]:
    state = find_lev0(binfile)
    if state is None:
        return None
    bp, r1, r2 = state
    children = []
    rpos = 1
    while True:
        pos = binfile.tell()
        line = binfile.readline()
        if not line:
            break
        try:
            s = line.decode("latin-1")
        except Exception:
            s = line.decode("utf-8", errors="replace")
        s = s.rstrip("\r\n")
        if s == "":
            continue
        parts = s.split(None, 2)
        if parts and parts[0] == '0':
            binfile.seek(pos)
            break
        if len(parts) >= 2:
            lab = parts[1]
            rval = parts[2] if len(parts) >= 3 else ""
            child = Record(rlab=lab, rval=utf8_of_string(rval), rcont="", rsons=[], rpos=rpos)
            children.append(child)
        rpos += 1
    rlab = r1 if r2 == "" else r2
    rval = "" if r2 == "" else r1
    rec = Record(rlab=rlab, rval=utf8_of_string(rval), rcont="", rsons=children, rpos=bp)
    return rec

# helper wrappers used by records.treat_notes/source to fetch by addr
def find_notes_record(gen, addr: str) -> Optional[Record]:
    pos = gen.g_not.get(addr)
    if pos is None:
        return None
    try:
        gen.g_ic.seek(pos)
        return get_lev0(gen.g_ic)
    except Exception:
        return None

def find_sources_record(gen, addr: str) -> Optional[Record]:
    pos = gen.g_src.get(addr)
    if pos is None:
        return None
    try:
        gen.g_ic.seek(pos)
        return get_lev0(gen.g_ic)
    except Exception:
        return None
