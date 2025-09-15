"""
GEDCOM record processing utilities
"""
from typing import List, Optional, Tuple, Dict, Any
from models import Gen, Record

def find_field(tag: str, records: List[Record]) -> Optional[Record]:
    """Find first field with given tag"""
    for record in records:
        if record.rlab == tag:
            record.rused = True
            return record
    return None

def find_all_fields(tag: str, records: List[Record]) -> List[Record]:
    """Find all fields with given tag"""
    result = []
    for record in records:
        if record.rlab == tag:
            record.rused = True
            result.append(record)
    return result

def find_field_with_value(tag: str, value: str, records: List[Record]) -> bool:
    """Find field with specific tag and value"""
    for record in records:
        if record.rlab == tag and record.rval == value:
            record.rused = True
            return True
    return False

def treat_notes(gen: Gen, note_records: List[Record]) -> str:
    """Process note records into text"""
    if not note_records:
        return ""

    notes = []
    for record in note_records:
        if record.rval:
            notes.append(record.rval)

    return "<br>\n".join(notes)

def source(gen: Gen, record: Record) -> Tuple[str, List[Record]]:
    """Process source record"""
    return "", []

def strip_spaces(s: str) -> str:
    """Strip spaces"""
    return s.strip()

def extract_addr(addr: str) -> str:
    """Extract GEDCOM address from reference"""
    if addr and addr.startswith('@'):
        try:
            end_pos = addr.index('@', 1)
            return addr[:end_pos + 1]
        except ValueError:
            return addr
    return addr

def parse_name(name_field: str) -> Tuple[str, str]:
    """Parse GEDCOM name field - exact OCaml logic"""
    if not name_field:
        return "x", "?"

    name_field = name_field.strip()

    invert = name_field.startswith('/')
    if invert:
        name_field = name_field[1:]

    parts = name_field.split('/')

    if len(parts) >= 2:
        first_name = parts[0].strip()
        surname = parts[1].strip()
    else:
        first_name = name_field.strip()
        surname = ""

    if invert:
        first_name, surname = surname, first_name

    first_name = first_name if first_name else "x"
    surname = surname if surname else "?"

    return first_name, surname

def add_string(gen: Gen, s: str) -> int:
    """Add string to string table and return index"""
    if s in gen.g_hstr:
        return gen.g_hstr[s]

    i = gen.g_str.tlen

    if i >= len(gen.g_str.arr):
        new_len = max(len(gen.g_str.arr) * 2, i + 10)
        gen.g_str.arr.extend([""] * (new_len - len(gen.g_str.arr)))

    gen.g_str.arr[i] = s
    gen.g_str.tlen += 1
    gen.g_hstr[s] = i

    return i

def rebuild_text(record: Record) -> str:
    """Rebuild text from GEDCOM record with CONC/CONT handling"""
    text = strip_spaces(record.rval)

    for r in record.rsons:
        r.rused = True
        content = r.rval

        end_space = ""
        if len(content) > 0 and content[-1] == ' ':
            end_space = " "

        content = strip_spaces(content)

        if r.rlab == "CONC":
            text += content + end_space
        elif r.rlab == "CONT":
            text += "<br>\n" + content + end_space

    return text

def designation(strings: List[str], person: Dict[str, Any]) -> str:
    """Create person designation string - exact OCaml logic"""
    first_name_idx = person.get("first_name", 0)
    surname_idx = person.get("surname", 0)
    occ = person.get("occ", 0)

    if first_name_idx < len(strings):
        fn = strings[first_name_idx]
    else:
        fn = "?"

    if surname_idx < len(strings):
        sn = strings[surname_idx]
    else:
        sn = "?"

    return f"{fn}.{occ} {sn}"

def extract_notes(gen: Gen, record_list: List[Record]) -> List[Tuple[str, str]]:
    """Extract notes from record list - exact OCaml logic"""
    lines = []

    for r in record_list:
        for sub_r in [r] + r.rsons:
            sub_r.rused = True

            if sub_r.rlab == "NOTE":
                if sub_r.rval.startswith('@') and sub_r.rval.endswith('@'):
                    addr = extract_addr(sub_r.rval)
                    note_record = gen.find_notes_record(addr)
                    if note_record:
                        flat_notes = flatten_notes(note_record.rsons)
                        lines.extend([("NOTE", note_record.rcont)] + flat_notes)
                    else:
                        print(f"Note {addr} not found")
                else:
                    lines.append((sub_r.rlab, sub_r.rval))
            elif sub_r.rlab in ["CONC", "CONT"]:
                lines.append((sub_r.rlab, sub_r.rval))

    return lines

def flatten_notes(records: List[Record]) -> List[Tuple[str, str]]:
    """Flatten note records recursively"""
    lines = []

    for r in records:
        if r.rlab in ["CONC", "CONT", "NOTE"]:
            lines.append((r.rlab, r.rval))
            lines.extend(flatten_notes(r.rsons))

    return lines

def notes_from_source_record(source_records: List[Record]) -> str:
    """Extract notes from source records"""
    title = ""
    text = ""

    for r in source_records:
        if r.rlab == "TITL":
            title_text = rebuild_text(r)
            if title_text:
                title = f"<b>{title_text}</b>"
        elif r.rlab == "TEXT":
            text_content = rebuild_text(r)
            if text_content:
                text = "<br>\n" + text_content if title else text_content

    return title + text

def decode_title(title_str: str) -> Tuple[str, str, int]:
    """Decode title string into components"""
    if not title_str:
        return "", "", 0

    parts = title_str.split(',')

    title = parts[0].strip() if len(parts) > 0 else ""

    if len(parts) == 1:
        return title, "", 0
    elif len(parts) == 2:
        second = parts[1].strip()
        try:
            nth = int(second)
            return title, "", nth
        except ValueError:
            return title, second, 0
    else:
        place = parts[1].strip()
        try:
            nth = int(parts[2].strip())
            return title, place, nth
        except ValueError:
            return title, place, 0

def list_of_string(s: str, separator: str = ',') -> List[str]:
    """Split string into list by separator"""
    if not s:
        return []

    parts = s.split(separator)
    return [part.strip() for part in parts if part.strip()]

def purge_list(string_list: List[str]) -> List[str]:
    """Remove empty strings from list"""
    return [s for s in string_list if s.strip()]

def p_index_from(s: str, start: int, char: str) -> int:
    """Find character in string starting from position"""
    try:
        return s.index(char, start)
    except ValueError:
        return len(s)

def strip_sub(s: str, start: int, length: int) -> str:
    """Extract and strip substring"""
    if start >= len(s):
        return ""

    end = min(start + length, len(s))
    return s[start:end].strip()

def html_text_of_tags(description: str, records: List[Record]) -> str:
    """Convert record tags to HTML text"""
    if not records:
        return ""

    lines = []
    if (description):
        lines.append(f"-- GEDCOM ({description}) --")
    else:
        lines.append("-- GEDCOM --")

    def format_record(r: Record, level: int) -> List[str]:
        result = []
        indent = "  " * level

        line = f"{indent}{level} {r.rlab}"
        if r.rval:
            line += f" {r.rval}"
        if r.rcont:
            line += f" {r.rcont}"

        result.append(line)

        for sub_r in r.rsons:
            result.extend(format_record(sub_r, level + 1))

        return result

    for r in records:
        lines.extend(format_record(r, 1))

    return "\n".join(lines)

def build_remain_tags(records: List[Record], processed_tags: set = None) -> List[Record]:
    """Build list of remaining unprocessed tags"""
    if processed_tags is None:
        processed_tags = {"NAME", "SEX", "BIRT", "DEAT", "BAPM", "CHR", "BURI",
                         "CREM", "FAMC", "FAMS", "MARR", "DIV", "HUSB", "WIFE",
                         "CHIL", "NOTE", "SOUR", "DATE", "PLAC"}

    remaining = []

    for r in records:
        if not r.rused and r.rlab not in processed_tags:
            sub_remaining = build_remain_tags(r.rsons, processed_tags)

            remaining_record = Record(
                rlab=r.rlab,
                rval=r.rval,
                rcont=r.rcont,
                rsons=sub_remaining,
                rpos=r.rpos,
                rused=r.rused
            )
            remaining.append(remaining_record)

    return remaining

def indi_lab(tag: str) -> bool:
    """Check if tag is a standard individual tag"""
    standard_tags = {
        "ADOP", "ASSO", "BAPM", "BIRT", "BURI", "CHR", "CREM", "DEAT",
        "FAMC", "FAMS", "NAME", "NOTE", "OBJE", "OCCU", "SEX", "SOUR", "TITL"
    }
    return tag in standard_tags

def treat_indi_title(gen: Gen, public_name: str, record: Record) -> Dict[str, Any]:
    """Process individual title record"""
    title, place, nth = decode_title(record.rval)

    date_start = None
    date_end = None
    date_field = find_field("DATE", record.rsons)
    if date_field:
        pass

    name_type = "Tnone"
    note_field = find_field("NOTE", record.rsons)
    if note_field:
        if title == "":
            name_type = "Tnone"
            title = note_field.rval.strip()
        elif note_field.rval == public_name:
            name_type = "Tmain"
        else:
            name_type = ("Tname", add_string(gen, note_field.rval.strip()))

    return {
        "t_name": name_type,
        "t_ident": add_string(gen, title),
        "t_place": add_string(gen, place),
        "t_date_start": date_start,
        "t_date_end": date_end,
        "t_nth": nth
    }

def concat_text(s1: str, s2: str, separator: str) -> str:
    """Concatenate two text strings with separator"""
    if not s1:
        return s2
    if not s2:
        return s1

    return s1 + separator + s2

def witness_kind_of_rval(rval: str) -> str:
    """Convert GEDCOM witness relationship to witness kind"""
    mapping = {
        "GODP": "Witness_GodParent",
        "officer": "Witness_CivilOfficer",
        "Civil officer": "Witness_CivilOfficer",
        "Registry officer": "Witness_CivilOfficer",
        "Religious officer": "Witness_ReligiousOfficer",
        "Officiating priest": "Witness_ReligiousOfficer",
        "Informant": "Witness_Informant",
        "Attending": "Witness_Attending",
        "Mentioned": "Witness_Mentioned",
        "Other": "Witness_Other"
    }

    return mapping.get(rval, "Witness")

def infer_death(birth_date: Optional[Any], baptism_date: Optional[Any]) -> str:
    """Infer death status based on birth/baptism dates"""
    try:
        from ged2gwb import alive_years, dead_years
        import datetime

        current_year = datetime.datetime.now().year

        birth_year = None
        if birth_date:
            pass

        if not birth_year and baptism_date:
            pass

        if birth_year:
            age = current_year - birth_year
            if age > dead_years:
                return "DeadDontKnowWhen"
            elif age < alive_years:
                return "NotDead"
            else:
                return "DontKnowIfDead"

        return "DontKnowIfDead"

    except ImportError:
        return "DontKnowIfDead"
