from gen_arrays import add_string
from typing import List, Dict, Optional, TextIO, Tuple
from models import Gen, Record
import sys
from charset import CharsetConverter, detect_charset_from_bom

class GedcomStream:
    def __init__(self, filename: str):
        self.filename = filename
        self.line_count = 0
        self.charset_converter = None
        self._file = None
        self.parser = None

    def open(self):
        self._file = open(self.filename, 'rb')
        header = self._file.read(1024)
        self._file.seek(0)

        charset = detect_charset_from_bom(header)
        if charset:
            self.charset_converter = CharsetConverter(charset)
        else:
            self.charset_converter = CharsetConverter('ASCII')

        self.parser = GedcomLineParser(self._file, self.charset_converter)

    def close(self):
        if self._file:
            self._file.close()
            self._file = None

    def seek(self, pos: int):
        if self._file:
            self._file.seek(pos)
            self.line_count = 0

class GedcomLineParser:
    def __init__(self, file_handle: TextIO, charset_converter: CharsetConverter):
        self.file = file_handle
        self.charset_converter = charset_converter
        self.line_count = 0

    def read_line(self) -> Optional[str]:
        line_bytes = self.file.readline()
        if not line_bytes:
            return None

        self.line_count += 1
        line_str = self.charset_converter.convert_bytes(line_bytes)
        return line_str.rstrip('\r\n')

    def parse_record_line(self, line: str) -> Optional[Tuple[int, str, str, str]]:
        if not line:
            return None

        parts = line.split(' ', 2)
        if len(parts) < 2:
            return None

        try:
            level = int(parts[0])
        except ValueError:
            return None

        if len(parts) == 2:
            tag = parts[1]
            value = ""
        else:
            tag = parts[1]
            value = parts[2]

        return level, tag, value, ""

    def find_lev0(self) -> Optional[Tuple[int, str, str]]:
        start_pos = self.file.tell()
        line = self.read_line()
        if not line:
            return None

        parsed = self.parse_record_line(line)
        if not parsed or parsed[0] != 0:
            return None

        level, tag, value, _ = parsed

        if tag.startswith('@') and tag.endswith('@'):
            return start_pos, tag, value
        else:
            return start_pos, tag, value

class GedcomParser:
    def __init__(self, filename: str):
        self.filename = filename
        self.line_count = 0

    def build_indices(self, gen: Gen):
        with open(self.filename, 'r', encoding='utf-8') as f:
            for line_num, line in enumerate(f, 1):
                line = line.strip()
                if not line:
                    continue

                parts = line.split(' ', 2)
                if len(parts) >= 3 and parts[0] == "0":
                    level, tag, value = parts[0], parts[1], parts[2]
                    if value == "NOTE":
                        gen.g_not[tag] = line_num
                    elif value == "SOUR":
                        gen.g_src[tag] = line_num

    def process_persons(self, gen: Gen):
        with open(self.filename, 'r', encoding='utf-8') as f:
            current_record = None
            for line in f:
                line = line.strip()
                if not line:
                    continue

                parts = line.split(' ', 2)
                if len(parts) >= 2 and parts[0] == "0":
                    if current_record and current_record.get('type') == 'INDI':
                        self._process_individual(gen, current_record)

                    if len(parts) >= 3 and parts[2] == "INDI":
                        current_record = {
                            'type': 'INDI',
                            'id': parts[1],
                            'data': []
                        }
                    else:
                        current_record = None

                elif current_record and parts[0] in ["1", "2"]:
                    current_record['data'].append(line)

            if current_record and current_record.get('type') == 'INDI':
                self._process_individual(gen, current_record)

    def process_families(self, gen: Gen):
        with open(self.filename, 'r', encoding='utf-8') as f:
            current_record = None
            trailer_found = False

            for line in f:
                line = line.strip()
                if not line:
                    continue

                parts = line.split(' ', 2)
                if len(parts) >= 2 and parts[0] == "0":
                    if current_record and current_record.get('type') == 'FAM':
                        self._process_family(gen, current_record)

                    if len(parts) >= 3 and parts[2] == "FAM":
                        current_record = {
                            'type': 'FAM',
                            'id': parts[1],
                            'data': []
                        }
                    elif parts[1] == "TRLR":
                        trailer_found = True
                        print("*** Trailer ok", file=sys.stderr)
                        current_record = None
                    else:
                        current_record = None

                elif current_record and parts[0] in ["1", "2"]:
                    current_record['data'].append(line)

            if current_record and current_record.get('type') == 'FAM':
                self._process_family(gen, current_record)

            if not trailer_found:
                print("Warning: No TRLR record found", file=sys.stderr)

    def _process_individual(self, gen: Gen, record: Dict):
        person_id = record['id']

        from gen_arrays import per_index
        ip = per_index(gen, person_id)

        first_name_str = "?"
        surname_str = "?"
        birth_place = ""
        baptism_place = ""

        for line in record['data']:
            parts = line.split(' ', 2)
            if len(parts) >= 2:
                tag = parts[1]
                value = parts[2] if len(parts) > 2 else ""

                if tag == "NAME":
                    name_parts = self._parse_gedcom_name(value)
                    if name_parts:
                        first_name_str, surname_str = name_parts

        current_event = None
        for line in record['data']:
            parts = line.split(' ', 2)
            if len(parts) >= 2:
                level = parts[0]
                tag = parts[1]
                value = parts[2] if len(parts) > 2 else ""

                if level == "1":
                    if tag == "BIRT":
                        current_event = "BIRT"
                    elif tag == "BAPM" or tag == "CHR":
                        current_event = "BAPM"
                    else:
                        current_event = None
                elif level == "2" and tag == "PLAC" and current_event:
                    if current_event == "BIRT":
                        birth_place = value.strip()
                        from gen_arrays import add_string
                        place_idx = add_string(gen, birth_place)
                    elif current_event == "BAPM":
                        baptism_place = value.strip()
                        from gen_arrays import add_string
                        place_idx = add_string(gen, baptism_place)

        from gen_arrays import add_string
        first_name_idx = add_string(gen, first_name_str)
        surname_idx = add_string(gen, surname_str)

        person_data = {
            'first_name': first_name_idx,
            'surname': surname_idx,
            'occ': ip,
            'key_index': ip,
            'sex': 'Neuter'
        }

        for line in record['data']:
            parts = line.split(' ', 2)
            if len(parts) >= 2 and parts[1] == "SEX":
                value = parts[2] if len(parts) > 2 else ""
                if value == "M":
                    person_data['sex'] = 'Male'
                elif value == "F":
                    person_data['sex'] = 'Female'

        ascend = {"parents": None, "consang": -1}
        union = {"family": []}

        gen.g_per.arr[ip] = ("Right3", (person_data, ascend, union))

    def _process_family(self, gen: Gen, record: Dict):
        family_id = record['id']

        from gen_arrays import fam_index
        ifam = fam_index(gen, family_id)

        husband_id = None
        wife_id = None
        children_ids = []
        marriage_place = ""

        for line in record['data']:
            parts = line.split(' ', 2)
            if len(parts) >= 2:
                tag = parts[1]
                value = parts[2] if len(parts) > 2 else ""

                if tag == "HUSB":
                    husband_id = value
                elif tag == "WIFE":
                    wife_id = value
                elif tag == "CHIL":
                    children_ids.append(value)

        current_event = None
        for line in record['data']:
            parts = line.split(' ', 2)
            if len(parts) >= 2:
                level = parts[0]
                tag = parts[1]
                value = parts[2] if len(parts) > 2 else ""

                if level == "1" and tag == "MARR":
                    current_event = "MARR"
                elif level == "2" and tag == "PLAC" and current_event == "MARR":
                    marriage_place = value.strip()
                    from gen_arrays import add_string
                    place_idx = add_string(gen, marriage_place)

        family_data = {
            'fam_index': ifam,
            'marriage': None,
            'relation': 'Married'
        }

        couple = (None, None)
        if husband_id:
            from gen_arrays import per_index
            husband_idx = per_index(gen, husband_id)
            couple = (husband_idx, couple[1])
        if wife_id:
            from gen_arrays import per_index
            wife_idx = per_index(gen, wife_id)
            couple = (couple[0], wife_idx)

        child_indices = []
        for child_id in children_ids:
            from gen_arrays import per_index
            child_idx = per_index(gen, child_id)
            child_indices.append(child_idx)

        descend = {"children": child_indices}
        gen.g_fam.arr[ifam] = ("Right3", (family_data, couple, descend))

    def _parse_gedcom_name(self, name_value: str) -> Optional[Tuple[str, str]]:
        if not name_value:
            return None

        if '/' in name_value:
            parts = name_value.split('/')
            if len(parts) >= 2:
                first_part = parts[0].strip()
                surname_part = parts[1].strip() if parts[1].strip() else "?"
                first_name = first_part if first_part else "?"
                return (first_name, surname_part)

        parts = name_value.strip().split()
        if len(parts) >= 2:
            first_name = ' '.join(parts[:-1])
            surname = parts[-1]
            return (first_name, surname)
        elif len(parts) == 1:
            return (parts[0], "?")

        return ("?", "?")

def create_gen() -> Gen:
    gen = Gen()
    add_ocaml_system_strings(gen)
    return gen

def add_ocaml_system_strings(gen: Gen):
    add_string(gen, "")
    add_string(gen, "?")
    add_string(gen, "x")

    particles = [
        "de", "du", "des", "d'", "da", "dal", "dall'", "dalla", "dalle",
        "del", "della", "delle", "dello", "di", "el", "la", "las", "le",
        "les", "lo", "los", "mac", "mc", "of", "på", "te", "ten", "ter",
        "van", "von", "zu", "zum", "zur"
    ]
    for particle in particles:
        add_string(gen, particle)

    pevent_strings = [
        "birth", "baptism", "death", "burial", "cremation",
        "accomplishment", "acquisition", "distinction", "bar mitzvah", "bat mitzvah",
        "benediction", "census", "circumcision", "confirmation", "diploma",
        "decoration", "education", "election", "emigration", "excommunication",
        "first communion", "funeral", "graduate", "hospitalization", "illness",
        "immigration", "membership", "military service", "naturalization",
        "occupation", "ordination", "property", "residence", "retired", "will"
    ]
    for event in pevent_strings:
        add_string(gen, event)

    fevent_strings = [
        "marriage", "unmarried", "nomen", "engagement", "divorce", "separation",
        "annulation", "marriage bann", "marriage contract", "marriage license", "pacs"
    ]
    for event in fevent_strings:
        add_string(gen, event)

    witness_strings = [
        "witness", "godparent", "civil officer", "religious officer",
        "informant", "attending", "mentioned", "other"
    ]
    for witness in witness_strings:
        add_string(gen, witness)

    add_string(gen, "gregorian")
