from typing import List, Dict, Tuple, Optional, Any
from models import Gen, Case
import re
import unicodedata
from charset import utf8_of_string

_lowercase_first_names = False
_case_surnames = Case.NoCase
_extract_first_names = False
_extract_public_names = True
_no_public_if_titles = False
_first_names_brackets: Optional[Tuple[str, str]] = None
_global_particles: List[str] = []

def set_lowercase_first_names(value: bool):
    """Set lowercase first names option"""
    global _lowercase_first_names
    _lowercase_first_names = value

def set_case_surnames(case: Case):
    """Set surname case option"""
    global _case_surnames
    _case_surnames = case

def set_extract_first_names(value: bool):
    """Set extract first names option"""
    global _extract_first_names
    _extract_first_names = value

def set_extract_public_names(value: bool):
    """Set extract public names option"""
    global _extract_public_names
    _extract_public_names = value

def get_extract_public_names() -> bool:
    """Get extract public names option"""
    global _extract_public_names
    return _extract_public_names

def get_lowercase_first_names() -> bool:
    """Get lowercase first names option"""
    global _lowercase_first_names
    return _lowercase_first_names

def get_case_surnames() -> Case:
    """Get case surnames option"""
    global _case_surnames
    return _case_surnames

def set_no_public_if_titles(value: bool):
    """Set no public if titles option"""
    global _no_public_if_titles
    _no_public_if_titles = value

def set_first_names_brackets(brackets: Tuple[str, str]):
    """Set first names brackets option"""
    global _first_names_brackets
    _first_names_brackets = brackets

def set_global_particles(particles: List[str]):
    """Set global particles list"""
    global _global_particles
    _global_particles = particles

def get_global_particles() -> List[str]:
    """Get global particles list"""
    global _global_particles
    return _global_particles

def default_particles() -> List[str]:
    """Return default particles list"""
    return [
        "de", "du", "des", "de la", "de l'", "del", "della", "di", "da",
        "van", "van der", "van den", "von", "vom", "zur", "zu", "af", "av",
        "le", "la", "les", "el", "al", "bin", "ibn", "abu", "ben", "bar",
        "d'", "o'", "mac", "mc", "fitz", "ap"
    ]

def load_particles_from_file(filename: str) -> List[str]:
    """Load particles from file"""
    try:
        with open(filename, 'r', encoding='utf-8') as f:
            particles = []
            for line in f:
                line = line.strip()
                if line and not line.startswith('#'):
                    particles.append(line)
            return particles
    except Exception:
        return default_particles()

def is_particle(word: str) -> bool:
    """Check if word is a particle"""
    return word.lower() in [p.lower() for p in _global_particles]

def capitalize_name(name: str) -> str:
    """Capitalize name properly, keeping particles lowercase"""
    if not name:
        return name

    words = name.split()
    result = []

    for i, word in enumerate(words):
        if i == 0:
            result.append(word.capitalize())
        elif is_particle(word):
            result.append(word.lower())
        else:
            result.append(word.capitalize())

    return ' '.join(result)

def uppercase_name(name: str) -> str:
    """Convert name to uppercase, keeping particles as-is"""
    if not name:
        return name

    words = name.split()
    result = []

    for word in words:
        if is_particle(word):
            result.append(word)
        else:
            result.append(word.upper())

    return ' '.join(result)

def process_individual_name(name_field: str, givn_field: str = "",
                          nick_field: str = "", surn_field: str = "",
                          aliases: List[str] = None) -> Dict[str, Any]:
    """Process individual name according to GEDCOM standards"""
    if aliases is None:
        aliases = []

    first_name, surname = parse_name_field(name_field)

    if givn_field and givn_field != first_name:
        first_names_aliases = [givn_field]
    else:
        first_names_aliases = []

    surname_aliases = []
    if surn_field:
        surname_parts = [s.strip() for s in surn_field.split(',') if s.strip()]
        for part in surname_parts:
            processed_surname = apply_case_surnames(part)
            if processed_surname != surname:
                surname_aliases.append(processed_surname)

    processed_first_name = first_name
    processed_surname = surname
    public_name = ""
    qualifiers = []

    if _first_names_brackets:
        bb, eb = _first_names_brackets
        processed_first_name, extracted = extract_brackets(first_name, bb, eb)
        if extracted:
            first_names_aliases.append(first_name)

    if _extract_first_names or _extract_public_names:
        extracted_info = extract_first_names_and_public(processed_first_name)
        if extracted_info:
            new_first, new_public, aliases_to_add = extracted_info
            processed_first_name = new_first
            if new_public and _extract_public_names:
                public_name = new_public
            first_names_aliases.extend(aliases_to_add)

    if _lowercase_first_names:
        processed_first_name = capitalize_name(processed_first_name)
        first_names_aliases = [capitalize_name(alias) for alias in first_names_aliases]
        if public_name:
            public_name = capitalize_name(public_name)

    processed_surname = apply_case_surnames(processed_surname)
    surname_aliases = [apply_case_surnames(alias) for alias in surname_aliases]

    if nick_field:
        qualifiers.append(nick_field)

    return {
        'first_name': processed_first_name,
        'surname': processed_surname,
        'public_name': public_name,
        'qualifiers': qualifiers,
        'first_names_aliases': first_names_aliases,
        'surnames_aliases': surname_aliases,
        'aliases': aliases
    }

def parse_name_field(name_field: str) -> Tuple[str, str]:
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

def extract_brackets(text: str, begin_char: str, end_char: str) -> Tuple[str, str]:
    """Extract text between brackets"""
    try:
        start = text.index(begin_char)
        end = text.index(end_char, start + 1)

        extracted = text[start + 1:end]
        remaining = text[:start] + text[end + 1:]

        return remaining.strip(), extracted
    except (ValueError, IndexError):
        return text, ""

def extract_first_names_and_public(first_name: str) -> Optional[Tuple[str, str, List[str]]]:
    """Extract first names and public name information"""
    if not first_name:
        return None

    words = first_name.split()
    if len(words) <= 1:
        return None

    if _extract_public_names and looks_like_public_name(first_name):
        return words[0], first_name, []

    if _extract_first_names:
        return words[0], "", [first_name]

    return None

def looks_like_public_name(name: str) -> bool:
    """Check if name looks like a public name"""
    public_name_words = ["Ier", "Ière", "der", "den", "die", "el", "le", "la", "the"]

    words = name.split()
    for i, word in enumerate(words[1:], 1):  # Skip first word
        if is_roman_numeral(word):
            return True

        if word.isdigit():
            return True

        if word.lower() in [w.lower() for w in public_name_words]:
            return True

    return False

def is_roman_numeral(text: str) -> bool:
    """Check if text is a roman numeral"""
    pattern = r'^M{0,4}(CM|CD|D?C{0,3})(XC|XL|L?X{0,3})(IX|IV|V?I{0,3})$'
    return bool(re.match(pattern, text.upper()))

def apply_case_surnames(surname: str) -> str:
    """Apply surname case transformation"""
    if not surname:
        return surname

    if _case_surnames == Case.LowerCase:
        return capitalize_name(surname)
    elif _case_surnames == Case.UpperCase:
        return uppercase_name(surname)
    else:
        return surname

class NameProcessor:
    """Process names with case, particles, and extraction rules"""

    def __init__(self, gen):
        self.gen = gen

    def process_complete_name(self, full_name: str, given_name: str = "",
                            nickname: str = "", surname: str = "",
                            aliases: List[str] = None) -> Dict[str, Any]:
        """Process complete name information"""
        if aliases is None:
            aliases = []

        first_name, family_name = self._parse_gedcom_name(full_name)

        processed_first = self._process_first_name(first_name, given_name)
        processed_surname = self._process_surname(family_name, surname)

        public_name = self._extract_public_name(processed_first) if get_extract_public_names() else ""

        return {
            "first_name": processed_first,
            "surname": processed_surname,
            "public_name": public_name,
            "nickname": nickname,
            "aliases": aliases,
            "first_names_aliases": [],
            "surnames_aliases": []
        }

    def _parse_gedcom_name(self, name: str) -> Tuple[str, str]:
        """Parse GEDCOM name format: First /Surname/"""
        if not name:
            return "x", "?"

        name = name.strip()

        invert = name.startswith('/')
        if invert:
            name = name[1:]

        parts = name.split('/')

        if len(parts) >= 2:
            first_name = parts[0].strip()
            surname = parts[1].strip()
        else:
            first_name = name.strip()
            surname = ""

        if invert:
            first_name, surname = surname, first_name

        first_name = first_name if first_name else "x"
        surname = surname if surname else "?"

        return first_name, surname

    def _process_first_name(self, first_name: str, given_name: str = "") -> str:
        """Process first name with case rules"""
        if not first_name or first_name == "x":
            return given_name if given_name else "x"

        if get_lowercase_first_names():
            return self._capitalize_name(first_name)

        return first_name

    def _process_surname(self, surname: str, surn_field: str = "") -> str:
        """Process surname with case and particle rules"""
        if not surname or surname == "?":
            return surn_field if surn_field else "?"

        case_rule = get_case_surnames()
        if case_rule == Case.LowerCase:
            return self._capitalize_name(surname)
        elif case_rule == Case.UpperCase:
            return surname.upper()

        return surname

    def _capitalize_name(self, name: str) -> str:
        """Capitalize name while preserving particles"""
        if not name:
            return name

        words = name.split()
        result = []

        for word in words:
            if self._is_particle(word.lower()):
                result.append(word.lower())
            else:
                result.append(word.capitalize())

        return ' '.join(result)

    def _is_particle(self, word: str) -> bool:
        """Check if word is a particle"""
        particles = get_global_particles()
        return word.lower() in particles

    def _extract_public_name(self, first_name: str) -> str:
        """Extract public name from first name if applicable"""
        if not first_name:
            return ""

        words = first_name.split()
        if len(words) > 1:
            for word in words:
                if word.isdigit() or self._is_roman_numeral(word):
                    return first_name

        return ""

    def _is_roman_numeral(self, word: str) -> bool:
        """Check if word is a roman numeral"""
        roman_chars = set('IVXLCDM')
        return word.upper() and all(c in roman_chars for c in word.upper())

    def add_name_to_gen(self, name_info: Dict[str, Any], person_index: int) -> Dict[str, int]:
        """Add name information to generation structure"""
        from gen_arrays import add_string

        return {
            "first_name": add_string(self.gen, name_info["first_name"]),
            "surname": add_string(self.gen, name_info["surname"]),
            "occ": person_index,
            "public_name": add_string(self.gen, name_info["public_name"]),
            "qualifiers": [add_string(self.gen, name_info["nickname"])] if name_info["nickname"] else [],
            "aliases": [add_string(self.gen, alias) for alias in name_info["aliases"]],
            "first_names_aliases": [add_string(self.gen, alias) for alias in name_info["first_names_aliases"]],
            "surnames_aliases": [add_string(self.gen, alias) for alias in name_info["surnames_aliases"]]
        }
