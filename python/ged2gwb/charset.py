import codecs
import unicodedata
from typing import Optional, Dict
from models import Charset

charset_option: Optional[str] = None
charset: Charset = Charset.Ascii

CHARSET_MAPPING: Dict[str, str] = {
    "ANSEL": "ansel",      # Requires special handling
    "ANSI": "cp1252",      # Windows-1252
    "ASCII": "ascii",
    "MSDOS": "cp850",      # Code page 850 (DOS Latin-1)
    "MACINTOSH": "mac_roman",  # MacRoman
    "UTF-8": "utf-8",
    "UTF8": "utf-8",
    "IBMPC": "cp437",      # Code page 437 (DOS US)
}

ANSEL_TO_UNICODE = {

    0x88: 0x0098,  # <control>
    0x89: 0x0099,  # <control>
    0x8D: 0x008D,  # <control>
    0x8E: 0x008E,  # <control>
    0x8F: 0x008F,  # <control>
    0x90: 0x0090,  # <control>
    0x9D: 0x009D,  # <control>
    0x9E: 0x009E,  # <control>

    0xE0: 0x0300,  # COMBINING GRAVE ACCENT
    0xE1: 0x0301,  # COMBINING ACUTE ACCENT
    0xE2: 0x0302,  # COMBINING CIRCUMFLEX ACCENT
    0xE3: 0x0303,  # COMBINING TILDE
    0xE4: 0x0304,  # COMBINING MACRON
    0xE5: 0x0306,  # COMBINING BREVE
    0xE6: 0x0307,  # COMBINING DOT ABOVE
    0xE7: 0x0308,  # COMBINING DIAERESIS
    0xE8: 0x030C,  # COMBINING CARON
    0xE9: 0x030A,  # COMBINING RING ABOVE
    0xEA: 0x0327,  # COMBINING CEDILLA
    0xEB: 0x0328,  # COMBINING OGONEK
    0xEC: 0x0323,  # COMBINING DOT BELOW
    0xED: 0x0324,  # COMBINING DIAERESIS BELOW
    0xEE: 0x0325,  # COMBINING RING BELOW
    0xEF: 0x0326,  # COMBINING COMMA BELOW

    0xA1: 0x0141,  # LATIN CAPITAL LETTER L WITH STROKE
    0xA2: 0x00D8,  # LATIN CAPITAL LETTER O WITH STROKE
    0xA3: 0x0110,  # LATIN CAPITAL LETTER D WITH STROKE
    0xA4: 0x00DE,  # LATIN CAPITAL LETTER THORN
    0xA5: 0x00C6,  # LATIN CAPITAL LETTER AE
    0xA6: 0x0152,  # LATIN CAPITAL LIGATURE OE
    0xA7: 0x02B9,  # MODIFIER LETTER PRIME
    0xA8: 0x00B7,  # MIDDLE DOT
    0xA9: 0x266D,  # MUSIC FLAT SIGN
    0xAA: 0x00AE,  # REGISTERED SIGN
    0xAB: 0x00B1,  # PLUS-MINUS SIGN
    0xAC: 0x01A0,  # LATIN CAPITAL LETTER O WITH HORN
    0xAD: 0x01AF,  # LATIN CAPITAL LETTER U WITH HORN
    0xAE: 0x02BC,  # MODIFIER LETTER APOSTROPHE

    0xB1: 0x0142,  # LATIN SMALL LETTER L WITH STROKE
    0xB2: 0x00F8,  # LATIN SMALL LETTER O WITH STROKE
    0xB3: 0x0111,  # LATIN SMALL LETTER D WITH STROKE
    0xB4: 0x00FE,  # LATIN SMALL LETTER THORN
    0xB5: 0x00E6,  # LATIN SMALL LETTER AE
    0xB6: 0x0153,  # LATIN SMALL LIGATURE OE
    0xB7: 0x02BA,  # MODIFIER LETTER DOUBLE PRIME
    0xB8: 0x0131,  # LATIN SMALL LETTER DOTLESS I
    0xB9: 0x00A3,  # POUND SIGN
    0xBA: 0x00F0,  # LATIN SMALL LETTER ETH
    0xBB: 0x2113,  # SCRIPT SMALL L
    0xBC: 0x01A1,  # LATIN SMALL LETTER O WITH HORN
    0xBD: 0x01B0,  # LATIN SMALL LETTER U WITH HORN

    0xC0: 0x00B0,  # DEGREE SIGN
    0xC1: 0x2113,  # SCRIPT SMALL L
    0xC2: 0x2117,  # SOUND RECORDING COPYRIGHT
    0xC3: 0x00A9,  # COPYRIGHT SIGN
    0xC4: 0x266F,  # MUSIC SHARP SIGN
    0xC5: 0x00BF,  # INVERTED QUESTION MARK
    0xC6: 0x00A1,  # INVERTED EXCLAMATION MARK
    0xC7: 0x00DF,  # LATIN SMALL LETTER SHARP S
    0xC8: 0x20AC,  # EURO SIGN
}

def detect_charset_from_bom(data: bytes) -> Optional[str]:
    """Detect charset from BOM"""
    if data.startswith(b'\xef\xbb\xbf'):
        return "UTF-8"
    elif data.startswith(b'\xff\xfe'):
        return "UTF-16LE"
    elif data.startswith(b'\xfe\xff'):
        return "UTF-16BE"
    elif data.startswith(b'\xff\xfe\x00\x00'):
        return "UTF-32LE"
    elif data.startswith(b'\x00\x00\xfe\xff'):
        return "UTF-32BE"
    return None

def ansel_to_unicode(data: bytes) -> str:
    """Convert ANSEL to Unicode"""
    result = []
    i = 0

    while i < len(data):
        byte_val = data[i]

        if byte_val < 0x80:
            result.append(chr(byte_val))
        elif 0xE0 <= byte_val <= 0xEF:
            if i + 1 < len(data):
                base_char = data[i + 1]
                if base_char < 0x80:
                    base_unicode = chr(base_char)
                    combining_unicode = chr(ANSEL_TO_UNICODE.get(byte_val, byte_val))
                    combined = unicodedata.normalize('NFC', base_unicode + combining_unicode)
                    result.append(combined)
                    i += 1  # Skip next character
                else:
                    result.append(chr(ANSEL_TO_UNICODE.get(byte_val, byte_val)))
            else:
                result.append(chr(ANSEL_TO_UNICODE.get(byte_val, byte_val)))
        else:
            unicode_val = ANSEL_TO_UNICODE.get(byte_val, byte_val)
            try:
                result.append(chr(unicode_val))
            except ValueError:
                result.append('\ufffd')

        i += 1

    return ''.join(result)

def decode_bytes_with_charset(data: bytes, charset_name: str) -> str:
    """Decode bytes with specified charset"""
    charset_name = charset_name.upper()

    if charset_name == "ANSEL":
        return ansel_to_unicode(data)

    encoding = CHARSET_MAPPING.get(charset_name)
    if not encoding:
        encoding = "latin-1"

    try:
        return data.decode(encoding, errors='replace')
    except (LookupError, UnicodeDecodeError):
        try:
            return data.decode('latin-1', errors='replace')
        except:
            return data.decode('utf-8', errors='replace')

def smart_decode_string(data: bytes, hint_charset: Optional[str] = None) -> str:
    """Smart string decoding with automatic detection"""
    # 1. Try to detect from BOM
    detected_charset = detect_charset_from_bom(data)
    if detected_charset:
        return decode_bytes_with_charset(data[3:], detected_charset)  # Skip BOM

    # 2. Use given charset parameter
    if hint_charset:
        try:
            return decode_bytes_with_charset(data, hint_charset)
        except:
            pass

    # 3. Use global charset
    global charset_option, charset
    active_charset = charset_option or charset
    if active_charset and active_charset != Charset.Ascii:
        try:
            charset_str = active_charset.name if hasattr(active_charset, 'name') else str(active_charset)
            return decode_bytes_with_charset(data, charset_str)
        except:
            pass

    # 4. Try automatic detection
    try:
        text = data.decode('utf-8')
        text.encode('utf-8')
        return text
    except UnicodeDecodeError:
        pass

    try:
        return data.decode('cp1252', errors='replace')
    except:
        pass

    return data.decode('latin-1', errors='replace')

def utf8_of_string(s: str) -> str:
    """Convert string to UTF-8 according to configured charset"""
    if not s:
        return s

    if isinstance(s, str):
        return s

    if isinstance(s, bytes):
        return smart_decode_string(s)

    return str(s)

def normalize_charset_name(charset_name: str) -> str:
    """Normalize charset name"""
    if not charset_name:
        return "ASCII"

    charset_name = charset_name.upper().replace('-', '').replace('_', '')

    mappings = {
        "UTF8": "UTF-8",
        "UNICODE": "UTF-8",
        "IBMPC": "MSDOS",  # Treat IBMPC as MSDOS
        "CP850": "MSDOS",
        "CP437": "MSDOS",
        "WINDOWS1252": "ANSI",
        "CP1252": "ANSI",
        "MACROMAN": "MACINTOSH",
        "LATIN1": "ASCII",
        "ISO88591": "ASCII",
    }

    return mappings.get(charset_name, charset_name)

def set_global_charset(charset_name: str):
    """Configure global charset - validate like OCaml"""
    global charset_option, charset

    normalized = normalize_charset_name(charset_name)

    if normalized not in ["ANSEL", "ASCII", "MSDOS", "UTF-8", "ANSI", "MACINTOSH"]:
        raise ValueError("bad -charset value")

    charset_option = normalized

    charset_map = {
        "ANSEL": Charset.Ansel,
        "ANSI": Charset.Ansi,
        "ASCII": Charset.Ascii,
        "MSDOS": Charset.Msdos,
        "MACINTOSH": Charset.MacIntosh,
        "UTF-8": Charset.Utf8,
    }

    if normalized in charset_map:
        charset = charset_map[normalized]

def get_effective_charset() -> str:
    """Return effectively used charset"""
    global charset_option, charset

    if charset_option:
        return charset_option

    if charset == Charset.Ansel:
        return "ANSEL"
    elif charset == Charset.Ansi:
        return "ANSI"
    elif charset == Charset.Msdos:
        return "MSDOS"
    elif charset == Charset.MacIntosh:
        return "MACINTOSH"
    elif charset == Charset.Utf8:
        return "UTF-8"
    else:
        return "ASCII"

def ascii_of_msdos(s: str) -> str:
    """Convert from MSDOS to ASCII (for compatibility)"""
    if isinstance(s, str):
        try:
            bytes_data = s.encode('latin-1', errors='replace')
            return decode_bytes_with_charset(bytes_data, "MSDOS")
        except:
            return s
    return s

def ascii_of_macintosh(s: str) -> str:
    """Convert from Macintosh to ASCII (for compatibility)"""
    if isinstance(s, str):
        try:
            bytes_data = s.encode('latin-1', errors='replace')
            return decode_bytes_with_charset(bytes_data, "MACINTOSH")
        except:
            return s
    return s

def convert_gedcom_string(s: str, source_charset: Optional[str] = None) -> str:
    """Convert GEDCOM string to UTF-8"""
    if not s:
        return s

    effective_charset = source_charset or get_effective_charset()

    if effective_charset == "UTF-8":
        return s

    if effective_charset == "ASCII" and s.isascii():
        return s

    try:
        if effective_charset in ["MSDOS", "MACINTOSH", "ANSI"]:
            bytes_data = s.encode('latin-1', errors='replace')
            return decode_bytes_with_charset(bytes_data, effective_charset)
        else:
            bytes_data = s.encode('latin-1', errors='replace')
            return decode_bytes_with_charset(bytes_data, effective_charset)
    except:
        return s

class CharsetConverter:
    """Charset converter for GEDCOM"""

    def __init__(self, charset_name: Optional[str] = None):
        self.charset_name = normalize_charset_name(charset_name or "ASCII")

    def convert(self, text: str) -> str:
        """Convert text to UTF-8"""
        return convert_gedcom_string(text, self.charset_name)

    def convert_bytes(self, data: bytes) -> str:
        """Convert bytes to UTF-8"""
        return decode_bytes_with_charset(data, self.charset_name)

    def is_utf8(self) -> bool:
        """Check if charset is UTF-8"""
        return self.charset_name == "UTF-8"

    def needs_conversion(self) -> bool:
        """Check if conversion is needed"""
        return self.charset_name not in ["UTF-8", "ASCII"]

def detect_text_encoding(text_sample: str) -> str:
    """Try to detect encoding of text sample"""

    if text_sample.isascii():
        return "ASCII"


    try:
        text_sample.encode('utf-8')
        return "UTF-8"
    except UnicodeEncodeError:
        pass

    if any(ord(c) in [0x80, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x89, 0x8a, 0x8b, 0x8c, 0x8e, 0x91, 0x92, 0x93, 0x94, 0x95, 0x96, 0x97, 0x99, 0x9a, 0x9b, 0x9c, 0x9e, 0x9f] for c in text_sample):
        return "ANSI"

    if any(ord(c) in [0x80, 0x81, 0x82, 0x83, 0x84, 0x85, 0x86, 0x87, 0x88, 0x89, 0x8a, 0x8b, 0x8c, 0x8d, 0x8e, 0x8f] for c in text_sample):
        return "MACINTOSH"

    return "ASCII"

def get_charset_converter() -> CharsetConverter:
    """Return converter with global charset"""
    return CharsetConverter(get_effective_charset())

