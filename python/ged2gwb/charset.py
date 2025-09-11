# Charset utilities (MSDOS / Macintosh partial maps + utf8_of_string)
_MSDOS_TO_ISO = {
    0x80: 0xE7, 0x81: 0xFC, 0x82: 0xE9, 0x83: 0xE2,
    0x84: 0xE4, 0x85: 0xE0, 0x86: 0xE5, 0x87: 0xE7,
}
_MAC_TO_ISO = {0x80: 0xC4, 0x81: 0xC5, 0x82: 0xC7}

charset_option = None  # set by caller: "MSDOS"/"MACINTOSH"/"UTF-8"/...

def ascii_of_msdos(s: str) -> str:
    out = bytearray()
    for b in s.encode("latin-1", errors="replace"):
        out.append(_MSDOS_TO_ISO.get(b, b))
    return out.decode("latin-1", errors="replace")

def ascii_of_macintosh(s: str) -> str:
    out = bytearray()
    for b in s.encode("latin-1", errors="replace"):
        out.append(_MAC_TO_ISO.get(b, b))
    return out.decode("latin-1", errors="replace")

def utf8_of_string(s: str) -> str:
    cs = (charset_option or "").upper()
    if cs == "MSDOS":
        return ascii_of_msdos(s)
    if cs == "MACINTOSH":
        return ascii_of_macintosh(s)
    # ANSEL/ANSI/ASCII/UTF-8 -> pass-through for now
    return s
