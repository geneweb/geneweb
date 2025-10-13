"""
Name processing utilities converted from OCaml.
Handles name normalization, abbreviations, and phonetic matching.
"""

import re
import unicodedata
from typing import Callable, List, Optional, Tuple


class Buff:
    """Buffer utility class for efficient string building."""

    def __init__(self):
        self.buffer = []

    def store(self, char: str) -> None:
        """Store a character in the buffer."""
        self.buffer.append(char)

    def mstore(self, text: str) -> None:
        """Store multiple characters in the buffer."""
        self.buffer.extend(text)

    def get(self) -> str:
        """Get the final string from buffer."""
        return "".join(self.buffer)

    def length(self) -> int:
        """Get current buffer length."""
        return len(self.buffer)


# List of forbidden characters
FORBIDDEN_CHAR = [":", "@", "#", "=", "$"]

# List of abbreviations. If abbreviation is mapped to a string, should be replaced by
# that string. If mapped to None, it should be removed from name.
ABBREV_LIST = [
    ("a", None),
    ("af", None),
    ("d", None),
    ("de", None),
    ("di", None),
    ("ier", "i"),
    ("of", None),
    ("saint", "st"),
    ("sainte", "ste"),
    ("van", None),
    ("von", None),
    ("zu", None),
    ("zur", None),
]


def unaccent_utf_8(lower_case: bool, s: str, i: int) -> Tuple[str, int]:
    """
    Remove accents from UTF-8 string starting at position i.
    Returns the processed character(s) and the next position.
    """
    if i >= len(s):
        return "", i

    char = s[i]
    # Handle ASCII characters
    if ord(char) < 0x80:
        result = char.lower() if lower_case else char
        return result, i + 1

    # Handle Unicode characters - remove accents
    try:
        # Normalize to NFD (decomposed form) and remove combining characters
        normalized = unicodedata.normalize("NFD", char)
        ascii_char = "".join(c for c in normalized if unicodedata.category(c) != "Mn")
        if not ascii_char:
            ascii_char = char
        result = ascii_char.lower() if lower_case else ascii_char
        return result, i + 1
    except:
        result = char.lower() if lower_case else char
        return result, i + 1


def next_chars_if_equiv(s: str, i: int, t: str, j: int) -> Optional[Tuple[int, int]]:
    """
    Check if characters at positions i and j are equivalent after unaccenting.
    Returns next positions if equivalent, None otherwise.
    """
    if i >= len(s) or j >= len(t):
        return None

    s1, i1 = unaccent_utf_8(True, s, i)
    t1, j1 = unaccent_utf_8(True, t, j)

    if s1 == t1:
        return (i1, j1)
    return None


def lower(s: str) -> str:
    """
    Name.lower:
    - uppercase -> lowercase
    - no accents
    - chars no letters and no numbers (except '.') => spaces (stripped)
    Key comparison (first name, surname, number) applies "lower" equality
    on first names and surnames
    """
    buff = Buff()
    special = False
    i = 0

    while i < len(s):
        if ord(s[i]) < 0x80:
            char = s[i]
            if char.isalnum() or char == ".":
                if special:
                    buff.store(" ")
                buff.store(char.lower())
                special = False
            else:
                special = buff.length() != 0
            i += 1
        else:
            if special:
                buff.store(" ")
            text, j = unaccent_utf_8(True, s, i)
            buff.mstore(text)
            special = False
            i = j

    return buff.get()


def title(s: str) -> str:
    """
    Convert string to title case, handling Unicode properly.
    """
    result = []
    capitalize_next = True

    for char in s:
        if char.isalpha():
            if capitalize_next:
                result.append(char.upper())
                capitalize_next = False
            else:
                result.append(char.lower())
        else:
            result.append(char)
            capitalize_next = True

    return "".join(result)


def is_word(s: str, i: int, p: str) -> bool:
    """
    Checks if the word starting at [i] in [s] is [p].
    """
    if i + len(p) > len(s):
        return False

    # Check if the substring matches
    if s[i : i + len(p)] != p:
        return False

    # Check word boundary
    end_pos = i + len(p)
    if end_pos == len(s):
        return True
    elif s[end_pos] == " ":
        return True
    else:
        return False


def search_abbrev(
    s: str, i: int, abbrev_list: List[Tuple[str, Optional[str]]]
) -> Optional[Tuple[int, Optional[str]]]:
    """
    Checks if word that starts at position [i] in [s] is one of abbreviation.
    """
    for word, abbrev in abbrev_list:
        if is_word(s, i, word):
            return (len(word), abbrev)
    return None


def abbrev(s: str) -> str:
    """
    Name.abbrev: suppress lowercase particles, shorten "saint" into "st"
    """
    buff = Buff()
    can_start_abbrev = True
    i = 0

    while i < len(s):
        if s[i] == " ":
            buff.store(" ")
            can_start_abbrev = True
            i += 1
        else:
            if can_start_abbrev:
                result = search_abbrev(s, i, ABBREV_LIST)
                if result is None:
                    buff.store(s[i])
                    can_start_abbrev = False
                    i += 1
                else:
                    n, replacement = result
                    if replacement is not None:
                        buff.mstore(replacement)
                        can_start_abbrev = False
                        i += n
                    else:
                        # Skip the abbreviation and the following space
                        can_start_abbrev = True
                        i += n
                        if i < len(s) and s[i] == " ":
                            i += 1
            else:
                buff.store(s[i])
                can_start_abbrev = False
                i += 1

    return buff.get()


def strip_c(s: str, c: str) -> str:
    """
    Name.strip_c = name without the character c given as parameter
    """
    return s.replace(c, "")


def strip(s: str) -> str:
    """
    Remove all spaces from string.
    """
    return strip_c(s, " ")


def purge(s: str) -> str:
    """
    String without any forbidden characters defined in forbidden_char.
    Removes all forbidden characters from the string.
    """
    result = s
    for char in FORBIDDEN_CHAR:
        result = strip_c(result, char)
    return result


def roman_number(s: str, i: int) -> Optional[int]:
    """
    If string starting from [i] contains roman number then returns the next position,
    else returns None.
    """
    if i == 0 or (i > 0 and s[i - 1] == " "):
        j = i
        while j < len(s):
            if s[j] == " ":
                return j
            elif s[j] in "ivxl":
                j += 1
            else:
                return None
        return j
    return None


def crush(s: str) -> str:
    """
    Name.crush, a custom sonnex/soundex-like phonetic algorithm:
    - no spaces
    - roman numbers are kept
    - vowels are suppressed, except in words starting with a vowel,
      where this vowel is converted into "e"
    - "k" and "q" replaced by "c"
    - "y" replaced by "i"
    - "z" replaced by "s"
    - "ph" replaced by "f"
    - others "h" deleted
    - s at end of words are deleted
    - no double lowercase consonants
    """
    buff = Buff()
    first_vowel = True
    i = 0

    while i < len(s):
        if s[i] == " ":
            first_vowel = True
            i += 1
        else:
            roman_end = roman_number(s, i)
            if roman_end is not None:
                # Copy roman number as-is
                while i < roman_end:
                    buff.store(s[i])
                    i += 1
                first_vowel = True
            else:
                char = s[i]
                if char in "aeiouy":
                    if first_vowel:
                        buff.store("e")
                    first_vowel = False
                    i += 1
                elif char == "h":
                    # Handle "ph" -> "f"
                    if i > 0 and s[i - 1] == "p" and buff.length() > 0:
                        # Remove the 'p' and add 'f'
                        buff.buffer = buff.buffer[:-1]
                        buff.store("f")
                    # Otherwise skip 'h'
                    i += 1
                elif char in "sz" and (
                    i == len(s) - 1 or (i + 1 < len(s) and s[i + 1] == " ")
                ):
                    # Remove trailing 's' or 'z' at end of words
                    # Also remove duplicate 's' or 'z'
                    while (
                        buff.length() > 0
                        and buff.buffer[-1] in "sz"
                        and buff.buffer[-1] == char
                    ):
                        buff.buffer = buff.buffer[:-1]
                    i += 1
                elif char == "s" and (
                    i == len(s) - 1 or (i + 1 < len(s) and s[i + 1] == " ")
                ):
                    # Skip trailing 's'
                    i += 1
                else:
                    # Handle character replacements and avoid duplicates
                    if i > 0 and s[i - 1] == char:
                        # Skip duplicate
                        pass
                    else:
                        # Apply character replacements
                        if char == "k" or char == "q":
                            char = "c"
                        elif char == "z":
                            char = "s"
                        buff.store(char)
                    first_vowel = False
                    i += 1

    return buff.get()


def strip_lower(s: str) -> str:
    """
    strip_lower = strip o lower, as first comparison of names.
    First names and Surnames comparison is strip_lower equality.
    """
    return strip(lower(s))


def crush_lower(s: str) -> str:
    """
    crush_lower = crush o abbrev o lower, as second comparison of names.
    In index by names, the "names" are crush_lowers
    """
    return crush(abbrev(lower(s)))


def concat(fn: str, sn: str) -> str:
    """
    Concatenate two strings with a space.
    """
    return f"{fn} {sn}"


def contains_forbidden_char(s: str) -> bool:
    """
    Check if string contains any forbidden character.
    """
    return any(char in s for char in FORBIDDEN_CHAR)


def split_sname_callback(fn: Callable[[int, int], None], s: str) -> None:
    """
    Split surname on spaces and hyphens, calling callback for each part.
    """
    j = len(s)
    for i in range(len(s) - 1, -1, -1):
        if s[i] in " -":
            fn(i + 1, j - i - 1)
            j = i
    fn(0, j)


def split_fname_callback(fn: Callable[[int, int], None], s: str) -> None:
    """
    Split first name on spaces, calling callback for each part.
    """
    j = len(s)
    for i in range(len(s) - 1, -1, -1):
        if s[i] == " ":
            fn(i + 1, j - i - 1)
            j = i
    fn(0, j)


def split_sname(s: str) -> List[str]:
    """
    Split surname into parts separated by spaces or hyphens.
    """
    result = []
    split_sname_callback(lambda i, length: result.append(s[i : i + length]), s)
    return list(reversed(result))


def split_fname(s: str) -> List[str]:
    """
    Split first name into parts separated by spaces.
    """
    result = []
    split_fname_callback(lambda i, length: result.append(s[i : i + length]), s)
    return list(reversed(result))


# Example usage and tests
if __name__ == "__main__":
    # Test the functions
    test_name = "Jean-Baptiste de Saint-Exupéry"

    print(f"Original: {test_name}")
    print(f"Lower: {lower(test_name)}")
    print(f"Abbrev: {abbrev(lower(test_name))}")
    print(f"Crush: {crush(abbrev(lower(test_name)))}")
    print(f"Strip lower: {strip_lower(test_name)}")
    print(f"Crush lower: {crush_lower(test_name)}")

    print(f"Split surname: {split_sname('Martin-Dupont')}")
    print(f"Split first name: {split_fname('Jean Baptiste')}")

    print(f"Contains forbidden char: {contains_forbidden_char('test@example.com')}")
    print(f"Purged: {purge('test@example.com')}")
