"""
Name processing utilities.

Functions for normalizing, comparing, and indexing names.
Handles accents, particles, case insensitivity, etc.
"""

import hashlib
import re
import unicodedata
from typing import List


class NameUtils:
    """Utilities for name processing in genealogical databases."""

    TABLE_SIZE = 0x3FFF  # 16383 - hash table size for name indexing

    @staticmethod
    def normalize(name: str) -> str:
        """
        Normalize name to lowercase with stripped whitespace.

        Args:
            name: Name to normalize

        Returns:
            Normalized name
        """
        return name.lower().strip()

    @staticmethod
    def crush_lower(name: str) -> str:
        """
        Apply aggressive normalization for fuzzy matching.

        Removes accents, special characters, and normalizes to lowercase.
        Used for matching names despite spelling variations.

        Args:
            name: Name to crush

        Returns:
            Crushed lowercase name
        """
        # Remove accents using Unicode normalization
        nfd = unicodedata.normalize("NFD", name)
        without_accents = "".join(
            char for char in nfd if unicodedata.category(char) != "Mn"
        )

        # Convert to lowercase
        lower = without_accents.lower()

        # Remove non-alphanumeric except spaces and hyphens
        crushed = re.sub(r"[^a-z0-9\s\-]", "", lower)

        # Normalize whitespace
        return " ".join(crushed.split())

    @staticmethod
    def split_fname(name: str) -> List[str]:
        """
        Split first name into components.

        Handles compound first names like "Jean-Pierre" or "Mary Ann".

        Args:
            name: First name to split

        Returns:
            List of name components
        """
        # Split on spaces and hyphens
        parts = re.split(r"[\s\-]+", name)
        return [part.strip() for part in parts if part.strip()]

    @staticmethod
    def split_sname(name: str) -> List[str]:
        """
        Split surname into components.

        Handles compound surnames and particles like "de", "van", etc.

        Args:
            name: Surname to split

        Returns:
            List of surname components
        """
        # Split on spaces
        parts = name.split()
        return [part.strip() for part in parts if part.strip()]

    @staticmethod
    def name_index(name: str) -> int:
        """
        Calculate hash index for name.

        Computes a hash of the crushed name and maps it to the table size.
        Used for quick name lookups in the database.

        Args:
            name: Name to hash

        Returns:
            Hash index in range [0, TABLE_SIZE)
        """
        crushed = NameUtils.crush_lower(name)
        # Use MD5 for consistent hashing
        h = hashlib.md5(crushed.encode("utf-8")).digest()
        # Convert first 4 bytes to integer
        hash_val = int.from_bytes(h[:4], "little")
        return hash_val % NameUtils.TABLE_SIZE

    @staticmethod
    def compare_after_particle(particles: List[str], s1: str, s2: str) -> int:
        """
        Compare surnames ignoring leading particles.

        Particles like "de", "van", "von" are ignored for sorting,
        so "de Gaulle" sorts under "G" not "D".

        Args:
            particles: List of known particles (lowercase)
            s1: First surname
            s2: Second surname

        Returns:
            -1 if s1 < s2, 0 if equal, 1 if s1 > s2
        """

        def strip_particle(s: str) -> str:
            """Remove leading particle from surname."""
            words = s.split()
            if not words:
                return s

            # Check if first word is a particle
            first_word_lower = words[0].lower()
            if first_word_lower in particles and len(words) > 1:
                # Remove particle and return rest
                return " ".join(words[1:])

            return s

        # Strip particles
        stripped1 = strip_particle(s1)
        stripped2 = strip_particle(s2)

        # Compare using crushed normalization
        crushed1 = NameUtils.crush_lower(stripped1)
        crushed2 = NameUtils.crush_lower(stripped2)

        if crushed1 < crushed2:
            return -1
        elif crushed1 > crushed2:
            return 1
        else:
            return 0

    @staticmethod
    def alphabetic_utf8(n1: str, n2: str) -> int:
        """
        Compare two UTF-8 strings alphabetically.

        Handles accented characters properly for alphabetic sorting.

        Args:
            n1: First string
            n2: Second string

        Returns:
            -1 if n1 < n2, 0 if equal, 1 if n1 > n2
        """
        # Normalize to NFD (decomposed form)
        nfd1 = unicodedata.normalize("NFD", n1)
        nfd2 = unicodedata.normalize("NFD", n2)

        # Remove combining characters for base comparison
        base1 = "".join(c for c in nfd1 if unicodedata.category(c) != "Mn")
        base2 = "".join(c for c in nfd2 if unicodedata.category(c) != "Mn")

        # Compare
        if base1 < base2:
            return -1
        elif base1 > base2:
            return 1
        else:
            # If base is same, compare original (with accents)
            if n1 < n2:
                return -1
            elif n1 > n2:
                return 1
            else:
                return 0

    @staticmethod
    def trim_trailing_spaces(s: str) -> str:
        """
        Remove trailing whitespace from string.

        Args:
            s: String to trim

        Returns:
            Trimmed string
        """
        return s.rstrip()

    @staticmethod
    def nominative(s: str) -> str:
        """
        Convert name to nominative form.

        In GeneWeb, this handles case conventions for names.
        For Python, we capitalize appropriately.

        Args:
            s: Name string

        Returns:
            Nominative form
        """
        return " ".join(word.capitalize() for word in s.split())
