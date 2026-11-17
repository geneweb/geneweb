val compare : string -> string -> int
(** [compare a b] compares the UTF-8 encoded strings [a] and [b] as the Unicode
    collation algorithm with its default table (DUCET), at three levels: the
    letters without accents nor case over the whole strings first, then the
    accents, then the case, lowercase first (uppercase first where the language
    asks for it), the strings themselves deciding last. The letters of the
    Latin, Greek and Cyrillic scripts and the combining marks have the weights
    of the table, each character and the combining marks following it being
    decomposed together, in the canonical order of the marks (NFD); the letters
    of other scripts come after them, by code point; spaces, punctuation,
    symbols and digits come before the letters, by their ASCII transliteration.
    The sort key of each string is computed once and kept, up to 100,000
    strings, until the language changes. *)

val set_lang : string -> unit
(** [set_lang lang] makes [compare] follow the alphabet of the language [lang]
    (ISO 639-1 code, region ignored), as tailored by the Common Locale Data
    Repository (CLDR): Breton, Czech, Danish, Esperanto, Estonian, Finnish,
    Icelandic, Latvian, Lithuanian, Norwegian, Polish, Romanian, Slovak,
    Slovenian, Spanish, Swedish, Turkish, Afrikaans, and Bulgarian and Russian
    (Cyrillic first). Other languages follow the common order. *)
