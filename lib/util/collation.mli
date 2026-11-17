val compare : string -> string -> int
(** [compare a b] compares the UTF-8 encoded strings [a] and [b] as the Unicode
    collation algorithm with its default table (DUCET), at three levels: the
    letters without accents nor case over the whole strings first, then the
    accents, then the case of each letter, a digraph (ǅ, ch) counting its two
    letters, lowercase first (uppercase first where the language asks for it),
    the strings themselves deciding last. The letters of the Latin, Greek and
    Cyrillic scripts, their ligatures, superscript and fullwidth forms included,
    and the combining marks have the weights of the table, each character and
    the combining marks following it being decomposed together, in the canonical
    order of the marks (NFD); spaces, punctuation, symbols and digits come
    before the letters, in the order of the table for the common blocks (from
    Basic Latin to Cyrillic, General Punctuation, Currency Symbols, Letterlike
    Symbols, Number Forms), the other spaces, punctuation and symbols just
    before the digits, by code point, the invisible characters being ignored;
    the letters and digits of other scripts come after the letters, by code
    point. The sort key of each string is computed once and kept, up to 100,000
    strings, until the language changes. *)

val set_lang : string -> unit
(** [set_lang lang] makes [compare] follow the alphabet of the language [lang]
    (ISO 639 code, region ignored), as tailored by the Common Locale Data
    Repository (CLDR): Afrikaans, Albanian, Breton, Croatian, Czech, Danish,
    Esperanto, Estonian, Faroese, Finnish, Hungarian, Icelandic, Latvian,
    Lithuanian, Lower and Upper Sorbian, Maltese, Norwegian, Polish, Romanian,
    Slovak, Slovenian, Spanish, Swedish, Turkish, Welsh, and Bulgarian and
    Russian (Cyrillic first). Other languages follow the common order. *)
