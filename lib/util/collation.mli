val compare : string -> string -> int
(** [compare a b] compares the UTF-8 encoded strings [a] and [b] as the Unicode
    collation algorithm with its default table (DUCET), at three levels: the
    letters without accents nor case over the whole strings first, then the
    accents, then the case, lowercase first, the strings themselves deciding
    last. The letters of the Latin, Greek and Cyrillic scripts and the combining
    marks have the weights of the table, their accented forms being decomposed
    (NFD); the letters of other scripts come after them, by code point; spaces,
    punctuation, symbols and digits come before the letters, by their ASCII
    transliteration. The sort key of each string is computed once and kept, up
    to 100,000 strings. *)
