val of_iso_8859_1 : string -> string
(** Convert ISO-8859-1 encoded string to ANSEL encoding used inside gedcom
    files. Limited to Latin-1 characters only. *)

val to_iso_8859_1 : string -> string
(** Convert ANSEL used inside gedcom files to ISO-8859-1 encoding. Limited to
    Latin-1 characters only. *)

val of_utf_8 : string -> string
(** Convert UTF-8 encoded string to ANSEL encoding per GEDCOM 5.5.1. Supports
    Latin-1, Latin Extended-A (U+0000-U+017F), ligatures Œ/œ, symbols (© ® ± £ ·
    ¿ ¡), and special characters (ı ʼ ʻ). Note: ANSEL was withdrawn as a
    standard in 2013; UTF-8 recommended. *)

val to_utf_8 : string -> string
(** Convert ANSEL encoding to UTF-8 per GEDCOM 5.5.1 Appendix C. Handles
    combining diacritics (0xE0-0xF9) and direct codes. *)
