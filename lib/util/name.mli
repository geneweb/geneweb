(* Copyright (c) 1998-2007 INRIA *)

val forbidden_char : char list
(** List of forbidden to use characters *)

val unaccent_utf_8 : bool -> string -> int -> string * int
(** [unaccent_utf_8 lower s i] checks UTF-8 characher that starts at position
    [i] inside [s] and returns couple (cs,np) where [cs] is ASCII representation
    of this character (characters between 0x00 and 0x7F) and [np] it's a
    position of next utf8 character inside [s]. If [lower] is true then [cs]
    will contain only lowercase letters. Example : unaccent_utf_8 "aÈa" 1 ->
    ("e",3) *)

val next_chars_if_equiv : string -> int -> string -> int -> (int * int) option
(** [next_chars_if_equiv s1 i1 s2 i2] checks if UTF-8 characters that start at
    position [i1] inside [s1] and at [i2] inside [s2] are equivalent (have the
    same ASCII representation). In this case returns position of the next
    charecter for each of them. Otherwise, returns None. *)

val lower : string -> string
(** Convert every letter to lowercase and use *unidecode* library to represent
    unicode characters with ASCII. Non-alphanumeric characters (except '.') are
    remplaced by space. *)

val title : string -> string
(** Apply uppercasing to the first letter of each name (sequence of alphabetic
    characters) part, and lowercasing to the rest of the text. *)

val abbrev : string -> string
(** Remplace by an abbreviation or remove particles inside the name *)

val strip : string -> string
(** Removes all the spaces inside the name *)

val strip_c : string -> char -> string
(** [strip_c s c] removes all the occurences of [c] inside the name *)

val purge : string -> string
(** Removes all the forbiden characters from [forbidden_char] inside the name *)

val crush : string -> string
(** A custom sonnex/soundex-like phonetic algorithm:
    - no spaces
    - roman numbers are keeped
    - vowels are suppressed, except in words starting with a vowel, where this
      vowel is converted into "e"
    - "k" and "q" replaced by "c"
    - "y" replaced by "i"
    - "z" replaced by "s"
    - "ph" replaced by "f"
    - others "h" deleted
    - s at end of words are deleted
    - no double lowercase consons *)

val strip_lower : string -> string
(** Equivalent to [strip o lower]. Used as:
    - First comparison of names.
    - Comparison for first names and surnames. *)

val crush_lower : string -> string
(** Equivalent to [crush o abbrev o lower]. Used as:
    - Second comparison of names.
    - Key when index by names *)

val concat : string -> string -> string
(** [concat fn sn] is [fn ^ " " ^ sn] but faster. *)

val contains_forbidden_char : string -> bool
(** [contains_forbidden_char s] is [true] iif s contains forbidden characters *)

val split_sname_callback : (int -> int -> unit) -> string -> unit
(** [split_sname_callback fn s] Same as [split_sname], but call [fn] with
    substring indexes instead of building a list *)

val split_fname_callback : (int -> int -> unit) -> string -> unit
(** [split_fname_callback fn s] Same as [split_fname], but call [fn] with
    substring indexes instead of building a list *)

val split_sname : string -> string list
(** [split_sname s] split the surname [s] in parts composing it. e.g.
    [split_sname base "Foo-Bar"] is [[ "Foo" ; "Bar"]] *)

val split_fname : string -> string list
(** [split_fname s] split the string [s] representing multiple first names into
    this list of firstname. e.g. [split_fname base "Foo-Bar Baz"] is
    [[ "Foo-Bar" ; "Baz"]] *)
