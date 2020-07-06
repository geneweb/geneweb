(* $Id: name.mli,v 5.2 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

val lower : string -> string
  (* Name.lower:
     - uppercase -> lowercase
     - no accents
     - chars no letters and no numbers (except '.') => spaces (stripped)
     Key comparison (first name, surname, number) applies "lower" equality
     on first names and surnames *)
val abbrev : string -> string
  (* Name.abbrev: suppress lowercase particles, shorten "saint" into "st" *)
val strip : string -> string
  (* Name.strip = name without spaces *)
val strip_c : string -> char -> string
  (* Name.strip_c = name without the charater c given as parameter *)
val crush : string -> string
  (* Name.crush:
     - no spaces
     - roman numbers are keeped
     - vowels are suppressed, except in words starting with a vowel,
       where this vowel is converted into "e"
     - "k" and "q" replaced by "c"
     - "y" replaced by "i"
     - "z" replaced by "s"
     - "ph" replaced by "f"
     - others "h" deleted
     - s at end of words are deleted
     - no double lowercase consons *)

val strip_lower : string -> string
  (* strip_lower = strip o lower, as first comparison of names.
     First names and Surnames comparison is strip_lower equality. *)

val purge : string -> string
  (* String without any forbidden caracters defined in forbidden_char *)

val crush_lower : string -> string
  (* crush_lower = crush o abbrev o lower, as second comparison of names.
     In index by names, the "names" are crush_lowers *)

val next_chars_if_equiv : string -> int -> string -> int -> (int * int) option

val unaccent_utf_8 : bool -> string -> int -> string * int

val forbidden_char : char list

(** [concat fn sn] is [fn ^ " " ^ sn] but faster. *)
val concat : string -> string -> string

(** [split_sname s] split the surname [s] in parts composing it.
    e.g. [split_sname base "Foo-Bar"] is [[ "Foo" ; "Bar"]] *)
val split_sname  : string -> string list

(** [split_fname s] split the string [s] representing multiple first names
    into this list of firstname.
    e.g. [split_fname base "Foo-Bar Baz"] is [[ "Foo-Bar" ; "Baz"]] *)
val split_fname : string -> string list

(** [split_sname_callback fn s]
    Same as [split_sname], but call [fn] with substring indices instead of building
    a list
*)
val split_sname_callback : (int -> int -> unit) -> string -> unit

(** [split_fname_callback fn s]
    Same as [split_fname], but call [fn] with substring indices instead of building
    a list
*)
val split_fname_callback : (int -> int -> unit) -> string -> unit
