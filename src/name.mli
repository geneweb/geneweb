(* $Id: name.mli,v 4.0 2001-03-16 19:34:51 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

value lower : string -> string;
  (* Name.lower:
     - uppercase -> lowercase
     - no accents
     - chars no letters and no numbers (except '.') => spaces (stripped)
     Key comparison (first name, surname, number) applies "lower" equality
     on first names and surnames *)
value abbrev : string -> string;
  (* Name.abbrev: suppress lowercase particles, shorten "saint" into "st" *)
value strip : string -> string;
  (* Name.strip = name without spaces *)
value crush : string -> string;
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

value strip_lower : string -> string;
  (* strip_lower = strip o lower, as first comparison of names.
     First names and Surnames comparison is strip_lower equality. *)
value crush_lower : string -> string;
  (* crush_lower = crush o abbrev o lower, as second comparison of names.
     In index by names, the "names" are crush_lowers *)
