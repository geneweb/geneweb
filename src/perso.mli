(* $Id: perso.mli,v 2.3 1999-04-16 18:35:07 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value copy_string_with_macros : config -> string -> unit;
value print_titles : config -> base -> bool -> string -> person -> unit;
value print_dates : config -> base -> bool -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
