(* $Id: perso.mli,v 2.6 1999-10-26 22:35:40 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_sources : config -> base -> bool -> person -> unit;
value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
