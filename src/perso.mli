(* $Id: perso.mli,v 3.1 2000-01-10 02:14:41 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value print_sources : config -> base -> bool -> person -> unit;
value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
