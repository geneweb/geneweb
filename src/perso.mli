(* $Id: perso.mli,v 3.2 2000-10-28 08:49:13 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
