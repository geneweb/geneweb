(* $Id: perso.mli,v 4.0 2001-03-16 19:34:56 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
