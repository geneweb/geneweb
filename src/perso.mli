(* $Id: perso.mli,v 3.3 2001-01-06 09:55:58 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
