(* $Id: perso.mli,v 4.1 2002-09-19 15:13:50 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_titles : config -> base -> bool -> string -> person -> unit;
value print_marriage_text : config -> base -> family -> unit;

value print : config -> base -> person -> unit;
