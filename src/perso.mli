(* $Id: perso.mli,v 4.2 2002-10-26 01:22:43 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_titles : config -> base -> bool -> string -> person -> unit;
value string_of_marriage_text : config -> base -> family -> string;

value print : config -> base -> person -> unit;
