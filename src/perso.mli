(* $Id: perso.mli,v 2.5 1999-08-21 11:45:44 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_sources : config -> base -> bool -> person -> unit;
value print_titles : config -> base -> bool -> string -> person -> unit;
value print_dates : config -> base -> bool -> person -> unit;
value print_marriage_text : config -> base -> bool -> family -> unit;

value print : config -> base -> person -> unit;
