(* $Id: perso.mli,v 4.3 2002-10-26 12:07:33 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;

value print : config -> base -> person -> unit;
