(* $Id: perso.mli,v 4.5 2005-05-10 22:07:02 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;
value interp_templ : string -> config -> base -> person -> unit;

value print : config -> base -> person -> unit;
