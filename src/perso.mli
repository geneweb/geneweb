(* $Id: perso.mli,v 4.4 2004-12-14 09:30:15 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;

value print : config -> base -> person -> unit;
