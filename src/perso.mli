(* $Id: perso.mli,v 5.1 2006-01-01 05:35:08 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Config;

value string_of_titles : config -> base -> bool -> string -> person -> string;
value string_of_marriage_text : config -> base -> family -> string;
value interp_templ : string -> config -> base -> person -> unit;

value print : config -> base -> person -> unit;
value print_ascend : config -> base -> person -> unit;

(**)

value infinite : int;
value limit_desc : config -> int;
value make_desc_level_table : config -> base -> int -> person -> array int;
value default_max_cousin_lev : int;
