(* $Id: updateInd.mli,v 1.1 1998-09-01 14:32:09 ddr Exp $ *)

open Config;
open Def;

value print_add1 : config -> base -> person string -> unit;
value print_mod1 : config -> base -> person string -> string -> unit;
value merge_call : config -> unit;
value print_person : config -> base -> person string -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

