(* $Id: updateInd.mli,v 2.1 1999-03-08 11:19:26 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value print_add1 : config -> base -> person string -> unit;
value print_mod1 : config -> base -> person string -> string -> unit;
value merge_call : config -> unit;
value print_person : config -> base -> person string -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

