(* $Id: updateInd.mli,v 1.2 1999-02-02 10:24:38 ddr Exp $ *)
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

