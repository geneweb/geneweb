(* $Id: updateInd.mli,v 2.4 1999-07-28 13:08:37 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value print_add1 : config -> base -> gen_person Update.key string -> unit;
value print_mod1 :
  config -> base -> gen_person Update.key string -> string -> unit;
value merge_call : config -> unit;
value print_person : config -> base -> gen_person Update.key string -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

