(* $Id: updateInd.mli,v 2.5 1999-10-06 08:47:56 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Config;
open Def;

value print_add1 :
  config -> base -> gen_person Update.key string -> unit;
value print_mod1 :
  config -> base -> gen_person Update.key string -> string -> unit;
value merge_call : config -> unit;
value print_person :
  config -> base -> gen_person Update.key string -> unit;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

