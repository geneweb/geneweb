(* $Id: updateInd.mli,v 4.0 2001-03-16 19:35:08 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;

value print_update_ind :
  config -> base -> gen_person Update.key string -> string -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

