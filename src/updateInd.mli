(* $Id: updateInd.mli,v 3.3 2001-02-13 00:23:45 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Config;
open Def;

value print_update_ind :
  config -> base -> gen_person Update.key string -> string -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

