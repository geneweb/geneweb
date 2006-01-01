(* $Id: updateInd.mli,v 5.1 2006-01-01 05:35:08 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;

value print_update_ind :
  config -> base -> gen_person Update.key string -> string -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

