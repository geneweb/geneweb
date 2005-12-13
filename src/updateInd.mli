(* $Id: updateInd.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Config;
open Def;

value print_update_ind :
  config -> base -> gen_person Update.key string -> string -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

