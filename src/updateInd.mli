(* $Id: updateInd.mli,v 5.2 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Config;
open Def;
open Gwdb;

value print_update_ind :
  config -> base -> gen_person Update.key string -> string -> unit
;

value print_add : config -> base -> unit;
value print_del : config -> base -> unit;
value print_mod : config -> base -> unit;

