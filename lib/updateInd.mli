(* $Id: updateInd.mli,v 5.4 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

val string_person_of : base -> person -> (Update.key, string) gen_person

val print_update_ind :
  config -> base -> (Update.key, string) gen_person -> string -> unit

val print_add : config -> base -> unit
val print_del : config -> base -> unit
val print_mod : config -> base -> unit
val print_change_event_order : config -> base -> unit

