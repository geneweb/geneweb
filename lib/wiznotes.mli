(* $Id: wiznotes.mli,v 5.3 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Config
open Gwdb

val print : config -> base -> unit
val print_mod : config -> unit
val print_mod_ok : config -> base -> unit
val print_view : config -> unit
val print_search : config -> base -> unit

val connected_wizards : config -> unit
val change_wizard_visibility : config -> unit
