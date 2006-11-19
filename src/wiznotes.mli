(* $Id: wiznotes.mli,v 5.2 2006-11-19 13:56:33 ddr Exp $ *)
(* Copyright (c) 2006 INRIA *)

open Config;
open Gwdb;

value dir : config -> base -> string;

value print : config -> base -> unit;
value print_mod : config -> base -> unit;
value print_mod_ok : config -> base -> unit;
value print_view : config -> base -> unit;
value print_search : config -> base -> unit;

value connected_wizards : config -> base -> unit;
value change_wizard_visibility : config -> base -> unit;
