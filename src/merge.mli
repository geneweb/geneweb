(* $Id: merge.mli,v 1.2 1998-09-30 14:04:43 ddr Exp $ *)

open Def;
open Config;

value print_someone : config -> base -> base_person -> unit;
value print : config -> base -> base_person -> unit;

