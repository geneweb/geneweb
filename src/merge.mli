(* $Id: merge.mli,v 1.1.1.1 1998-09-01 14:32:10 ddr Exp $ *)

open Def;
open Config;

value print_someone : base -> base_person -> unit;
value print : config -> base -> base_person -> unit;

