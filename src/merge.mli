(* $Id: merge.mli,v 1.3 1999-02-02 10:24:18 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> base_person -> unit;
value print : config -> base -> base_person -> unit;

