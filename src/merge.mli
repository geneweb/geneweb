(* $Id: merge.mli,v 3.1 2000-01-10 02:14:39 ddr Exp $ *)
(* Copyright (c) 2000 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

