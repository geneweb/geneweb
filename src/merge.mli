(* $Id: merge.mli,v 2.1 1999-03-08 11:18:51 ddr Exp $ *)
(* Copyright (c) 1999 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> base_person -> unit;
value print : config -> base -> base_person -> unit;

