(* $Id: merge.mli,v 4.0 2001-03-16 19:34:48 ddr Exp $ *)
(* Copyright (c) 2001 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

