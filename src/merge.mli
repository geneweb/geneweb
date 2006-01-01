(* $Id: merge.mli,v 5.1 2006-01-01 05:35:07 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

