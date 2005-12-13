(* $Id: merge.mli,v 5.0 2005-12-13 11:51:27 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

