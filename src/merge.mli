(* $Id: merge.mli,v 4.1 2004-12-14 09:30:14 ddr Exp $ *)
(* Copyright (c) 1998-2005 INRIA *)

open Def;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

