(* $Id: merge.mli,v 5.2 2006-09-15 11:45:37 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Gwdb;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

