(* $Id: merge.mli,v 5.3 2007-01-11 12:57:05 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Gwdb;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;
