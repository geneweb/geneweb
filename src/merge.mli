(* $Id: merge.mli,v 5.4 2007-01-12 05:24:45 ddr Exp $ *)
(* Copyright (c) 1998-2006 INRIA *)

open Gwdb;
open Config;

value print_someone : config -> base -> person -> unit;
value print : config -> base -> person -> unit;

value print_possible_continue_merging : config -> base -> unit;
