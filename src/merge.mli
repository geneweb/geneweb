(* $Id: merge.mli,v 5.5 2007-01-19 01:53:16 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

val print_someone : config -> base -> person -> unit
val print : config -> base -> person -> unit

val print_possible_continue_merging : config -> base -> unit
