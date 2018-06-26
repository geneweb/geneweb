(* $Id: srcfile.mli,v 5.5 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

val print : config -> base -> string -> unit
val print_source : config -> base -> string -> unit
val print_start : config -> base -> unit
val incr_welcome_counter : config -> (int * int * string) option
val incr_request_counter : config -> (int * int * string) option

val print_lexicon : config -> base -> unit

val adm_file : string -> string
val source_file_name : config -> string -> string
