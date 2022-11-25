(* $Id: srcfile.mli,v 5.5 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Gwdb
open Config

type src_mode = Lang | Source

val print : config -> base -> string -> unit
val print_source : config -> base -> string -> unit
val print_start : config -> base -> unit
val incr_welcome_counter : config -> (int * int * string) option
val incr_request_counter : config -> (int * int * string) option

val adm_file : string -> string
(** Compute administration file path with giving name (search inside {i cnt} directory) *)

val source_file_name : config -> string -> string
val copy_from_stream : config -> base -> char Stream.t -> src_mode -> unit
