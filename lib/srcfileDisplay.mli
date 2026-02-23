(* $Id: srcfile.mli,v 5.5 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

type src_mode = Lang | Source

val print : Config.config -> Gwdb.base -> string -> unit
val print_source : Config.config -> Gwdb.base -> string -> unit
val print_start : Config.config -> Gwdb.base -> unit
val incr_welcome_counter : Config.config -> (int * int * string) option
val incr_request_counter : Config.config -> (int * int * string) option

val adm_file : string -> string
(** Compute administration file path with giving name (search inside
    {i cnt} directory) *)

val source_file_name : Config.config -> string -> string
