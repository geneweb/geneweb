(* $Id: srcfile.mli,v 5.5 2007-01-19 01:53:17 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config

type src_mode = Lang | Source

val print : config -> Geneweb_db.Driver.base -> string -> unit
val print_source : config -> Geneweb_db.Driver.base -> string -> unit
val incr_welcome_counter : config -> (int * int * string) option
val incr_request_counter : config -> (int * int * string) option
val source_file_name : config -> string -> string

val copy_from_stream :
  config -> Geneweb_db.Driver.base -> char Stream.t -> src_mode -> unit

val propose_base : Config.config -> unit
(** [propose_base conf] renders a fallback page presenting an input form to let
    the user select a base by name. Used when no base name was provided in the
    request and the welcome page is not appropriate (typical case: gwd is
    reachable but no [b=] parameter was sent). *)

val print_welcome : config -> Geneweb_db.Driver.base -> unit
