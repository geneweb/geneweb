(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

val special_vars : string list

val treat_request : Config.config -> unit

val make_senv : Config.config -> Gwdb.base -> unit

val make_henv : Config.config -> Gwdb.base -> unit
