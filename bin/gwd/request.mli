(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

val make_senv : Config.config -> Gwdb.base -> Config.config

val make_henv : Config.config -> Gwdb.base -> Config.config

(** [w_lock ~none callback conf base]
    Acquire a write lock on the base and call [callback], or fail with [none].
*)
val w_base
  : none:(Config.config -> 'a)
  -> (Config.config -> Gwdb.base -> 'a)
  -> Config.config
  -> Gwdb.base option
  -> 'a

(** [w_lock ~onerror callback conf base]
    Acquire a write lock on the base and call the callback, or fail with [onerror].
*)
val w_lock
  : onerror:(Config.config -> Gwdb.base -> 'a)
  -> (Config.config -> Gwdb.base -> 'a)
  -> Config.config
  -> Gwdb.base
  -> 'a

(** [w_person ~none callback conf base]
    Find a person in environement and call [callback], or fail with [none].
*)
val w_person
  : none:(Config.config -> Gwdb.base -> 'a)
  -> (Config.config -> Gwdb.base -> Gwdb.person -> 'a)
  -> Config.config
  -> Gwdb.base
  -> 'a

(**/**)
(* Used internally by [gwd]. Not intended to be used by other programs. *)
val special_vars : string list
val treat_request : Config.config -> unit
(**/**)
