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

(** [w_wizard callback conf base]
    Run [callback conf base] if conf has wizard rights or
    return [Forbidden] or [Unauthorized].
*)
val w_wizard
  : (Config.config -> Gwdb.base -> unit)
  -> Config.config
  -> Gwdb.base
  -> unit

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
val treat_request : Config.config -> unit
(**/**)

(**/**)
(* Used by v7 plugin *)
val incorrect_request : Config.config -> unit
val unknown : Config.config -> string -> unit
val specify : Config.config -> Gwdb.base -> string -> Gwdb.person list -> unit
val very_unknown : Config.config -> Gwdb.base -> unit
val only_special_env : (string * string) list -> bool
val relation_print : Config.config -> Gwdb.base -> Gwdb.person -> unit
(**/**)
