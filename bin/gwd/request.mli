(* Copyright (c) 1998-2007 INRIA *)

val make_senv : Geneweb.Config.config -> Gwdb.base -> Geneweb.Config.config
val make_henv : Geneweb.Config.config -> Gwdb.base -> Geneweb.Config.config

val w_base :
  none:(Geneweb.Config.config -> 'a) ->
  (Geneweb.Config.config -> Gwdb.base -> 'a) ->
  Geneweb.Config.config ->
  string option ->
  'a
(** [w_lock ~none callback conf base]
    Acquire a write lock on the base and call [callback], or fail with [none].
*)

val w_lock :
  onerror:(Geneweb.Config.config -> string option -> 'a) ->
  (Geneweb.Config.config -> string option -> 'a) ->
  Geneweb.Config.config ->
  string option ->
  'a
(** [w_lock ~onerror callback conf base]
    Acquire a write lock on the base and call the callback, or fail with [onerror].
*)

val w_wizard :
  (Geneweb.Config.config -> Gwdb.base -> unit) ->
  Geneweb.Config.config ->
  Gwdb.base ->
  unit
(** [w_wizard callback conf base]
    Run [callback conf base] if conf has wizard rights or
    return [Forbidden] or [Unauthorized].
*)

val w_person :
  none:(Geneweb.Config.config -> Gwdb.base -> 'a) ->
  (Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> 'a) ->
  Geneweb.Config.config ->
  Gwdb.base ->
  'a
(** [w_person ~none callback conf base]
    Find a person in environement and call [callback], or fail with [none].
*)

(**/**)

(* Used internally by [gwd]. Not intended to be used by other programs. *)
val treat_request : Geneweb.Config.config -> unit

(**/**)

(**/**)

(* Used by v7 plugin *)
val incorrect_request : Geneweb.Config.config -> unit
val very_unknown : Geneweb.Config.config -> Gwdb.base -> unit
val only_special_env : (string * _) list -> bool

(**/**)
