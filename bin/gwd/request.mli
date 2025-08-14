(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

val make_senv : Config.config -> Geneweb_db.Driver.base -> Config.config
val make_henv : Config.config -> Geneweb_db.Driver.base -> Config.config

val w_base :
  none:(Config.config -> 'a) ->
  (Config.config -> Geneweb_db.Driver.base -> 'a) ->
  Config.config ->
  string option ->
  'a
(** [w_lock ~none callback conf base] Acquire a write lock on the base and call
    [callback], or fail with [none]. *)

val w_lock :
  onerror:(Config.config -> string option -> 'a) ->
  (Config.config -> string option -> 'a) ->
  Config.config ->
  string option ->
  'a
(** [w_lock ~onerror callback conf base] Acquire a write lock on the base and
    call the callback, or fail with [onerror]. *)

val w_wizard :
  (Config.config -> Geneweb_db.Driver.base -> unit) ->
  Config.config ->
  Geneweb_db.Driver.base ->
  unit
(** [w_wizard callback conf base] Run [callback conf base] if conf has wizard
    rights or return [Forbidden] or [Unauthorized]. *)

val w_person :
  none:(Config.config -> Geneweb_db.Driver.base -> 'a) ->
  (Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> 'a) ->
  Config.config ->
  Geneweb_db.Driver.base ->
  'a
(** [w_person ~none callback conf base] Find a person in environement and call
    [callback], or fail with [none]. *)

(**/**)

(* Used internally by [gwd]. Not intended to be used by other programs. *)
val treat_request : Config.config -> unit

(**/**)

(**/**)

(* Used by v7 plugin *)
val incorrect_request : ?comment:string -> Config.config -> unit
val only_special_env : (string * _) list -> bool

(**/**)
