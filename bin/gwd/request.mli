(* Copyright (c) 1998-2007 INRIA *)

open Geneweb

val w_base :
  none:(Config.config -> 'a) ->
  (Config.config -> Geneweb_db.Driver.base -> 'a) ->
  Config.config ->
  string option ->
  'a
(** [w_base ~none callback conf bfile] opens the database at path [bfile] in
    read-only mode, sets up [conf.henv] / [conf.senv] / [conf.default_sosa_ref]
    via {!make_henv} and {!make_senv}, then calls [callback conf base]. If
    [bfile] is [None], [none conf] is invoked instead. No locking is performed —
    see {!w_lock} for write protection. *)

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
