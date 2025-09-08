(* Copyright (c) 1998-2007 INRIA *)

val w_base :
  none:(Geneweb.Config.config -> 'a) ->
  (Geneweb.Config.config -> Gwdb.base -> 'a) ->
  Geneweb.Config.config ->
  string option ->
  'a
(** [w_lock ~none callback conf base] Acquire a write lock on the base and call
    [callback], or fail with [none]. *)

val w_lock :
  onerror:(Geneweb.Config.config -> string option -> 'a) ->
  (Geneweb.Config.config -> string option -> 'a) ->
  Geneweb.Config.config ->
  string option ->
  'a
(** [w_lock ~onerror callback conf base] Acquire a write lock on the base and
    call the callback, or fail with [onerror]. *)

val w_person :
  none:(Geneweb.Config.config -> Gwdb.base -> 'a) ->
  (Geneweb.Config.config -> Gwdb.base -> Gwdb.person -> 'a) ->
  Geneweb.Config.config ->
  Gwdb.base ->
  'a
(** [w_person ~none callback conf base] Find a person in environement and call
    [callback], or fail with [none]. *)

(**/**)

(* Used internally by [gwd]. Not intended to be used by other programs. *)
val treat_request : Geneweb.Config.config -> unit

(**/**)

(**/**)

val person_selected_with_redirect :
  conf:Geneweb.Config.config ->
  base:Gwdb.base ->
  ?parameters:(string * string) list ->
  person:Gwdb.person ->
  unit ->
  unit

(* Used by v7 plugin *)
val incorrect_request : Geneweb.Config.config -> unit

(**/**)
