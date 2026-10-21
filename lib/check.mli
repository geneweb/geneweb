(* Copyright (c) 2006-2007 INRIA *)

(* checking database ; independent from its implementation on disk *)

val print_base_error :
  out_channel -> Geneweb_db.Driver.base -> CheckItem.base_error -> unit
(** Print database specification error on the giving channel *)

val print_base_warning :
  out_channel -> Geneweb_db.Driver.base -> CheckItem.base_warning -> unit
(** Print database specification warning on the giving channel *)

val check_base :
  ?verbose:bool ->
  ?mem:bool ->
  Geneweb_db.Driver.base ->
  (CheckItem.base_error -> unit) ->
  (CheckItem.base_warning -> unit) ->
  (Geneweb_db.Driver.iper
   * Geneweb_db.Driver.person
   * Def.sex option
   * Geneweb_db.Driver.relation list option ->
  unit) ->
  unit
(** [check_base base onwarning onerror _] makes full database proprety check.
    Checks every person and family separetely with corresponding function inside
    [CheckItem] module. Checks also person's graph in order to find cycles (if
    person is own ancestor). *)
