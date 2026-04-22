(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type base_error = Gwdb.person Def.error
(** Database specification error *)

val person :
  ?onchange:bool ->
  Gwdb.base ->
  (Warning.base_warning -> unit) ->
  Gwdb.person ->
  (Gwdb.iper * Gwdb.person * Def.sex option * Gwdb.relation list option) list
  option
(** [person onchange base warn p] checks person's properties:

    - personal events
    - person's age
    - person's titles dates
    - etc. If [onchange] is set then sort person's events Calls [warn] on
      corresponding [base_warning] when find some inconsistencies. *)

val family :
  ?onchange:bool ->
  Gwdb.base ->
  (Warning.base_warning -> unit) ->
  Gwdb.ifam ->
  Gwdb.family ->
  unit
(** [family onchange base warn f] checks family properties like :

    - familial events
    - parents marraige
    - children age gap and birth
    - etc. If [onchange] is set then sort family's events Calls [warn] on
      corresponding [base_warning] when find some inconsistencies. *)

val on_person_update :
  Gwdb.base -> (Warning.base_warning -> unit) -> Gwdb.person -> unit
(** Unlike [person] who checks directly the properties of a person, checks the
    properties of a person in relation to other people (his children, parents,
    spouses, witnesses, etc). Calls [warn] on corresponding [base_warning] when
    find some inconsistencies. *)

val sort_children :
  Gwdb.base -> Gwdb.iper array -> (Gwdb.iper array * Gwdb.iper array) option
(** Sort array of children by their birth date from oldest to youngest. Returns
    old array and sorted version. *)

val check_other_fields :
  Gwdb.base -> (Warning.base_misc -> unit) -> Gwdb.ifam -> Gwdb.family -> unit
(** Cheks if family, father and mother have sources. Otherwise call [misc] on
    [base_misc] *)

val eq_warning : Warning.base_warning -> Warning.base_warning -> bool
(** equality between base_warnings *)

val person_warnings :
  Config.config -> Gwdb.base -> Gwdb.person -> Warning.base_warning list
(** [person_warnings conf base p] Shorthand for [CheckItem.person] and
    [CheckItem.on_person_update] on [p] and [CheckItem.check_siblings] on they
    children using [auth_warning] for filtering. *)
