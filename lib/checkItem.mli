(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb

type base_error = person Def.error
(** Database specification error *)

val person :
  ?onchange:bool ->
  base ->
  (Warning.base_warning -> unit) ->
  person ->
  (iper * person * Def.sex option * relation list option) list option
(** [person onchange base warn p] checks person's properties:

    - personal events
    - person's age
    - person's titles dates
    - etc.
    If [onchange] is set then sort person's events
    Calls [warn] on corresponding [base_warning] when find some inconsistencies. *)

val family :
  ?onchange:bool ->
  base ->
  (Warning.base_warning -> unit) ->
  ifam ->
  family ->
  unit
(** [family onchange base warn f] checks family properties like :

    - familial events
    - parents marraige
    - children age gap and birth
    - etc.
    If [onchange] is set then sort family's events
    Calls [warn] on corresponding [base_warning] when find some inconsistencies. *)

val on_person_update : base -> (Warning.base_warning -> unit) -> person -> unit
(** Unlike [person] who checks directly the properties of a person, checks the properties
    of a person in relation to other people (his children, parents, spouses, witnesses, etc).
    Calls [warn] on corresponding [base_warning] when find some inconsistencies.
 *)

val sort_children : base -> iper array -> (iper array * iper array) option
(** Sort array of children by their birth date from oldest to youngest.
    Returns old array and sorted version. *)

val check_other_fields :
  base -> (Warning.base_misc -> unit) -> ifam -> family -> unit
(** Cheks if family, father and mother have sources. Otherwise call [misc] on [base_misc] *)

val eq_warning : Warning.base_warning -> Warning.base_warning -> bool
(** equality between base_warnings *)

val person_warnings :
  Config.config -> base -> person -> Warning.base_warning list
(** [person_warnings conf base p]
    Shorthand for [CheckItem.person] and [CheckItem.on_person_update] on [p]
    and [CheckItem.check_siblings] on they children
    using [auth_warning] for filtering.
*)
