(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb

(** Database specification error *)
type base_error = person Def.error

(** Database specification warning *)
type base_warning = (iper, person, ifam, family, title, pers_event, fam_event) Def.warning

(* *)
type base_misc = (person, family, title) Def.misc

(** Event name that unites personal and familial event names *)
type 'string event_name =
  | Psort of 'string Def.gen_pers_event_name (** Personal event name *)
  | Fsort of 'string Def.gen_fam_event_name (** Familial event name *)

(** Sort events (both peronal and familial) by their date and their name *)
val sort_events :
  ('a -> 'string event_name) -> ('a -> Adef.cdate) -> 'a list -> 'a list

(** Merge two sorted event lists (returns sorted list) *)
val merge_events :
  ('a -> 'string event_name) -> ('a -> Adef.cdate) -> 'a list -> 'a list ->
    'a list

(** [check_siblings ?onchange base warning (ifam, fam) callback]
    Checks birth date consistency between siblings.
    Also calls [callback] with each child. *)
val check_siblings :
 ?onchange:bool
 -> base
 -> (base_warning -> unit)
 -> (ifam * family)
 -> (person -> unit)
 -> unit

(** [person onchange base warn p] checks person's properties:

    - personal events
    - person's age
    - person's titles dates
    - etc.
    If [onchange] is set then sort person's events
    Calls [warn] on corresponding [base_warning] when find some inconsistencies. *)
val person
  : ?onchange:bool
  -> base
  -> (base_warning -> unit)
  -> person
  -> (iper * person * Def.sex option * relation list option) list option

(** [family onchange base warn f] checks family properties like :

    - familial events
    - parents marraige
    - children age gap and birth
    - etc.
    If [onchange] is set then sort family's events
    Calls [warn] on corresponding [base_warning] when find some inconsistencies. *)
val family
  : ?onchange:bool
  -> base
  -> (base_warning -> unit)
  -> ifam
  -> family
  -> unit

(** Unlike [person] who checks directly the properties of a person, checks the properties
    of a person in relation to other people (his children, parents, spouses, witnesses, etc).
    Calls [warn] on corresponding [base_warning] when find some inconsistencies.
 *)
val on_person_update
  : base
  -> (base_warning -> unit)
  -> person
  -> unit

(** Sort array of children by their birth date from oldest to youngest.
    Returns old array and sorted version. *)
val sort_children :
  base -> iper array -> (iper array * iper array) option

(** Cheks if family, father and mother have sources. Otherwise call [misc] on [base_misc] *)
val check_other_fields :
  base -> (base_misc -> unit) -> ifam -> family -> unit
