(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

type base_error = Geneweb_db.Driver.person Def.error
(** Database specification error *)

type base_warning =
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.person,
    Geneweb_db.Driver.ifam,
    Geneweb_db.Driver.family,
    Geneweb_db.Driver.title,
    Geneweb_db.Driver.pers_event,
    Geneweb_db.Driver.fam_event )
  Def.warning
(** Database specification warning *)

(* *)
type base_misc =
  ( Geneweb_db.Driver.person,
    Geneweb_db.Driver.family,
    Geneweb_db.Driver.title )
  Def.misc

val union_date : Geneweb_db.Driver.family -> Adef.date option
(** [union_date fam] is the date ordering family [fam] among the families of a
    person: its marriage date, else the date of its first dated event; textual
    dates are ignored. *)

val compare_unions :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.ifam ->
  int
(** [compare_unions base f1 f2] is [-1] when every day the union date of [f1]
    may denote precedes every day the one of [f2] may denote, [1] the other way
    round, [0] otherwise, notably without union date. *)

val free_order : ('a -> 'a -> int) -> 'a list -> bool
(** [free_order cmp l] tells whether [cmp] leaves the order of [l] open: an item
    of [l] is not strictly before ([cmp x y < 0]) an item following it. False
    for fewer than two items. *)

val misplaced : ('a -> 'a -> int) -> 'a list -> ('a * 'a) option
(** [misplaced cmp l] is the first pair [(x, y)] of [l], [x] coming before [y],
    that [cmp] orders the other way round ([cmp x y > 0]). *)

val compare_pevents :
  Geneweb_db.Driver.pers_event -> Geneweb_db.Driver.pers_event -> int
(** [compare_pevents e1 e2] orders two personal events as {!Event.compare}. *)

val compare_fevents :
  Geneweb_db.Driver.fam_event -> Geneweb_db.Driver.fam_event -> int
(** [compare_fevents e1 e2] orders two events of a family as {!Event.compare};
    events it leaves unordered are ordered by kind: an event of the relation
    (engagement, banns, contract, licence, marriage, PACS, residence, no
    mention, no marriage) comes before a divorce, a separation or an annulment.
    A remarriage of the couple after a divorce thus needs both events dated. *)

val check_siblings :
  ?onchange:bool ->
  Geneweb_db.Driver.base ->
  (base_warning -> unit) ->
  Geneweb_db.Driver.ifam * Geneweb_db.Driver.family ->
  (Geneweb_db.Driver.person -> unit) ->
  unit
(** [check_siblings ?onchange base warning (ifam, fam) callback] Checks birth
    date consistency between siblings. Also calls [callback] with each child. *)

val person :
  ?onchange:bool ->
  Geneweb_db.Driver.base ->
  (base_warning -> unit) ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.iper
  * Geneweb_db.Driver.person
  * Def.sex option
  * Geneweb_db.Driver.relation list option)
  list
  option
(** [person onchange base warn p] checks person's properties:

    - personal events
    - person's age
    - person's titles dates
    - etc. If [onchange] is set then sort person's events Calls [warn] on
      corresponding [base_warning] when find some inconsistencies. *)

val family :
  ?onchange:bool ->
  Geneweb_db.Driver.base ->
  (base_warning -> unit) ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.family ->
  unit
(** [family onchange base warn f] checks family properties like :

    - familial events
    - parents marraige
    - children age gap and birth
    - etc. If [onchange] is set then sort family's events Calls [warn] on
      corresponding [base_warning] when find some inconsistencies. *)

val on_person_update :
  Geneweb_db.Driver.base ->
  (base_warning -> unit) ->
  Geneweb_db.Driver.person ->
  unit
(** Unlike [person] who checks directly the properties of a person, checks the
    properties of a person in relation to other people (his children, parents,
    spouses, witnesses, etc). Calls [warn] on corresponding [base_warning] when
    find some inconsistencies. *)

val sort_children :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper array ->
  (Geneweb_db.Driver.iper array * Geneweb_db.Driver.iper array) option
(** Sort array of children by their birth date from oldest to youngest. Returns
    old array and sorted version. *)

val check_other_fields :
  Geneweb_db.Driver.base ->
  (base_misc -> unit) ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.family ->
  unit
(** Cheks if family, father and mother have sources. Otherwise call [misc] on
    [base_misc] *)

val eq_warning : Geneweb_db.Driver.base -> base_warning -> base_warning -> bool
(** equality between base_warnings *)
