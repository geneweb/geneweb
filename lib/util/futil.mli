(* Copyright (c) 2006-2007 INRIA *)

open Def

val map_title_strings :
  ?fd:(Def.date -> Def.date) -> ('a -> 'b) -> 'a gen_title -> 'b gen_title
(** Convert generic type used to represent name, id and the place of
    [Def.gen_title] into another one. If [fd] is present, apply it on the date
    of the start and date of the end of a title *)

val map_pers_event :
  ?fd:(Def.date -> Def.date) ->
  ('a -> 'c) ->
  ('b -> 'd) ->
  ('a, 'b) gen_pers_event ->
  ('c, 'd) gen_pers_event
(** Convert:

    - Generic type used to represent witnesses of [Def.gen_pers_event] into
      another one.
    - Generic type used to represent name, place, reason, note and source of
      [Def.gen_pers_event] into another one. If [fd] is present, apply it on
      date of the personal event. *)

val map_fam_event :
  ?fd:(Def.date -> Def.date) ->
  ('a -> 'c) ->
  ('b -> 'd) ->
  ('a, 'b) gen_fam_event ->
  ('c, 'd) gen_fam_event
(** Convert:

    - Generic type used to represent witnesses of [Def.gen_fam_event] into
      another one.
    - Generic type used to represent name, place, reason, note and source of
      [Def.gen_fam_event] into another one. If [fd] is present, apply it on date
      of the familial event. *)

val map_relation_ps :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_relation -> ('c, 'd) gen_relation
(** Convert:

    - Generic type used to represent father and mother inside [Def.gen_relation]
      into another one.
    - Generic type used to represent sources of [Def.gen_relation] into another
      one. *)

val map_person_ps :
  ?fd:(Def.date -> Def.date) ->
  ('b -> 'd) ->
  ('c -> 'e) ->
  ('a, 'b, 'c) gen_person ->
  ('a, 'd, 'e) gen_person
(** Convert:

    - Generic type used to represent related persons (parents, witnesses of a
      personal event, etc.) of [Def.gen_person] into another one.
    - Generic type used to represent another large part of information of
      [Def.gen_person] into another one. If [fd] is present, apply it on every
      date (birth, death, titles,, personal events, etc.). Generic type that is
      used to represent indexation key isn't converted. *)

val map_ascend_f : ('a -> 'b) -> 'a gen_ascend -> 'b gen_ascend
(** Convert generic type used to represent family inside [Def.gen_ascend] into
    another one. *)

val map_union_f : ('a -> 'b) -> 'a gen_union -> 'b gen_union
(** Convert generic type used to represent one of the famillies inside
    [Def.gen_union] into another one. *)

val map_family_ps :
  ?fd:(Def.date -> Def.date) ->
  ('a -> 'b) ->
  ('c -> 'd) ->
  ('e -> 'f) ->
  ('a, 'c, 'e) gen_family ->
  ('b, 'd, 'f) gen_family
(** Convert:

    - Generic type used to represent faimily indexation key into another one.
    - Generic type used to represent witnesses (of the marriage or of a
      famillial events, etc.) of [Def.gen_family] into another one.
    - Generic type used to represent another large part of information of
      [Def.gen_family] into another one. If [fd] is present, apply it on it on
      every date (marriage, divorce, famillial events, etc.).*)

val map_couple_p : bool -> ('a -> 'b) -> 'a gen_couple -> 'b gen_couple
(** Convert generic type used to represent father and mother inside
    [Def.gen_couple] into another one. If first argument is true then use
    multi-parent functionality. *)

val parent : bool -> 'a array -> 'a gen_couple
(** @deprecated Use [Adef.parent] instead. *)

val map_descend_p : ('a -> 'b) -> 'a gen_descend -> 'b gen_descend
(** Convert generic type used to represent children inside [Def.gen_descend]
    into another one.*)

val eq_lists : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
(** Says if two lists with different element's type are equal with given
    comparison function. *)

val eq_titles : ('a -> 'b -> bool) -> 'a gen_title -> 'b gen_title -> bool
(** Says if two titles with different types are equal with given comparison
    function. *)

val eq_title_names :
  ('a -> 'b -> bool) -> 'a gen_title_name -> 'b gen_title_name -> bool
(** Says if two title names with different types are equal with given comparison
    function. *)

val gen_person_misc_names :
  ('a -> string) ->
  'a ->
  'a ->
  'a ->
  'a ->
  'a ->
  'a list ->
  'a list ->
  'a list ->
  'a list ->
  'a Def.gen_title list ->
  ('a * 'a list) array ->
  'a Def.gen_title list ->
  string list
(** Return a list of string corresponding to various mix between all kind of
    names. It can contain duplicates. Strings are used raw (not lowered). *)
