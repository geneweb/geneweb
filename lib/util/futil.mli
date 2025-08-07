(* Copyright (c) 2006-2007 INRIA *)

val map_title_strings :
  ?fd:(Date.date -> Date.date) ->
  ('a -> 'b) ->
  'a Def.gen_title ->
  'b Def.gen_title
(** Convert generic type used to represent name, id and the place of
    [Def.gen_title] into another one. If [fd] is present, apply it on the date
    of the start and date of the end of a title *)

val map_pers_event :
  ?fd:(Date.date -> Date.date) ->
  ('a -> 'c) ->
  (?format:[ `Plain_text | `Html ] -> 'b -> 'd) ->
  ('a, 'b) Def.gen_pers_event ->
  ('c, 'd) Def.gen_pers_event
(** Convert:

    - Generic type used to represent witnesses of [Def.gen_pers_event] into
      another one.
    - Generic type used to represent name, place, reason, note and source of
      [Def.gen_pers_event] into another one. If [fd] is present, apply it on
      date of the personal event. *)

val map_fam_event :
  ?fd:(Date.date -> Date.date) ->
  ('a -> 'c) ->
  (?format:[ `Plain_text | `Html ] -> 'b -> 'd) ->
  ('a, 'b) Def.gen_fam_event ->
  ('c, 'd) Def.gen_fam_event
(** Convert:

    - Generic type used to represent witnesses of [Def.gen_fam_event] into
      another one.
    - Generic type used to represent name, place, reason, note and source of
      [Def.gen_fam_event] into another one. If [fd] is present, apply it on date
      of the familial event. *)

val map_relation_ps :
  ('a -> 'c) ->
  (?format:[ `Plain_text | `Html ] -> 'b -> 'd) ->
  ('a, 'b) Def.gen_relation ->
  ('c, 'd) Def.gen_relation
(** Convert:

    - Generic type used to represent father and mother inside [Def.gen_relation]
      into another one.
    - Generic type used to represent sources of [Def.gen_relation] into another
      one. *)

val map_person_ps :
  ?fd:(Date.date -> Date.date) ->
  ('b -> 'd) ->
  (?format:[ `Plain_text | `Html ] -> 'c -> 'e) ->
  ?f_first_name:('c -> 'e) ->
  ?f_surname:('c -> 'e) ->
  ('a, 'b, 'c) Def.gen_person ->
  ('a, 'd, 'e) Def.gen_person
(** Convert:

    - Generic type used to represent related persons (parents, witnesses of a
      personal event, etc.) of [Def.gen_person] into another one.
    - Generic type used to represent another large part of information of
      [Def.gen_person] into another one. If [fd] is present, apply it on every
      date (birth, death, titles,, personal events, etc.). Generic type that is
      used to represent indexation key isn't converted. *)

val map_ascend_f : ('a -> 'b) -> 'a Def.gen_ascend -> 'b Def.gen_ascend
(** Convert generic type used to represent family inside [Def.gen_ascend] into
    another one. *)

val map_union_f : ('a -> 'b) -> 'a Def.gen_union -> 'b Def.gen_union
(** Convert generic type used to represent one of the famillies inside
    [Def.gen_union] into another one. *)

val map_divorce : (Date.date -> Date.date) -> Def.divorce -> Def.divorce
(** Convert generic type used to represent a divorce into another one. *)

val map_family_ps :
  ?fd:(Date.date -> Date.date) ->
  ('a -> 'b) ->
  ('c -> 'd) ->
  (?format:[ `Plain_text | `Html ] -> 'e -> 'f) ->
  ('a, 'c, 'e) Def.gen_family ->
  ('b, 'd, 'f) Def.gen_family
(** Convert:

    - Generic type used to represent faimily indexation key into another one.
    - Generic type used to represent witnesses (of the marriage or of a
      famillial events, etc.) of [Def.gen_family] into another one.
    - Generic type used to represent another large part of information of
      [Def.gen_family] into another one. If [fd] is present, apply it on it on
      every date (marriage, divorce, famillial events, etc.).*)

val map_couple_p : ('a -> 'b) -> 'a Def.gen_couple -> 'b Def.gen_couple
(** Convert generic type used to represent father and mother inside
    [Def.gen_couple] into another one. *)

val map_descend_p : ('a -> 'b) -> 'a Def.gen_descend -> 'b Def.gen_descend
(** Convert generic type used to represent children inside [Def.gen_descend]
    into another one.*)

val eq_titles :
  ('a -> 'b -> bool) -> 'a Def.gen_title -> 'b Def.gen_title -> bool
(** Says if two titles with different types are equal with given comparison
    function. *)

val eq_title_names :
  ('a -> 'b -> bool) -> 'a Def.gen_title_name -> 'b Def.gen_title_name -> bool
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

val map_cdate : (Date.date -> Date.date) -> Adef.cdate -> Adef.cdate
val map_death : (Date.date -> Date.date) -> Def.death -> Def.death
val map_burial : (Date.date -> Date.date) -> Def.burial -> Def.burial
