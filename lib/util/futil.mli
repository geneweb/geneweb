(* Copyright (c) 2006-2007 INRIA *)

open Def

val map_title_strings
  : ?fd:(Def.date -> Def.date)
  -> ('a -> 'b)
  -> 'a gen_title
  -> 'b gen_title

val map_pers_event
  : ?fd:(Def.date -> Def.date)
  -> ('a -> 'c)
  -> ('b -> 'd)
  -> ('a, 'b) gen_pers_event
  -> ('c, 'd) gen_pers_event

val map_fam_event
  : ?fd:(Def.date -> Def.date)
  -> ('a -> 'c)
  -> ('b -> 'd)
  -> ('a, 'b) gen_fam_event
  -> ('c, 'd) gen_fam_event

val map_relation_ps :
  ('a -> 'c) -> ('b -> 'd) -> ('a, 'b) gen_relation -> ('c, 'd) gen_relation

val map_person_ps
  : ?fd:(Def.date -> Def.date)
  -> ('b -> 'd)
  -> ('c -> 'e)
  -> ('a, 'b, 'c) gen_person
  -> ('a, 'd, 'e) gen_person

val map_ascend_f : ('a -> 'b) -> 'a gen_ascend -> 'b gen_ascend
val map_union_f : ('a -> 'b) -> 'a gen_union -> 'b gen_union

val map_family_ps
  : ?fd:(Def.date -> Def.date)
  -> ('a -> 'b)
  -> ('c -> 'd)
  -> ('e -> 'f)
  -> ('a, 'c, 'e) gen_family
  -> ('b, 'd, 'f) gen_family

val map_couple_p : bool -> ('a -> 'b) -> 'a gen_couple -> 'b gen_couple
val map_descend_p : ('a -> 'b) -> 'a gen_descend -> 'b gen_descend

val eq_lists : ('a -> 'b -> bool) -> 'a list -> 'b list -> bool
val eq_titles : ('a -> 'b -> bool) -> 'a gen_title -> 'b gen_title -> bool
val eq_title_names :
  ('a -> 'b -> bool) -> 'a gen_title_name -> 'b gen_title_name -> bool

val parent : bool -> 'a array -> 'a gen_couple

(** Return a list of string corresponding to various
    mix between all kind of names.
    It can contain duplicates. Strings are used raw (not lowered).
 *)
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
  'a Def.gen_title list -> string list
