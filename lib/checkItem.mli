(* $Id: checkItem.mli,v 1.12 2007-09-05 13:19:25 ddr Exp $ *)
(* Copyright (c) 2006-2007 INRIA *)

open Gwdb

type base_error = person Def.error
type base_warning = (person, family, title, pers_event, fam_event) Def.warning
type base_misc = (person, family, title) Def.misc

val leap_year : int -> bool
val nb_days_in_month : int -> int -> int
val time_elapsed : Def.dmy -> Def.dmy -> Def.dmy
val strictly_before_dmy : Def.dmy -> Def.dmy -> bool
val strictly_before : Def.date -> Def.date -> bool
val strictly_after_dmy : Def.dmy -> Def.dmy -> bool
val strictly_after : Def.date -> Def.date -> bool
val date_of_death : Def.death -> Adef.date option


type 'string event_name =
    Psort of 'string Def.gen_pers_event_name
  | Fsort of 'string Def.gen_fam_event_name
val sort_events :
  ('a -> 'string event_name) * ('a -> Adef.codate) -> 'a list -> 'a list

val merge_events :
  ('a -> 'string event_name) * ('a -> Adef.codate) -> 'a list -> 'a list ->
    'a list

val person :
  base -> (base_warning -> unit) -> person ->
    (Adef.iper * person * Def.sex option * relation list option) list option

val family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> unit

val reduce_family :
  base -> (base_error -> unit) -> (base_warning -> unit) -> Def.ifam ->
    family -> unit

val sort_children :
  base -> Adef.iper array -> (Adef.iper array * Adef.iper array) option

val check_other_fields :
  base -> (base_misc -> unit) -> Def.ifam -> family -> unit

val list_uniq : 'a list -> 'a list
val stable_list_uniq : 'a list -> 'a list
