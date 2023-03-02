open Gwdb

(* NOTE : checkItem defined this as 'string event_same, instead of just istr
   .. why make it complicated *)

type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

type 'a event_item

val get_name : 'a event_item -> 'a event_name
val get_date : 'a event_item -> Def.cdate
val get_place : 'a event_item -> istr
val get_note : 'a event_item -> istr
val get_src : 'a event_item -> istr
val get_witnesses : 'a event_item -> (iper * Def.witness_kind) array
val get_witness_notes : 'a event_item ->  istr array
val get_witnesses_and_notes : 'a event_item -> (iper * Def.witness_kind * istr) array
val get_spouse_iper : 'a event_item -> iper option

val has_witnesses : 'a event_item -> bool

val pevent_name : 'a Def.gen_pers_event_name -> 'a event_name
val fevent_name : 'a Def.gen_fam_event_name -> 'a event_name

val event_item_of_pevent : pers_event -> istr event_item
val event_item_of_fevent : ?sp:iper -> fam_event -> istr event_item

val compare_event_name : 'a event_name -> 'a event_name -> int

val sort_events :
  ('a -> 'b event_name) -> ('a -> Adef.cdate) -> 'a list -> 'a list
(** Sort events (both personal and familial) by their date and their name *)

val events : Config.config -> base -> person -> istr event_item list
(** [events conf base p] is the list of [p]'s events *)

val sorted_events : Config.config -> base -> person -> istr event_item list
(** [sorted_events conf base p] is the list of [p]'s events, sorted by Checkitem.sorted_events *)
