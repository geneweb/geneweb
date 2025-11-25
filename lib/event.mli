type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

type 'a event_item

val get_name : 'a event_item -> 'a event_name
val get_date : 'a event_item -> Def.cdate
val get_place : 'a event_item -> Gwdb.istr
val get_note : 'a event_item -> Gwdb.istr
val get_src : 'a event_item -> Gwdb.istr
val get_witnesses : 'a event_item -> (Gwdb.iper * Def.witness_kind) array
val get_witness_notes : 'a event_item -> Gwdb.istr array

val get_witnesses_and_notes :
  'a event_item -> (Gwdb.iper * Def.witness_kind * Gwdb.istr) array

val get_spouse_iper : 'a event_item -> Gwdb.iper option
val has_witnesses : 'a event_item -> bool
val has_witness_note : 'a event_item -> bool
val pevent_name : 'a Def.gen_pers_event_name -> 'a event_name
val fevent_name : 'a Def.gen_fam_event_name -> 'a event_name
val event_item_of_pevent : Gwdb.pers_event -> Gwdb.istr event_item

val event_item_of_fevent :
  sp:Gwdb.iper option -> Gwdb.fam_event -> Gwdb.istr event_item

val event_item_of_gen_pevent :
  (Gwdb.iper, Gwdb.istr) Def.gen_pers_event -> Gwdb.istr event_item

val event_item_of_gen_fevent :
  sp:Gwdb.iper option ->
  (Gwdb.iper, Gwdb.istr) Def.gen_fam_event ->
  Gwdb.istr event_item

val compare_event_name : 'a event_name -> 'a event_name -> int

val sort_events :
  ('a -> 'b event_name) -> ('a -> Adef.cdate) -> 'a list -> 'a list
(** Sort events (both personal and familial) by their date and their name *)

val events :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.istr event_item list
(** [events conf base p] is the list of [p]'s events *)

val sorted_events :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.istr event_item list
(** [sorted_events conf base p] is the list of [p]'s events, sorted by Checkitem.sorted_events *)

val other_events :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.istr event_item list
(** [other_events conf base p] is the list of [p]'s other events
    (i.e. all [p]'s events except birth, , baptism, death, burial and
    marriage).  **)

type ('string, 'person) witness = private {
  person : 'person;
  kind : Def.witness_kind;
  note : 'string;
}

type ('string, 'person) union = private {
  kind : Def.relation_kind;
  date : Adef.cdate;
  place : 'string;
  note : 'string;
  source : 'string;
  witnesses : ('string, 'person) witness array;
}

type ('string, 'person) main_family_events = private {
  main_union : ('string, 'person) union option;
  main_separation : Def.divorce option;
}

val get_main_family_events :
  ('person, 'string) Def.gen_fam_event list ->
  ('string, 'person) main_family_events
