(* NOTE : checkItem defined this as 'string event_same, instead of just istr
   .. why make it complicated *)
type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

type 'a event_item =
  'a event_name
  * Adef.cdate
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr
  * (Geneweb_db.Driver.iper * Def.witness_kind * Geneweb_db.Driver.istr) array
  * Geneweb_db.Driver.iper option
(** a representation of events *)

val compare_event_name : 'a event_name -> 'a event_name -> int

val compare : ('a -> 'b event_name) -> ('a -> Adef.cdate) -> 'a -> 'a -> int
(** [compare get_name get_date e1 e2] is the order of {!sort_events}: by date
    when both events have one and their periods differ, otherwise by name.
    Birth, baptism, death, funeral, cremation and burial keep this order among
    themselves; a family event comes after a birth and before a death, a
    funeral, a cremation or a burial; any other pair is not ordered ([0]), so
    that a personal event may come before the birth or after the burial. *)

val pevent_key : Geneweb_db.Driver.pers_event -> string
(** [pevent_key e] identifies the personal event [e] by a digest of its content.
*)

val fevent_key : Geneweb_db.Driver.fam_event -> string
(** Same as {!pevent_key} for a family event. *)

val sort_events :
  ('a -> 'b event_name) -> ('a -> Adef.cdate) -> 'a list -> 'a list
(** Sort events (both personal and familial) by their date and their name *)

val events :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.istr event_item list
(** [events conf base p] is the list of [p]'s events *)

val sorted_events :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.istr event_item list
(** [sorted_events conf base p] is the list of [p]'s events sorted by
    {!compare}, an undated event being sorted with the date of the event before
    it in its own list (the personal events, or the events of one family), so
    that it keeps its stored place among dated events. *)
