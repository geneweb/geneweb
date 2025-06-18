(* NOTE : checkItem defined this as 'string event_same, instead of just istr
   .. why make it complicated *)
type 'a event_name =
  | Pevent of 'a Def.gen_pers_event_name
  | Fevent of 'a Def.gen_fam_event_name

type 'a event_item =
  'a event_name
  * Def.cdate
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr
  * Geneweb_db.Driver.istr
  * (Geneweb_db.Driver.iper * Def.witness_kind) array
  * Geneweb_db.Driver.iper option
(** a representation of events *)

val compare_event_name : 'a event_name -> 'a event_name -> int

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
(** [sorted_events conf base p] is the list of [p]'s events, sorted by
    Checkitem.sorted_events *)
