open Config
open Def
open Gwdb

val effective_del_no_commit : base -> (iper, iper, string) gen_person -> unit
(** Removes a person from the base *)

val effective_del_commit :
  config -> base -> (iper, iper, string) gen_person -> unit
(** Adds to the diff the deletion of a person *)

val effective_del : config -> base -> person -> unit
(** [effective_del] applies [effective_del_no_commit] and [effective_del_commit] *)

val effective_mod :
  ?prerr:(config -> base -> Update.update_error -> unit) ->
  ?skip_conflict:iper ->
  config ->
  base ->
  (iper, Update.key, string) gen_person ->
  (iper, iper, istr) gen_person
(** [effective_mod prerr ?skip_conflict conf base sp] *)

val print_mod :
  ?prerr:(config -> base -> Update.update_error -> unit) ->
  config ->
  base ->
  unit
(** Tries to modifies a person and displays a success page if successful *)

val all_checks_person :
  base ->
  (iper, iper, istr) gen_person ->
  ifam gen_ascend ->
  ifam gen_union ->
  CheckItem.base_warning list
(** Patches the informations of a person by checking the order of events:
    for example, a birth should happen before the death of a mother. *)

val print_mod_aux :
  config -> base -> ((iper, Update.key, string) gen_person -> unit) -> unit

val print_add : config -> base -> unit
(** Tries to add a person to the base and displays a success HTML page if
    successful *)

val print_del : config -> base -> unit
(** Tries to remove a person from the base and displays a success HTML page if
    successful *)

val print_change_event_order : config -> base -> unit
(** Tries to change the order of events for a person and displays a success HTML
    page if successful *)

(* Ajout pour l'API *)
val effective_add :
  config ->
  base ->
  (iper, Update.key, string) Def.gen_person ->
  (iper, iper, istr) Def.gen_person * ifam Def.gen_ascend

val strip_person :
  (iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person ->
  (iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person

val check_person :
  config ->
  base ->
  (iper, string * string * 'b * 'c * 'd, string) Def.gen_person ->
  Update.update_error option

val error_person : config -> Update.update_error -> unit
val update_relations_of_related : base -> iper -> iper list -> unit

val reconstitute_death :
  config ->
  Def.date option ->
  Def.date option ->
  string ->
  Def.burial ->
  string ->
  Def.death

val reconstitute_from_pevents :
  ('a, string) Def.gen_pers_event list ->
  bool ->
  Def.cdate * string * string * string ->
  Def.cdate * string * string * string ->
  Def.death * string * string * string ->
  Def.burial * string * string * string ->
  (Def.cdate * string * string * string)
  * (Def.cdate * string * string * string)
  * (Def.death * string * string * string)
  * (Def.burial * string * string * string)
  * ('a, string) Def.gen_pers_event list
