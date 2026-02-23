val effective_del_no_commit :
  Gwdb.base -> (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person -> unit
(** Removes a person from the base *)

val effective_del_commit :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  unit
(** Adds to the diff the deletion of a person *)

val effective_del : Config.config -> Gwdb.base -> Gwdb.person -> unit
(** [effective_del] applies [effective_del_no_commit] and [effective_del_commit]
*)

val effective_mod :
  ?prerr:(Config.config -> Gwdb.base -> Update.update_error -> unit) ->
  ?skip_conflict:Gwdb.iper ->
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Update.key, string) Def.gen_person ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person
(** [effective_mod prerr ?skip_conflict conf base sp] *)

val print_mod :
  ?prerr:(Config.config -> Gwdb.base -> Update.update_error -> unit) ->
  Config.config ->
  Gwdb.base ->
  unit
(** Tries to modifies a person and displays a success page if successful *)

val all_checks_person :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  Gwdb.ifam Def.gen_ascend ->
  Gwdb.ifam Def.gen_union ->
  Warning.base_warning list
(** Patches the informations of a person by checking the order of events: for
    example, a birth should happen before the death of a mother. *)

val print_mod_aux :
  ?check_person_f:
    (Config.config ->
    Gwdb.base ->
    (Gwdb.iper, Update.key, string) Def.gen_person ->
    Update.update_error option) ->
  Config.config ->
  Gwdb.base ->
  ((Gwdb.iper, Update.key, string) Def.gen_person -> unit) ->
  unit

val print_add : Config.config -> Gwdb.base -> unit
(** Tries to add a person to the base and displays a success HTML page if
    successful *)

val print_del : Config.config -> Gwdb.base -> unit
(** Tries to remove a person from the base and displays a success HTML page if
    successful *)

val print_change_event_order : Config.config -> Gwdb.base -> unit
(** Tries to change the order of events for a person and displays a success HTML
    page if successful *)

(* Ajout pour l'API *)
val effective_add :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Update.key, string) Def.gen_person ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person * Gwdb.ifam Def.gen_ascend

val strip_person :
  (Gwdb.iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person ->
  (Gwdb.iper, string * 'a * 'b * 'c * 'd, string) Def.gen_person

val check_person :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, string * string * 'b * 'c * 'd, string) Def.gen_person ->
  Update.update_error option

val error_person : Config.config -> Update.update_error -> unit

val update_relations_of_related :
  Gwdb.base -> Gwdb.iper -> Gwdb.iper list -> unit

val reconstitute_from_pevents :
  ('a, string) Def.gen_pers_event list ->
  Def.death * string * string * string ->
  Def.burial * string * string * string ->
  (Def.cdate * string * string * string)
  * (Def.cdate * string * string * string)
  * (Def.death * string * string * string)
  * (Def.burial * string * string * string)
  * ('a, string) Def.gen_pers_event list
