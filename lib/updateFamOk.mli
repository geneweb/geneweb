val reconstitute_from_fevents :
  bool ->
  'string ->
  ('person, 'string) Def.gen_fam_event list ->
  (Def.relation_kind * Def.cdate * 'string * 'string * 'string)
  * Def.divorce
  * ('person * Def.witness_kind) array
(** [reconstitute_from_fevents nsck empty_string family_events]
    Iterate over family's events and returns a tuple with:

    - marriage information (relation kind, date, place, notes, source);
    - divorce information;
    - marriage witnesses;

    Boolean `nsck' is true if no check have been made on the married
    persons sex.
 *)

val effective_mod :
  Config.config ->
  Gwdb.base ->
  bool ->
  (Update.key, Gwdb.ifam, string) Def.gen_family ->
  Update.key Def.gen_couple ->
  Update.key Def.gen_descend ->
  Gwdb.ifam
  * (Gwdb.iper, Gwdb.ifam, Gwdb.istr) Def.gen_family
  * Gwdb.iper Def.gen_couple
  * Gwdb.iper Def.gen_descend

val effective_del :
  Config.config -> Gwdb.base -> Gwdb.iper -> Gwdb.family -> unit
(** Removes a family from the base *)

val all_checks_family :
  Config.config ->
  Gwdb.base ->
  Gwdb.ifam ->
  (Gwdb.iper, Gwdb.ifam, Gwdb.istr) Def.gen_family ->
  Gwdb.iper Def.gen_couple ->
  Gwdb.iper Def.gen_descend ->
  Update.key Def.gen_couple
  * Update.key Def.gen_descend
  * (('i array * 'j array) * ('i array * 'j array)) option ->
  CheckItem.base_warning list * CheckItem.base_misc list

val print_family :
  Config.config ->
  Gwdb.base ->
  CheckItem.base_warning list * CheckItem.base_misc list ->
  Gwdb.iper Adef.gen_couple ->
  Gwdb.iper Def.gen_descend ->
  unit
(** Displays a family page in HTML after an update.
    Used by MergeFamOk *)

val print_del : Config.config -> Gwdb.base -> unit
(** Deletes a family and displays a page confirming its deletion *)

val print_add : Config.config -> Gwdb.base -> unit
(** Displays the page after validating the addition of a family in the base *)

val print_add_parents : Config.config -> Gwdb.base -> unit

val print_mod_aux :
  Config.config ->
  Gwdb.base ->
  (( string * string * int * Update.create * string,
     Gwdb.ifam,
     string )
   Def.gen_family ->
  (string * string * int * Update.create * string) Def.gen_couple ->
  (string * string * int * Update.create * string) Def.gen_descend ->
  unit) ->
  unit

val print_mod : Config.config -> Gwdb.base -> unit

val print_inv : Config.config -> Gwdb.base -> unit
(** Reverses families *)

val print_change_order_ok : Config.config -> Gwdb.base -> unit
(** Changes the family order for a person *)

val print_change_event_order : Config.config -> Gwdb.base -> unit
(** Changes the evenements order for a family *)

val check_family :
  Config.config ->
  (string * string * _ * _ * _, _, _) Def.gen_family ->
  (string * string * _ * _ * _) Def.gen_couple ->
  Update.update_error option * Update.update_error option
(** [check_family conf fam cpl]
    Checks that no name is missing.
*)

val strip_family :
  (string * 'a * 'b * 'c * 'd, 'e, string) Def.gen_family ->
  (string * 'f * 'g * 'h * 'i) Def.gen_descend ->
  (string * 'a * 'b * 'c * 'd, 'e, string) Def.gen_family
  * (string * 'f * 'g * 'h * 'i) Def.gen_descend
(** [strip_family fam des]
    Removes empty person from witness list and empty events from event list. *)

val effective_add :
  Config.config ->
  Gwdb.base ->
  bool ->
  (Update.key, 'a, string) Def.gen_family ->
  Update.key Def.gen_couple ->
  Update.key Def.gen_descend ->
  Gwdb_driver.ifam
  * (Gwdb.iper, Gwdb.ifam, Gwdb.istr) Def.gen_family
  * Gwdb.iper Def.gen_couple
  * Gwdb.iper Def.gen_descend
(** [effective_add conf base nsck sfam scpl sdes]
    Patch base without commiting changes. *)

val patch_person_with_pevents : Gwdb.base -> Gwdb.iper -> unit
(** [patch_parent_with_pevents base cpl]
    If parents have information about birth or death but no pevent,
    add a pevent with these info *)

val patch_parent_with_pevents : Gwdb.base -> Gwdb.iper Adef.gen_couple -> unit
(** See patch_person_with_pevents  *)

val patch_children_with_pevents : Gwdb.base -> Gwdb.iper Def.gen_descend -> unit
(** See patch_person_with_pevents  *)

val family_structure :
  Gwdb.base -> Gwdb.ifam -> Gwdb.iper array * Gwdb.iper array
(** [family_structure base ifam]
    Returns (parents, children) *)
