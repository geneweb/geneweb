(* Type pour ne pas créer à chaque fois un tableau tstab et mark *)
type sosa_t = {
  tstab : (Geneweb_db.Driver.iper, int) Geneweb_db.Collection.Marker.t;
  mark : (Geneweb_db.Driver.iper, bool) Geneweb_db.Collection.Marker.t;
  mutable last_zil : (Geneweb_db.Driver.iper * Geneweb_sosa.t) list;
  sosa_ht :
    ( Geneweb_db.Driver.iper,
      (Geneweb_sosa.t * Geneweb_db.Driver.person) option )
    Hashtbl.t;
}

val build_sosa_tree_ht :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** Construts from the given person sosa table strored in the cache. Sosa table
    contains association {i person_id -> sosa number} for each person in the
    base. Person has sosa [Sosa.one] and his ancestors have sosa > [Sosa.one].
    For non ancestor person sosa number is set to [Sosa.zero]. If multiple sosa
    (implex) only smalest sosa is stored *)

val init_sosa_t :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  sosa_t option
(** Create sosa hash table for person if sosa_ref exists *)

val build_sosa_ht : Config.config -> Geneweb_db.Driver.base -> unit
(** Extract referenced person from environement and constructs for him sosa
    table wiht [build_sosa_tree_ht]. *)

val get_sosa_person : Geneweb_db.Driver.person -> Geneweb_sosa.t
(** get sosa value for a person. returns Sosa.zero if none assumes that
    init_sosa_t has been executed *)

val get_single_sosa :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_sosa.t
(** get sosa value for a person. returns Sosa.zero if none calls init_sosa_t if
    needed *)

val print_sosa :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool (* print link if true *) ->
  unit
(** Prints the sosa pictogram and a link to display relationship with sosa 1 if
    requested *)

val find_sosa :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person option (* sosa_ref *) ->
  sosa_t (* sosa hash table *) ->
  (Geneweb_sosa.t * Geneweb_db.Driver.person) option
(** TODO Needs better documentation!! Seems to check if person has a sosa number
    relative to sosa_ref *)

val next_sosa : Geneweb_sosa.t -> Geneweb_sosa.t * Geneweb_db.Driver.iper
(** Get next sosa; returns Sosa.zero if none *)

val prev_sosa : Geneweb_sosa.t -> Geneweb_sosa.t * Geneweb_db.Driver.iper
(** Get previous sosa; returns Sosa.zero if none *)
