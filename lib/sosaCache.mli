open Gwdb

(* Type pour ne pas créer à chaque fois un tableau tstab et mark *)
type sosa_t = {
  tstab : (Gwdb.iper, int) Gwdb.Marker.t;
  mark : (Gwdb.iper, bool) Gwdb.Marker.t;
  mutable last_zil : (Gwdb.iper * Sosa.t) list;
  sosa_ht : (Gwdb.iper, (Sosa.t * Gwdb.person) option) Hashtbl.t;
}

val build_sosa_tree_ht : Config.config -> base -> person -> unit
(** Construts from the given person sosa table strored in the cache. Sosa table contains association
    {i person_id -> sosa number} for each person in the base.
    Person has sosa [Sosa.one] and his ancestors have sosa > [Sosa.one].
    For non ancestor person sosa number is set to [Sosa.zero]. 
    If multiple sosa (implex) only smalest sosa is stored 
    *)

val init_sosa_t : Config.config -> base -> person -> sosa_t option
(** Create sosa hash table for person if sosa_ref exists *)

val build_sosa_ht : Config.config -> base -> unit
(** Extract referenced person from environement and constructs for him sosa table wiht [build_sosa_tree_ht]. *)

val get_sosa_person : person -> Sosa.t
(** get sosa value for a person. returns Sosa.zero if none
    assumes that init_sosa_t has been executed 
    *)

val get_single_sosa : Config.config -> base -> person -> Sosa.t
(** get sosa value for a person. returns Sosa.zero if none
    calls init_sosa_t if needed
    *)

val print_sosa :
  Config.config -> base -> person -> bool (* print link if true *) -> unit
(** Prints the sosa pictogram
    and a link to display relationship with sosa 1 if requested
    *)

val find_sosa :
  Config.config ->
  base ->
  person ->
  person option (* sosa_ref *) ->
  sosa_t (* sosa hash table *) ->
  (Sosa.t * person) option
(** TODO Needs better documentation!!
    Seems to check if person has a sosa number relative to sosa_ref 
    *)

val next_sosa : Sosa.t -> Sosa.t * Gwdb.iper
(** Get next sosa; returns Sosa.zero if none *)

val prev_sosa : Sosa.t -> Sosa.t * Gwdb.iper
(** Get previous sosa; returns Sosa.zero if none *)
