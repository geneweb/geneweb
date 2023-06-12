open Config
open Gwdb

(* Relation path computations *)

(* find shortest path :
 * parents, siblings, mates and children are at distance 1.
 *)
type famlink = Self | Parent | Sibling | HalfSibling | Mate | Child

type 'a dag_ind = {
  di_val : 'a;
  mutable di_famc : 'a dag_fam;
  mutable di_fams : 'a dag_fam;
}

and 'a dag_fam = {
  mutable df_pare : 'a dag_ind list;
  df_chil : 'a dag_ind list;
}

val dag_ind_list_of_path : ('a * famlink) list -> 'a option dag_ind list

val add_missing_parents_of_siblings :
  config -> base -> iper option dag_ind list -> iper option dag_ind list

val dag_fam_list_of_ind_list : 'a dag_ind list -> 'a dag_fam list

val add_phony_children :
  'a option dag_ind list -> 'a option dag_fam list -> 'a option dag_ind list

val ind_set_of_relation_path : base -> (iper * famlink) list -> iper list
val excl_faml : config -> base -> ifam list

val get_shortest_path_relation :
  config ->
  base ->
  iper ->
  iper ->
  ifam list ->
  ((iper * famlink) list * ifam) option

val nb_fields : string -> int

val get_piece_of_branch :
  config ->
  base ->
  (((iper, 'a) Marker.t * (person * 'b) list) * int)
  * ('a -> (int * 'c * iper list) list) ->
  int * int ->
  iper list

val compute_relationship :
  config ->
  base ->
  bool ->
  person ->
  person ->
  ((person option
   * person option
   * (int * int * (person * int) list)
   * (iper, Consang.relationship) Marker.t)
   list
  * Sosa.t
  * float)
  option

val simplify_path :
  Gwdb_driver.base ->
  (Gwdb_driver.iper * famlink) list ->
  (Gwdb_driver.iper * famlink) list

(* ----------- *)
(* TODO put them in perso.ml? *)

val get_related_parents : config -> base -> person -> (person * relation) list

val get_event_witnessed :
  config ->
  base ->
  person ->
  (person * Def.witness_kind * string * istr Event.event_item) list
(** [get_event_witness_related conf base p] is a list of tuple
      with informations on events the person participated to.
      ( person that has the event * witness_kind * witness_note * event_item )
  *)
