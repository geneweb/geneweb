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
  Config.config ->
  Gwdb.base ->
  Gwdb.iper option dag_ind list ->
  Gwdb.iper option dag_ind list

val dag_fam_list_of_ind_list : 'a dag_ind list -> 'a dag_fam list

val add_phony_children :
  'a option dag_ind list -> 'a option dag_fam list -> 'a option dag_ind list

val ind_set_of_relation_path :
  Gwdb.base -> (Gwdb.iper * famlink) list -> Gwdb.iper list

val excl_faml : Config.config -> Gwdb.base -> Gwdb.ifam list

val get_shortest_path_relation :
  Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  Gwdb.iper ->
  Gwdb.ifam list ->
  ((Gwdb.iper * famlink) list * Gwdb.ifam) option

val nb_fields : string -> int

val get_piece_of_branch :
  Config.config ->
  Gwdb.base ->
  (((Gwdb.iper, 'a) Gwdb.Marker.t * (Gwdb.person * 'b) list) * int)
  * ('a -> (int * 'c * Gwdb.iper list) list) ->
  int * int ->
  Gwdb.iper list

val compute_relationship :
  Config.config ->
  Gwdb.base ->
  bool ->
  Gwdb.person ->
  Gwdb.person ->
  ((Gwdb.person option
   * Gwdb.person option
   * (int * int * (Gwdb.person * int) list)
   * (Gwdb.iper, Consang.relationship) Gwdb.Marker.t)
   list
  * Sosa.t
  * float)
  option

val simplify_path :
  Gwdb.base -> (Gwdb.iper * famlink) list -> (Gwdb.iper * famlink) list

(* ----------- *)
(* TODO put them in perso.ml? *)

val get_others_related :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.person * Gwdb.relation) list

val get_event_witnessed :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.person * Def.witness_kind * string * Gwdb.istr Event.event_item) list
(** [get_event_witness_related conf base p] is a list of tuple with informations
    on events the person participated to. ( person that has the event *
    witness_kind * witness_note * event_item ) *)
