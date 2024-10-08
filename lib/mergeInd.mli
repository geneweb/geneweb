exception Error_loop of Gwdb.person
exception Same_person
exception Different_sexes of Gwdb.person * Gwdb.person

(* TODOOCP *)
val reparent_ind :
  Gwdb.base ->
  (CheckItem.base_warning -> unit) ->
  Gwdb.iper ->
  Gwdb.iper ->
  unit

val merge :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Gwdb.person ->
  (Config.config ->
  Gwdb.base ->
  (Gwdb.iper * Gwdb.iper) list ->
  Gwdb.person ->
  Gwdb.person ->
  unit) ->
  (Config.config ->
  Gwdb.base ->
  (Gwdb.iper * Gwdb.iper) list ->
  Gwdb.ifam * Gwdb.family ->
  Gwdb.ifam * Gwdb.family ->
  Gwdb.person ->
  Gwdb.person ->
  unit) ->
  bool * CheckItem.base_warning list

val kill_ancestors :
  Config.config ->
  Gwdb.base ->
  bool ->
  Gwdb.person ->
  int ref ->
  int ref ->
  unit

val is_ancestor : Gwdb.base -> Gwdb.person -> Gwdb.person -> bool
(** [is_ancestor base p1 p2]
    Checks if [p1] is an ancestor of [p2].
    Raises [Same_person] if [p1] and [p2] have the same iper.
 *)
