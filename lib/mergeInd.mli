exception Error_loop of Geneweb_db.Driver.person
exception Same_person
exception Different_sexes of Geneweb_db.Driver.person * Geneweb_db.Driver.person

(* TODOOCP *)
val reparent_ind :
  Geneweb_db.Driver.base ->
  (CheckItem.base_warning -> unit) ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper ->
  unit

val merge :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  (Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper) list ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  unit) ->
  (Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper) list ->
  Geneweb_db.Driver.ifam * Geneweb_db.Driver.family ->
  Geneweb_db.Driver.ifam * Geneweb_db.Driver.family ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  unit) ->
  bool * CheckItem.base_warning list

val kill_ancestors :
  Config.config ->
  Geneweb_db.Driver.base ->
  bool ->
  Geneweb_db.Driver.person ->
  int ref ->
  int ref ->
  unit

val is_ancestor :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  bool
(** [is_ancestor base p1 p2] Checks if [p1] is an ancestor of [p2]. Raises
    [Same_person] if [p1] and [p2] have the same iper. *)
