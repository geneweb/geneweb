
(* TODOOCP *)
val reparent_ind :
  Gwdb.base ->
  (CheckItem.base_warning -> unit) -> Gwdb.iper -> Gwdb.iper -> unit
exception Error_loop of Gwdb.person
exception Same_person
exception Different_sexes of Gwdb.person * Gwdb.person
val merge :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Gwdb.person ->
  (Config.config ->
   Gwdb.base ->
   (Gwdb.iper * Gwdb.iper) list -> Gwdb.person -> Gwdb.person -> 'a) ->
  (Config.config ->
   Gwdb.base ->
   (Gwdb.iper * Gwdb.iper) list ->
   Gwdb.ifam * Gwdb.family ->
   Gwdb.ifam * Gwdb.family -> Gwdb.person -> Gwdb.person -> 'b) ->
  bool * CheckItem.base_warning list
val kill_ancestors :
  Config.config ->
  Gwdb.base -> bool -> Gwdb.person -> int ref -> int ref -> unit