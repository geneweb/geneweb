(* TODOOCP *)
val print_merge :
  ?continue:
    (Config.config ->
    Gwdb.base ->
    (Gwdb.iper, Update.key, string) Def.gen_person ->
    string ->
    unit) ->
  Config.config ->
  Gwdb.base ->
  unit

val print_mod_merge :
  ?person:(Gwdb.iper, Update.key, string) Def.gen_person ->
  Config.config ->
  Gwdb.base ->
  unit
