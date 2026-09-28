(* TODOOCP *)
val print_merge :
  ?continue:
    (Config.config ->
    Gwdb.base ->
    (Update.key, Gwdb.ifam, string) Def.gen_family
    * Update.key Adef.gen_couple
    * Update.key Def.gen_descend ->
    string ->
    unit) ->
  Config.config ->
  Gwdb.base ->
  unit

val print_mod_merge :
  ?family:
    (Update.key, Gwdb.ifam, string) Def.gen_family
    * Update.key Adef.gen_couple
    * Update.key Def.gen_descend ->
  Config.config ->
  Gwdb.base ->
  unit
