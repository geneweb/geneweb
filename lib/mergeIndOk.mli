(* TODOOCP *)
val reconstitute :
  Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  Gwdb.person ->
  ( Gwdb.iper,
    string * string * int * Update.create * string,
    string )
  Def.gen_person

val effective_mod_merge :
  Config.config ->
  Gwdb.base ->
  (Gwdb.iper, Gwdb.iper, string) Def.gen_person ->
  (Gwdb.iper, Gwdb.iper, string) Def.gen_person ->
  (Gwdb.iper, Update.key, string) Def.gen_person ->
  (Config.config ->
  Gwdb.base ->
  CheckItem.base_warning list ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page list ->
  string ->
  string ->
  int ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page list ->
  string ->
  string ->
  int ->
  'a) ->
  'a
