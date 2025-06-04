(* TODOOCP *)

val merge_carrousel :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  ( Geneweb_db.Driver.iper,
    string * string * int * Update.create * string,
    string )
  Def.gen_person ->
  unit
(** merge person 1 and 2 carrousel into third *)

val reconstitute :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.person ->
  ( Geneweb_db.Driver.iper,
    string * string * int * Update.create * string,
    string )
  Def.gen_person

val effective_mod_merge :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) Def.gen_person ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.iper, string) Def.gen_person ->
  (Geneweb_db.Driver.iper, Update.key, string) Def.gen_person ->
  (Config.config ->
  Geneweb_db.Driver.base ->
  CheckItem.base_warning list ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  Def.gen_person ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  string ->
  string ->
  int ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  string ->
  string ->
  int ->
  'a) ->
  'a
