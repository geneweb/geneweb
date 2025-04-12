val init_cache :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  int ->
  int ->
  int ->
  unit)
  ref
(** [init_cache conf base ip nb_asc from_gen_desc nb_desc] *)

val max_ancestor_level :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  string ->
  int ->
  int ->
  int)
  ref

val max_descendant_level :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  int ->
  int)
  ref

val tree_generation_list :
  (Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.person * Geneweb_db.Driver.ifam * string) option
  * (Geneweb_db.Driver.person * Geneweb_db.Driver.ifam * string) option)
  ref

val get_father :
  (Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.iper ->
  ((Geneweb_db.Driver.person * bool) * string) option)
  ref

val get_mother :
  (Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.iper ->
  ((Geneweb_db.Driver.person * bool) * string) option)
  ref

val get_person :
  (Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.iper ->
  ((Geneweb_db.Driver.person * bool) * string) option)
  ref

val get_father' :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (string
  * (Geneweb_db.Driver.person * bool)
  * Geneweb_db.Driver.ifam
  * Geneweb_db.Driver.family
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * Geneweb_db.Driver.iper))
  option)
  ref

val get_mother' :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  (string
  * (Geneweb_db.Driver.person * bool)
  * Geneweb_db.Driver.ifam
  * Geneweb_db.Driver.family
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * Geneweb_db.Driver.iper))
  option)
  ref

val get_family :
  (Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.ifam ->
  (Geneweb_db.Driver.family
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * Geneweb_db.Driver.iper)
  * bool)
  option)
  ref

val get_families :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  (Geneweb_db.Driver.ifam
  * Geneweb_db.Driver.family
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * Geneweb_db.Driver.person)
  * string
  * bool)
  list)
  ref

val get_children_of_parents :
  (Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper ->
  (Geneweb_db.Driver.person * string) list)
  ref

val get_children :
  (Geneweb_db.Driver.base ->
  string ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper ->
  ((Geneweb_db.Driver.person * bool) * string) list)
  ref

val get_children' :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.family ->
  Geneweb_db.Driver.iper ->
  (string
  * (Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * Geneweb_db.Driver.iper)
  * ((Geneweb_db.Driver.person * bool) * string * bool) list)
  list)
  ref

val has_children :
  (Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  Geneweb_db.Driver.family ->
  bool)
  ref

val has_family_correspondance : (string -> Geneweb_db.Driver.iper -> bool) ref
val has_parents_link : (string -> Geneweb_db.Driver.iper -> bool) ref
val has_siblings : (string -> Geneweb_db.Driver.iper -> bool) ref
val nb_children : (string -> Geneweb_db.Driver.ifam -> int) ref
val nb_families : (string -> Geneweb_db.Driver.iper -> int) ref
