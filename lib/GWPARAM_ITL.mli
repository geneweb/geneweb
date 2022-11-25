val init_cache :
  (Config.config -> Gwdb.base -> Gwdb.iper -> int -> int -> int -> unit) ref
(** [init_cache conf base ip nb_asc from_gen_desc nb_desc] *)

val max_ancestor_level :
  (Config.config -> Gwdb.base -> Gwdb.iper -> string -> int -> int -> int) ref

val max_descendant_level :
  (Config.config -> Gwdb.base -> Gwdb.iper -> int -> int) ref

val tree_generation_list :
  (Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.person ->
  (Gwdb.person * Gwdb.ifam * string) option
  * (Gwdb.person * Gwdb.ifam * string) option)
  ref

val get_father :
  (Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.iper ->
  ((Gwdb.person * bool) * string) option)
  ref

val get_mother :
  (Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.iper ->
  ((Gwdb.person * bool) * string) option)
  ref

val get_person :
  (Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.iper ->
  ((Gwdb.person * bool) * string) option)
  ref

val get_father' :
  (Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  (string
  * (Gwdb.person * bool)
  * Gwdb.ifam
  * Gwdb.family
  * (Gwdb.iper * Gwdb.iper * Gwdb.iper))
  option)
  ref

val get_mother' :
  (Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  (string
  * (Gwdb.person * bool)
  * Gwdb.ifam
  * Gwdb.family
  * (Gwdb.iper * Gwdb.iper * Gwdb.iper))
  option)
  ref

val get_family :
  (Config.config ->
  Gwdb.base ->
  string ->
  Gwdb.person ->
  Gwdb.ifam ->
  (Gwdb.family * (Gwdb.iper * Gwdb.iper * Gwdb.iper) * bool) option)
  ref

val get_families :
  (Config.config ->
  Gwdb.base ->
  Gwdb.person ->
  (Gwdb.ifam
  * Gwdb.family
  * (Gwdb.iper * Gwdb.iper * Gwdb.person)
  * string
  * bool)
  list)
  ref

val get_children_of_parents :
  (Gwdb.base ->
  string ->
  Gwdb.ifam ->
  Gwdb.iper ->
  Gwdb.iper ->
  (Gwdb.person * string) list)
  ref

val get_children :
  (Gwdb.base ->
  string ->
  Gwdb.ifam ->
  Gwdb.iper ->
  Gwdb.iper ->
  ((Gwdb.person * bool) * string) list)
  ref

val get_children' :
  (Config.config ->
  Gwdb.base ->
  Gwdb.iper ->
  Gwdb.family ->
  Gwdb.iper ->
  (string
  * (Gwdb.iper * Gwdb.iper * Gwdb.iper)
  * ((Gwdb.person * bool) * string * bool) list)
  list)
  ref

val has_children :
  (Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.family -> bool) ref

val has_family_correspondance : (string -> Gwdb.iper -> bool) ref
val has_parents_link : (string -> Gwdb.iper -> bool) ref
val has_siblings : (string -> Gwdb.iper -> bool) ref
val nb_children : (string -> Gwdb.ifam -> int) ref
val nb_families : (string -> Gwdb.iper -> int) ref
