val path_of_fnotes : string -> string

val commit_notes :
  Config.config -> Geneweb_db.Driver.base -> string -> string -> unit

val notes_links_db :
  Config.config ->
  Geneweb_db.Driver.base ->
  bool ->
  (Mutil.StrSet.elt
  * (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list)
  list

val update_notes_links_db :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string ->
  unit

val update_notes_links_person :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, _, Geneweb_db.Driver.istr) Def.gen_person ->
  unit

val update_notes_links_family :
  Geneweb_db.Driver.base ->
  (_, Geneweb_db.Driver.ifam, Geneweb_db.Driver.istr) Def.gen_family ->
  unit

val file_path : Config.config -> Geneweb_db.Driver.base -> string -> string

val read_notes :
  Geneweb_db.Driver.base -> string -> (string * string) list * string

val merge_possible_aliases :
  Config.config ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list

val update_ind_key :
  Config.config ->
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list ->
  Def.NLDB.key ->
  string * string * int ->
  unit

val source :
  Config.config -> Geneweb_db.Driver.base -> string -> Adef.safe_string
(** [source conf base str] Interprets wiki syntax in a "source" context:
    - supposed to be one line
    - no <p> surrounding tag *)

val note :
  Config.config ->
  Geneweb_db.Driver.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [note conf base env str] Interprets wiki syntax in a "note" context:
    - [env] is available during [str] interpretation *)

val person_note :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  Adef.safe_string
(** [person_note conf base person str] Interprets wiki syntax in a "note"
    context:
    - env is available during [str] interpretation with [i] variable bound to
      person image *)

val source_note :
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  string ->
  Adef.safe_string
(** [source_note conf base person str] Interprets wiki syntax in a "source"
    context:
    - env is available during [str] interpretation with [i] variable bound to
      person image *)

val source_note_with_env :
  Config.config ->
  Geneweb_db.Driver.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [source_note_with_env conf base env str] Interprets wiki syntax in a
    "source" context with a predefined env. *)

type mode = Delete | Rename | Merge

val links_to_ind :
  Config.config ->
  Geneweb_db.Driver.base ->
  ((Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page
  * (string list * (Def.NLDB.key * Def.NLDB.ind) list))
  list ->
  Def.NLDB.key ->
  string option ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page list

val links_to_cache_entries :
  Config.config ->
  Geneweb_db.Driver.base ->
  ((Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page
  * (string list * (Def.NLDB.key * Def.NLDB.ind) list))
  list ->
  Def.NLDB.key ->
  (Def.NLDB.key * Def.NLDB.ind) list

val has_linked_pages :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> bool

val linked_pages_nbr :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.iper -> int

val cache_linked_pages_name : string

val update_cache_linked_pages :
  Config.config -> mode -> Def.NLDB.key -> Def.NLDB.key -> int -> unit

val json_extract_img : Config.config -> string -> string * string
val safe_gallery : Config.config -> Geneweb_db.Driver.base -> string -> string
