val path_of_fnotes : string -> string
val commit_notes : Config.config -> Gwdb.base -> string -> string -> unit

val notes_links_db :
  Config.config ->
  Gwdb.base ->
  bool ->
  (Mutil.StrSet.elt * (Gwdb.iper, Gwdb.ifam) Def.NLDB.page list) list

val update_notes_links_db :
  Gwdb.base -> (Gwdb.iper, Gwdb.ifam) Def.NLDB.page -> string -> unit

val file_path : Config.config -> Gwdb.base -> string -> string
val read_notes : Gwdb.base -> string -> (string * string) list * string

val merge_possible_aliases :
  Config.config ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list ->
  (('a, 'b) Def.NLDB.page * (string list * 'c list)) list

val source : Config.config -> Gwdb.base -> string -> Adef.safe_string
(** [source conf base str]
    Interprets wiki syntax in a "source" context:
    - supposed to be one line
    - no <p> surrounding tag
*)

val note :
  Config.config ->
  Gwdb.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [note conf base env str]
    Interprets wiki syntax in a "note" context:
    - [env] is available during [str] interpretation
*)

val person_note :
  Config.config -> Gwdb.base -> Gwdb.person -> string -> Adef.safe_string
(** [person_note conf base person str]
    Interprets wiki syntax in a "note" context:
    - env is available during [str] interpretation with [i] variable bound to person image
*)

val source_note :
  Config.config -> Gwdb.base -> Gwdb.person -> string -> Adef.safe_string
(** [source_note conf base person str]
    Interprets wiki syntax in a "source" context:
    - env is available during [str] interpretation with [i] variable bound to person image
*)

val source_note_with_env :
  Config.config ->
  Gwdb.base ->
  (char * (unit -> string)) list ->
  string ->
  Adef.safe_string
(** [source_note_with_env conf base env str]
    Interprets wiki syntax in a "source" context with a predefined env.
*)
