type wiki_link =
  | WLpage of int * (string list * string) * string * string * string
  | WLperson of int * Def.NLDB.key * string * string option
  | WLwizard of int * string * string
  | WLnone

val char_dir_sep : char
val check_file_name : string -> (string list * string) option
val misc_notes_link : string -> int -> wiki_link

val add_in_db :
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.t ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.t

val update_db :
  Gwdb.base ->
  (Gwdb.iper, Gwdb.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  unit
