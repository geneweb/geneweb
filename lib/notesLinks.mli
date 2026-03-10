type wiki_link =
  | WLpage of int * (string list * string) * string * string * string
  | WLperson of int * Def.NLDB.key * string option * string option * int option
  | WLwizard of int * string * string
  | WLimage of int * (string list * string) * string * string option
      (** [WLimage (end_pos, fpath, alt, width_opt)] inline image from the image
          directory and it's subdirectories. [fpath] is the validated path (same
          format as WLpage), [alt] is the alt text (may be empty), [width_opt]
          is an optional CSS width value (e.g. ["200px"]). Syntax:
          {v [[image:photo.jpg]] v}
          {v [[image:subdir:photo.jpg/alt text]] v}
          {v [[image:subdir:photo.jpg/alt text/200px]] v} *)
  | WLnone of int * string

val char_dir_sep : char
val dir_sep : string
val check_file_name : string -> (string list * string) option
val misc_notes_link : string -> int -> wiki_link

val add_in_db :
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.t ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.t

val update_db :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  unit
