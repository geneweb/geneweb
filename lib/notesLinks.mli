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

val advances_pos : wiki_link -> bool
(** Whether a link takes a slot in the [#p_N] anchor numbering shared by
    [Wiki.syntax_links] and nldb's [lnPos]. *)

val fold_links :
  (pos:int -> wiki_link -> 'acc -> 'acc) -> 'acc -> string -> 'acc
(** [fold_links f acc s] folds [f] over the links of [s] in order, with the same
    escapes as [Wiki.syntax_links] (a percent sign followed by a bracket, a
    brace or a quote). [pos] is the 1-based ordinal among occurrences counted by
    [advances_pos], i.e. the [N] of the rendered [#p_N] anchor. *)

val add_in_db :
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.t ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.t

val update_db :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, Geneweb_db.Driver.ifam) Def.NLDB.page ->
  string list * (Def.NLDB.key * Def.NLDB.ind) list ->
  (string list * (Def.NLDB.key * Def.NLDB.ind) list) option
(** [update_db base who list] replaces [who]'s nldb entry with [list] and
    returns its previous entry, if any. *)
