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

val end_pos : wiki_link -> int
(** The end position (the [j] every constructor carries) of a [wiki_link], for
    callers that just want to continue scanning past it. *)

val advances_pos : wiki_link -> bool
(** Whether an occurrence consumes a slot in the shared "pos" counter used both
    to number the #p_N anchors rendered for person links (see Wiki.syntax_links)
    and to record [Def.NLDB.ind.lnPos] when a note is scanned for its outgoing
    links (see Notes.update_notes_links_db and bin/update_nldb). This is the
    ONLY place that should decide this: every caller that numbers occurrences
    must go through this function (or [fold_links] below) rather than
    re-deciding it, or the numbering can silently desync between rendering and
    indexing. *)

val fold_links :
  (pos:int -> i:int -> j:int -> wiki_link -> 'acc -> 'acc) ->
  'acc ->
  string ->
  int ->
  'acc
(** [fold_links f acc s i0] walks [s] from [i0], calling
    [f ~pos ~i ~j link acc] for every occurrence [misc_notes_link] finds
    (skipping "%%"-escaped positions), maintaining [pos] per
    [advances_pos]. [f] sees [pos] *before* any increment for the
    current occurrence, matching what Wiki.syntax_links uses for its
    #p_%d anchors. This is the single scanning loop shared by
    Notes.update_notes_links_db and bin/update_nldb's batch rebuild.
    Wiki.syntax_links itself has extra scanning rules (bold/italic
    markup, '{...}' spans, quotes) that aren't part of link-scanning, so
    it cannot go through this loop - it calls [advances_pos] directly
    instead to stay in sync. *)

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
