(** Corpus of the person, family and event notes of a base, for
    [m=NOTES_SEARCH].

    Reading every person of a large base on each search request is too slow
    (Roglo: 11 million persons), so the non-empty notes are extracted once, by
    [cache_files -notes] (typically run nightly), into
    [etc/<base>/cache/<base>_notes_search.cache]. The corpus is only used to
    find candidates: each hit is re-read from the base, checked for access and
    matched again on its current text before being shown.

    Changes made after the build are followed through the base's
    [synchro_patches]: the persons and families modified since then are read
    live instead of from the corpus (see {!changes}). A rebuilt base (new [gwc]
    run) makes the corpus [Outdated]. *)

type kind =
  | Person  (** person notes *)
  | Family  (** family notes (comment) *)
  | Person_event  (** note of a person event *)
  | Family_event  (** note of a family event *)

type entry = {
  kind : kind;
  id : string;  (** [Iper.to_string] or [Ifam.to_string], following [kind] *)
  evt : int;
      (** index of the event in [get_pevents] / [get_fevents]; [-1] for [Person]
          and [Family] *)
  text : string;  (** {!NotesSearch.plain_text} of the note *)
}

type t = {
  fingerprint : string;  (** of the base at build time *)
  built : float;  (** build time, [Unix.time] *)
  synchro_len : int;  (** number of [synchro_patches] commits at build time *)
}
(** Header of a corpus file. The entries are not kept in memory: they are
    streamed by {!fold_entries}. *)

val person_entries :
  Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> entry list
(** Entries of a person: its notes and the notes of its events, with no access
    check. *)

val family_entries :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.ifam ->
  Geneweb_db.Driver.family ->
  entry list
(** Entries of a family: its notes (comment) and the notes of its events, with
    no access check. *)

val build : ?progress:(int -> int -> unit) -> Geneweb_db.Driver.base -> int
(** [build base] reads every person and family of [base] and writes the corpus
    file as it goes (see {!write}); it returns the number of entries.
    [progress i n] is called as the work advances, [n] being the number of
    persons plus families. *)

val file : string -> string
(** [file bname] is the corpus file of the base [bname] (without [.gwb]). *)

val write : string -> entry list -> unit
(** [write bname entries] writes the corpus file of [bname] with [entries],
    their text being stored {!NotesSearch.fold}ed. The file is written to a
    temporary name and renamed when complete, so that a running gwd never reads
    a partial file. *)

type status =
  | Missing  (** no corpus file, or unreadable *)
  | Outdated
      (** built for another version of the base (new gwc run), or in an older
          file format *)
  | Available of t

val read : string -> status
(** [read bname] reads and checks the corpus of the base [bname]. *)

val fold_entries : string -> (kind -> string -> string -> 'a -> 'a) -> 'a -> 'a
(** [fold_entries bname f acc] reads the corpus file of [bname] line by line,
    calling [f kind id folded_text] on each entry. *)

val changes :
  string -> t -> Geneweb_db.Driver.Iper.Set.t * Geneweb_db.Driver.Ifam.Set.t
(** [changes bname corpus] is the set of persons and of families modified since
    [corpus] was built, from the base's [synchro_patches]. *)
