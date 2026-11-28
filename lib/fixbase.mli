(** All the function of this module scan the base and fix what is considered as
    corrupted data.

    They all share a same signature :
    [let check_XXX ?report progress base = ...]

    The optionnal [report] function should be used to track changes.

    [progress i max] keep tracks of the progress of a task. When called, task is
    about [i/max] done.

    Note that it does not actually commit the changes, so if you do not want a
    dry run, apply [Geneweb_db.commit_patches] *)

(** All possible patches that could be automatically deducted from inconsistent
    or absent information in the database *)
type patch =
  | Fix_NBDS of Geneweb_db.Driver.iper * string list
      (** person, synchronized fields among [birth], [baptism], [death],
          [burial] and [events] *)
  | Unfixed_UnknownBurial of Geneweb_db.Driver.iper
      (** person whose burial is unknown while he has a burial or cremation
          event with neither date nor place; reported, not fixed *)
  | Unfixed_NotDead of Geneweb_db.Driver.iper
      (** person not dead while he has a death event without date, a burial or a
          cremation event; reported, not fixed *)
  | Fix_AddedUnion of Geneweb_db.Driver.iper
  | Fix_AddedParents of Geneweb_db.Driver.iper
  | Fix_ParentDeleted of Geneweb_db.Driver.iper
  | Fix_AddedChild of Geneweb_db.Driver.ifam
  | Fix_RemovedUnion of Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam
  | Fix_RemovedDuplicateUnion of Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam
  | Fix_RemovedDuplicateChild of Geneweb_db.Driver.ifam * Geneweb_db.Driver.iper
  | Fix_AddedRelatedFromPevent of
      Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
  | Fix_AddedRelatedFromFevent of
      Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
  | Fix_MarriageDivorce of
      Geneweb_db.Driver.ifam
      * Def.relation_kind
      * Def.relation_kind
      * string list
      (** family, relation before and after, other changed fields among
          [marriage], [marriage_place], [marriage_note] and [marriage_src] *)
  | Fix_MissingSpouse of Geneweb_db.Driver.ifam * Geneweb_db.Driver.iper
  | Fix_WrongUTF8Encoding of
      Geneweb_db.Driver.ifam option
      * Geneweb_db.Driver.iper option
      * (string * string)
      (** family or person, text before and after normalization *)
  | Fix_UpdatedOcc of Geneweb_db.Driver.iper * int * int
  | Fix_ChildrenOrder of
      Geneweb_db.Driver.ifam
      * Geneweb_db.Driver.iper array
      * Geneweb_db.Driver.iper array
      (** family, children before and after reordering *)
  | Fix_UnidecodeKey of
      Geneweb_db.Driver.iper * string * string * Geneweb_db.Driver.iper option
      (** person, former and new names of its files (see
          [Image.key_dir_basename]), person keeping its former occurrence number
          if renumbered *)
  | Fix_RenamedFile of string * string  (** former and new paths *)
  | Unfixed_RenamedFile of string * string * string
      (** former and new paths, reason why the file is not renamed *)
  | Fix_NotesLinks of Geneweb_db.Driver.iper * int
      (** person, number of pages whose links to it are rewritten *)
  | Unfixed_AmbiguousLinks of
      Geneweb_db.Driver.iper * Geneweb_db.Driver.iper * int
      (** renumbered person, person keeping the key, number of pages linking
          both, not rewritten *)
  | Unfixed_FoldingCollision of Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
      (** person and person keeping its key, whose keys differ with the folding
          of the names prior to 2026-11; left to [fix_unidecode_keys] *)

val check_NBDS :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person in the base synchronise his birth, death, baptism and
    burial events with his fields and vice versa. An unknown burial is set from
    a burial or cremation event only when this event has a date or a place;
    otherwise it is reported with [Unfixed_UnknownBurial]. *)

val check_not_dead :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person not dead having a death event without date, a burial or a
    cremation event, report [Unfixed_NotDead]. Nothing is patched. A dated death
    event is left to [check_NBDS], which sets the death from it. *)

val check_families_parents :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family's parent in the base add current family to the parent's
    union if absent. *)

val check_families_children :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family's children in the base remove the children listed more than
    once, and add current family to the children's ascendants if absent. Doesn't
    modify consanguinity rate. *)

val fix_children_order :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family, put the children in birth order (baptism date when there
    is no birth date), moving few children. Only children with such a date are
    moved; a child without a comparable date keeps its place relative to the
    children that do not move (e.g. [A(1800) ? B(1790)] becomes [B A ?]).
    Children whose dates cannot be strictly compared (overlapping imprecise
    dates) keep their relative order when possible. Fixes the
    [ChildrenNotInOrder] anomaly, except for a child known only by its death
    date. *)

val check_persons_parents :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person checks their ascendants. If it references to the dummy
    family, then remove this reference. Otherwise add the person to the family's
    children if absent. *)

val check_persons_families :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person in the base removes all duplicate families and families
    where person isn't a parent. *)

val check_pevents_witnesses :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person's event's witness add current person to the list of related
    of the witness if absent. *)

val check_fevents_witnesses :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family's event's witness add family's father to the list of
    related of the witness if absent. *)

val fix_marriage_divorce :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family having a marriage, no marriage, no mention, engagement,
    annulment, banns, contract, licence, PACS or residence event, set its
    relation and marriage fields from its events, as the family edition does. A
    family without such an event is left unchanged. The no sex check of a
    [NoSexesCheckMarried] or [NoSexesCheckNotMarried] relation is kept. Dates
    are compared as dates and strings as texts, so that a family is patched only
    when its data changes. The divorce field is rewritten with the family, but
    not compared, since [Driver.get_divorce] reads the events. *)

val fix_missing_spouses :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family whose parent is a deleted person (with the dummy iper),
    restores this person, with the families it is a parent of as union list.
    This happens when a person was deleted instead of having their personal
    details erased. *)

val fix_utf8_sequence :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person's and family's string and text date, replace it with its
    NFC normalization, invalid UTF-8 sequences becoming U+FFFD. ASCII strings
    are skipped and only the changed strings are inserted. *)

val fix_unidecode_keys :
  apply:bool ->
  Config.config ->
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** [fix_unidecode_keys ~apply conf] moves a base built with the folding of the
    names prior to 2026-11 ([Unidecode.former]), by its former version or by gwc
    for a [.gw] whose persons collide, to the current [Name.lower]. For every
    person whose key changes:
    - if another person holds the new key, the person whose key does not change
      (else the oldest one) keeps it, and the others get the first free
      occurrence numbers; the links of the notes to a renumbered person are
      rewritten ([Notes.update_ind_key]), except in the pages also linking the
      person keeping the key, which are reported;
    - its portraits, blasons, carrousel and history file are renamed after the
      new key, unless the target exists.

    With [apply], the files and the notes are written, and the notes links
    database is rebuilt if it holds former keys; otherwise only the person
    patches are made, and the rest is reported. The links to a renumbered person
    are found only in a notes links database built with the former folding. On a
    base built with the current folding, only the files are renamed. *)

val note_entry : Geneweb_db.Driver.base -> patch -> (int * string) option
(** [note_entry base patch] is, for a renumbered person or a case to verify, its
    section in the note written by [write_note] and its line, with links to the
    persons; [None] for the other patches. *)

val write_note :
  Config.config ->
  Geneweb_db.Driver.base ->
  string ->
  (int * string) list ->
  unit
(** [write_note conf base origin entries] appends to the note [gwfixbase]
    (created with its title) a heading with [origin] and the date, and a section
    per kind of [entries] (from [note_entry]), in their order; nothing if
    [entries] is empty. *)

val fix_key :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every key (first name and surname folded by [Name.lower]) held by
    several persons with the same occurrence number, the oldest person (lowest
    iper) keeps it and the others get the first free ones; a negative occurrence
    number is replaced the same way. A person whose key differs from the one of
    the person keeping it with the folding of the names prior to 2026-11 is not
    renumbered but reported with [Unfixed_FoldingCollision], since only
    [fix_unidecode_keys] follows its files and the links of the notes. The
    persons are grouped in one pass over the base, without its name index, which
    may not match the current folding of the names; the groups take a few tens
    of bytes per person. *)
