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
  | Fix_NBDS of Geneweb_db.Driver.iper
  | Fix_AddedUnion of Geneweb_db.Driver.iper
  | Fix_AddedParents of Geneweb_db.Driver.iper
  | Fix_ParentDeleted of Geneweb_db.Driver.iper
  | Fix_AddedChild of Geneweb_db.Driver.ifam
  | Fix_RemovedUnion of Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam
  | Fix_RemovedDuplicateUnion of Geneweb_db.Driver.iper * Geneweb_db.Driver.ifam
  | Fix_AddedRelatedFromPevent of
      Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
  | Fix_AddedRelatedFromFevent of
      Geneweb_db.Driver.iper * Geneweb_db.Driver.iper
  | Fix_MarriageDivorce of Geneweb_db.Driver.ifam
  | Fix_MissingSpouse of Geneweb_db.Driver.ifam * Geneweb_db.Driver.iper
  | Fix_WrongUTF8Encoding of
      Geneweb_db.Driver.ifam option
      * Geneweb_db.Driver.iper option
      * (Geneweb_db.Driver.istr * Geneweb_db.Driver.istr) option
  | Fix_UpdatedOcc of Geneweb_db.Driver.iper * int * int

val check_NBDS :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person in the base synchronise his birth, death, baptism and
    burial events with his fields and vice versa. *)

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
(** For every family's children in the base add current family to the children's
    ascendants if absent. Doesn't modify consanguinity rate. *)

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
(** For every family in the base synchronise its fields with marriage and
    divorce events. *)

val fix_missing_spouses :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every family, if a parent refers to a person dummy person (with dummy
    iper). Fix this person and add the family to their union list. If this
    situation happens, an explaination is that the person has been incorrectly
    deleted, instead of just erasing their personal details. *)

val fix_utf8_sequence :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person's and family's field, remplace it with normalized UTF8
    version. *)

val fix_key :
  ?report:(patch -> unit) ->
  (int -> int -> unit) ->
  Geneweb_db.Driver.base ->
  unit
(** For every person in the base, update their occurence number if someone with
    same key (normalized first name and last name, and occurence number) already
    exists. *)
