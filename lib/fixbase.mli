(** All the function of this module scan the base and fix what is considered as corrupted data.

    They all share a same signature : [let check_XXX ?report progress base = ...]

    The optionnal [report] function should be used to track changes.

    [progress i max] keep tracks of the progress of a task. When called, task is
    about [i/max] done.

    Note that it does not actually commit the changes, so if you do not want a dry run, apply
    [Gwdb.commit_patches]
*)

(** All possible patches that could be automatically deducted from incontinent
    or absent information in the database *)
type patch =
  | Fix_NBDS of Gwdb.iper
  | Fix_AddedUnion of Gwdb.iper
  | Fix_AddedParents of Gwdb.iper
  | Fix_ParentDeleted of Gwdb.iper
  | Fix_AddedChild of Gwdb.ifam
  | Fix_RemovedUnion of Gwdb.iper * Gwdb.ifam
  | Fix_RemovedDuplicateUnion of Gwdb.iper * Gwdb.ifam
  | Fix_AddedRelatedFromPevent of Gwdb.iper * Gwdb.iper
  | Fix_AddedRelatedFromFevent of Gwdb.iper * Gwdb.iper
  | Fix_MarriageDivorce of Gwdb.ifam
  | Fix_MissingSpouse of Gwdb.ifam * Gwdb.iper
  | Fix_WrongUTF8Encoding of Gwdb.ifam option * Gwdb.iper option * (Gwdb.istr * Gwdb.istr) option
  | Fix_UpdatedOcc of Gwdb.iper * int * int

(** For every person in the base synchronise his birth, death, baptism and burial events with
    his fields and vice versa. *)
val check_NBDS : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every family's parent in the base add current family to the parent's union (if absent). *)
val check_families_parents : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every family's childran in the base add current family to the childran's ascendants (if absent).
    Doesn't modify consanguinity rate. *)
val check_families_children : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every person checks it ascendants. If it references to the dummy family, then remove this reference.
    Otherwise add person to the family's children if he is absent. *)
val check_persons_parents : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every person in the base removes all duplicate families and families where person isn't a parent  *)
val check_persons_families : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every person's event's witness add current person to the list of related persons if absent. *)
val check_pevents_witnesses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every family's event's witness add family's father to the list of related persons if absent. *)
val check_fevents_witnesses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every family in the base synchronise its fields with marriage and divorce events. *)
val fix_marriage_divorce : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every family's missing parent (or spouse) fix his id and add current family to the parent's union  *)
val fix_missing_spouses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every person's and family's string remplace it with normalized UTF8 version *)
val fix_utf8_sequence : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

(** For every person in the base update his occurence number if someone with same name and same occurence
    number already exists in the base. *)
val fix_key : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit
