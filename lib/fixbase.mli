(** All the function of this module scan the base and fix what is considered as corrupted data.

    They all share a same signature : [let check_XXX ?report progress base = ...]

    The optionnal [report] function should be used to track changes.

    [progress i max] keep tracks of the progress of a task. When called, task is
    about [i/max] done.

    Note that it does not actually commit the changes, so if you do not want a dry run, apply
    [Gwdb.commit_patches]
*)


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
  | Fix_WrongUTF8Encoding of Gwdb.ifam option * Gwdb.iper option * Gwdb.istr * Gwdb.istr

val check_NBDS : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_families_parents : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_families_children : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_persons_parents : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_persons_families : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_pevents_witnesses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val check_fevents_witnesses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val fix_marriage_divorce : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val fix_missing_spouses : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit

val fix_utf8_sequence : ?report:(patch -> unit) -> (int -> int -> unit) -> Gwdb.base -> unit
