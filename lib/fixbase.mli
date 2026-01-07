(** All the function of this module scan the base and fix what is considered as
    corrupted data.

    They all share a same signature :
    [let check_XXX ?report progress base = ...]

    The optionnal [report] function should be used to track changes.

    [progress i max] keep tracks of the progress of a task. When called, task is
    about [i/max] done.

    Note that it does not actually commit the changes, so if you do not want a
    dry run, apply [Gwdb.commit_patches] *)

(** All possible patches that could be automatically deducted from inconsistent
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
  | Fix_WrongString of
      Gwdb.ifam option * Gwdb.iper option * (Gwdb.istr * Gwdb.istr) option
  | Fix_UpdatedOcc of Gwdb.iper * int * int

val string_of_patch : Gwdb.base -> patch -> string

type person_fix
type family_fix

val fix_nbds : person_fix
(** For every person in the base synchronise his birth, death, baptism and
    burial events with his fields and vice versa. *)

val fix_family_parents : family_fix
(** For every family's parent in the base add current family to the parent's
    union if absent. *)

val fix_family_children : family_fix
(** For every family's children in the base add current family to the children's
    ascendants if absent. Doesn't modify consanguinity rate. *)

val fix_person_parents : person_fix
(** For every person checks their ascendants. If it references to the dummy
    family, then remove this reference. Otherwise add the person to the family's
    children if absent. *)

val fix_person_unions : person_fix
(** For every person in the base removes all duplicate families and families
    where person isn't a parent. *)

val fix_person_events_witnesses : person_fix
(** For every person's event's witness add current person to the list of related
    of the witness if absent. *)

val fix_family_events_witnesses : family_fix
(** For every family's event's witness add family's father to the list of
    related of the witness if absent. *)

val fix_family_divorce : family_fix
(** For every family in the base synchronise its fields with marriage and
    divorce events. *)

val fix_family_spouses : family_fix
(** For every family, if a parent refers to a person dummy person (with dummy
    iper). Fix this person and add the family to their union list. If this
    situation happens, an explaination is that the person has been incorrectly
    deleted, instead of just erasing their personal details. *)

val fix_person_strings : person_fix

val fix_family_strings : family_fix
(** For every person's and family's field, remplace it with normalized strings.
*)

val fix_person_key : Gwdb.base -> person_fix
(** For every person in the base, update their occurence number if someone with
    same key (normalized first name and last name, and occurence number) already
    exists. *)

val fix_invalid_occurrence_number : person_fix

val perform_fixes :
  report:(patch -> unit) option ->
  progress:(int -> int -> unit) ->
  base:Gwdb.base ->
  person_fixes:person_fix list ->
  family_fixes:family_fix list ->
  int
