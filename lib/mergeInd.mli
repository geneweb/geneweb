exception Error_loop of Gwdb.person
exception Different_sexes of Gwdb.person * Gwdb.person

(* TODOOCP *)
val reparent_ind :
  Gwdb.base -> Gwdb.iper -> Gwdb.iper -> Warning.base_warning list
(** [reparent_ind base ip1 ip2] reparents [ip1] and [ip2] and returns any resulting warnings. *)

type merge_ind_job = { p1 : Gwdb.person; p2 : Gwdb.person }
(** Job representing two individuals to be merged. *)

type merge_fam_job = { f1 : Gwdb.family; f2 : Gwdb.family }
(** Job representing two families to be merged. *)

type merge_job = IndJob of merge_ind_job | FamJob of merge_fam_job
type stuck_job = merge_job
type remaining_merge_jobs

type merge_result =
  | Stuck of stuck_job * remaining_merge_jobs * Warning.base_warning list
      (** The merge halted due to a conflict or ambiguity requiring user intervention.
      Provides the stuck job, remaining merge jobs, and any accumulated warnings. *)
  | Finished of Warning.base_warning list
      (** The merge completed successfully. Returns all accumulated warnings. *)

val person_pairs_of_jobs : remaining_merge_jobs -> (Gwdb.iper * Gwdb.iper) list
(** [person_pairs_of_jobs jobs] returns the list of individual pairs
    representing the ancestry branches leading to the current conflict. *)

val merge :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.person -> merge_result
(** [merge conf base p1 p2] attempts to merge individual [p2] into [p1],
    recursively merging their parents and families if necessary.
    @raise Person.Same_person if [p1] and [p2] are the same individual.
    @raise Different_sexes if [p1] and [p2] have different, defined sexes.
    @raise Error_loop if [p1] is an ancestor of [p2] or vice versa. *)

val kill_ancestors :
  Config.config ->
  Gwdb.base ->
  bool ->
  Gwdb.person ->
  int ref ->
  int ref ->
  unit

val compatible_strings : Gwdb.istr -> Gwdb.istr -> bool
(** [compatible_strings s1 s2] returns [true] if string references [s1] and [s2]
    are equal or if either is empty. *)

val compatible_notes : Gwdb.base -> Gwdb.istr -> Gwdb.istr -> bool
(** [compatible_notes base s1 s2] returns [true] if notes [s1] and [s2]
    contain compatible content. *)
