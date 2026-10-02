exception Error_loop of Gwdb.person
exception Different_sexes of Gwdb.person * Gwdb.person

(* TODOOCP *)
val reparent_ind :
  Gwdb.base -> Gwdb.iper -> Gwdb.iper -> Warning.base_warning list

type merge_ind_job = { p1 : Gwdb.person; p2 : Gwdb.person }
type merge_fam_job = { f1 : Gwdb.family; f2 : Gwdb.family }
type merge_job = IndJob of merge_ind_job | FamJob of merge_fam_job
type stuck_job = merge_job
type remaining_merge_jobs

type merge_result =
  | Stuck of stuck_job * remaining_merge_jobs * Warning.base_warning list
  | Finished of Warning.base_warning list

val person_pairs_of_jobs : remaining_merge_jobs -> (Gwdb.iper * Gwdb.iper) list

val merge :
  Config.config -> Gwdb.base -> Gwdb.person -> Gwdb.person -> merge_result

val kill_ancestors :
  Config.config ->
  Gwdb.base ->
  bool ->
  Gwdb.person ->
  int ref ->
  int ref ->
  unit

val compatible_strings : Gwdb.istr -> Gwdb.istr -> bool
val compatible_notes : Gwdb.base -> Gwdb.istr -> Gwdb.istr -> bool
val has_continuation : Config.config -> bool
