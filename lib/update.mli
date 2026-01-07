(* Copyright (c) 1998-2007 INRIA *)

type update_error =
  | UERR of Adef.safe_string
  | UERR_sex_married of Gwdb.person
  | UERR_sex_incoherent of Gwdb.base * Gwdb.person
  | UERR_sex_undefined of string * string * int
  | UERR_unknow_person of string * string * int
  | UERR_already_defined of Gwdb.base * Gwdb.person * string
  | UERR_own_ancestor of Gwdb.base * Gwdb.person
  | UERR_digest
  | UERR_bad_date of [ `Missing_year | `Date of Date.dmy ]
  | UERR_missing_field of Adef.safe_string
  | UERR_already_has_parents of Gwdb.base * Gwdb.person
  | UERR_missing_surname of Adef.safe_string
  | UERR_missing_first_name of Adef.safe_string
  | UERR_locked_base
  | UERR_illegal_access_update of Def.access * Def.access
  | UERR_not_plain_text of Adef.escaped_string
  | UERR_invalid_occurrence_number of {
      first_name : string;
      last_name : string;
      occurrence_number : int;
    }

val not_plain_text_error : string -> update_error

exception ModErr of update_error

type create_info = {
  ci_birth_date : Date.date option;
  ci_birth_place : string;
  ci_death : Def.death;
  ci_death_date : Date.date option;
  ci_death_place : string;
  ci_occupation : string;
  ci_public : bool;
}

type create = Create of Def.sex * create_info option | Link
type key = string * string * int * create * string

val infer_death : Config.config -> Gwdb.base -> Gwdb.person -> Def.death

val infer_death_bb :
  Config.config -> Date.date option -> Date.date option -> Def.death

val infer_death_from_parents :
  Config.config -> Gwdb.base -> Gwdb.family -> Def.death
(** [infer_death_from_parents conf base fam] infer death status for a new
    children in this family *)

val infer_witness_death_from_event :
  conf:Config.config ->
  base:Gwdb.base ->
  date:Date.date option ->
  existing_witnesses:Gwdb.iper list ->
  Def.death

val print_same_name : Config.config -> Gwdb.base -> Gwdb.person -> unit

val print_person_parents_and_spouse :
  Config.config -> Gwdb.base -> Gwdb.person -> unit

val insert_person :
  Config.config ->
  Gwdb.base ->
  string ->
  (Gwdb.iper, Gwdb.iper, Gwdb.istr) Def.gen_person list ref ->
  key ->
  Gwdb.iper

val delete_topological_sort_v : Config.config -> Gwdb.base -> unit
val delete_topological_sort : Config.config -> Gwdb.base -> unit

val update_related_pointers :
  Gwdb.base -> Gwdb.iper -> Gwdb.iper list -> Gwdb.iper list -> unit

val print_return : Config.config -> unit
(** Helper function printing a hidden form containing current env, with a submit
    button "return", plus a hidden field [return=on]. *)

val print_continue :
  Config.config ->
  ?continue:Adef.encoded_string ->
  string ->
  Adef.encoded_string ->
  unit
(** [print_continue conf param value] Helper function printing a hidden form
    containing current env, with a submit button "continue", plus a hidden field
    [param=value]. Optionnal [continue] parameter is the label used for the
    submit button. *)

val prerr : Config.config -> update_error -> (unit -> unit) -> 'a
(** [prerr conf err callback] Regular mode: print error page using [callback]
    (wrapped in header/trailer) and and raise [ModErr err] API mode: only raise
    [ModErr err] *)

val string_of_error : Config.config -> update_error -> Adef.safe_string
val print_error : Config.config -> update_error -> unit

val print_warnings :
  Config.config -> Gwdb.base -> Warning.base_warning list -> unit

val print_miscs : Config.config -> Gwdb.base -> Warning.base_misc list -> unit

val print_warnings_and_miscs :
  Config.config ->
  Gwdb.base ->
  Warning.base_warning list ->
  Warning.base_misc list ->
  unit

val def_error : Config.config -> Gwdb.base -> Gwdb.person Def.error -> unit
val error : Config.config -> update_error -> 'exn
val error_locked : Config.config -> 'exn
val error_digest : Config.config -> 'exn
val digest_person : (Gwdb.iper, key, string) Def.gen_person -> Digest.t

val digest_family :
  (key, _, string) Def.gen_family * key Def.gen_couple * key Def.gen_descend ->
  Digest.t

val reconstitute_date : Config.config -> string -> Date.date option
val print_someone : Config.config -> Gwdb.base -> Gwdb.person -> unit
val update_conf : Config.config -> Config.config
val bad_date : Config.config -> [ `Missing_year | `Date of Date.dmy ] -> 'a
val check_greg_day : Config.config -> Date.dmy -> unit

val check_missing_witnesses_names :
  Config.config ->
  ('a -> ((string * string * 'b * 'c * 'd) * 'e * string) array) ->
  'a list ->
  update_error option

val check_missing_name :
  Gwdb.base -> (Gwdb.iper, 'b, string) Def.gen_person -> update_error option

val is_illegal_access_update :
  previous_access:Def.access -> new_access:Def.access -> bool

val check_illegal_access_update :
  Gwdb.base -> (Gwdb.iper, 'a, 'b) Def.gen_person -> update_error option

val check_occurrence_number :
  first_name:string -> last_name:string -> int -> update_error option

val check_person_occurrence_number :
  (_, _, string) Def.gen_person -> update_error option

val print_create_conflict :
  Config.config -> Gwdb.base -> Gwdb.person -> string -> 'exn
(** [print_create_conflict conf base p var] Print a message because a personne
    with same key already exists, and display a form with two options:
    - create a personne with the next occurence number available
    - go back to the previous pre-filled form.

    [var] is used for the input with name "field". Leave it empty if unused. *)

val print_order_changed :
  Config.config ->
  ('a array -> bool array -> unit) ->
  'a array ->
  'a array ->
  unit
(** [print_order_changed conf print_list before after] *)
