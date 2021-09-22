(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

type update_error =
  | UERR of string
  | UERR_sex_married of person
  | UERR_sex_incoherent of base * person
  | UERR_sex_undefined of string * string * int
  | UERR_unknow_person of string * string * int
  | UERR_already_defined of base * person * string
  | UERR_own_ancestor of base * person
  | UERR_digest
  | UERR_bad_date of Def.dmy
  | UERR_missing_field of string
  | UERR_already_has_parents of base * person
  | UERR_missing_surname of string
  | UERR_missing_first_name of string

exception ModErr of update_error

type create_info =
  { ci_birth_date : date option;
    ci_birth_place : string;
    ci_death : death;
    ci_death_date : date option;
    ci_death_place : string;
    ci_occupation : string;
    ci_public : bool }
type create =
    Create of sex * create_info option
  | Link
type key = string * string * int * create * string

val infer_death : config -> base -> person -> death
val infer_death_bb : config -> date option -> date option -> death
val infer_death_from_age : int -> death
val infer_death_from_parents : config -> base -> family -> death

val print_same_name : config -> base -> person -> unit
val print_person_parents_and_spouse : config -> base -> person -> unit

val insert_person
  : config
  -> base
  -> string
  -> (iper, iper, istr) gen_person list ref
  -> key
  -> iper

val delete_topological_sort_v : config -> base -> unit
val delete_topological_sort : config -> base -> unit

val update_related_pointers : base -> iper -> iper list -> iper list -> unit

(** Helper function printing a hidden form containing current env,
    with a submit button "return", plus a hidden field [return=on].  *)
val print_return : config -> unit

(** [print_continue conf param value]
    Helper function printing a hidden form containing current env,
    with a submit button "continue", plus a hidden field [param=value].
    Optionnal [continue] parameter is the label used for the submit button.
*)
val print_continue : config -> ?continue:string -> string -> string -> unit

(** [prerr conf err callback]
    Regular mode: print error page using [callback] (wrapped in header/trailer)
    and and raise [ModErr err]
    API mode: only raise [ModErr err]
*)
val prerr : config -> update_error -> (unit -> unit) -> 'a

val string_of_error : config -> update_error -> string
val print_error : config -> base -> update_error -> unit
val print_warnings : config -> base -> CheckItem.base_warning list -> unit
val print_miscs : config -> base -> CheckItem.base_misc list -> unit
val print_warnings_and_miscs :
  config -> base -> CheckItem.base_warning list -> CheckItem.base_misc list ->
    unit

val def_error : config -> base -> person Def.error -> unit

val error : config -> update_error -> 'exn

val error_locked : config -> unit
val error_digest : config -> 'exn

val digest_person : (iper, key, string) gen_person -> Digest.t
val digest_family :
  (key, _, string) gen_family * key gen_couple * key gen_descend -> Digest.t

val reconstitute_date : config -> string -> date option

val print_someone : config -> base -> person -> unit

val update_conf : config -> config

val bad_date : config -> dmy -> 'a
val check_greg_day : config -> dmy -> unit

val check_missing_witnesses_names
  : config
  -> ('a -> ((string * string * 'b * 'c * 'd) * 'e) array)
  -> 'a list
  -> update_error option

val check_missing_name
  : config
  -> ('a, 'b, string) Def.gen_person
  -> update_error option
