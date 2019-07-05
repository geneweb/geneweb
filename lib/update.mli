(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

exception ModErr of string

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

val insert_person :
  config -> base -> string -> (iper, iper, istr) gen_person list ref -> key ->
    iper
val delete_topological_sort_v : config -> base -> unit
val delete_topological_sort : config -> base -> unit

val update_related_pointers : base -> iper -> iper list -> iper list -> unit

val print_return : config -> unit
val string_of_error : config -> base -> CheckItem.base_error -> string
val print_error : config -> base -> CheckItem.base_error -> unit
val print_warnings : config -> base -> CheckItem.base_warning list -> unit
val print_miscs : config -> base -> CheckItem.base_misc list -> unit
val print_warnings_and_miscs :
  config -> base -> CheckItem.base_warning list -> CheckItem.base_misc list ->
    unit
val error : config -> base -> CheckItem.base_error -> 'a

val error_locked : config -> unit
val error_digest : config -> unit

val digest_person : (iper, key, string) gen_person -> Digest.t
val digest_family :
  (key, _, string) gen_family * key gen_couple * key gen_descend -> Digest.t

val reconstitute_date : config -> string -> date option

val print_someone : config -> base -> person -> unit

val update_conf : config -> config

val bad_date : config -> dmy -> 'a
val check_greg_day : config -> dmy -> unit

