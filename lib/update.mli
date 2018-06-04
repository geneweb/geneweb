(* $Id: update.mli,v 5.12 2008-01-08 02:08:00 ddr Exp $ *)
(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def
open Gwdb

exception ModErr
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

val infer_death : config -> date option -> date option -> death
val print_same_name : config -> base -> person -> unit
val print_person_parents_and_spouses : config -> base -> person -> unit

val insert_person :
  config -> base -> string -> (iper, istr) gen_person list ref -> key ->
    Adef.iper
val add_misc_names_for_new_persons :
  base -> (iper, istr) gen_person list -> unit
val update_misc_names_of_family : base -> sex -> ifam gen_union -> unit
val delete_topological_sort_v : config -> base -> unit
val delete_topological_sort : config -> base -> unit

val update_related_pointers : base -> iper -> iper list -> iper list -> unit

val print_return : config -> unit
val print_error : config -> base -> CheckItem.base_error -> unit
val print_warnings : config -> base -> CheckItem.base_warning list -> unit
val print_miscs : config -> base -> CheckItem.base_misc list -> unit
val print_warnings_and_miscs :
  config -> base -> CheckItem.base_warning list * CheckItem.base_misc list ->
    unit
val error : config -> base -> CheckItem.base_error -> 'a

val error_locked : config -> unit
val error_digest : config -> unit

val digest_person : (key, string) gen_person -> Digest.t
val digest_family :
  (key, string) gen_family * key gen_couple * key gen_descend -> Digest.t

val reconstitute_date : config -> string -> date option

val print_someone : config -> base -> person -> unit

val update_conf : config -> config

(* Ajout pour l'API *)
(* Erreurs possibles :
     - "UnknownPerson"
     - "AlreadyDefined"
     - "OwnAncestor"
     - "BadSexOfMarriedPerson"
     - "BaseChanged"
     - "BadDateFormat"
     - "CreateConflictOcc"
     - "AlreadyHasParent"
     - "FatherShouldBeMale"
     - "MotherShouldBeFemale"
     - "Disconnected"
     - "error"
*)
exception ModErrApi of string
val bad_date : config -> dmy -> 'a
val check_greg_day : config -> dmy -> unit
