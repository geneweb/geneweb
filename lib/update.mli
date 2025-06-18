(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

type update_error =
  | UERR of Adef.safe_string
  | UERR_sex_married of Geneweb_db.Driver.person
  | UERR_sex_incoherent of Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_sex_undefined of string * string * int
  | UERR_unknow_person of string * string * int
  | UERR_already_defined of
      Geneweb_db.Driver.base * Geneweb_db.Driver.person * string
  | UERR_own_ancestor of Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_digest
  | UERR_bad_date of Def.dmy
  | UERR_missing_field of Adef.safe_string
  | UERR_already_has_parents of
      Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_missing_surname of Adef.safe_string
  | UERR_missing_first_name of Adef.safe_string
  | UERR_locked_base

exception ModErr of update_error

type create_info = {
  ci_birth_date : date option;
  ci_birth_place : string;
  ci_death : death;
  ci_death_date : date option;
  ci_death_place : string;
  ci_occupation : string;
  ci_public : bool;
}

type create = Create of sex * create_info option | Link
type key = string * string * int * create * string

val infer_death :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> death

val infer_death_bb : config -> date option -> date option -> death

val infer_death_from_parents :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.family -> death
(** [infer_death_from_parents conf base fam] infer death status for a new
    children in this family *)

val print_same_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit

val print_person_parents_and_spouses :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit

val insert_person :
  config ->
  Geneweb_db.Driver.base ->
  string ->
  ( Geneweb_db.Driver.iper,
    Geneweb_db.Driver.iper,
    Geneweb_db.Driver.istr )
  gen_person
  list
  ref ->
  key ->
  Geneweb_db.Driver.iper

val delete_topological_sort_v : config -> Geneweb_db.Driver.base -> unit
val delete_topological_sort : config -> Geneweb_db.Driver.base -> unit

val update_related_pointers :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper list ->
  Geneweb_db.Driver.iper list ->
  unit

val print_return : config -> unit
(** Helper function printing a hidden form containing current env, with a submit
    button "return", plus a hidden field [return=on]. *)

val print_continue :
  config ->
  ?continue:Adef.encoded_string ->
  string ->
  Adef.encoded_string ->
  unit
(** [print_continue conf param value] Helper function printing a hidden form
    containing current env, with a submit button "continue", plus a hidden field
    [param=value]. Optionnal [continue] parameter is the label used for the
    submit button. *)

val prerr : config -> update_error -> (unit -> unit) -> 'a
(** [prerr conf err callback] Regular mode: print error page using [callback]
    (wrapped in header/trailer) and and raise [ModErr err] API mode: only raise
    [ModErr err] *)

val string_of_error : config -> update_error -> Adef.safe_string
val print_error : config -> update_error -> unit

val print_warnings :
  config -> Geneweb_db.Driver.base -> CheckItem.base_warning list -> unit

val print_miscs :
  config -> Geneweb_db.Driver.base -> CheckItem.base_misc list -> unit

val print_warnings_and_miscs :
  config ->
  Geneweb_db.Driver.base ->
  CheckItem.base_warning list ->
  CheckItem.base_misc list ->
  unit

val def_error :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person Def.error -> unit

val error : config -> update_error -> 'exn
val error_locked : config -> 'exn
val error_digest : config -> 'exn

val digest_person :
  ?salt:string -> (Geneweb_db.Driver.iper, key, string) gen_person -> string
(** [digest_person ?salt per] generates a digest of the person [pers]. The
    function is intended for use in form to help prevent:
    - Concurrent edits of the same person.
    - Cross-site Request Forgery (CSRF) attacks.

    The optional [salt] parameter should be set to a secret value generated at
    server startup to strengthen security. *)

val digest_family :
  ?salt:string ->
  (key, Geneweb_db.Driver.ifam, string) gen_family
  * key gen_couple
  * key gen_descend ->
  string
(** [digest_family ?salt fam] generates a digest of the family [fam]. The
    function is intended for use in form to help prevent:
    - Concurrent edits of the same family.
    - Cross-site Request Forgery (CSRF) attacks.

    The optional [salt] parameter should be set to a secret value generated at
    server startup to strengthen security. *)

val reconstitute_date : config -> string -> date option

val print_someone :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit

val update_conf : config -> config
val bad_date : config -> dmy -> 'a
val check_greg_day : config -> dmy -> unit

val check_missing_witnesses_names :
  config ->
  ('a -> ((string * string * 'b * 'c * 'd) * 'e) array) ->
  'a list ->
  update_error option

val check_missing_name :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, 'b, string) Def.gen_person ->
  update_error option

val print_create_conflict :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string -> 'exn
(** [print_create_conflict conf base p var] Print a message because a personne
    with same key already exists, and display a form with two options:
    - create a personne with the next occurence number available
    - go back to the previous pre-filled form.

    [var] is used for the input with name "field". Leave it empty if unused. *)

val print_order_changed :
  config -> ('a array -> bool array -> unit) -> 'a array -> 'a array -> unit
(** [print_order_changed conf print_list before after] *)
