(* Copyright (c) 1998-2007 INRIA *)

open Config
open Def

(** Errors of the update forms, raised as [ModErr]; {!string_of_error} gives
    their message. *)
type update_error =
  | UERR of Adef.safe_string
  | UERR_sex_married of Geneweb_db.Driver.person
  | UERR_sex_incoherent of Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_unknow_person of string * string * int
  | UERR_already_defined of
      Geneweb_db.Driver.base * Geneweb_db.Driver.person * string
  | UERR_own_ancestor of Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_digest
  | UERR_bad_date of Adef.dmy
  | UERR_missing_field of Adef.safe_string
  | UERR_already_has_parents of
      Geneweb_db.Driver.base * Geneweb_db.Driver.person
  | UERR_missing_surname of Adef.safe_string
  | UERR_missing_first_name of Adef.safe_string
  | UERR_locked_base
  | UERR_same_file

exception ModErr of update_error

type create_info = {
  ci_birth_date : Adef.date option;
  ci_birth_place : string;
  ci_death : death;
  ci_death_date : Adef.date option;
  ci_death_place : string;
  ci_occupation : string;
  ci_public : bool;
}
(** Data typed in the short form creating a person from a family or relation
    form. A [ci_birth_place] starting with ["b/"] gives a baptism place, and
    [ci_birth_date] is then the baptism date. *)

(** Whether a person referenced in a form is created, with its sex and optional
    data, or linked to an existing person. *)
type create = Create of sex * create_info option | Link

type key = string * string * int * create * string
(** [(first_name, surname, occ, create, var)]: a person referenced in a form.
    For a "?" first name or surname, [occ] is the person index. [var] is the
    name of the form field, used to point back to it in a conflict page. *)

val infer_death :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> death
(** [infer_death conf base p] returns [OfCourseDead] when [p] is certainly dead,
    and [DontKnowIfDead] otherwise. It tries in turn the birth or baptism date
    of [p] (see {!infer_death_bb}), the marriage dates of the families of [p],
    the children of its undated families (recursively), and the family of its
    parents (see {!infer_death_from_parents}). *)

val infer_death_bb : config -> Adef.date option -> Adef.date option -> death
(** [infer_death_bb conf birth baptism] returns [OfCourseDead] when [birth], or
    else [baptism], is more than the maximum lifespan (125 years) ago, and
    [DontKnowIfDead] otherwise. *)

val infer_death_from_cdate : config -> ?max_age:int -> Adef.cdate -> death
(** [infer_death_from_cdate conf cdate] returns [OfCourseDead] when [cdate]
    (usually a birth date) is more than [max_age] years ago (default: the
    maximum lifespan, 125 years), and [DontKnowIfDead] otherwise or when the
    date is unknown. *)

val infer_death_from_parents :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.family -> death
(** [infer_death_from_parents conf base fam] infers the death status of a new
    child of [fam]: [OfCourseDead] when a parent died more than the maximum
    lifespan ago, was born more than the maximum lifespan plus 25 years ago, or
    when the marriage is more than the maximum lifespan ago; [DontKnowIfDead]
    otherwise. *)

val print_same_name :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** [print_same_name conf base p] prints, when other persons share the name of
    [p], a table of all of them with their occurrence number, their occupation
    (according to the [occu_in_homonyms] base parameter) and the description
    given by {!print_person_parents_and_spouses}. The person whose index is the
    [i] parameter is highlighted. Prints nothing when [p] has no homonym. *)

val print_person_parents_and_spouses :
  Config.config ->
  Geneweb_db.Driver.base ->
  ?alias:string option ->
  ?snalias:string option ->
  Geneweb_db.Driver.person ->
  unit
(** [print_person_parents_and_spouses conf base ?alias ?snalias p] prints a link
    to [p] (public name or first name, occurrence number when the [show_occ]
    base parameter is set, surname), followed by [snalias] in brackets, the
    short dates of [p], [alias], the first name when a public name is shown, its
    parents and its spouses. Prints nothing when [p] is not accessible. *)

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
(** [insert_person conf base src new_persons key] returns the person referenced
    by [key] in a form. An empty first name or surname is read as "?".
    - [Link]: returns the existing person; prints the "unknown person" error
      page and raises [ModErr] when there is none.
    - [Create]: prints the create-conflict page (see {!print_create_conflict})
      and raises [ModErr] when a person with this key exists. Otherwise inserts
      a new person with the source [src] and the data of the [create_info]; its
      death status, when not given, is inferred from its birth or baptism date.
      A new person with a known name is added to [new_persons]. *)

val delete_topological_sort_v : config -> Geneweb_db.Driver.base -> unit
(** [delete_topological_sort_v conf base] deletes the cached files
    [tstab_visitor] (topological sort for visitors) and [restrict] (restricted
    persons) of the base, so that they are rebuilt on next use. *)

val delete_topological_sort : config -> Geneweb_db.Driver.base -> unit
(** Same as {!delete_topological_sort_v}, and also deletes [tstab]. *)

val update_related_pointers :
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.iper ->
  Geneweb_db.Driver.iper list ->
  Geneweb_db.Driver.iper list ->
  unit
(** [update_related_pointers base ip old_list new_list] updates the [related]
    field of the persons referenced by [ip] (as relation parent or witness) when
    this list changes from [old_list] to [new_list]: [ip] is added to the
    persons only in [new_list] and removed from the persons only in [old_list].
    Patches are not committed. *)

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
    [param=value]. Optional [continue] parameter is the label used for the
    submit button. *)

val prerr : config -> update_error -> (unit -> unit) -> 'a
(** [prerr conf err callback] In regular mode, prints an error page whose body
    is written by [callback] (between header and trailer), then raises
    [ModErr err]. In API mode, only raises [ModErr err]. *)

val string_of_error : config -> update_error -> Adef.safe_string
(** [string_of_error conf err] returns the translated message of [err]. *)

val print_error : config -> update_error -> unit
(** [print_error conf err] prints the message of [err]. *)

val print_warnings :
  config -> Geneweb_db.Driver.base -> CheckItem.base_warning list -> unit
(** [print_warnings conf base wl] prints the "warnings" heading and the list
    [wl] without duplicates. Prints nothing when [wl] is empty. *)

val print_warnings_and_miscs :
  config ->
  Geneweb_db.Driver.base ->
  CheckItem.base_warning list ->
  CheckItem.base_misc list ->
  unit
(** [print_warnings_and_miscs conf base wl ml] prints the warnings [wl] and the
    miscellaneous notices [ml] (such as missing sources) under the same
    "warnings" heading. Prints nothing when both lists are empty. *)

val def_error :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person Def.error -> unit
(** [def_error conf base err] converts a database consistency error into the
    matching [update_error] and reports it with {!error}. *)

val error : config -> update_error -> 'exn
(** [error conf err] prints an error page with the message of [err] and a
    "return" form (see {!prerr}), then raises [ModErr err]. *)

val error_locked : config -> 'exn
(** [error_locked conf] prints the "file temporarily locked" page, with a form
    resubmitting the current request and a form cancelling it, then raises
    [ModErr UERR_locked_base]. *)

val error_digest : config -> 'exn
(** [error_digest conf] reports that the base changed since the form was
    displayed (see {!digest_person}), then raises [ModErr UERR_digest]. *)

val error_same_file : config -> 'exn
(** [error_same_file conf] reports that the notes file already exists, then
    raises [ModErr UERR_same_file]. *)

val digest_person :
  salt:string -> (Geneweb_db.Driver.iper, key, string) gen_person -> string
(** [digest_person ~salt per] generates a digest of the person [per]. The
    function is intended for use in form to help prevent:
    - Concurrent edits of the same person.
    - Cross-site Request Forgery (CSRF) attacks. [salt] is a secret value
      generated at server startup; it is mandatory so that the compiler rejects
      any call site that would omit it. *)

val digest_family :
  salt:string ->
  (key, Geneweb_db.Driver.ifam, string) gen_family
  * key Adef.gen_couple
  * key gen_descend ->
  string
(** [digest_family ~salt fam] generates a digest of the family [fam]. The
    function is intended for use in form to help prevent:
    - Concurrent edits of the same family.
    - Cross-site Request Forgery (CSRF) attacks. [salt] is a secret value
      generated at server startup; it is mandatory so that the compiler rejects
      any call site that would omit it. *)

val reconstitute_date : config -> string -> Adef.date option
(** [reconstitute_date conf var] builds the date typed in the form fields
    prefixed by [var] (year, month, day, precision, calendar); a French
    republican month forces the French calendar, and a complete date is stored
    in Gregorian. Without a year, returns the text date of the [var_text] field
    when not empty, and [None] otherwise. Reports an invalid date with
    {!bad_date}. *)

val print_someone :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** [print_someone conf base p] prints "first_name[.occ] surname" of [p], the
    occurrence number being omitted when it is 0. *)

val update_conf : config -> config
(** [update_conf conf] applies the choice made on the create-conflict page: when
    the request comes from its "create" or "link" button, the field named by the
    [field] parameter is set to create or link with the chosen occurrence
    number, and the page's own parameters are removed. Returns [conf] unchanged
    otherwise. *)

val bad_date : config -> Adef.dmy -> 'a
(** [bad_date conf d] reports the invalid date [d], then raises
    [ModErr (UERR_bad_date d)]. *)

val check_greg_day : config -> Adef.dmy -> unit
(** [check_greg_day conf d] reports [d] with {!bad_date} when its day exceeds
    the number of days of its month. *)

val check_missing_witnesses_names :
  config ->
  ('a -> ((string * string * 'b * 'c * 'd) * 'e * 'f) array) ->
  'a list ->
  update_error option
(** [check_missing_witnesses_names conf get list] returns the error of the first
    witness, among the witnesses [get x] of each element [x] of [list], whose
    first name or surname is empty or "?" while the other is given. Witnesses
    with both names empty are ignored. *)

val check_missing_name :
  Geneweb_db.Driver.base ->
  (Geneweb_db.Driver.iper, 'b, string) Def.gen_person ->
  update_error option
(** [check_missing_name base p] returns an error when the first name or the
    surname of [p] is empty, or is "?" for an existing person whose stored name
    is not "?". *)

val print_create_conflict :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> string -> 'exn
(** [print_create_conflict conf base p var] prints a message because a person
    with the same key as [p] already exists, and a form offering to:
    - create the person with the first free occurrence number;
    - go back to the form, restoring the previous occurrence number when it was
      changed;
    - go back to change the number or to link instead of create.

    The persons with the same name are listed below. [var] is used for the input
    with name "field". Leave it empty if unused. Raises [ModErr]. *)

val print_order_changed :
  config -> ('a array -> bool array -> unit) -> 'a array -> 'a array -> unit
(** [print_order_changed conf print_list before after] prints [before] and
    [after] side by side; [print_list] receives each array with the flags of the
    elements that differ from the other one. *)

val redirect_unchanged :
  config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> unit
(** [redirect_unchanged conf base p] answers a form submitted without any change
    with a temporary redirection to the page of [p], addressed by key or by
    index according to [access_by_key], with [nomod=1] so the page shows a "no
    modification" notification. *)
