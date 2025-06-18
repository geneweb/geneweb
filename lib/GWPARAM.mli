val nb_errors : int ref
val errors_undef : string list ref
val errors_other : string list ref
val set_vars : string list ref
val gwd_cmd : string ref
val cnt_dir : string ref
val sock_dir : string ref
val bases : string ref

val reorg : bool ref
(** set to true when base is in reorg format *)

val force : bool ref
(** force creation of database if already existing *)

type init_s = { status : bool; bname : string }

val init_done : init_s ref
val config_reorg : string -> string
val config_legacy : string -> string

type my_fun_2 = string -> string
type my_fun_3 = string -> string -> string

val config : my_fun_2 ref
val cnt_d : my_fun_2 ref
val adm_file : my_fun_2 ref
val src_d : my_fun_2 ref
val etc_d : my_fun_2 ref
val config_d : my_fun_2 ref
val lang_d : my_fun_3 ref
val bpath : my_fun_2 ref
val portraits_d : my_fun_2 ref
val images_d : my_fun_2 ref

(* S: Move it to gwd_lib?  *)

val init : string -> unit
(** Function called before gwd starts e.g. inititialise assets folders in Secure
    module. *)

val init_etc : string -> unit
(** Function called before gwd starts e.g. inititialise etc folders in reorg
    mode. *)

val is_reorg_base : string -> bool
(** returns true if mybase.gwb/config/mybase.gwf exists *)

val test_reorg : string -> unit
(** set reorg to true if !reorg of is_reorg_base; then calls init *)

val test_base : string -> unit
(** for gwc and ged2gwb, test_reorg, alert if base exists and force is not set
*)

module Default : sig
  val config : string -> string
  val cnt_d : string -> string
  val adm_file : string -> string
  val portraits_d : string -> string
  val src_d : string -> string
  val etc_d : string -> string
  val config_d : string -> string
  val lang_d : string -> string -> string
  val images_d : string -> string

  val bpath : string -> string
  (** [Filename.concat (Secure.base_dir ())] *)
end

module Legacy : sig
  val config : string -> string
  val cnt_d : string -> string
  val adm_file : string -> string
  val portraits_d : string -> string
  val src_d : string -> string
  val etc_d : string -> string
  val config_d : string -> string
  val lang_d : string -> string -> string
  val images_d : string -> string

  val bpath : string -> string
  (** [Filename.concat (Secure.base_dir ()) (string ^ ".gwb") ] *)
end

val output_error :
  ?headers:string list ->
  ?content:Adef.safe_string ->
  Config.config ->
  Def.httpStatus ->
  unit
(** If [?content] is not set, sends page content from
    [/etc/<status-code>-<lang>.html]. If the current lang is not available, use
    `en` *)

val is_semi_public : Geneweb_db.Driver.person -> bool
(** determines if the person has SemiPublic status *)

val split_key : string -> string * string * string
(** split a key of the form first_name.occ surname into its three components the
    .occ part may be absent. No spaces in first_name and surnames *)

val is_related :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** determines if the person is a descendant a sibling or an ancestor of
    conf.userkey. conf.userkey is the person visiting the base the search for
    ancestors is limited to 3 generations *)

val p_auth :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** Calculate the access rights to the person's information in according to his
    age. Returns (in the order of the tests) :
    - `true` if requester is wizard or friend or person is public
    - `true` if person has at least one title and {i public_if_title} is set to
      {i yes} in gwf config file
    - `false` if person is alive and {i private_years} > 0
    - `true` if person is older (depending on the date of birth or baptism date)
      then {i privates_years}
    - `false` if person is younger (depending on the date of birth or baptism
      date) then {i privates_years}
    - `true` if person has been deceased for more than {i privates_years}
    - `false` if person has been deceased for less than {i privates_years}
    - `true` if person is between 80 and 120 years old and he is not beeing
      private and {i public_if_no_date} is set to {i yes} in gwf config file
    - `true` if person has been married for more than {i private_years}
    - `false` otherwise *)

val p_auth_sp :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** returns p_auth or true if both user and p are SemiPublic *)

val wrap_output : Config.config -> Adef.safe_string -> (unit -> unit) -> unit
(** Display in a very basic HTML doc, with no CSS or JavaScript. *)
