val nb_errors : int ref
val errors_undef : string list ref
val errors_other : string list ref
val set_vars : string list ref
val gwd_cmd : string ref
val cnt_dir : string ref
val bases : string ref

val reorg : bool ref
(** set to true when base is in reorg format *)

val force : bool ref
(** force creation of database if already existing *)

val config_reorg : string -> string
val config_legacy : string -> string

val cache_d : string -> string
(** [cache_d bname] Directory of the binary caches of base [bname], written by
    cache_files and read by chk_data: bases/bname.gwb/caches, whatever the base
    format, next to the caches of the cousins tool. Unlike
    bases/etc/bname/cache, which holds the .gz datalists that browsers download,
    it is never served. *)

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
val albums_d : my_fun_2 ref

(* S: Move it to gwd_lib?  *)

val init : unit -> unit
(** Function called to initialize path functions based on the reorg flag. *)

val is_reorg_base : string -> bool
(** returns true iff mybase.gwb/config/mybase.gwf exists *)

val set_reorg : string -> bool option -> unit
(** if bool option = Some bool -> set reorg to bool if bool option = None -> set
    reorg according to is_reorg_base; call init *)

val check_base_exists : string -> unit
(** Check if database exists and exit if so (unless -f flag is set) *)

val create_base_and_config : string -> string
(** Create base directory and configuration. *)

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
  val albums_d : string -> string

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
  val albums_d : string -> string

  val bpath : string -> string
  (** [Filename.concat (Secure.base_dir ()) (string ^ ".gwb") ] *)
end

val output_error :
  ?headers:string list ->
  ?content:Adef.safe_string ->
  Config.config ->
  Geneweb_http.Code.status ->
  unit
(** If [?content] is not set, sends page content from
    [/etc/<status-code>-<lang>.html]. If the current lang is not available, use
    `en` *)

val is_semi_public : Geneweb_db.Driver.person -> bool
(** determines if the person has SemiPublic status *)

val split_key : string -> string * string * string
(** split a key of the form first_name.occ surname into its three components the
    .occ part may be absent. No spaces in first_name and surnames *)

val person_of_string_user_key :
  Geneweb_db.Driver.base -> string -> Geneweb_db.Driver.iper option
(** [person_of_string_user_key base key] resolves a user/person key
    ([first_name.occ surname], '+' allowed for spaces) to an individual. It
    tries {!split_key} (first-space split) then the occ-aware dot-key parser, so
    it also resolves multi-word first names. *)

val is_related :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** determines if the person is a descendant a sibling or an ancestor of
    conf.userkey. conf.userkey is the person visiting the base the search for
    ancestors is limited to 3 generations *)

type view = As_is | As_friend | As_visitor

val p_auth :
  ?view:view ->
  Config.config ->
  Geneweb_db.Driver.base ->
  Geneweb_db.Driver.person ->
  bool
(** [p_auth ?view conf base p] tells whether the data of [p] can be shown,
    [view] selecting whose rights are tested (default: the current user). It
    holds for a wizard, a friend outside semi-public mode or [p] himself; when
    [p] is public, titled with [public_if_titles], consenting or related in
    semi-public mode, or undated with [public_if_no_date] (the last two unless
    private); when [private_years] is negative; and when [p] was born or
    baptised more than [private_years] ago, died or was buried more than
    [private_years_death] ago, or married more than [private_years_marriage]
    ago. Without birth nor baptism date, any date of a child, the death or
    burial of a spouse more than [private_years] ago, or of a parent more than
    [private_years] + 1 years ago, gives the same proof. A date known only as
    "after" proves nothing; "or" and "between" dates count by their later year.
*)

val p_auth_sp :
  Config.config -> Geneweb_db.Driver.base -> Geneweb_db.Driver.person -> bool
(** returns p_auth or true if both user and p are SemiPublic *)

val wrap_output : Config.config -> Adef.safe_string -> (unit -> unit) -> unit
(** Display in a very basic HTML doc, with no CSS or JavaScript. *)
