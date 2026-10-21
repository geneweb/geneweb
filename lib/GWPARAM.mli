val nb_errors : int ref
val errors_undef : string list ref
val errors_other : string list ref
val set_vars : string list ref
val gwd_cmd : string ref

val reorg : bool ref
(** Set by the option [-reorg] of gwc and ged2gwb: the bases they create are in
    the reorg layout. *)

val force : bool ref
(** force creation of database if already existing *)

val bpath : string -> string
(** [bpath bname] is the directory of the database [bname],
    [<bases_dir>/<bname>.gwb], in both layouts.
    @raise Invalid_argument if [bname] is not a valid name ({!Mutil.good_name}).
*)

val bname_of_string : string -> string option
(** [bname_of_string s] is the name of the database given as [s] on a command
    line, without its [.gwb] suffix, if this name is not empty and valid
    ({!Mutil.good_name}). *)

val bname_of_arg : prog:string -> string -> string
(** [bname_of_arg ~prog s] is [bname_of_string s] for an argument of the command
    [prog]. If [s] does not name a database, it prints an error and exits with
    code 2. *)

val cache_d : string -> string
(** [cache_d bname] Directory of the binary caches of base [bname], written by
    cache_files and read by chk_data: bases/bname.gwb/caches, whatever the base
    format, next to the caches of the cousins tool. Unlike
    bases/etc/bname/cache, which holds the .gz datalists that browsers download,
    it is never served. *)

val adm_dir : unit -> string
(** [adm_dir ()] is the directory of the files of the server, shared by all the
    bases: [<bases_dir>/cnt] (lock, tokens, keys, [STOP_SERVER]). *)

val adm_file : string -> string
(** [adm_file file] is the file [file] of {!adm_dir}. *)

val init : unit -> unit
(** Adds the current directory to the assets ({!Secure.add_assets}). *)

val base_config_dir : string -> string
(** [base_config_dir bname] is [<bases>/<bname>.gwb/config], whatever the mode
    (reorg or classic), for per-base working files such as the anomalies report
    and its whitelist. The directory alone does not make the base reorg: only
    [config/<bname>.gwf] does. Requires [Secure.set_bases_dir]. *)

val is_reorg_base : string -> bool
(** {!Layout.is_reorg_base}. *)

val check_base_exists : string -> unit
(** Check if database exists and exit if so (unless -f flag is set) *)

val create_base_and_config : string -> string
(** Create base directory and configuration. *)

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
