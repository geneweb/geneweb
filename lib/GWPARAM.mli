type syslog_level =
  [ `LOG_EMERG  (** A panic condition. *)
  | `LOG_ALERT
    (** A condition that should be corrected immediately, such as a corrupted
        system database. *)
  | `LOG_CRIT  (** Critical conditions, such as hard device errors. *)
  | `LOG_ERR  (** Errors. *)
  | `LOG_WARNING  (** Warning messages. *)
  | `LOG_DEBUG
    (** Conditions that are not error conditions, but that may require special
        handling. *)
  | `LOG_INFO  (** Informational messages. *)
  | `LOG_NOTICE
    (** Messages that contain information normally of use only when debugging a
        program. *) ]
(** The level of log gravity. See SYSLOG(3) *)

(* S: Move it to gwd_lib?  *)

val init : unit -> unit
(** Function called before gwd starts e.g. inititialise assets folders in Secure
    module. *)

val base_path : string list -> string -> string
(** [base_path pref fname] function that returns a path to a file identified by
    [pref] [fname] related to bases. [pref] is like a category for file [fname].

    See {!val:GWPARAM.Default.base_path} for a concrete example. *)

val bpath : string -> string
(** Same as {!val:base_path}, but without the prefix (avoid unecessary empty
    list). *)

val output_error :
  ?headers:string list ->
  ?content:Adef.safe_string ->
  Config.config ->
  Def.httpStatus ->
  unit

(** [output_error ?headers ?content conf status] default function that send the
    http status [status]. Also send [headers] and use [content] (typically a
    HTML string describing the error) if provided. *)

val syslog : syslog_level -> string -> unit
(** [syslog level log] log message [log] with gravity level [level] on stderr.
*)

val wrap_output : Config.config -> Adef.safe_string -> (unit -> unit) -> unit
(** [wrap_output conf title content] Wrap the display of [title] and [content]
    in a defined template. *)

val has_ignored_duplicates : Config.config -> Gwdb.base -> bool
val set_init : (unit -> unit) -> unit
val set_base_path : (string list -> string -> string) -> unit
val set_bpath : (string -> string) -> unit

val set_output_error :
  (?headers:string list ->
  ?content:Adef.safe_string ->
  Config.config ->
  Def.httpStatus ->
  unit) ->
  unit

val set_syslog : (syslog_level -> string -> unit) -> unit

val set_wrap_output :
  (Config.config -> Adef.safe_string -> (unit -> unit) -> unit) -> unit

val set_has_ignored_duplicates : (Config.config -> Gwdb.base -> bool) -> unit
