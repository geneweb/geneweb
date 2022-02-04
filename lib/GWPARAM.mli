(** The level of log gravity. See SYSLOG(3) *)
type syslog_level =
    [ `LOG_EMERG                (** A panic condition. *)
    | `LOG_ALERT                (** A condition that should be corrected immediately,
                                    such as a corrupted system database. *)
    | `LOG_CRIT                 (** Critical conditions, such as hard device errors. *)
    | `LOG_ERR                  (** Errors. *)
    | `LOG_WARNING              (** Warning messages.  *)
    | `LOG_DEBUG                (** Conditions that are not error conditions,
                                    but that may require special handling. *)
    | `LOG_INFO                 (** Informational messages. *)
    | `LOG_NOTICE               (** Messages that contain information
                                    normally of use only when debugging a program.  *)
    ]

(* S: Move it to gwd_lib?  *)
(** Function called before gwd starts
    e.g. inititialise assets folders in Secure module. *)
val init : (unit -> unit) ref

(** [!base_path pref fname] function that returns a path to a file identified by [pref] [fname]
    related to bases. [pref] is like a category for file [fname].

    See {!val:GWPARAM.Default.base_path} for a concrete example.
*)
val base_path : (string list -> string -> string) ref

(** Same as {!val:base_path}, but without the prefix (avoid unecessary empty list). *)
val bpath : (string -> string) ref

(** [!output_error ?headers ?content conf status] default function that send the http status [status].
    Also send [headers] and use [content] (typically a HTML string describing the error) if provided.
*)
val output_error :
  (?headers:string list ->
   ?content:string -> Config.config -> Def.httpStatus -> unit)
  ref

(** Check if a person should be displayed or not *)
val p_auth : (Config.config -> Gwdb.base -> Gwdb.person -> bool) ref

(** [!syslog level log] log message [log] with gravity level [level] on stderr. *)
val syslog : (syslog_level -> string -> unit) ref

(** [wrap_output conf title content]
    Wrap the display of [title] and [content] in a defined template.
*)
val wrap_output : (Config.config -> string -> (unit -> unit) -> unit) ref

module Default : sig

  (** Inititialise assets directoris for gwd server:
      * current directory
      * /usr/share/geneweb  *)
  val init : (unit -> unit)

  (** Use concatenation of [Secure.base_dir ()], [pref] and [fname] *)
  val base_path : (string list -> string -> string)

  (** [Filename.concat (Secure.base_dir ())] *)
  val bpath : (string -> string)

  (** If [?content] is not set, sends page content from {/etc/<status-code>-<lang>.html}.
      If the current lang is not available, use `en` *)
  val output_error :
    (?headers:string list ->
     ?content:string -> Config.config -> Def.httpStatus -> unit)

  (** Calculate the access rights to the person's information in
      according to his age.
      Returns (in the order of the tests) :
      - `true` if requester is wizard or friend or person is public
      - `true` if person has at least one title and {i public_if_title}
        is set to {i yes} in gwf config file
      - `false` if person is alive and {i private_years} > 0
      - `true` if person is older (depending on the date of
        birth or baptism date) then {i privates_years}
      - `false` if person is younger (depending on the date of
        birth or baptism date) then {i privates_years}
      - `true` if person has been deceased for more than {i privates_years}
      - `false` if person has been deceased for less than {i privates_years}
      - `true` if person is between 80 and 120 years old and he is not beeing
        private and  {i public_if_no_date} is set to {i yes} in gwf config file
      - `true` if person has been married for more than {i private_years}
      - `false` otherwise
  *)
  val p_auth : (Config.config -> Gwdb.base -> Gwdb.person -> bool)

  (** Prints on stderr using `"[date]: level message"` format. *)
  val syslog : (syslog_level -> string -> unit)

  (** Display in a very basic HTML doc, with no CSS or JavaScript. *)
  val wrap_output : (Config.config -> string -> (unit -> unit) -> unit)

end
