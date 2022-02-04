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

(** Inititialise assets for gwd server (one in current directory one in /usr/share/geneweb) *)
val init : (unit -> unit) ref

(** [!base_path pref fname] default function that returns path to [fname] inside base directory where [pref]
    is a list of subdirectories between base directory and [fname]. *)
val base_path : (string list -> string -> string) ref

(** [!bpath fname] default function that returns path to [fname] inside base directory *)
val bpath : (string -> string) ref

(** [!output_error ?headers ?content conf status] default function that send the http status [status], [headers] and
    [content] if provided. Otherwise send default content from {/etc/<status-code>-<lang>.html} *)
val output_error :
  (?headers:string list ->
   ?content:string -> Config.config -> Def.httpStatus -> unit)
  ref

(** Calculate the access rights to the person's information in
    according to his age.
    Returns (in the order of the tests) :
    - True if : requester is wizard or friend or person is public
    - True if : person has at least one title and {i public_if_title}
                is set to {i yes} in gwf config file
    - False if : person is alive and {i private_years} > 0
    - True if : person is older (depending on the date of
                birth or baptism date) then {i privates_years}
    - False if : person is younger (depending on the date of
                birth or baptism date) then {i privates_years}
    - True if : person has been deceased for more than {i privates_years}
    - False if : person has been deceased for less than {i privates_years}
    - True if : person is between 80 and 120 years old and he is not beeing
                private and  {i public_if_no_date} is set to {i yes} in
                gwf config file
    - True if : person has been married for more than {i private_years}
    - False otherwise *)
val p_auth : (Config.config -> Gwdb.base -> Gwdb.person -> bool) ref

(** [!syslog level log] log message [log] with gravity level [level] on stderr. *)
val syslog : (syslog_level -> string -> unit) ref

val wrap_output :
  (Config.config -> string -> (unit -> unit) -> unit) ref
