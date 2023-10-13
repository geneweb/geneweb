val verbosity : int ref
(** Verbosity level: defines the verbosity level that will
    allow the [syslog] function to print anything. *)

val debug : bool ref
(** If set to [true], prints backtrace when printing log. *)

val oc : out_channel option ref
(** The output channel in which log is written. *)

val log : (out_channel -> unit) -> unit
(** Prints on [oc] *)

type level =
  [ `LOG_EMERG  (** Print if `!verbosity >= 0` *)
  | `LOG_ALERT  (** Print if `!verbosity >= 1` *)
  | `LOG_CRIT  (** Print if `!verbosity >= 2` *)
  | `LOG_ERR  (** Print if `!verbosity >= 3` *)
  | `LOG_WARNING  (** Print if `!verbosity >= 4` *)
  | `LOG_NOTICE  (** Print if `!verbosity >= 5` *)
  | `LOG_INFO  (** Print if `!verbosity >= 6` *)
  | `LOG_DEBUG  (** Print if `!verbosity >= 7` *) ]
(** The level of log. *)

val syslog : level -> string -> unit
(** [syslog level msg]
    Prints [msg] on [!oc] depending on the verbosity. *)
