val verbosity_level : int ref
(** Verbosity level: defines the verbosity level that will allow the [syslog]
    function to print anything.

    The default is [6]. *)

val debug_flag : bool ref
(** If set to [true], prints backtrace when printing log. *)

type output = Stdout | Stderr | Channel of out_channel

val set_output_channel : output -> unit
(** Set the output channel for logs. The default is [Stderr]. If the previous
    output was a file, it is properly closed. *)

type level =
  [ `LOG_EMERG  (** A panic condition. Print if [!verbosity_level >= 0]. *)
  | `LOG_ALERT
    (** A condition that should be corrected immediately, such as a corrupted
        system database. Print if [!verbosity_level >= 1]. *)
  | `LOG_CRIT
    (** Critical conditions, such as hard device errors. Print if
        [!verbosity_level >= 2]. *)
  | `LOG_ERR  (** Errors. Print if [!verbosity_level >= 3]. *)
  | `LOG_WARNING  (** Warning messages. Print if [!verbosity_level >= 4]. *)
  | `LOG_NOTICE
    (** Messages that contain information normally of use only when debugging a
        program. Print if [!verbosity_level >= 5]. *)
  | `LOG_INFO  (** Informational messages. Print if [!verbosity_level >= 6]. *)
  | `LOG_DEBUG
    (** Conditions that are not error conditions, but that may require special
        handling. Print if [!verbosity_level >= 7]. *) ]
(** The level of log gravity. See SYSLOG(3) *)

val syslog : level -> string -> unit
(** [syslog level msg] Prints [msg] on [!oc] depending on the verbosity. *)

(* TODO: The gwd serveur uses Syslog library for log management. This library
      is no longer maintained and is not the standard logging approach in
      OCaml. Logs is the recommend library for this purpose. In order
      to limit changes within a single PR, the following code wraps the Syslog
      API to provide an interface similar to Logs.
*)
type 'a msgf = (('a, Format.formatter, unit, unit) format4 -> 'a) -> unit

val info : 'a msgf -> unit
val debug : 'a msgf -> unit
val warn : 'a msgf -> unit
val err : 'a msgf -> unit
