type syslog_level =
    [ `LOG_ALERT
    | `LOG_CRIT
    | `LOG_DEBUG
    | `LOG_EMERG
    | `LOG_ERR
    | `LOG_INFO
    | `LOG_NOTICE
    | `LOG_WARNING ]

(** Inititialise assets for gwd server (one in current directory one in /usr/share/geneweb) *)
val init : (unit -> unit) ref

(** [!base_path pref fname] default function that returns path to [fname] inside base directory where [pref] 
    is a list of subdirectories between base directory and [fname]. *)
val base_path : (string list -> string -> string) ref

(** [!bpath fname] default function that returns path to [fname] inside base directory *)
val bpath : (string -> string) ref
val output_error :
  (?headers:string list ->
   ?content:string -> Config.config -> Def.httpStatus -> unit)
  ref
val p_auth : (Config.config -> Gwdb.base -> Gwdb.person -> bool) ref
val syslog : (syslog_level -> string -> unit) ref
val wrap_output :
  (Config.config -> string -> (unit -> unit) -> unit) ref