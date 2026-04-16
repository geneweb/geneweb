type log = Stdout | Stderr | File of string | Syslog

type plugin = {
  path : string;  (** Filesystem path to the plugin's directory. *)
  unsafe : bool;
      (** If [true], bypasses the checksum verification before loading. *)
  forced : bool;
      (** If [true], the plugin is loaded globally for all databases. *)
  collection : bool;
      (** If [true], [path] is treated as a parent directory containing multiple
          sub-plugins to be discovered and loaded. If [false], [path] points
          directly to a single plugin. *)
}

type connection = {
  interface : string option;
  port : int;
  timeout : float;
  max_requests : int;
}

type t = {
  (* Directories *)
  base_dir : string;
  socket_dir : string option;
  gw_prefix : string;
  etc_prefix : string;
  images_prefix : string;
  images_dir : string;
  index_dir : string;
  (* Data management *)
  cache_databases : string list;
  lexicon_files : string list;
  cache_langs : string list;
  particles_file : string option;
  no_lock : bool;
  (* Security *)
  authorization_file : string option;
  login_timeout : int;
  predictable_mode : bool;
  secret_salt : string option;
  wizard_just_friend : bool;
  wizard_password : string option;
  friend_password : string option;
  digest_password : bool;
  allowed_tags_file : string option;
  allowed_addresses : string list;
  no_reverse_host : bool;
  ban_threshold : (int * int) option;
  min_disp_req : int;
  tls : (string * string) option;
  (* HTTP server *)
  http_connection : connection;
  redirect_interface : string option;
  n_workers : int;
  cgi : bool;
  daemon : bool;
  (* RPC server *)
  rpc : bool;
  rpc_connection : connection;
  index_fuel : int;
  task_timeout : float;
  (* Web interface *)
  default_lang : string;
  browser_lang : bool;
  setup_link : bool;
  (* Plugin *)
  plugins : plugin list;
  (* Tracing & debugging *)
  debug : bool;
  check : bool;
  verbosity : int;
  log : log;
  trace_failed_password : bool;
}

val parse : unit -> t Cmdliner.Cmd.eval_exit
(** [parse ()] parses the [argv] and produces a structure of type [t] or an
    error. *)

(* These default values and parsers are exposed in order to be used by
   the legacy CLI. *)

val default_images_prefix : string
val default_images_dir : string
val default_etc_prefix : string
val default_http_timeout : float
val default_login_timeout : int
val default_default_lang : string
val default_verbosity : int
val default_http_max_requests : int
val default_n_workers : int
val default_http_port : int
val log_parser : string -> (log, string) result
