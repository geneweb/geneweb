type log = Stdout | Stderr | File of string | Syslog
type opt = Safe | Unsafe | Force
type path = File of string | Dir of string
type plugin = { path : path; opts : opt list }

type t = {
  (* Directories *)
  base_dir : string;
  socket_dir : string;
  gw_prefix : string;
  etc_prefix : string;
  images_prefix : string;
  (* Data management *)
  cache_databases : string list;
  lexicon_files : string list;
  cache_langs : string list;
  particles_files : string option;
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
  ban_threshold : int;
  min_disp_req : int;
  (* HTTP server *)
  interface : string option;
  redirect_interface : string option;
  port : int;
  connection_timeout : int;
  max_pending_requests : int;
  n_workers : int;
  cgi : bool;
  daemon : bool;
  (* Web interface *)
  default_lang : string;
  browser_lang : bool;
  setup_link : string option;
  (* Plugin *)
  plugins : plugin list;
  (* Tracing & debugging *)
  debug : bool;
  verbosity : int;
  log : log;
  trace_failed_password : bool;
}

val parse : ?file:string -> unit -> t Cmdliner.Cmd.eval_exit
(** [parse ?file ()] parses the [argv] and produces a structure of type [t] or
    an error. *)

(* These default values and parsers are exposed in order to be used by 
   the legacy CLI. *)

val default_gw_prefix : string
val default_images_prefix : string
val default_base_dir : string
val default_socket_dir : string
val default_etc_prefix : string
val default_connection_timeout : int
val default_login_timeout : int
val default_default_lang : string
val default_verbosity : int
val default_max_pending_requests : int
val default_n_workers : int
val default_port : int
val log_parser : string -> (log, string) result

val parse_directories :
  string ->
  string ->
  string option ->
  string option ->
  string option ->
  string * string * string * string * string
