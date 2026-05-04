module Util = Geneweb.Util
module Version = Geneweb.Version
module Gutil = Geneweb_db.Gutil
module Dirs = Geneweb_dirs

let deprecated_warning_images_dir () =
  Logs.warn (fun k ->
      k
        "The `-images_dir` option is deprecated and may be removed in a future \
         release.@ Use `-images_prefix` instead.")

let deprecated_warning_max_clients () =
  Logs.warn (fun k ->
      k
        "The `-max_clients` option is deprecated and may be removed in a \
         future release.@ It has no effect.@ Use `-n_workers` and\n\
        \    `-max_pending_requests` instead.")

let deprecated_warning_no_fork () =
  Logs.warn (fun k ->
      k
        "The `-no-fork` option is deprecated and may be removed in a future \
         release.@ To achieve the same behavior, use `-n_workers 0` instead.")

let ( // ) = Filename.concat
let gw_prefix : string option ref = ref None
let set_gw_prefix s = gw_prefix := Some s
let images_prefix : string option ref = ref None
let set_images_prefix s = images_prefix := Some s
let etc_prefix : string option ref = ref None
let set_etc_prefix s = etc_prefix := Some s
let socket_dir : string option ref = ref None
let set_socket_dir s = socket_dir := Some s
let auth_file : string option ref = ref None
let cache_langs : string list ref = ref []
let cache_databases : string list ref = ref []
let conn_timeout = ref Cmd.default_connection_timeout
let daemon = ref false
let friend_passwd : string option ref = ref None
let default_lang = ref Cmd.default_default_lang
let images_dir = ref Cmd.default_images_dir
let lexicon_list : string list ref = ref []
let login_timeout = ref Cmd.default_login_timeout
let n_workers = ref Cmd.default_n_workers
let max_pending_requests = ref Cmd.default_max_pending_requests
let no_host_address = ref false
let only_addresses : string list ref = ref []
let plugins : Cmd.plugin list ref = ref []
let redirected_addr : string option ref = ref None
let robot_xcl : (int * int) option ref = ref None
let selected_addr : string option ref = ref None
let selected_port = ref Cmd.default_port
let setup_link = ref false
let trace_failed_passwd = ref false
let debug = ref false
let check = ref false

(* FIXME: This option must be turn on by default but it seems to be
   incompatible with CGI mode. *)
let digest_password = ref false
let wizard_just_friend = ref false
let wizard_passwd : string option ref = ref None
let predictable_mode = ref false
let log_file : Cmd.log ref = ref Cmd.Stderr
let verbosity_level = ref Cmd.default_verbosity
let set_verbosity_level lvl = verbosity_level := lvl
let force_cgi = ref false
let cgi_secret_salt : string option ref = ref None
