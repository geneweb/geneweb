module Sites = Geneweb_sites.Sites
module GWPARAM = Geneweb.GWPARAM
module Version = Geneweb.Version
module Compat = Geneweb_compat
module Dirs = Geneweb_dirs
module C = Cmdliner

let ( // ) = Filename.concat

type log = Stdout | Stderr | File of string | Syslog
type plugin = { path : string; unsafe : bool; forced : bool; collection : bool }

type t = {
  (* Directories *)
  base_dir : string;
  socket_dir : string;
  gw_prefix : string;
  etc_prefix : string;
  images_prefix : string;
  images_dir : string;
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
  setup_link : bool;
  (* Plugins *)
  plugins : plugin list;
  (* Tracing & debugging *)
  debug : bool;
  check : bool;
  verbosity : int;
  log : log;
  trace_failed_password : bool;
}

(* Environment variables. *)
let var_lang =
  let doc =
    "Set the default language for the web interface. For more information, \
     refer to the --default-lang option."
  in
  C.Cmd.Env.info ~doc "LANG"

let var_gwd_slow_query_threshold =
  let doc = "Set the threshold for emitting warnings on slow requests." in
  C.Cmd.Env.info ~doc "GWD_SLOW_QUERY_THRESHOLD"

let var_secret_salt =
  let doc = "Internal variable used to transmit a secret salt to workers." in
  C.Cmd.Env.info ~doc "SECRET_SALT"

let var_wserver =
  let doc =
    "Internal variable used to recognize the main process on Windows."
  in
  C.Cmd.Env.info ~doc "WSERVER"

let var_query_string =
  let doc = "Internal variable used by the CGI mode." in
  C.Cmd.Env.info ~doc "QUERY_STRING"

let var_request_method =
  let doc = "Internal variable used by the CGI mode." in
  C.Cmd.Env.info ~doc "REQUEST_METHOD"

let var_remote_host =
  let doc = "Internal variable used by the CGI mode." in
  C.Cmd.Env.info ~doc "REMOTE_HOST"

let var_script_name =
  let doc = "Internal variable used by the CGI mode." in
  C.Cmd.Env.info ~doc "SCRIPT_NAME"

(* Helper functions to reject some options on non-UNIX platforms. *)

let unix_only_opt ~error ~default t =
  let open C.Term.Syntax in
  C.Term.ret
  @@
  let+ t = t in
  match t with
  | Some _ when not Sys.unix -> `Error (false, error)
  | Some x -> `Ok x
  | None -> `Ok default

let unix_only_flag ~error t =
  let open C.Term.Syntax in
  C.Term.ret
  @@
  let+ t = t in
  if t && not Sys.unix then `Error (false, error) else `Ok t

(* Custom parsers *)

let log_parser s =
  match s with
  | "-" | "<stdout>" -> Ok Stdout
  | "2" | "<stderr>" -> Ok Stderr
  | "<syslog>" -> Ok Syslog
  | _ -> Ok (File s)

let log_pp ppf l =
  match l with
  | Stdout -> Fmt.pf ppf "<stdout>"
  | Stderr -> Fmt.pf ppf "<stderr>"
  | Syslog -> Fmt.pf ppf "<syslog>"
  | File s -> Fmt.string ppf s

let log_conv = C.Arg.Conv.make ~docv:"LOG" ~parser:log_parser ~pp:log_pp ()
let error = Fmt.kstr (fun s -> Error s)

let pluginpath_parser s =
  match String.index_from s 0 ':' with
  | exception Not_found -> Ok (s, false, false)
  | offset -> (
      let len = String.length s in
      let postfix = String.sub s (offset + 1) (len - offset - 1) in
      match String.sub s 0 offset with
      | "u" -> Ok (postfix, true, false)
      | "f" -> Ok (postfix, false, true)
      | "uf" | "fu" -> Ok (postfix, true, true)
      | prefix -> error "Unexpected prefix %S" prefix)

let pluginpath_conv =
  let pp ppf (s, unsafe, forced) = Fmt.pf ppf "(%s, %b, %b)" s unsafe forced in
  C.Arg.Conv.make ~docv:"PLUGIN_PATH" ~parser:pluginpath_parser ~pp ()

(* Directories commands *)
let dirs_section = "DIRECTORIES"
let default_base_dir = Secure.default_base_dir
let default_socket_dir = ""

let default_gw_prefix =
  match Sites.hd with
  | s :: _ -> s
  | _ ->
      (* This case occurs if gwd hasn't been installed with dune. *)
      Filename.current_dir_name // "gw"

let default_images_prefix = default_gw_prefix // "images"
let default_etc_prefix = default_gw_prefix // "etc"
let default_images_dir = ""

let base_dir =
  let doc = "$(docv) is the directory where GeneWeb databases are stored." in
  C.Arg.(
    value
    & opt dirpath (Dirs.name default_base_dir)
    & info [ "bd"; "base-dir" ] ~docs:dirs_section ~doc)

let socket_dir =
  let doc =
    "$(docv) specifies where socket communication and access count are \n\
    \  stored on Windows."
  in
  C.Arg.(
    value
    & opt dirpath default_socket_dir
    & info [ "wd"; "socket-dir" ] ~docs:dirs_section ~doc)

let gw_prefix =
  let doc =
    "$(docv) specifies where \"etc\", \"images\" and \"lang\" directories are \
     installed."
  in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "hd"; "gw-prefix" ] ~docs:dirs_section ~doc)

let images_prefix =
  let doc = "$(docv) specifies where the \"images\" directory is installed." in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "images-prefix" ] ~docs:dirs_section ~doc)

let images_dir =
  let doc =
    "Same as --image-prefix but directory name relative to \n  current."
  in
  let deprecated = "Use `-images_prefix` instead." in
  C.Arg.(
    value
    & opt dirpath default_images_dir
    & info [ "images-dir" ] ~docs:dirs_section ~doc ~deprecated)

let etc_prefix =
  let doc =
    "$(docv) specifies where \"etc\" directory is installed. The default\n\
    \  is gw_prefix/etc."
  in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "etc-prefix" ] ~docs:dirs_section ~doc)

let parse_directories bd wd gw_prefix images_prefix etc_prefix images_dir =
  let images_prefix =
    match (gw_prefix, images_prefix) with
    | Some s, None -> s // "images"
    | _, Some s -> s
    | None, None -> default_images_prefix
  in
  let etc_prefix =
    match (gw_prefix, etc_prefix) with
    | Some s, None -> s // "etc"
    | _, Some s -> s
    | None, None -> default_etc_prefix
  in
  let gw_prefix = Option.value ~default:default_gw_prefix gw_prefix in
  (bd, wd, gw_prefix, images_prefix, etc_prefix, images_dir)

let directories =
  let open C.Term.Syntax in
  let+ base_dir = base_dir
  and+ socket_dir = socket_dir
  and+ gw_prefix = gw_prefix
  and+ images_prefix = images_prefix
  and+ etc_prefix = etc_prefix
  and+ images_dir = images_dir in
  parse_directories base_dir socket_dir gw_prefix images_prefix etc_prefix
    images_dir

(* Data management commands *)

let data_section = "DATA MANAGEMENT"

let cache_databases =
  let doc = "Load these databases in memory before starting the server." in
  C.Arg.(
    value
    & opt_all (list string) []
    & info [ "cache-database" ] ~docs:data_section ~doc)

let cache_databases =
  let open C.Term.Syntax in
  let+ cache_databases = cache_databases in
  List.concat cache_databases

let lexicon_files =
  let doc = "Add file $(docv) as lexicon." in
  C.Arg.(
    value
    & opt_all (list filepath) []
    & info [ "lexicon-file" ] ~docs:data_section ~doc)

let lexicon_files =
  let open C.Term.Syntax in
  let+ lexicon_files = lexicon_files in
  List.concat lexicon_files

let cache_langs =
  let doc = "Cached lexicon languages." in
  C.Arg.(
    value & opt (list string) [] & info [ "cache-lang" ] ~docs:data_section ~doc)

let particles_files =
  let doc = "Particles file path." in
  C.Arg.(
    value
    & opt (some filepath) None
    & info [ "particles-file" ] ~docs:data_section ~doc)

let no_lock =
  let doc = "Do not lock database files before writing." in
  C.Arg.(value & flag & info [ "no-lock" ] ~docs:data_section ~doc)

(* Security commands *)

let security_section = "SECURITY"
let default_login_timeout = 1800

let authorization_file =
  let doc =
    "$(docv) is an authorization file to restrict access. The file must hold \
     lines of the form 'user:password'."
  in
  C.Arg.(
    value
    & opt (some filepath) None
    & info [ "authorization-file" ] ~docs:security_section ~doc)

let login_timeout =
  let doc = "Login timeout for entries with passwords in CGI mode." in
  C.Arg.(
    value
    & opt int default_login_timeout
    & info [ "login-timeout" ] ~docs:security_section ~doc)

let predictable_mode =
  let doc =
    "Turn on the predictable mode. In this mode, the behavior of the server is \
     predictable, which is helpful for debugging or testing (UNIX only). This\n\
    \     option MUST not be used in production."
  in
  let error = "--predictable-mode is available only on UNIX." in
  C.Arg.(
    unix_only_flag ~error & value & flag
    & info [ "predictable-mode" ] ~docs:security_section ~doc)

let secret_salt =
  let doc = "Add a secret salt to form digests." in
  C.Arg.(
    value
    & opt (some string) None
    & info [ "secret-salt" ] ~docs:security_section ~doc)

let wizard_just_friend =
  let doc = "Wizard just friend permanetly." in
  C.Arg.(
    value & flag
    & info [ "wjf"; "wizard-just-friend" ] ~docs:security_section ~doc)

let wizard_password =
  let doc =
    "Set a password to grant administrative and editing rights \n\
    \  via the web interface."
  in
  C.Arg.(
    value
    & opt (some string) None
    & info [ "wizard-password" ] ~docs:security_section ~doc)

let friend_password =
  let doc = "Set a wizard password." in
  C.Arg.(
    value
    & opt (some string) None
    & info [ "friend-password" ] ~docs:security_section ~doc)

let digest_password =
  let doc =
    "Enables HTTP digest authentification schema instead of plain \n\
    \  schema. This feature is not compatible with the CGI mode."
  in
  C.Arg.(value & flag & info [ "digest-password" ] ~docs:security_section ~doc)

let allowed_tags_file =
  let doc = "File of allowed HTML tags. One tag par line in the file." in
  C.Arg.(
    value
    & opt (some filepath) None
    & info [ "allowed-tags-file" ] ~docs:security_section ~doc)

let allowed_addresses =
  let doc = "A whitelist of IP addresses allowed to connect to the server." in
  C.Arg.(
    value
    & opt (list string) []
    & info [ "allowed-address" ] ~docs:security_section ~doc)

let no_reverse_host =
  let doc = "Force no reverse host by address." in
  C.Arg.(value & flag & info [ "no-reverse-host" ] ~docs:security_section ~doc)

let ban_threshold =
  let doc =
    "Bans IP addresses making more than $(docv) requests per second. Set \n\
    \  to 0 to disable."
  in
  C.Arg.(
    value & opt int 0 & info [ "ban-threshold" ] ~docs:security_section ~doc)

let min_disp_req =
  let doc = "Set minimum traced requests by robot to $(docv)." in
  C.Arg.(
    value & opt int 0 & info [ "min-disp-req" ] ~docs:security_section ~doc)

(* HTTP server commands *)

let http_section = "HTTP SERVER"
let default_port = 2317
let default_connection_timeout = 120
let default_max_pending_requests = 150
let default_n_workers = 20

let interface =
  let doc = "Bind the HTTP server to the network interface $(docv)." in
  C.Arg.(
    value
    & opt (some string) None
    & info [ "i"; "interface" ] ~docs:http_section ~docv:"INTERFACE" ~doc)

let redirect_interface =
  let doc = "Send a message to say that this service has been redirected." in
  C.Arg.(
    value
    & opt (some string) None
    & info
        [ "redirect-interface"; "redirect" ]
        ~docs:http_section ~docv:"INTERFACE" ~doc)

let port =
  let doc = "Set the TCP port listen by the HTTP server to $(docv)." in
  C.Arg.(
    value & opt int default_port
    & info [ "p"; "port" ] ~docs:http_section ~docv:"PORT" ~doc)

let connection_timeout =
  let doc = "Connection timeout (UNIX only)" in
  let error = "--connection-timeout is available only on UNIX." in
  C.Arg.(
    unix_only_opt ~error ~default:default_connection_timeout
    & value
    & opt (some int) None
    & info [ "connection-timeout" ] ~docs:http_section ~doc)

let max_pending_requests =
  let doc = "Maximum number of pending requests handled by the server." in
  let error = "--max-pending-requests is available only on UNIX." in
  C.Arg.(
    unix_only_opt ~error ~default:default_max_pending_requests
    & value
    & opt (some int) None
    & info [ "max-pending-requests" ] ~docs:http_section ~doc)

let max_clients =
  let doc = "Maximum number of clients treated at the same time (UNIX only)." in
  let error = "--max-clients is available only on UNIX." in
  let deprecated =
    "No effect. Use `--n-workers` and  `--max-pending-requests` instead."
  in
  C.Arg.(
    unix_only_opt ~error ~default:0
    & value
    & opt (some int) None
    & info [ "max-clients" ] ~docs:http_section ~doc ~deprecated)

let n_workers =
  let doc =
    "$(docv) is the number of workers available to process \n\
    \  incoming HTTP requests (UNIX only)."
  in
  let error = "--n-workers is available only on UNIX." in
  C.Arg.(
    unix_only_opt ~error ~default:default_n_workers
    & value
    & opt (some int) None
    & info [ "n-workers" ] ~docs:http_section ~doc)

let cgi =
  let doc = "Force the server to behave as a CGI script." in
  C.Arg.(value & flag & info [ "cgi" ] ~docs:http_section ~doc)

let daemon =
  let doc = "Run the process in the background (UNIX only)." in
  C.Arg.(value & flag & info [ "daemon" ] ~docs:http_section ~doc)

(* Web interface commands *)

let web_interface_section = "WEB INTERFACE"
let default_default_lang = "fr"

let default_lang =
  let doc =
    "Set the fallback language for the user interface if no \n\
    \  language is specified."
  in
  C.Arg.(
    value
    & opt string default_default_lang
    & info [ "default-lang" ] ~env:var_lang ~docs:web_interface_section ~doc)

let browser_lang =
  let doc =
    "Select the user interface language based on the client\n  configuration."
  in
  C.Arg.(
    value & flag & info [ "browser-lang" ] ~docs:web_interface_section ~doc)

let setup_link =
  let doc =
    "Display a shortcut link at the bottom of the pages to gwsetup tool."
  in
  C.Arg.(
    value & flag
    & info [ "setup-link" ] ~docs:web_interface_section ~docv:"URL" ~doc)

(* Plugin commands *)

let plugin_section = "PLUGIN"

let plugin =
  let doc =
    "Specify where the “bases” directory with databases is installed."
  in
  C.Arg.(
    value
    & opt_all (list pluginpath_conv) []
    & info [ "plugin" ] ~docs:plugin_section ~doc)

let plugins =
  let doc =
    "Specify where the “bases” directory with databases is installed."
  in
  C.Arg.(
    value
    & opt_all (list pluginpath_conv) []
    & info [ "plugins" ] ~docs:plugin_section ~doc)

let plugins =
  let open C.Term.Syntax in
  let+ plugin = plugin and+ plugins = plugins in
  let acc =
    List.fold_left
      (fun acc (path, unsafe, forced) ->
        { path; unsafe; forced; collection = false } :: acc)
      [] (List.concat plugin)
  in
  let acc =
    List.fold_left
      (fun acc (path, unsafe, forced) ->
        { path; unsafe; forced; collection = true } :: acc)
      acc (List.concat plugins)
  in
  List.rev acc

(* Tracing & debugging commands *)

let tracing_section = "TRACING & DEBUGGING"
let default_verbosity = 6
let default_log = Stderr

let debug =
  let doc =
    "Enable debug mode. Provides more verbose output and traces. The option \n\
    \  turns predicatable mode on."
  in
  C.Arg.(value & flag & info [ "d"; "debug" ] ~docs:tracing_section ~doc)

let check =
  let doc =
    " Run only the server startup sequence for test purpose. This flag implies \
     -debug."
  in
  C.Arg.(value & flag & info [ "check" ] ~docs:tracing_section ~doc)

let verbosity =
  let doc =
    "Adjust the level of logging detail to $(docv). Higher values provide\n\
    \  more details."
  in
  C.Arg.(
    value & opt int default_verbosity
    & info [ "verbosity" ] ~docs:tracing_section ~doc)

let log =
  let doc = "Log trace to a file or a socket." in
  C.Arg.(
    value & opt log_conv default_log & info [ "log" ] ~docs:tracing_section ~doc)

let trace_failed_password =
  let doc = "Trace failed authentification attempts." in
  C.Arg.(
    value & flag & info [ "trace-failed-password" ] ~docs:tracing_section ~doc)

let no_fork =
  let doc = "Prevent from forking processes (only UNIX)." in
  let error = "--no-fork is available only on UNIX." in
  let deprecated = "No effect. Use `-n_workers 0` instead." in
  C.Arg.(
    unix_only_flag ~error & value & flag
    & info [ "no-fork" ] ~docs:tracing_section ~doc ~deprecated)

(* TODO: Remove this option after switching to the new CLI. *)
let noop =
  let doc = "Internal option. DO NOT USE" in
  C.Arg.(value & flag & info [ "noop" ] ~docs:tracing_section ~doc)

let t =
  let open C.Term.Syntax in
  let doc = "Geneweb daemon" in
  let envs =
    [
      var_secret_salt;
      var_gwd_slow_query_threshold;
      var_wserver;
      var_query_string;
      var_request_method;
      var_remote_host;
      var_script_name;
    ]
  in
  C.Cmd.make (C.Cmd.info "gwd" ~envs ~version:Version.ver ~doc)
  @@
  let+ base_dir, socket_dir, gw_prefix, images_prefix, etc_prefix, images_dir =
    directories
  and+ cache_databases = cache_databases
  and+ lexicon_files = lexicon_files
  and+ cache_langs = cache_langs
  and+ particles_files = particles_files
  and+ no_lock = no_lock
  and+ authorization_file = authorization_file
  and+ digest_password = digest_password
  and+ login_timeout = login_timeout
  and+ predictable_mode = predictable_mode
  and+ secret_salt = secret_salt
  and+ wizard_just_friend = wizard_just_friend
  and+ wizard_password = wizard_password
  and+ friend_password = friend_password
  and+ allowed_tags_file = allowed_tags_file
  and+ no_reverse_host = no_reverse_host
  and+ allowed_addresses = allowed_addresses
  and+ ban_threshold = ban_threshold
  and+ min_disp_req = min_disp_req
  and+ interface = interface
  and+ redirect_interface = redirect_interface
  and+ port = port
  and+ connection_timeout = connection_timeout
  and+ max_pending_requests = max_pending_requests
  and+ _ : int = max_clients
  and+ n_workers = n_workers
  and+ cgi = cgi
  and+ daemon = daemon
  and+ default_lang = default_lang
  and+ browser_lang = browser_lang
  and+ setup_link = setup_link
  and+ plugins = plugins
  and+ debug = debug
  and+ check = check
  and+ verbosity = verbosity
  and+ log = log
  and+ trace_failed_password = trace_failed_password
  and+ _ : bool = no_fork
  and+ _ : bool = noop in
  {
    base_dir;
    socket_dir;
    gw_prefix;
    images_prefix;
    images_dir;
    etc_prefix;
    cache_databases;
    lexicon_files;
    cache_langs;
    particles_files;
    no_lock;
    authorization_file;
    login_timeout;
    predictable_mode;
    secret_salt;
    wizard_just_friend;
    wizard_password;
    friend_password;
    digest_password;
    allowed_tags_file;
    allowed_addresses;
    no_reverse_host;
    ban_threshold;
    min_disp_req;
    interface;
    redirect_interface;
    port;
    connection_timeout;
    max_pending_requests;
    n_workers;
    cgi;
    daemon;
    default_lang;
    browser_lang;
    setup_link;
    plugins;
    debug;
    check;
    verbosity;
    log;
    trace_failed_password;
  }

let legacy_arguments =
  [
    ("-a", "-i");
    ("-add_lexicon", "--lexicon-file");
    ("-allowed_tags", "--allowed-tags-file");
    ("-auth", "--authorization-file");
    ("-bd", "--bd");
    ("-blang", "--browser-lang");
    ("-daemon", "--daemon");
    ("-debug", "--debug");
    ("-digest", "--digest-password");
    ("-cache-in-memory", "--cache-database");
    ("-cache_langs", "--cache-langs");
    ("-cgi", "--cgi");
    ("-cgi_secret_salt", "--secret-salt");
    ("-conn_tmout", "--connection-timeout");
    ("-etc_prefix", "--etc-prefix");
    ("-friend", "--friend-password");
    ("-help", "--help");
    ("-hd", "--gw-prefix");
    ("-images_prefix", "--images-prefix");
    ("-images_dir", "--images-dir");
    ("-lang", "--default-lang");
    ("-log", "--log");
    ("-log_level", "--verbosity");
    ("-login_tmout", "--login-timeout");
    ("-max_clients", "--max-clients");
    ("-max_pending_requests", "--max-pending-requests");
    ("-min_disp_req", "--min-disp-req");
    ("-no_host_address", "--no-reverse-host");
    ("-no-fork", "--no-fork");
    ("-nolock", "--no-lock");
    ("-n_workers", "--n-workers");
    ("-only", "--allowed-address");
    ("-p", "--port");
    ("-particles", "--particles-file");
    ("-plugin", "--plugin");
    ("-plugins", "--plugin");
    ("-redirect", "--redirect-interface");
    ("-robot_xcl", "--ban-threshold");
    ("-setup_link", "--setup-link");
    ("-trace_failed_passwd", "--trace-failed-password");
    ("-version", "--version");
    ("-wd", "--socket-dir");
    ("-wizard", "--wizard-password");
    ("-wjf", "--wizard-just-friend");
  ]

let noop_arguments = [ ("-unsafe", ()); ("-force", ()) ]

let preprocess_legacy_arguments =
  let legacy_tbl : (string, string) Hashtbl.t =
    Hashtbl.of_seq @@ List.to_seq legacy_arguments
  in
  let noop_tbl : (string, unit) Hashtbl.t =
    Hashtbl.of_seq @@ List.to_seq noop_arguments
  in
  Array.map (fun a ->
      match Hashtbl.find legacy_tbl a with
      | new_ ->
          Fmt.epr "The CLI option %S is deprecated. Please use %S instead.@." a
            new_;
          new_
      | exception Not_found -> (
          match Hashtbl.find noop_tbl a with
          | () ->
              Fmt.epr
                "The CLI option %S is noop. Please refer to the documentation \
                 for more details.@."
                a;
              "--noop"
          | exception Not_found -> a))

let arguments_in_file file =
  Compat.In_channel.with_open_text file @@ fun ic ->
  let rec loop acc =
    match input_line ic with
    | exception End_of_file -> List.rev acc
    | l -> loop (l :: acc)
  in
  Array.of_list @@ loop []

let parse ?file () =
  let argv_file =
    match file with Some f -> arguments_in_file f | None -> [||]
  in
  let argv =
    let l1 = Array.length argv_file in
    let l2 = Array.length Sys.argv in
    Array.init (l1 + l2) (fun i ->
        if i = 0 then Sys.argv.(0)
        else if i <= l1 then argv_file.(i - 1)
        else Sys.argv.(i - l1))
  in
  let argv = preprocess_legacy_arguments argv in
  Cmdliner.Cmd.eval_value' ~argv t
