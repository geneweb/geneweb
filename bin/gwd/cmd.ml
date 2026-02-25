module Sites = Geneweb_sites.Sites
module GWPARAM = Geneweb.GWPARAM
module Version = Geneweb.Version
module C = Cmdliner

let ( // ) = Filename.concat

type log = Stdout | Stderr | File of string | Syslog
type kind = Safe | Unsafe
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
  (* Plugins *)
  plugins : plugin list;
  (* Tracing & debugging *)
  debug : bool;
  verbosity : int;
  log : log;
  trace_failed_password : bool;
}

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

let base_dir =
  let doc = "$(docv) is the directory where GeneWeb databases are stored." in
  C.Arg.(
    value
    & opt dirpath default_base_dir
    & info [ "bd"; "base-dir" ] ~docs:dirs_section ~docv:"PATH" ~doc)

let socket_dir =
  let doc =
    "$(docv) specifies where socket communication and access count are \n\
    \  stored on Windows."
  in
  C.Arg.(
    value
    & opt dirpath default_socket_dir
    & info [ "wd"; "socket-dir" ] ~docs:dirs_section ~docv:"PATH" ~doc)

let gw_prefix =
  let doc =
    "$(docv) specifies where \"etc\", \"images\" and \"lang\" directories are \
     installed."
  in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "hd"; "gw-prefix" ] ~docs:dirs_section ~docv:"PATH" ~doc)

let images_prefix =
  let doc = "$(docv) specifies where the \"images\" directory is installed." in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "images-prefix" ] ~docs:dirs_section ~docv:"PATH" ~doc)

let etc_prefix =
  let doc =
    "$(docv) specifies where \"etc\" directory is installed. The default\n\
    \  is gw_prefix/etc."
  in
  C.Arg.(
    value
    & opt (some dirpath) None
    & info [ "etc-prefix" ] ~docs:dirs_section ~docv:"PATH" ~doc)

let parse_directories bd wd gw_prefix images_prefix etc_prefix =
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
  (bd, wd, gw_prefix, images_prefix, etc_prefix)

let directories =
  let open C.Term.Syntax in
  let+ base_dir = base_dir
  and+ socket_dir = socket_dir
  and+ gw_prefix = gw_prefix
  and+ images_prefix = images_prefix
  and+ etc_prefix = etc_prefix in
  parse_directories base_dir socket_dir gw_prefix images_prefix etc_prefix

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
    & info [ "lexicon-file" ] ~docv:"FILE" ~docs:data_section ~doc)

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
    & info [ "authorization-file" ] ~docs:security_section ~docv:"PATH" ~doc)

let login_timeout =
  let doc = "" in
  C.Arg.(
    value
    & opt int default_login_timeout
    & info [ "login-timeout" ] ~docs:security_section ~docv:"FLOAT" ~doc)

let predictable_mode =
  let doc =
    "Turn on the predictable mode. In this mode, the behavior of the server is \
     predictable, which is helpful for debugging or testing."
  in
  C.Arg.(value & flag & info [ "predictable-mode" ] ~docs:security_section ~doc)

let secret_salt =
  let doc = "" in
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
    & info [ "allowed-address" ] ~docs:security_section ~docv:"FILES" ~doc)

let no_reverse_host =
  let doc = "Force no reverse host by address." in
  C.Arg.(value & flag & info [ "no-reverse-host" ] ~docs:security_section ~doc)

let ban_threshold =
  let doc =
    "Bans IP addresses making more than $(docv) requests per second. Set \n\
    \  to 0 to disable."
  in
  C.Arg.(
    value & opt int 0
    & info [ "ban-threshold" ] ~docs:security_section ~docv:"INT" ~doc)

let min_disp_req =
  let doc = "Set minimum traced requests by robot to $(docv)." in
  C.Arg.(
    value & opt int 0
    & info [ "min-disp-req" ] ~docs:security_section ~docv:"INT" ~doc)

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
  let doc = "" in
  C.Arg.(
    value
    & opt int default_connection_timeout
    & info [ "connection-timeout" ] ~docs:http_section ~docv:"INT" ~doc)

let max_pending_requests =
  let doc = "" in
  C.Arg.(
    value
    & opt int default_max_pending_requests
    & info [ "max-pending-requests" ] ~docs:http_section ~docv:"INT" ~doc)

let n_workers =
  let doc =
    "$(docv) is the number of workers available to process \n\
    \  incoming HTTP requests."
  in
  C.Arg.(
    value & opt int default_n_workers
    & info [ "n-workers" ] ~docs:http_section ~docv:"INT" ~doc)

let cgi =
  let doc = "Force the server to behave as a CGI script." in
  C.Arg.(value & flag & info [ "cgi" ] ~docs:http_section ~doc)

let daemon =
  let doc = "Run the process in the background (UNIX only)." in
  C.Arg.(value & flag & info [ "daemon" ] ~docs:http_section ~docv:"INT" ~doc)

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
    & info [ "default-lang" ] ~docs:web_interface_section ~doc)

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
    value
    & opt (some string) None
    & info [ "setup-link" ] ~docs:web_interface_section ~docv:"URL" ~doc)

(* Plugin commands *)

let plugin_section = "PLUGIN"

let plugin_files =
  let doc =
    "Specify where the “bases” directory with databases is installed."
  in
  C.Arg.(
    value
    & opt_all (list filepath) []
    & info [ "plugin-file" ] ~docs:plugin_section ~docv:"PATH" ~doc)

let plugin_dirs =
  let doc =
    "Specify where the “bases” directory with databases is installed."
  in
  C.Arg.(
    value
    & opt_all (list dirpath) []
    & info [ "plugin-dir" ] ~docs:plugin_section ~docv:"PATH" ~doc)

let plugins =
  let open C.Term.Syntax in
  let+ plugin_files = plugin_files and+ plugin_dirs = plugin_dirs in
  let acc =
    List.fold_left
      (fun acc file -> { path = File file; opts = [ Safe ] } :: acc)
      [] (List.concat plugin_files)
  in
  let acc =
    List.fold_left
      (fun acc dir -> { path = Dir dir; opts = [ Safe ] } :: acc)
      acc (List.concat plugin_dirs)
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

let verbosity =
  let doc =
    "Adjust the level of logging detail to $(docv). Higher values provide\n\
    \  more details."
  in
  C.Arg.(
    value & opt int default_verbosity
    & info [ "verbosity" ] ~docs:tracing_section ~docv:"INT" ~doc)

let log =
  let doc = "" in
  C.Arg.(
    value & opt log_conv default_log & info [ "log" ] ~docs:tracing_section ~doc)

let trace_failed_password =
  let doc = "" in
  C.Arg.(
    value & flag & info [ "trace-failed-password" ] ~docs:tracing_section ~doc)

let t =
  let open C.Term.Syntax in
  let doc = "Geneweb daemon" in
  C.Cmd.make (C.Cmd.info "gwd" ~version:Version.ver ~doc)
  @@
  let+ base_dir, socket_dir, gw_prefix, images_prefix, etc_prefix = directories
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
  and+ n_workers = n_workers
  and+ cgi = cgi
  and+ daemon = daemon
  and+ default_lang = default_lang
  and+ browser_lang = browser_lang
  and+ setup_link = setup_link
  and+ plugins = plugins
  and+ debug = debug
  and+ verbosity = verbosity
  and+ log = log
  and+ trace_failed_password = trace_failed_password in
  {
    base_dir;
    socket_dir;
    gw_prefix;
    images_prefix;
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
    ("-force", "");
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
    ("-nolock", "--no-lock");
    ("-n_workers", "--n-workers");
    ("-only", "--allowed-address");
    ("-p", "--port");
    ("-particles", "--particles-file");
    ("-plugin", "--plugin-file");
    ("-plugins", "--plugin-dir");
    ("-redirect", "--redirect-interface");
    ("-robot_xcl", "--ban-threshold");
    ("-safe", "");
    ("-setup_link", "--setup-link");
    ("-trace_failed_passwd", "--trace-failed-password");
    ("-unsafe", "");
    ("-version", "--version");
    ("-wd", "--socket-dir");
    ("-wizard", "--wizard-password");
    ("-wjf", "--wizard-just-friend");
  ]

let preprocess_legacy_arguments =
  let tbl : (string, string) Hashtbl.t = Hashtbl.create 17 in
  List.iter (fun (old, new_) -> Hashtbl.add tbl old new_) legacy_arguments;
  Array.map (fun a ->
      match Hashtbl.find tbl a with
      | new_ ->
          (* TODO: Turn this log on when we are ready to deprecated the legacy CLI. *)
          (* Fmt.epr "The CLI option %S is deprecated. Please use %S instead.@." a new_; *)
          new_
      | exception Not_found -> a)

let arguments_in_file file =
  In_channel.with_open_text file @@ fun ic ->
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
        else Sys.argv.(l1 + i))
  in
  let argv = preprocess_legacy_arguments argv in
  Cmdliner.Cmd.eval_value' ~argv t
