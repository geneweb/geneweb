module Util = Geneweb.Util
module Version = Geneweb.Version
module Gutil = Geneweb_db.Gutil

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
let socket_dir = ref Cmd.default_socket_dir
let set_socket_dir s = socket_dir := s
let auth_file : string option ref = ref None
let cache_langs : string list ref = ref []
let cache_databases : string list ref = ref []
let choose_browser_lang = ref false
let conn_timeout = ref Cmd.default_connection_timeout
let daemon = ref false
let friend_passwd : string option ref = ref None
let default_lang = ref Cmd.default_default_lang
let images_dir = ref ""
let lexicon_list = ref [ Filename.concat "lang" "lexicon.txt" ]
let login_timeout = ref Cmd.default_login_timeout
let n_workers = ref Cmd.default_n_workers
let max_pending_requests = ref Cmd.default_max_pending_requests
let no_host_address = ref false
let only_addresses : string list ref = ref []
let plugins : string list ref = ref []
let forced_plugins : string list ref = ref []
let unsafe_plugins : string list ref = ref []
let redirected_addr : string option ref = ref None
let robot_xcl : (int * int) option ref = ref None
let selected_addr : string option ref = ref None
let selected_port = ref Cmd.default_port
let setup_link = ref false
let trace_failed_passwd = ref false
let debug = ref false
let set_debug () = debug := true

let robot_exclude_arg s =
  try robot_xcl := Scanf.sscanf s "%d,%d" (fun cnt sec -> Some (cnt, sec))
  with _ ->
    Printf.eprintf "Bad use of option -robot_xcl\n";
    Printf.eprintf "Use option -help for usage.\n";
    flush Stdlib.stderr;
    exit 2

(* FIXME: This option must be turn on by default but it seems to be 
   incompatible with CGI mode. *)
let digest_password = ref false
let wizard_just_friend = ref false
let wizard_passwd : string option ref = ref None
let predictable_mode = ref false

let set_predictable_mode () =
  Logs.warn (fun k ->
      k
        "Predictable mode must not be enabled in production. It disables \
         security enhancements and caching.");
  predictable_mode := true

let log_file : Cmd.log ref = ref Cmd.Stderr

let set_log_file s =
  match Cmd.log_parser s with Ok l -> log_file := l | Error _ -> assert false

let verbosity_level = ref Cmd.default_verbosity
let set_verbosity_level lvl = verbosity_level := lvl
let force_cgi = ref false
let cgi_secret_salt : string option ref = ref None

let arg_plugin_doc opt doc =
  doc
  ^ " Combine with -force to enable for every base. Combine with -unsafe to \
     allow unverified plugins. e.g. \"" ^ opt ^ " -unsafe -force\"."

let arg_plugin_aux () =
  let aux (unsafe, force, p) =
    incr Arg.current;
    assert (!Arg.current < Array.length Sys.argv);
    match Sys.argv.(!Arg.current) with
    | "-unsafe" -> (true, force, p)
    | "-force" -> (unsafe, true, p)
    | p' ->
        assert (p = "");
        (unsafe, force, p')
  in
  let rec loop ((_, _, p) as acc) = if p = "" then loop (aux acc) else acc in
  loop (false, false, "")

let arg_plugin opt doc =
  ( opt,
    Arg.Unit
      (fun () ->
        let unsafe, force, s = arg_plugin_aux () in
        if unsafe then unsafe_plugins := !unsafe_plugins @ [ s ];
        if force then
          forced_plugins := !forced_plugins @ [ Filename.basename s ];
        plugins := !plugins @ [ s ]),
    arg_plugin_doc opt doc )

let arg_plugins opt doc =
  ( opt,
    Arg.Unit
      (fun () ->
        let _unsafe, _force, _s = arg_plugin_aux () in
        ()),
    arg_plugin_doc opt doc )

let parse_prefixes () =
  let _, _, new_gw_prefix, new_images_prefix, new_etc_prefix =
    Cmd.parse_directories (Secure.base_dir ()) !socket_dir !gw_prefix
      !images_prefix !etc_prefix
  in
  gw_prefix := Some new_gw_prefix;
  images_prefix := Some new_images_prefix;
  etc_prefix := Some new_etc_prefix

let print_version_commit () =
  Printf.printf "Geneweb version %s\nRepository %s\n" Version.ver Version.src;
  Printf.printf "Branch %s\nLast commit %s\n" Version.branch Version.commit_id;
  exit 0

(** Parse line and extract separated arguments ("" and '' are used to indlude
    spaces inside the argument) *)
let arg_list_of_string line =
  let rec loop list i len quote =
    if i = String.length line then
      if len = 0 then List.rev list else List.rev (Buff.get len :: list)
    else
      match (quote, line.[i]) with
      | Some c1, c2 ->
          if c1 = c2 then loop list (i + 1) len None
          else loop list (i + 1) (Buff.store len c2) quote
      | None, ' ' ->
          let list = if len = 0 then list else Buff.get len :: list in
          loop list (i + 1) 0 quote
      | None, (('"' | '\'') as c) -> loop list (i + 1) 0 (Some c)
      | None, c -> loop list (i + 1) (Buff.store len c) None
  in
  loop [] 0 0 None

let arg_parse_in_file fname speclist anonfun errmsg =
  try
    let ic = open_in fname in
    let list =
      let rec loop acc =
        match input_line ic with
        | line -> loop (if line <> "" then line :: acc else acc)
        | exception End_of_file ->
            close_in ic;
            List.rev acc
      in
      loop []
    in
    let list = match list with [ x ] -> arg_list_of_string x | _ -> list in
    Arg.parse_argv ~current:(ref 0)
      (Array.of_list @@ (Sys.argv.(0) :: list))
      speclist anonfun errmsg
  with Sys_error _ -> ()

let parse () =
  let usage =
    "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"
  in
  let speclist =
    [
      ( "-hd",
        Arg.String set_gw_prefix,
        Fmt.str
          "<DIR> Specify where the “etc”, “images” and “lang” directories are \
           installed (default if empty is %S)."
          Cmd.default_gw_prefix );
      ( "-bd",
        Arg.String Secure.set_base_dir,
        Fmt.str
          "<DIR> Specify where the “bases” directory with databases is \
           installed (default if empty is %S)."
          Cmd.default_base_dir );
      ( "-wd",
        Arg.String set_socket_dir,
        "<DIR> Directory for socket communication (Windows) and access count."
      );
      ( "-cache_langs",
        Arg.String
          (fun s ->
            List.iter (Mutil.list_ref_append cache_langs)
            @@ String.split_on_char ',' s),
        " Lexicon languages to be cached." );
      ("-cgi", Arg.Set force_cgi, " Force CGI mode.");
      ( "-cgi_secret_salt",
        Arg.String (fun s -> cgi_secret_salt := Some s),
        "<STRING> Add a secret salt to form digests." );
      ( "-etc_prefix",
        Arg.String (fun x -> set_etc_prefix x),
        "<DIR> Specify where the “etc” directory is installed (default if \
         empty is [-hd value]/etc)." );
      ( "-images_prefix",
        Arg.String set_images_prefix,
        "<DIR> Specify where the “images” directory is installed (default if \
         empty is [-hd value]/images)." );
      ( "-images_dir",
        Arg.String
          (fun x ->
            deprecated_warning_images_dir ();
            images_dir := x),
        "<DIR> Same than previous but directory name relative to current." );
      ( "-a",
        Arg.String (fun x -> selected_addr := Some x),
        "<ADDRESS> Select a specific address (default = any address of this \
         computer)." );
      ( "-p",
        Arg.Int (fun x -> selected_port := x),
        "<NUMBER> Select a port number (default = "
        ^ string_of_int !selected_port
        ^ ")." );
      ( "-setup_link",
        Arg.Set setup_link,
        " Display a link to local gwsetup in bottom of pages." );
      ( "-allowed_tags",
        Arg.String (fun x -> Geneweb.Util.allowed_tags_file := x),
        "<FILE> HTML tags which are allowed to be displayed. One tag per line \
         in file." );
      ( "-wizard",
        Arg.String (fun x -> wizard_passwd := Some x),
        "<PASSWD> Set a wizard password." );
      ( "-friend",
        Arg.String (fun x -> friend_passwd := Some x),
        "<PASSWD> Set a friend password." );
      ("-wjf", Arg.Set wizard_just_friend, " Wizard just friend (permanently).");
      ( "-lang",
        Arg.String (fun x -> default_lang := x),
        "<LANG> Set a default language (default: " ^ Cmd.default_default_lang
        ^ ")." );
      ( "-blang",
        Arg.Set choose_browser_lang,
        " Select the user browser language if any." );
      ( "-only",
        Arg.String (fun x -> only_addresses := x :: !only_addresses),
        "<ADDRESS> Only inet address accepted." );
      ( "-auth",
        Arg.String (fun x -> auth_file := Some x),
        "<FILE> Authorization file to restrict access. The file must hold \
         lines of the form \"user:password\"." );
      ( "-no_host_address",
        Arg.Set no_host_address,
        " Force no reverse host by address." );
      ( "-digest",
        Arg.Set digest_password,
        " Use Digest authorization scheme (more secure on passwords)" );
      ( "-add_lexicon",
        Arg.String (Mutil.list_ref_append lexicon_list),
        "<FILE> Add file as lexicon." );
      ( "-particles",
        Arg.String (fun x -> Mutil.particles_file := x),
        "<FILE> Particles file." );
      ( "-log",
        Arg.String set_log_file,
        {|<FILE> Log trace to this file. Use "-" or "<stdout>" to redirect output to stdout or "<stderr>" to output log to stderr.|}
      );
      ( "-log_level",
        Arg.Int set_verbosity_level,
        {|<N> Send messages with severity <= <N> to syslog (default: |}
        ^ string_of_int !verbosity_level
        ^ {|).|} );
      ( "-robot_xcl",
        Arg.String robot_exclude_arg,
        "<CNT>,<SEC> Exclude connections when more than <CNT> requests in \
         <SEC> seconds." );
      ( "-min_disp_req",
        Arg.Int (fun x -> Robot.min_disp_req := x),
        " Minimum number of requests in robot trace (default: "
        ^ string_of_int !Robot.min_disp_req
        ^ ")." );
      ( "-login_tmout",
        Arg.Int (fun x -> login_timeout := x),
        "<SEC> Login timeout for entries with passwords in CGI mode (default "
        ^ string_of_int !login_timeout
        ^ "s)." );
      ( "-redirect",
        Arg.String (fun x -> redirected_addr := Some x),
        "<ADDR> Send a message to say that this service has been redirected to \
         <ADDR>." );
      ( "-trace_failed_passwd",
        Arg.Set trace_failed_passwd,
        " Print the failed passwords in log (except if option -digest is set). "
      );
      ("-debug", Arg.Unit set_debug, " Enable debug mode");
      ( "-nolock",
        Arg.Set Lock.no_lock_flag,
        " Do not lock files before writing." );
      arg_plugin "-plugin" "<PLUGIN>.cmxs load a safe plugin.";
      arg_plugins "-plugins" "<DIR> load all plugins in <DIR>.";
      ( "-version",
        Arg.Unit print_version_commit,
        " Print the Geneweb version, the source repository and last commit id \
         and message." );
    ]
  in
  let speclist =
    if Sys.unix then
      speclist
      @ [
          ( "-max_clients",
            Arg.Unit deprecated_warning_max_clients,
            "<NUM> Max number of clients treated at the same time (default: no \
             limit) (not cgi) (DEPRECATED)." );
          ( "-n_workers",
            Arg.Int (fun x -> n_workers := x),
            "<NUM> Number of workers used by the server (default: "
            ^ string_of_int Cmd.default_n_workers
            ^ ")" );
          ( "-max_pending_requests",
            Arg.Int (fun x -> max_pending_requests := x),
            "<NUM> Maximum number of pending requests (default: "
            ^ string_of_int Cmd.default_max_pending_requests
            ^ ")" );
          ( "-conn_tmout",
            Arg.Int (fun x -> conn_timeout := x),
            "<SEC> Connection timeout (only on Unix) (default "
            ^ string_of_int Cmd.default_connection_timeout
            ^ "s; 0 means no limit)." );
          ("-daemon", Arg.Set daemon, " Unix daemon mode.");
          ( "-no-fork",
            Arg.Unit
              (fun () ->
                deprecated_warning_no_fork ();
                n_workers := 0),
            " Prevent forking processes (DEPRECATED)" );
          ( "-cache-in-memory",
            Arg.String
              (fun s ->
                if Gw_ancient.is_available then
                  cache_databases := s :: !cache_databases
                else
                  failwith "-cache-in-memory option unavailable for this build."),
            "<DATABASE> Preload this database in memory" );
          ( "-predictable_mode",
            Arg.Unit set_predictable_mode,
            " Turn on the predictable mode. In this mode, the behavior of the \
             server is predictable, which is helpful for debugging or testing. \
             (default: false)" );
        ]
    else speclist
  in
  let speclist = List.sort compare speclist in
  let speclist = Arg.align speclist in
  let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s)) in
  (if Sys.unix then
     default_lang :=
       let s = try Sys.getenv "LANG" with Not_found -> "" in
       if List.mem s Version.available_languages then s
       else
         let s = try Sys.getenv "LC_CTYPE" with Not_found -> "" in
         if String.length s >= 2 then
           let s = String.sub s 0 2 in
           if List.mem s Version.available_languages then s else "en"
         else "en");
  arg_parse_in_file
    (Filename.chop_extension Sys.argv.(0) ^ ".arg")
    speclist anonfun usage;
  Arg.parse speclist anonfun usage;
  parse_prefixes ()
