(** This module allows plugins to modify geneweb configuration.

    This approch is preffered to Functors or library variants
    for simple functions if it does not come with a performance cost.
*)

let nb_errors = ref 0
let errors_undef = ref []
let errors_other = ref []
let set_vars = ref []
let gwd_cmd = ref ""
let reorg = ref false
let force = ref false
let cnt_dir = ref ""
let sock_dir = ref ""
let bases = ref (Secure.base_dir ())

type init_s = { status : bool; bname : string }

let init_done : init_s ref = ref { status = false; bname = "" }

(** allows testing of reorg config in classic mode *)
let config_reorg bname =
  String.concat Filename.dir_sep
    [ Secure.base_dir (); bname ^ ".gwb"; "config"; bname ^ ".gwf" ]

type syslog_level =
  [ `LOG_ALERT
  | `LOG_CRIT
  | `LOG_DEBUG
  | `LOG_EMERG
  | `LOG_ERR
  | `LOG_INFO
  | `LOG_NOTICE
  | `LOG_WARNING ]

module Default = struct
  (* Attention, ajuster is_reorg_base en conséquence *)
  let config bname =
    let bname = Filename.remove_extension bname in
    config_reorg bname

  let cnt_d bname =
    let bname = Filename.remove_extension bname in
    if !sock_dir = "" then
      cnt_dir :=
        if bname <> "" then
          String.concat Filename.dir_sep
            [ Secure.base_dir (); bname ^ ".gwb"; "config"; "cnt" ]
        else String.concat Filename.dir_sep [ Secure.base_dir (); "cnt" ]
    else cnt_dir := !sock_dir;
    !cnt_dir

  let adm_file file = Filename.concat !cnt_dir file

  let portraits_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep
      [ Secure.base_dir (); bname ^ ".gwb"; "documents"; "portraits" ]

  let src_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep
      [ Secure.base_dir (); bname ^ ".gwb"; "documents"; "src" ]

  let etc_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep [ Secure.base_dir (); bname ^ ".gwb"; "etc" ]

  let config_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep
      [ Secure.base_dir (); bname ^ ".gwb"; "config" ]

  let lang_d bname lang =
    let bname = Filename.remove_extension bname in
    if lang = "" then
      String.concat Filename.dir_sep
        [ Secure.base_dir (); bname ^ ".gwb"; "etc"; "lang" ]
    else
      String.concat Filename.dir_sep
        [ Secure.base_dir (); bname ^ ".gwb"; "etc"; "lang"; lang ]

  let images_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep
      [ Secure.base_dir (); bname ^ ".gwb"; "documents"; "images" ]

  let bpath bname =
    let bname = Filename.remove_extension bname in
    if bname = "" then Secure.base_dir ()
    else Filename.concat (Secure.base_dir ()) (bname ^ ".gwb")
end

module Legacy = struct
  (* Attention, ajuster is_reorg_base en conséquence *)
  let config bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep [ Secure.base_dir (); bname ^ ".gwf" ]

  let cnt_d _bname =
    if !cnt_dir = "" then (
      let str = String.concat Filename.dir_sep [ Secure.base_dir (); "cnt" ] in
      cnt_dir := str;
      str)
    else !cnt_dir

  let adm_file file = Filename.concat !cnt_dir file

  let portraits_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep [ Secure.base_dir (); "images"; bname ]

  let src_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep [ Secure.base_dir (); "src"; bname ]

  let etc_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep [ Secure.base_dir (); "etc"; bname ]

  let config_d _bname = Secure.base_dir ()

  let lang_d bname lang =
    let bname = Filename.remove_extension bname in
    if lang = "" then
      String.concat Filename.dir_sep [ Secure.base_dir (); "lang"; bname ]
    else
      String.concat Filename.dir_sep [ Secure.base_dir (); "lang"; lang; bname ]

  let images_d bname =
    let bname = Filename.remove_extension bname in
    String.concat Filename.dir_sep
      [ Secure.base_dir (); "src"; bname; "images" ]

  let bpath bname =
    let bname = Filename.remove_extension bname in
    if bname = "" then Secure.base_dir ()
    else Filename.concat (Secure.base_dir ()) (bname ^ ".gwb")

  (** [output_error ?headers ?content conf code]
      Send the http status [code], [headers] and
      [content] if provided, or default content otherwise.
  *)
  let output_error =
    let output_file conf fn =
      let ic = open_in fn in
      try
        in_channel_length ic |> really_input_string ic
        |> Output.print_sstring conf;
        close_in ic
      with _ -> ( try close_in ic with _ -> ())
    in
    fun ?(headers = []) ?(content : Adef.safe_string option) conf code ->
      Output.status conf code;
      List.iter (Output.header conf "%s") headers;
      Output.print_string conf (Adef.encoded "<h1>Incorrect request</h1>");
      match content with
      | Some content -> Output.print_string conf content
      | None -> (
          let code =
            match code with
            | Def.Bad_Request -> "400"
            | Unauthorized -> "401"
            | Forbidden -> "403"
            | Not_Found -> "404"
            | Conflict -> "409"
            | Internal_Server_Error -> "500"
            | Service_Unavailable -> "503"
            | OK | Moved_Temporarily -> assert false
          in
          let fname lang =
            code ^ "-" ^ lang ^ ".html"
            |> Filename.concat "etc" |> Mutil.search_asset_opt
          in
          match fname conf.lang with
          | Some fn -> output_file conf fn
          | None -> (
              match fname "en" with
              | Some fn -> output_file conf fn
              | None -> Output.print_sstring conf ""))

  (** Calcul les droits de visualisation d'une personne en
      fonction de son age.
      Renvoie (dans l'ordre des tests) :
      - Vrai si : magicien ou ami ou la personne est public
      - Vrai si : la personne est en si_titre, si elle a au moins un
                  titre et que public_if_title = yes dans le fichier gwf
      - Faux si : la personne n'est pas décédée et private_years > 0
      - Vrai si : la personne est plus agée (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Faux si : la personne est plus jeune (en fonction de la date de
                  naissance ou de la date de baptème) que privates_years
      - Vrai si : la personne est décédée depuis plus de privates_years
      - Faux si : la personne est décédée depuis moins de privates_years
      - Vrai si : la personne a entre 80 et 120 ans et qu'elle n'est pas
                  privée et public_if_no_date = yes
      - Vrai si : la personne s'est mariée depuis plus de private_years
      - Faux dans tous les autres cas *)
  let p_auth conf base p =
    conf.Config.wizard || conf.friend
    || Gwdb.get_access p = Public
    || conf.public_if_titles
       && Gwdb.get_access p = IfTitles
       && Gwdb.nobtitles base conf.allowed_titles conf.denied_titles p <> []
    ||
    let death = Gwdb.get_death p in
    if death = NotDead then conf.private_years < 1
    else
      let check_date d lim none =
        match d with
        | None -> none ()
        | Some d ->
            let a = Date.time_elapsed d conf.today in
            if a.Def.year > lim then true
            else if a.year < conf.private_years then false
            else a.month > 0 || a.day > 0
      in
      check_date (Gwdb.get_birth p |> Date.cdate_to_dmy_opt) conf.private_years
      @@ fun () ->
      check_date
        (Gwdb.get_baptism p |> Date.cdate_to_dmy_opt)
        conf.private_years
      @@ fun () ->
      check_date
        (Gwdb.get_death p |> Date.dmy_of_death)
        conf.private_years_death
      @@ fun () ->
      (Gwdb.get_access p <> Def.Private && conf.public_if_no_date)
      ||
      let families = Gwdb.get_family p in
      let len = Array.length families in
      let rec loop i =
        i < len
        && check_date
             (Array.get families i |> Gwdb.foi base |> Gwdb.get_marriage
            |> Date.cdate_to_dmy_opt)
             conf.private_years_marriage
             (fun () -> loop (i + 1))
      in
      loop 0

  let syslog (level : syslog_level) msg =
    let tm = Unix.(time () |> localtime) in
    let level =
      match level with
      | `LOG_EMERG -> "EMERGENCY"
      | `LOG_ALERT -> "ALERT"
      | `LOG_CRIT -> "CRITICAL"
      | `LOG_ERR -> "ERROR"
      | `LOG_WARNING -> "WARNING"
      | `LOG_NOTICE -> "NOTICE"
      | `LOG_INFO -> "INFO"
      | `LOG_DEBUG -> "DEBUG"
    in
    Printf.eprintf "[%s]: %s %s\n"
      (Mutil.sprintf_date tm : Adef.safe_string :> string)
      level msg

  let wrap_output (conf : Config.config) (title : Adef.safe_string)
      (content : unit -> unit) =
    let robot = List.assoc_opt "robot_index" conf.base_env = Some "yes" in
    Output.print_sstring conf {|<!DOCTYPE html><head><title>|};
    Output.print_string conf title;
    Output.print_sstring conf {|</title>|};
    Output.print_sstring conf
      (if robot then {|<meta name="robots" content="index,follow">|}
      else {|<meta name="robots" content="none">|});
    Output.print_sstring conf {|<meta charset="|};
    Output.print_sstring conf conf.charset;
    Output.print_sstring conf {|">|};
    Output.print_sstring conf
      {|<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">|};
    Output.print_sstring conf {|</head>|};
    Output.print_sstring conf "<body>";
    content ();
    Output.print_sstring conf {|</body></html>|}
end

type my_fun_2 = string -> string
type my_fun_3 = string -> string -> string

let config = ref (Legacy.config : my_fun_2)
let cnt_d = ref (Legacy.cnt_d : my_fun_2)
let adm_file = ref (Legacy.adm_file : my_fun_2)
let src_d = ref (Legacy.src_d : my_fun_2)
let etc_d = ref (Legacy.etc_d : my_fun_2)
let config_d = ref (Legacy.config_d : my_fun_2)
let lang_d = ref (Legacy.lang_d : my_fun_3)
let bpath = ref (Legacy.bpath : my_fun_2)
let portraits_d = ref (Legacy.portraits_d : my_fun_2)
let images_d = ref (Legacy.images_d : my_fun_2)

(* attention; ne pas utiliser !config! *)
let is_reorg_base bname =
  let bname = Filename.remove_extension bname in
  Sys.file_exists
    (String.concat Filename.dir_sep
       [ Secure.base_dir (); bname ^ ".gwb"; "config"; bname ^ ".gwf" ])

let output_error = ref Legacy.output_error
let p_auth = ref Legacy.p_auth
let syslog = ref Legacy.syslog

let init bname =
  Secure.add_assets Filename.current_dir_name;

  reorg := !reorg || is_reorg_base bname;
  if !reorg then (
    config := Default.config;
    cnt_d := Default.cnt_d;
    adm_file := Default.adm_file;
    src_d := Default.src_d;
    etc_d := Default.etc_d;
    config_d := Default.config_d;
    lang_d := Default.lang_d;
    bpath := Default.bpath;
    portraits_d := Default.portraits_d;
    images_d := Default.images_d)
  else (
    config := Legacy.config;
    cnt_d := Legacy.cnt_d;
    adm_file := Legacy.adm_file;
    src_d := Legacy.src_d;
    etc_d := Legacy.etc_d;
    config_d := Legacy.config_d;
    lang_d := Legacy.lang_d;
    bpath := Legacy.bpath;
    portraits_d := Legacy.portraits_d;
    images_d := Legacy.images_d)

let init_etc bname =
  if !init_done.status && bname = !init_done.bname then ()
  else init_done := { status = true; bname };
  if !reorg then (
    (if not (Sys.file_exists (!bpath bname)) then
     try
       Unix.mkdir (!bpath bname) 0o755;
       force := true
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING
         (Printf.sprintf "Failure when creating base_dir: %s" (!bpath bname)));

    (if not (Sys.file_exists (!etc_d bname)) then
     try Unix.mkdir (!etc_d bname) 0o755
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING
         (Printf.sprintf "Failure when creating etc_dir: %s" (!etc_d bname)));

    (if not (Sys.file_exists (!config_d bname)) then
     try Unix.mkdir (!config_d bname) 0o755
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING
         (Printf.sprintf "Failure when creating config_dir: %s"
            (!config_d bname)));

    if not (Sys.file_exists (!cnt_d bname)) then
      try Unix.mkdir (!cnt_d bname) 0o755
      with Unix.Unix_error (_, _, _) ->
        !syslog `LOG_WARNING
          (Printf.sprintf "Failure when creating cnt_dir: %s" (!cnt_d bname)))
  else (
    (if not (Sys.file_exists "etc") then
     try
       Unix.mkdir "etc" 0o755;
       force := true
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING (Printf.sprintf "Failure when creating etc"));

    (if not (Sys.file_exists "lang") then
     try
       Unix.mkdir "lang" 0o755;
       force := true
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING (Printf.sprintf "Failure when creating lang"));

    (if not (Sys.file_exists "cnt") then
     try
       Unix.mkdir "cnt" 0o755;
       force := true
     with Unix.Unix_error (_, _, _) ->
       !syslog `LOG_WARNING (Printf.sprintf "Failure when creating cnt"));

    if not (Sys.file_exists (!etc_d bname)) then
      try
        Unix.mkdir (!etc_d bname) 0o755;
        force := true
      with Unix.Unix_error (_, _, _) ->
        !syslog `LOG_WARNING
          (Printf.sprintf "Failure when creating etc_dir: %s" (!etc_d bname)))

let test_reorg bname =
  if !reorg || is_reorg_base bname then (
    reorg := true;
    init bname)

let test_base bname =
  let bdir = !bpath bname in
  if Sys.file_exists (config_reorg bname) && !force then (
    reorg := true;
    init_done := { status = false; bname };
    init bname);
  Printf.eprintf "Mode: %s, for base %s\n"
    (if !reorg then "reorg" else "classic")
    (Filename.concat (!bpath "") (bname ^ ".gwb"));
  if (not !force) && Sys.file_exists bdir then (
    Printf.eprintf
      "The database \"%s\" already exists. Use option -f to overwrite it." bname;
    flush stderr;
    exit 2);
  init_etc bname

(** [wrap_output conf title content]
    Plugins defining a page content but not a complete UI
    may want to wrap their page using [wrap_output].
*)
let wrap_output = ref Legacy.wrap_output
