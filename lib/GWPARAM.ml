module Logs = Geneweb_logs.Logs
module Driver = Geneweb_db.Driver

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

let config_reorg bname =
  let bname = Filename.remove_extension bname in
  String.concat Filename.dir_sep
    [ Secure.base_dir (); bname ^ ".gwb"; "config"; bname ^ ".gwf" ]

let config_legacy bname =
  String.concat Filename.dir_sep [ Secure.base_dir (); bname ^ ".gwf" ]

type my_fun_2 = string -> string
type my_fun_3 = string -> string -> string

(* Function references that will be set based on mode *)
let config = ref config_legacy
let cnt_d = ref (fun _ -> "")
let adm_file = ref (fun _ -> "")
let src_d = ref (fun _ -> "")
let etc_d = ref (fun _ -> "")
let config_d = ref (fun _ -> "")
let lang_d = ref (fun _ _ -> "")
let bpath = ref (fun _ -> "")
let portraits_d = ref (fun _ -> "")
let images_d = ref (fun _ -> "")
let clean_bname bname = Filename.remove_extension bname
let path_concat parts = String.concat Filename.dir_sep parts
let base_dir () = Secure.base_dir ()

(* Module for reorg mode paths *)
module Default = struct
  let config bname = config_reorg bname

  let cnt_d bname =
    let bname = clean_bname bname in
    if !sock_dir = "" then
      cnt_dir :=
        if bname <> "" then
          path_concat [ base_dir (); bname ^ ".gwb"; "config"; "cnt" ]
        else path_concat [ base_dir (); "cnt" ]
    else cnt_dir := !sock_dir;
    !cnt_dir

  let adm_file file = Filename.concat !cnt_dir file

  let portraits_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); bname ^ ".gwb"; "documents"; "portraits" ]

  let src_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); bname ^ ".gwb"; "src" ]

  let etc_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); bname ^ ".gwb"; "etc" ]

  let config_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); bname ^ ".gwb"; "config" ]

  let lang_d bname file =
    let bname = clean_bname bname in
    Filename.concat (path_concat [ base_dir (); bname ^ ".gwb"; "lang" ]) file

  let images_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); bname ^ ".gwb"; "documents"; "images" ]

  let bpath bname =
    let bname = clean_bname bname in
    Filename.concat (base_dir ()) (bname ^ ".gwb")
end

(* Module for legacy mode paths *)
module Legacy = struct
  let config = config_legacy

  let cnt_d bname =
    let _ = bname in
    if !sock_dir = "" then cnt_dir := path_concat [ base_dir (); "cnt" ]
    else cnt_dir := !sock_dir;
    !cnt_dir

  let adm_file file = Filename.concat !cnt_dir file

  let portraits_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); "images"; bname ]

  let src_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); "src"; bname ]

  let etc_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); "etc"; bname ]

  let config_d bname =
    let _ = bname in
    base_dir ()

  let lang_d bname file =
    let bname = clean_bname bname in
    Filename.concat (path_concat [ base_dir (); "lang"; bname ]) file

  let images_d bname =
    let bname = clean_bname bname in
    path_concat [ base_dir (); "src"; bname; "images" ]

  let bpath bname =
    let bname = clean_bname bname in
    Filename.concat (base_dir ()) (bname ^ ".gwb")
end

(* Check if a base is in reorg format *)
let is_reorg_base bname =
  let bname = Filename.remove_extension bname in
  Sys.file_exists (config_reorg bname)

(* Initialize path functions based on mode *)
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

let test_reorg bname =
  if !reorg || is_reorg_base bname then (
    reorg := true;
    init bname)

let get_timestamp () =
  let tm = Unix.localtime (Unix.time ()) in
  Printf.sprintf "%04d%02d%02d-%02d%02d%02d" (1900 + tm.Unix.tm_year)
    (1 + tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    tm.Unix.tm_sec

let create_default_gwf config_path =
  let gwf_defaults =
    [
      "access_by_key=yes";
      "disable_forum=yes";
      "hide_private_names=no";
      "use_restrict=no";
      "show_consang=yes";
      "display_sosa=yes";
      "place_surname_link_to_ind=yes";
      "max_anc_level=8";
      "max_anc_tree=7";
      "max_desc_level=12";
      "max_desc_tree=4";
      "max_cousins=2000";
      "max_cousins_level=5";
      "latest_event=20";
      "template=*";
      "long_date=no";
      "counter=yes";
      "full_siblings=yes";
      "hide_advanced_request=no";
      "p_mod=";
    ]
  in
  let oc = open_out config_path in
  List.iter (fun s -> Printf.fprintf oc "%s\n" s) gwf_defaults;
  close_out oc

let rec create_base_and_config bname =
  let clean_bname = Filename.remove_extension bname in
  let bdir = Filename.concat (Secure.base_dir ()) (clean_bname ^ ".gwb") in
  if (not !force) && Sys.file_exists bdir then (
    Printf.eprintf "Database \"%s\" already exists. Use -f to overwrite." bname;
    exit 2);
  Filesystem.create_dir bdir;
  let user_wants_reorg = !reorg in
  migrate_gwf_bidirectional clean_bname user_wants_reorg;
  reorg := !reorg || is_reorg_base clean_bname;
  init clean_bname;
  bdir

and migrate_gwf_bidirectional bname user_wants_reorg =
  let bname = Filename.remove_extension bname in
  let legacy_path = Filename.concat (Secure.base_dir ()) (bname ^ ".gwf") in
  let reorg_path =
    String.concat Filename.dir_sep
      [ Secure.base_dir (); bname ^ ".gwb"; "config"; bname ^ ".gwf" ]
  in
  let legacy_exists = Sys.file_exists legacy_path in
  let reorg_exists = Sys.file_exists reorg_path in
  match (user_wants_reorg, legacy_exists, reorg_exists) with
  | true, true, false ->
      (* Migration classic → reorg *)
      Printf.eprintf "Migration: moving .gwf from classic to reorg mode\n";
      Filesystem.create_dir ~parent:true (Filename.dirname reorg_path);
      let timestamp = get_timestamp () in
      let backup_path = legacy_path ^ ".classic." ^ timestamp in
      Filesystem.copy_file legacy_path backup_path;
      Printf.eprintf "Backup created: %s\n" (Filename.basename backup_path);
      Filesystem.copy_file legacy_path reorg_path;
      Sys.remove legacy_path;
      Printf.eprintf "Configuration migrated: %s\n"
        (Filename.basename reorg_path)
  | false, false, true ->
      (* Migration reorg → classic *)
      Printf.eprintf "Migration: moving .gwf from reorg to classic mode\n";
      let timestamp = get_timestamp () in
      let backup_path = reorg_path ^ ".reorg." ^ timestamp in
      Filesystem.copy_file reorg_path backup_path;
      Printf.eprintf "Backup created: %s\n" (Filename.basename backup_path);
      Filesystem.copy_file reorg_path legacy_path;
      Sys.remove reorg_path;
      Printf.eprintf "Configuration migrated: %s\n"
        (Filename.basename legacy_path)
  | true, false, false ->
      (* Créer nouveau .gwf en mode reorg *)
      Filesystem.create_dir ~parent:true (Filename.dirname reorg_path);
      Printf.eprintf "Creating default configuration: %s\n"
        (Filename.basename reorg_path);
      create_default_gwf reorg_path
  | false, false, false ->
      (* Créer nouveau .gwf en mode classic *)
      Printf.eprintf "Creating default configuration: %s\n"
        (Filename.basename legacy_path);
      create_default_gwf legacy_path
  | _, true, true ->
      (* Conflit : .gwf existe dans les deux modes *)
      let remove_path = if user_wants_reorg then legacy_path else reorg_path in
      let remove_mode = if user_wants_reorg then "classic" else "reorg" in
      let keep_mode = if user_wants_reorg then "reorg" else "classic" in
      Printf.eprintf "Warning: .gwf exists in both modes, keeping %s mode\n"
        keep_mode;
      let timestamp = get_timestamp () in
      let backup_path = remove_path ^ "." ^ remove_mode ^ "." ^ timestamp in
      Filesystem.copy_file remove_path backup_path;
      Printf.eprintf "Backup created: %s\n" (Filename.basename backup_path);
      Sys.remove remove_path
  | _ ->
      (* .gwf existe déjà au bon endroit selon l'intention utilisateur *)
      ()

(* Utility functions for error handling *)
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

let is_semi_public p = Driver.get_access p = Def.SemiPublic

let split_key key =
  let dot = match String.index_opt key '.' with Some i -> i | _ -> -1 in
  let space = match String.index_opt key ' ' with Some i -> i | _ -> -1 in
  if dot <> -1 && space <> -1 then
    ( String.sub key 0 dot,
      String.sub key (dot + 1) (space - dot - 1),
      String.sub key (space + 1) (String.length key - space - 1) )
  else ("?", "", "?")

(* Determine if person is related to the current user *)
let ancestors _conf base max_generations family ip =
  let rec loop gen acc ips =
    if gen <= 0 then acc
    else
      let new_ancestors =
        List.fold_left
          (fun acc ip ->
            match Driver.get_parents (Driver.poi base ip) with
            | Some ifam ->
                let fam = Driver.foi base ifam in
                let father = Driver.get_father fam in
                let mother = Driver.get_mother fam in
                father :: mother :: acc
            | None -> acc)
          [] ips
      in
      loop (gen - 1) (new_ancestors @ acc) new_ancestors
  in
  loop max_generations family [ ip ]

let descendants _conf base family ip =
  let rec loop acc ips =
    if ips = [] then acc
    else
      let new_descendants =
        List.fold_left
          (fun _acc ip ->
            let ifams = Driver.get_family (Driver.poi base ip) in
            Array.fold_left
              (fun acc ifam ->
                let children = Driver.get_children (Driver.foi base ifam) in
                Array.to_list children @ acc)
              [] ifams)
          [] ips
      in
      loop (new_descendants @ acc) new_descendants
  in
  loop family [ ip ]

let is_related conf base p =
  if conf.Config.userkey <> "" then
    let fname =
      String.concat Filename.dir_sep
        [
          Secure.base_dir ();
          conf.Config.bname ^ ".gwb";
          "caches";
          "family-" ^ conf.Config.userkey;
        ]
    in
    match (List.assoc_opt "fmode" conf.Config.env :> string option) with
    | Some "on" -> (
        let family =
          Mutil.read_or_create_value fname (fun () ->
              match conf.Config.user_iper with
              | Some ip ->
                  let family = [ ip ] in
                  let max =
                    try List.assoc "is_semi_public_max" conf.Config.base_env
                    with Not_found -> "2" |> String.trim
                  in
                  let max = if max = "" then 2 else int_of_string max in
                  let family = ancestors conf base (max + 1) family ip in
                  (* Add spouses *)
                  let family =
                    (let ifams = Driver.get_family (Driver.poi base ip) in
                     Array.fold_left
                       (fun acc ifam ->
                         let sp =
                           let f = Driver.foi base ifam in
                           if ip = Driver.get_father f then Driver.get_mother f
                           else Driver.get_father f
                         in
                         if
                           Driver.sou base
                             (Driver.get_first_name (Driver.poi base sp))
                           <> "?"
                           && Driver.sou base
                                (Driver.get_surname (Driver.poi base sp))
                              <> "?"
                         then sp :: acc
                         else acc)
                       [] ifams)
                    @ family
                  in
                  let family = descendants conf base family ip in
                  List.sort_uniq compare family
              | _ -> [])
        in
        match conf.Config.user_iper with
        | Some ip ->
            Driver.get_access (Driver.poi base ip) = Def.SemiPublic
            && List.mem (Driver.get_iper p) family
        | _ -> false)
    | _ ->
        if Sys.file_exists fname then (
          try
            Sys.remove fname;
            false
          with Sys_error _ ->
            Printf.eprintf "Error when removing %s\n" fname;
            false)
        else false
  else false

(** Calcul les droits de visualisation d'une personne en fonction de son age.
    pour les test impliquant une date, si elle existe, on renvoie vrai ou faux
    sinon, on passe au test suivant dans l'ordre ci dessous) :
    - Vrai si : magicien ou ami ou la personne est public ou la personne est en
      IfTitles, si elle a au moins un titre et que public_if_title = yes dans le
      fichier gwf ou la personne s'est mariée depuis plus de
      private_years_marriage
    - Vrai si : la personne est plus agée (en fonction de la date de naissance
      ou de la date de baptème) que privates_years
    - Vrai si : la personne est décédée depuis plus de privates_years_death
    - Vrai si : la personne n'est pas Private et public_if_no_date = yes
    - Faux si : la personne n'est pas décédée et private_years > 0
    - Faux si : la personne est plus jeune (en fonction de la date de naissance
      ou de la date de baptème) que privates_years
    - Faux si : la personne est décédée depuis moins de privates_years
    - Faux dans tous les autres cas *)
(* check that p is parent or descendant of conf.key *)

(* kept for later debugging
   let check_p_auth conf base p =
     let mode_semi_public =
       begin try List.assoc "semi_public" conf.Config.base_env = "yes" with
         Not_found -> false
       end;
     in
     let access = Geneweb_db.get_access p in
     let not_private = access <> Private in
     Printf.eprintf "P_auth for %s\n" (Gutil.designation base p);
     if conf.Config.wizard
         then Printf.eprintf "Wizard\n";
     if (not mode_semi_public) && conf.Config.friend
         then Printf.eprintf "Friend and not mode_semi_public\n";
     if conf.user_iper = Some (Geneweb_db.get_iper p)
         then Printf.eprintf "Self\n";
     if access = Public
         then Printf.eprintf "Public\n";
     if conf.Config.public_if_titles
        && access = IfTitles
        && Geneweb_db.nobtitles base conf.allowed_titles conf.denied_titles p <> []
         then Printf.eprintf "Has titles\n";
     if conf.Config.friend && conf.Config.semi_public
        && (is_semi_public p || is_related conf base p)
        && not_private
         then Printf.eprintf "visiting a semi_public or family person\n";
     if (conf.Config.public_if_no_date && access <> Private)
         then Printf.eprintf "not private and no dates\n";
     let birth_d = (Geneweb_db.get_birth p |> Date.cdate_to_dmy_opt) in
     if conf.Config.private_years < -1
         then Printf.eprintf "private_years < -1\n";
     let baptism_d = (Geneweb_db.get_baptism  p |> Date.cdate_to_dmy_opt) in
     let death_d = (Geneweb_db.get_death p |> Date.dmy_of_death) in
     Printf.eprintf "other conditions date driven\n";
     begin match birth_d with
     | None -> Printf.eprintf "no birth date\n"
     | Some d -> (
         let a = Date.time_elapsed d conf.today in
         if (a.Def.year > conf.Config.private_years)
           || a.Def.year = 0 && (a.month > 0 || a.day > 0)
         then Printf.eprintf "old enough (bir) (%d)\n" conf.Config.private_years
         else Printf.eprintf "too young (bir) (%d)\n" conf.Config.private_years)
     end;
     begin match baptism_d with
     | None -> Printf.eprintf "no baptism date\n"
     | Some d -> (
         let a = Date.time_elapsed d conf.today in
         if (a.Def.year > conf.Config.private_years)
           || a.Def.year = 0 && (a.month > 0 || a.day > 0)
         then Printf.eprintf "old enough (bap) (%d)\n" conf.Config.private_years
         else Printf.eprintf "too young (bap) (%d)\n" conf.Config.private_years)
     end;
     Printf.eprintf "check death date\n";
     begin match death_d with
     | None -> Printf.eprintf "no death date\n"
     | Some d -> (
         let a = Date.time_elapsed d conf.today in
         if (a.Def.year > conf.Config.private_years_death)
           || a.Def.year = 0 && (a.month > 0 || a.day > 0)
         then Printf.eprintf "old enough (dea) (%d)\n" conf.Config.private_years_death
         else Printf.eprintf "too young  (%d)\n" conf.Config.private_years_death)
     end;
     Printf.eprintf "no check on marriage date(s)\n"
*)

(* Authorization checks for person access *)
let p_auth conf base p =
  let mode_semi_public =
    try List.assoc "semi_public" conf.Config.base_env = "yes"
    with Not_found -> false
  in
  let access = Driver.get_access p in
  let not_private = access <> Def.Private in
  conf.Config.wizard
  || ((not mode_semi_public) && conf.Config.friend)
  || conf.user_iper = Some (Driver.get_iper p)
  || access = Def.Public
  || conf.Config.public_if_titles && access = Def.IfTitles
     && Driver.nobtitles base conf.allowed_titles conf.denied_titles p <> []
  || conf.Config.friend && conf.Config.semi_public
     && (is_semi_public p || is_related conf base p)
     && not_private
  || (conf.Config.public_if_no_date && access <> Def.Private)
  ||
  if conf.Config.private_years <= -1 then true
  else
    let check_date d lim none =
      match d with
      | None -> none ()
      | Some d ->
          let a = Date.time_elapsed d conf.today in
          if a.Def.year > lim then true
          else if a.Def.year = 0 then a.month > 0 || a.day > 0
          else false
    in
    check_date
      (Driver.get_birth p |> Date.cdate_to_dmy_opt)
      conf.Config.private_years
    @@ fun () ->
    check_date
      (Driver.get_baptism p |> Date.cdate_to_dmy_opt)
      conf.Config.private_years
    @@ fun () ->
    check_date
      (Driver.get_death p |> Date.dmy_of_death)
      conf.Config.private_years_death
    @@ fun () ->
    let families = Driver.get_family p in
    let len = Array.length families in
    let rec loop i =
      i < len
      && check_date
           (Array.get families i |> Driver.foi base |> Driver.get_marriage
          |> Date.cdate_to_dmy_opt)
           conf.Config.private_years_marriage
           (fun () -> loop (i + 1))
    in
    loop 0

let p_auth_sp conf base p =
  p_auth conf base p
  || (conf.Config.friend && Driver.get_access p <> Def.Private)

(* Wrap content in a basic HTML page *)
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
