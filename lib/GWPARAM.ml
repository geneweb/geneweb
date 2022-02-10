(** This module allows plugins to modify geneweb configuration.

    This approch is preffered to Functors or library variants
    for simple functions if it does not come with a performance cost.
*)

type syslog_level =
  [ `LOG_ALERT
  | `LOG_CRIT
  | `LOG_DEBUG
  | `LOG_EMERG
  | `LOG_ERR
  | `LOG_INFO
  | `LOG_NOTICE
  | `LOG_WARNING
  ]

module Default = struct

  let init = fun () ->
    List.fold_right Filename.concat [ Gwlib.prefix ; "share" ] "geneweb"
    |> Secure.add_assets ;
    Secure.add_assets Filename.current_dir_name

  let base_path = fun pref bname ->
    List.fold_right Filename.concat (Secure.base_dir () :: pref) bname

  let bpath = fun bname -> Filename.concat (Secure.base_dir ()) bname

  (** [output_error ?headers ?content conf code]
      Send the http status [code], [headers] and
      [content] if provided, or default content otherwise.
  *)
  let output_error =
    let output_file conf fn =
      let ic = open_in fn in
      try
        in_channel_length ic
        |> really_input_string ic
        |> Output.print_string conf ;
        close_in ic
      with _ -> try close_in ic with _ -> ()
    in
    fun ?(headers = []) ?content conf code ->
      Output.status conf code ;
      List.iter (Output.header conf "%s") headers ;
      match content with
      | Some content -> Output.print_string conf content
      | None ->
        let code = match code with
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
          (code ^ "-" ^ lang ^ ".html")
          |> Filename.concat "etc"
          |> Mutil.search_asset_opt
        in
        match fname conf.lang with
        | Some fn -> output_file conf fn
        | None ->
          match fname "en" with
          | Some fn -> output_file conf fn
          | None -> Output.print_string conf ""

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
  let p_auth = fun conf base p ->
    conf.Config.wizard
    || conf.friend
    || Gwdb.get_access p = Public
    || (conf.public_if_titles
        && Gwdb.get_access p = IfTitles
        && Gwdb.nobtit base conf.allowed_titles conf.denied_titles p <> [])
    || begin
      let death = Gwdb.get_death p in
      if death = NotDead then conf.private_years < 1
      else
        let check_date d none = match d with
          | Some (Def.Dgreg (d, _)) ->
            let a  = Date.time_elapsed d conf.today in
            if a.Def.year > conf.Config.private_years then true
            else if a.year < conf.private_years then false
            else a.month > 0 || a.day > 0
          | _ -> none ()
        in
        check_date (Gwdb.get_birth p |> Adef.od_of_cdate) @@ fun () ->
        check_date (Gwdb.get_baptism p |> Adef.od_of_cdate) @@ fun () ->
        check_date (Gwdb.get_death p |> Date.date_of_death) @@ fun () ->
        (Gwdb.get_access p <> Def.Private && conf.public_if_no_date)
        || begin
          let families = Gwdb.get_family p in
          let len = Array.length families in
          let rec loop i =
            i < len
            && check_date
              (Array.get families i
               |> Gwdb.foi base
               |> Gwdb.get_marriage
               |> Adef.od_of_cdate)
              (fun () -> loop (i + 1))
          in
          loop 0
        end
    end

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
    Printf.eprintf "[%s]: %s %s\n" (Mutil.sprintf_date tm) level msg

  let wrap_output (conf : Config.config) (title : string) (content : unit -> unit) =
    Output.print_string conf {|<!DOCTYPE html><head><title>|};
    Output.print_string conf title ;
    Output.print_string conf {|</title>|} ;
    Output.print_string conf {|<meta name="robots" content="none">|} ;
    Output.print_string conf {|<meta charset="|} ;
    Output.print_string conf conf.charset ;
    Output.print_string conf {|">|} ;
    Output.print_string conf
      {|<meta name="viewport" content="width=device-width, initial-scale=1, shrink-to-fit=no">|} ;
    Output.print_string conf {|</head>|} ;
    Output.print_string conf "<body>" ;
    content () ;
    Output.print_string conf {|</body></html>|}

end

let init = ref Default.init
let base_path = ref Default.base_path
let bpath = ref Default.bpath
let output_error = ref Default.output_error
let p_auth = ref Default.p_auth
let syslog = ref Default.syslog

(** [wrap_output conf title content]
    Plugins defining a page content but not a complete UI
    may want to wrap their page using [wrap_output].
*)
let wrap_output = ref Default.wrap_output
