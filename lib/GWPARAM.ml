(** This module allows plugins to modify geneweb configuration.

    This approch is preffered to Functors or library variants for simple
    functions if it does not come with a performance cost. *)

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
  let init () =
    List.fold_right Filename.concat [ Gwlib.prefix; "share" ] "geneweb"
    |> Secure.add_assets;
    Secure.add_assets Filename.current_dir_name

  let base_path pref bname =
    List.fold_right Filename.concat (Secure.base_dir () :: pref) bname

  let bpath bname = Filename.concat (Secure.base_dir ()) bname

  (** [output_error ?headers ?content conf code] Send the http status [code],
      [headers] and [content] if provided, or default content otherwise. *)
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
            | Gateway_Timeout -> "504"
            | OK | Moved_Temporarily | Moved_Permanently -> assert false
          in
          let fname lang =
            code ^ "-" ^ lang ^ ".html"
            |> Filename.concat "etc" |> Files.search_asset_opt
          in
          match fname conf.lang with
          | Some fn -> output_file conf fn
          | None -> (
              match fname "en" with
              | Some fn -> output_file conf fn
              | None -> (
                  match fname "en" with
                  | Some fn -> output_file conf fn
                  | None -> Output.print_sstring conf "")))

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
    Printf.eprintf "[%s]: %s %s\n" (Ext_unix.sprintf_date tm) level msg

  let wrap_output (conf : Config.config) (title : Adef.safe_string)
      (content : unit -> unit) =
    Output.print_sstring conf {|<!DOCTYPE html><head><title>|};
    Output.print_string conf title;
    Output.print_sstring conf {|</title>|};
    Output.print_sstring conf {|<meta name="robots" content="none">|};
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

let init = ref Default.init
let base_path = ref Default.base_path
let bpath = ref Default.bpath
let output_error = ref Default.output_error
let syslog = ref Default.syslog

(** [wrap_output conf title content] Plugins defining a page content but not a
    complete UI may want to wrap their page using [wrap_output]. *)
let wrap_output = ref Default.wrap_output

let has_ignored_duplicates = ref (fun _ _ -> false)
let set_init f = init := f
let set_base_path f = base_path := f
let set_bpath f = bpath := f
let set_output_error f = output_error := f
let set_syslog f = syslog := f
let set_wrap_output f = wrap_output := f
let set_has_ignored_duplicates f = has_ignored_duplicates := f
let init () = !init ()
let base_path pref bname = !base_path pref bname
let bpath bname = !bpath bname

let output_error ?headers ?content conf code =
  !output_error ?headers ?content conf code

let syslog syslog_level msg = !syslog syslog_level msg
let wrap_output conf title content = !wrap_output conf title content
let has_ignored_duplicates conf base = !has_ignored_duplicates conf base
