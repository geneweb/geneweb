module Compat = Geneweb_compat

type backend = {
  ppf : Format.formatter;
  close : unit -> unit;
  flush : (unit -> unit) -> unit;
}

let infer_renderer fd =
  let dumb =
    match Sys.getenv "TERM" with
    | (exception Not_found) | "dumb" | "" -> true
    | _ -> false
  in
  let isatty =
    match Unix.isatty fd with exception Unix.Unix_error _ -> false | r -> r
  in
  if (not dumb) && isatty then `Ansi_tty else `None

let make_backend fd =
  let oc = Unix.out_channel_of_descr fd in
  let ppf = Format.formatter_of_out_channel oc in
  let style = infer_renderer fd in
  Fmt.set_style_renderer ppf style;
  let close () = close_out_noerr oc in
  let flush over = over () in
  { ppf; close; flush }

let timestamp_tag : unit Logs.Tag.def =
  Logs.Tag.def "timestamp" ~doc:"POSIX timestamp" Fmt.nop

let timestamp = Logs.Tag.(empty |> add timestamp_tag ())
let pp_brackets ~style pp = Fmt.(brackets @@ styled style @@ pp)

let pp_level ppf l =
  match l with
  | Logs.App -> ()
  | Logs.Error -> pp_brackets ~style:`Red Fmt.string ppf "ERROR"
  | Logs.Warning -> pp_brackets ~style:`Yellow Fmt.string ppf "WARN"
  | Logs.Info -> pp_brackets ~style:`Blue Fmt.string ppf "INFO"
  | Logs.Debug -> pp_brackets ~style:`Green Fmt.string ppf "DEBUG"

let pp_header ppf timestamp src level =
  match level with
  | Logs.App -> ()
  | _ ->
      Format.fprintf ppf "%a%a%a: "
        Fmt.(
          option ~none:nop @@ pp_brackets ~style:`Magenta (Ptime.pp_rfc3339 ()))
        timestamp pp_level level
        Fmt.(pp_brackets ~style:`Magenta string)
        (Logs.Src.name src)

type opened_file = { path : string; mutable backend : backend option }

let reporter ~predictable_mode { ppf; flush; _ } =
  let report src level ~over k msgf =
    let k ppf =
      Format.pp_close_box ppf ();
      Format.pp_print_newline ppf ();
      flush over;
      k ()
    in
    msgf @@ fun ?header:_ ?tags fmt ->
    let timestamp =
      Option.bind tags @@ fun tags ->
      Option.bind (Logs.Tag.find timestamp_tag tags) @@ fun () ->
      if predictable_mode then Some Ptime.epoch else Some (Ptime_clock.now ())
    in
    pp_header ppf timestamp src level;
    Format.pp_open_box ppf 0;
    Format.kfprintf k ppf fmt
  in
  { Logs.report }

let setup ~predictable_mode mk t =
  let set_reporter b = Logs.set_reporter @@ reporter ~predictable_mode b in
  let refresh o =
    Option.iter (fun b -> b.close ()) o.backend;
    let fd = Unix.openfile o.path Unix.[ O_WRONLY; O_CREAT; O_APPEND ] 0o644 in
    let backend = mk fd in
    set_reporter backend;
    o.backend <- Some backend
  in
  let set_sighup_signal o =
    if Sys.unix then
      Sys.set_signal Sys.sighup (Sys.Signal_handle (fun _ -> refresh o))
  in
  match t with
  | Cmd.Stdout -> set_reporter @@ mk Unix.stdout
  | Stderr -> set_reporter @@ mk Unix.stderr
  | File path ->
      let o = { path; backend = None } in
      refresh o;
      set_sighup_signal o
  | Syslog ->
      let addr = Unix.inet_addr_of_string "127.0.0.1" in
      Logs.set_reporter (Logs_syslog_unix.udp_reporter addr ~port:514 ())
