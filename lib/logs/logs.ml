(* By default, we do not want to print debug messages. *)
let verbosity_level = ref 6
let debug_flag = ref false

let oc : out_channel option ref = ref None

let log fn =
  match !oc with
  | Some oc -> fn oc
  | None -> ()

type level =
  [ `LOG_EMERG
  | `LOG_ALERT
  | `LOG_CRIT
  | `LOG_ERR
  | `LOG_WARNING
  | `LOG_NOTICE
  | `LOG_INFO
  | `LOG_DEBUG
  ]

let pp_tm oc tm =
  Printf.fprintf oc
    "%04d-%02d-%02d %02d:%02d:%02d"
    (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon)
    tm.Unix.tm_mday
    tm.Unix.tm_hour
    tm.Unix.tm_min
    tm.Unix.tm_sec

let syslog (level : level) msg =
  let lvl =
    match level with
     | `LOG_EMERG -> 0
     | `LOG_ALERT -> 1
     | `LOG_CRIT -> 2
     | `LOG_ERR -> 3
     | `LOG_WARNING -> 4
     | `LOG_NOTICE -> 5
     | `LOG_INFO -> 6
     | `LOG_DEBUG -> 7
  in
#ifdef SYSLOG
  let flags = if !debug_flag then [`LOG_PERROR] else [] in
  if !verbosity_level >= lvl
  then begin
    let log = Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log ;
    if !debug_flag then Printexc.print_backtrace stderr ;
  end
#else
  let () = () in
  if !verbosity_level >= lvl
  then begin
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
    let print oc = Printf.fprintf oc "%a %s %s\n%!" pp_tm tm level msg in
    begin match Sys.getenv_opt "GW_SYSLOG_FILE" with
      | Some fn ->
          Compat.Out_channel.with_open_gen
            [ Open_wronly ; Open_creat ; Open_append ] 0o644 fn print
      | None ->
          print stderr
    end ;
    if !debug_flag then Printexc.print_backtrace stderr ;
  end
#endif

type 'a msgf = (('a, Format.formatter, unit, unit) format4 -> 'a) -> unit

let report level fmt = Fmt.kstr (syslog level) ("@[" ^^ fmt ^^ "@]@?")
let info (msgf : 'a msgf) = msgf @@ report `LOG_INFO
let debug (msgf : 'a msgf) = msgf @@ report `LOG_DEBUG
let warn (msgf : 'a msgf) = msgf @@ report `LOG_WARNING
let err (msgf : 'a msgf) = msgf @@ report `LOG_ERR
