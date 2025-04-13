let verbosity = ref 7
let debug = ref false

let oc : out_channel option ref = ref None

let log fn =
  match !oc with
  | Some oc -> fn oc
  | None -> ()

type level =
  [ `LOG_ALERT
  | `LOG_CRIT
  | `LOG_DEBUG
  | `LOG_EMERG
  | `LOG_ERR
  | `LOG_INFO
  | `LOG_NOTICE
  | `LOG_WARNING
  ]

let syslog (level : level) msg =
  let verbosity_level =
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
  let flags = if !debug then [`LOG_PERROR] else [] in
  if !verbosity >= verbosity_level
  then begin
    let log = Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log ;
    if !debug then Printexc.print_backtrace stderr ;
  end
#else
  let () = () in
  if !verbosity >= verbosity_level
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
    let print oc = Printf.fprintf oc "[%s]: %s %s\n%!"
        (Mutil.sprintf_date tm :> string) level msg in
    begin match Sys.getenv_opt "GW_SYSLOG_FILE" with
      | Some fn ->
        let oc = open_out_gen [ Open_wronly ; Open_creat ; Open_append ] 0o644 fn in
        print oc ;
        close_out oc
      | None -> print stderr
    end ;
    if !debug then Printexc.print_backtrace stderr ;
  end
#endif
