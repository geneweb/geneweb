let verbosity = ref 7

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

#ifdef SYSLOG
let syslog (level : level) msg =
#ifdef DEBUG
    let flags = [`LOG_PERROR] in
#else
    let flags = [] in
#endif
  if !verbosity
     >=
     match level with
     | `LOG_EMERG -> 0
     | `LOG_ALERT -> 1
     | `LOG_CRIT -> 2
     | `LOG_ERR -> 3
     | `LOG_WARNING -> 4
     | `LOG_NOTICE -> 5
     | `LOG_INFO -> 6
     | `LOG_DEBUG -> 7
  then begin
    let log = Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log ;
#ifdef DEBUG
    Printexc.print_backtrace stderr ;
#endif
  end
#endif

#ifndef SYSLOG
let syslog =
  let fname =
    try Sys.getenv "GW_SYSLOG_FILE"
    with Not_found -> Filename.basename Sys.executable_name ^ ".syslog"
  in
  fun (level : level) msg ->
    if !verbosity
       >=
       match level with
       | `LOG_EMERG -> 0
       | `LOG_ALERT -> 1
       | `LOG_CRIT -> 2
       | `LOG_ERR -> 3
       | `LOG_WARNING -> 4
       | `LOG_NOTICE -> 5
       | `LOG_INFO -> 6
       | `LOG_DEBUG -> 7
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
      let oc = open_out_gen [ Open_wronly ; Open_creat ; Open_append ] 0o644 fname in
      Printf.fprintf oc "[%s]: %s %s\n" (Mutil.sprintf_date tm) level msg ;
      close_out oc ;
#ifdef DEBUG
    Printexc.print_backtrace stderr ;
#endif
  end
#endif
