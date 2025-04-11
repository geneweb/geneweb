include Geneweb_log_common.Common

let syslog (level : level) msg =
  let flags = if !debug then [ `LOG_PERROR ] else [] in
  if
    !verbosity
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
  then (
    let log =
      Syslog.openlog ~flags @@ Filename.basename @@ Sys.executable_name
    in
    Syslog.syslog log level msg;
    Syslog.closelog log;
    if !debug then Printexc.print_backtrace stderr)
