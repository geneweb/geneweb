include Geneweb_log_common.Common

let syslog (level : level) msg =
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
    let print oc =
      Printf.fprintf oc "[%s]: %s %s\n"
        (Ext_unix.sprintf_date tm :> string)
        level msg
    in
    (match Sys.getenv_opt "GW_SYSLOG_FILE" with
    | Some fn ->
        let oc =
          open_out_gen [ Open_wronly; Open_creat; Open_append ] 0o644 fn
        in
        print oc;
        close_out oc
    | None -> print stderr);
    if !debug then Printexc.print_backtrace stderr)
