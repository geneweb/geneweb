let verbosity = ref 7

let oc : out_channel option ref = ref None

let log fn =
  match !oc with
  | Some oc -> fn oc
  | None -> ()

let syslog level msg =
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
    let log = Syslog.openlog Filename.(dirname @@ basename @@ Sys.executable_name) in
    Syslog.syslog log level msg ;
    Syslog.closelog log
  end
