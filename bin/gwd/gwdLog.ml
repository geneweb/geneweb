open Geneweb

let verbosity = ref 5 (* default is Emergency to Notice level*)

let oc : out_channel option ref = ref None

let log fn =
  match !oc with
  | Some oc -> fn oc; flush oc
  | None -> ()

#ifdef UNIX  
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
    let log = Syslog.openlog @@ Filename.basename @@ Sys.executable_name in
    Syslog.syslog log level msg ;
    Syslog.closelog log
  end
#endif

#ifdef WINDOWS
let systime () =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime (now) in
  let sd = Float.to_int @@ 1000000.0 *. (mod_float now 1.0)  in 
  Printf.sprintf "%02d:%02d:%02d.%06d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec sd

let syslog level msg =
  let severityLevel, severity = 
  match level with
      | `LOG_EMERG -> 0, "Emergency"
      | `LOG_ALERT -> 1, "Alert"
      | `LOG_CRIT -> 2, "Critical"
      | `LOG_ERR -> 3, "Error"
      | `LOG_WARNING -> 4, "Warning"
      | `LOG_NOTICE -> 5, "Notice"
      | `LOG_INFO -> 6, "Info"
      | `LOG_DEBUG -> 7, "Debug"
  in 
  if !verbosity >= severityLevel then
    let tag = Filename.basename Sys.executable_name in
    let logfn =  Filename.concat !(Util.cnt_dir) "syslog.txt" in
    let oc = open_out_gen [Open_wronly; Open_append; Open_creat] 0o777 logfn in
    Printf.fprintf oc "%s\t%s[%s]\t%d\t%s\n" (systime ()) tag severity severityLevel msg;
    let logsize = out_channel_length oc in
    close_out_noerr oc;
    if logsize > 16000 then
      begin
        let bakfn = Filename.concat !(Util.cnt_dir) "syslog-bak.txt" in
        try 
          Unix.rename logfn bakfn; 
          let oc = open_out_gen [Open_wronly; Open_trunc; Open_creat] 0o777 logfn in
          Printf.fprintf oc "%s\t%s[Info]\t6\tClear log and save previous log to syslog-bak.txt\n" (systime ()) tag;
          close_out_noerr oc
        with _ -> 
          Printf.eprintf "Error saving %s file to %s; not saved !\n!" logfn bakfn;
          close_out_noerr oc
      end
#endif
