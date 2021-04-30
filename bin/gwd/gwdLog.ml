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
  then 
    try 
      let log = Syslog.openlog @@ Filename.basename @@ Sys.executable_name in
      Syslog.syslog log level msg ;
      Syslog.closelog log
    with e -> 
      Printf.fprintf stderr  "----- Syslog writing, exception ignored : %s\n%!" (Printexc.to_string e);         
      Printexc.print_backtrace stderr;
      Prinf.stderr "- syslog is %s - %s\n%!" (Filename.basename @@ Sys.executable_name) msg;
      flush stderr
#endif

#ifdef WINDOWS
let systime () =
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime (now) in
  let sd = Float.to_int @@ 1000000.0 *. (mod_float now 1.0)  in 
  Printf.sprintf "%02d:%02d:%02d.%06d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec sd

let syslog_block = ref false

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
    let ident = Filename.basename @@ Sys.executable_name in
    let tag = (if String.length ident > 32 then String.sub ident 0 32 else ident) in
    let logfn =  Filename.concat !(Util.cnt_dir) "syslog.txt" in
    let log fname flag = try open_out_gen (flag::[Open_wronly; Open_creat]) 0o777 fname with e -> 
      if not !syslog_block then
        begin
          Printf.eprintf "Error : Syslog disabled, messages redirected to stderr !\n%s\n%!"  (Printexc.to_string e);
          syslog_block:=true
        end; 
      stderr;
    in
    let oc = log logfn Open_append in
    Printf.fprintf oc "%s\t%s[%s]\t%d\t%s\n" (systime ()) tag severity severityLevel msg;
    if oc <> stderr then
      let logsize = out_channel_length oc in
      if !syslog_block then
        begin
          Printf.fprintf oc "%s\t%s[Notice]\t5\tSome syslog messages were lost; error writing file\n%!" (systime ()) tag;
          Printf.eprintf "Syslog enabled; Writing successfull\n%!";
          syslog_block:=false;
        end;
      close_out oc;
      if logsize > 16000 then
        begin
          let bakfn = Filename.concat !(Util.cnt_dir) "syslog-bak.txt" in
          try Unix.rename logfn bakfn with _ -> ();
          let oc = log logfn Open_trunc in
          Printf.fprintf oc "%s\t%s[Info]\t6\tClear log and save previous log to %s\n" (systime ()) tag bakfn;
          close_out_noerr oc
        end
#endif
