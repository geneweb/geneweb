open Geneweb

let verbosity = ref 5 (* default is Emergency to Notice level*)

let oc : out_channel option ref = ref None

let open_log fname =
  let fname = 
    if String.index_opt fname '\\' = None && String.index_opt fname '/' = None
    then Filename.concat !(Util.cnt_dir) fname 
    else fname 
  in
  let gtw_mode = try let _ = Sys.getenv "GATEWAY_INTERFACE" in true with Not_found -> false in
  match fname with 
  | "2" | "stderr" -> oc:= Some stderr 
  | _ -> 
    oc := 
      Some (
        if gtw_mode then open_out_gen [Open_wronly; Open_creat; Open_append] 0o644 fname
        else open_out_gen [Open_wronly; Open_creat; Open_trunc] 0o644 fname )

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
      Printf.eprintf "----- Syslog writing, exception ignored : %s\n%!" (Printexc.to_string e);
#ifdef DEBUG    
      Printexc.print_backtrace stderr;
#endif
      Printf.eprintf "- syslog is %s - %s\n%!" (Filename.basename @@ Sys.executable_name) msg;
      flush stderr
#endif

let log_exn e backtrace addr path query = 
  let systime () =
    let now = Unix.gettimeofday () in
    let tm = Unix.localtime (now) in
    let sd = Float.to_int @@ 1000000.0 *. (mod_float now 1.0)  in 
    Printf.sprintf "%02d:%02d:%02d.%06d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec sd
  in
  let rec env_vars lvar =
    match lvar with
    | var::lvar -> 
        (Printf.sprintf 
            "   %s=%s\n" 
            var (try Sys.getenv var with Not_found -> "")
        ) ^ (env_vars lvar)
    | _ -> ""
  in
  let fname = Filename.concat !(Util.cnt_dir) ((Filename.basename Sys.executable_name) ^ "-error.txt ") in
  Printf.eprintf "%s : Error encountered, see %s\n%!" (systime())  fname;
  let msg = Printf.sprintf 
              "---- %s - Unexcepted exception : %s\n\
              - Raised with request %s?%s%s\n\
              - Mode : %s, process id = %d\n\
              - Environnement :\n%s\
              - if available, backtrace :\n%s\n"
              (systime()) 
              (match e with
              | Sys_error msg -> "Sys_error - " ^ msg
              | e -> Printexc.to_string e) 
              path query 
              ((if addr <> "" then " from " else "") ^ addr)
              (if !Wserver.cgi then "CGI script" else "HTTP server") (Unix.getpid ())
              (env_vars [ "LANG"; "REMOTE_HOST"; "REMOTE_ADDR"; "SCRIPT_NAME"; "PATH_INFO"; "QUERY_STRING"
                        ; "SERVER_NAME"; "SERVER_PORT"; "SERVER_PROTOCOL"; "SERVER_SOFTWARE"
                        ; "AUTH_TYPE"; "REQUEST_METHOD"; "CONTENT_TYPE"; "CONTENT_LENGTH"
                        ; "HTTP_ACCEPT_LANGUAGE"; "HTTP_REFERER"; "HTTP_USER_AGENT";"GATEWAY_INTERFACE" ]
              )
              backtrace 
  in
  try
    let oc = open_out_gen ([Open_wronly; Open_append; Open_creat]) 0o777 fname in
    let logsize = out_channel_length oc in 
    let oc = if logsize < 16000 then oc 
    else (close_out oc; open_out_gen ([Open_wronly; Open_trunc; Open_creat]) 0o777 fname)
    in 
    output_string oc msg;
    close_out_noerr oc;
    fname
  with _ -> 
    output_string stderr msg;
    flush stderr;
    ""

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
    let logfn = Filename.concat !(Util.cnt_dir) "syslog.txt" in
    let log fname flag = 
      try 
        let oc = open_out_gen (flag::[Open_wronly; Open_creat]) 0o644 fname in
        oc, (out_channel_length oc)
      with e -> 
      if not !syslog_block then
        begin
          Printf.eprintf "Error : Syslog disabled, messages redirected to stderr !\n%s\n%!"  (Printexc.to_string e);
          syslog_block:=true
        end; 
      stderr, -1;
    in
    let oc, logsize = log logfn Open_append in
    Printf.fprintf oc "%s\t%s[%s]\t%d\t%s\n" (systime ()) tag severity severityLevel msg;
    if oc <> stderr then
      begin
        if !syslog_block then
          begin
            Printf.fprintf oc "%s\t%s[Notice]\t5\tSome syslog messages were lost (error writing file)\n%!" (systime ()) tag;
            Printf.eprintf "Syslog enabled; Writing successfull\n%!";
            syslog_block:=false
          end;
        close_out oc;
        if logsize > 16000 then
          begin
            let bakfn = Filename.concat !(Util.cnt_dir) "syslog-bak.txt" in
            let saved = (try Unix.rename logfn bakfn; true with _ ->
              Printf.eprintf "Error : backup %s to %s failed\n%!" logfn bakfn; false) 
            in
            let oc, _ = log logfn Open_trunc in
            Printf.fprintf oc "%s\t%s[Info]\t6\t%s log %s saved to %s and cleaned\n" 
                (systime ()) tag (if not saved then "note" else "") logfn bakfn;
            close_out oc
          end
      end
#endif
