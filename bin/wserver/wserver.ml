(* Copyright (c) 1998-2007 INRIA *)

#ifdef WINDOWS
(* temporary files for process exchange *)
let in_fname = ref ("~" ^ (Filename.remove_extension (Filename.basename Sys.executable_name)) ^ "_post_rq.txt")
let out_fname = ref ("~" ^ (Filename.remove_extension (Filename.basename Sys.executable_name)) ^ "_response.txt")
#endif 
  
let stop_server = ref "STOP_SERVER"
let noproc = ref false
let cgi = ref false

let wserver_sock = ref Unix.stdout

let shutdown_noerr fd = 
  try Unix.shutdown fd Unix.SHUTDOWN_ALL with _ -> ()

type printing_state = Nothing | Status | Contents

let printing_state = ref Nothing

let status_string status = 
  match status with
  | Def.OK -> "200 OK"
  | Def.Moved_Permanently -> "301 Moved Permanently"
  | Def.Found -> "302 Found"
  | Def.Bad_Request -> "400 Bad Request"
  | Def.Unauthorized -> "401 Unauthorized"
  | Def.Forbidden -> "403 Forbidden"
  | Def.Not_Found -> "404 Not Found"
  | Def.Method_Not_Allowed -> "405 Method Not Allowed"
  | Internal_Server_Error -> "500 Internal Server Error"
  | Service_Unavailable -> "503 Service Unavailable"
  | HTTP_Version_Not_Supported -> "505 HTTP Version Not Supported"

(* max_http like TCP PDU size : typical max MTU IP (1400) to MAX TCP PDU (64k) *)
let max_http = ref 65536
let http_buff = Buffer.create !max_http
let last_status = ref ""
let buffer_add_nl s =
  Buffer.add_string http_buff s;
  if not !cgi then Buffer.add_int8 http_buff 13;
  Buffer.add_int8 http_buff 10

(* printing header cgi mode : see https://www.ietf.org/rfc/rfc3875.txt (CGI/1.1) :
                              "Status:" status-code SP reason-phrase NL
    otherwise http mode      : see https://tools.ietf.org/html/rfc7231 (HTTP/1.1 )
                              "HTTP/1.1" SP status-code SP reason-phrase NL *)
let http status =
  if !printing_state <> Nothing then failwith "HTTP Status already sent";
  printing_state := Status;
  last_status := status_string status;
  let response_status = 
    if !cgi then Printf.sprintf  "Status:%s" !last_status
    else Printf.sprintf "HTTP/1.1 %s" !last_status;
  in
  Buffer.clear http_buff;
  buffer_add_nl response_status

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
    (* In CGI mode, it MUST NOT return any header fields that relate to client-side 
    communication issues and could affect the server's ability to send the response to the client.*)
    let f, v = try let i = String.index s ':' in
      (String.sub s 0 i),
      (String.sub s (i + 2) (String.length s - i - 2))
    with Not_found -> (s, "")
    in
    match String.lowercase_ascii f with
    | "connection" -> ()
    | "date"
    | "server" ->  (* ignore HTTP field, not need in CGI mode*)
      if not !cgi then buffer_add_nl s
    | _ ->
      buffer_add_nl (if !cgi then f ^ ":" ^ v else s)

let print_buffer s =
  let len = String.length s in
  let olen = Buffer.length http_buff in
  let n = !max_http - olen in
  if n < len then begin
    if n > 0 then Buffer.add_string http_buff (String.sub s 0 n);
    let written = Unix.write_substring !wserver_sock (Buffer.contents http_buff) 0 (olen+n) in
    if written <> (olen+n) then
      Printf.eprintf "Error sending response,  %d/%d bytes sent\n%!" written (olen+n);
    Buffer.clear http_buff;
    if (len - n) < !max_http then begin
      if len > n then Buffer.add_string http_buff (String.sub s n (len-n))
    end else begin
      let written = Unix.write_substring !wserver_sock s n (len-n) in
      if written <> (len-n) then
        Printf.eprintf "Error sending response,  %d/%d bytes sent\n%!" written (len-n)
    end
  end
  else
    Buffer.add_string http_buff s

let print_string s =
  if !printing_state <> Contents then begin
    if !printing_state = Nothing then http Def.OK;
    (*  see RFC 7320, §6.1 : https://tools.ietf.org/html/rfc7230#page-50
        A server that does not support persistent connections MUST send the
        "close" connection option in every response message that does not
        have a 1xx (Informational) status code.*)
    if not !cgi then buffer_add_nl "Connection: close";
    buffer_add_nl "";
    if !cgi then begin
      (* ignore translation \n to \r\n under Windows *)
      set_binary_mode_out stdout true;
      print_string (Buffer.contents http_buff)
    end;
    printing_state := Contents
  end;
  if !cgi then print_string s
  else if s <> "" then print_buffer s

let printf fmt = Printf.ksprintf print_string fmt

let wflush () =
  if !cgi then flush stdout 
  else
  let olen = Buffer.length http_buff in
  if olen > 0 then
    let written = Unix.write_substring !wserver_sock (Buffer.contents http_buff) 0 olen in 
    if written <> olen then 
      Printf.eprintf "Error sending response (flush),  %d/%d bytes sent\n%!" written olen;
    Buffer.reset http_buff

let print_filename fname ctype priv = 
  let res = if priv then "private" else "public" in
  match try Some (open_in_bin fname) with _ -> None with
    Some ic ->
      let buf = Bytes.create !max_http in
      let flen = in_channel_length ic in
      http Def.OK;
      header (Printf.sprintf "Content-Type: %s" ctype);
      header (Printf.sprintf "Content-Length: %d" flen);
      header (Printf.sprintf "Content-Disposition: inline; filename=%s" (Filename.basename fname));
      header (Printf.sprintf "Cache-Control: %s, max-age=%d" res (60 * 60 * 24 * 365));
      print_string "";
      wflush ();
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          let written = Unix.write !wserver_sock buf 0 olen in
          if written <> olen then
            Printf.eprintf "Error sending response,  %d/%d bytes sent\n%!" written olen;
          loop (len - olen)
      in
      loop flen;
      close_in ic;
      true
  | None ->
      false
      
let print_http_filename fname = 
  match try Some (open_in_bin fname) with _ -> None with
    Some ic ->
      let buf = Bytes.create !max_http in
      let flen = in_channel_length ic in
      let rec loop len =
        if len = 0 then ()
        else
          let olen = min (Bytes.length buf) len in
          really_input ic buf 0 olen;
          let written = Unix.write !wserver_sock buf 0 olen in
          if written <> olen then 
            Printf.eprintf "Error sending response,  %d/%d bytes sent\n%!" written olen;
          loop (len - olen)
      in
      loop flen;
      close_in ic;
      true
  | None ->
      false

let http_redirect_temporarily url =
  http Def.Found;
  header ("Location: " ^ url);
  print_string "";
  wflush ()

let buff = ref (Bytes.create 80)
let store len x =
  if len >= Bytes.length !buff then
    buff := Bytes.extend !buff 0 (Bytes.length !buff);
  Bytes.set !buff len x;
  succ len
let get_buff len = Bytes.sub_string !buff 0 len

(* HTTP/1.1 method see  https://tools.ietf.org/html/rfc7231 , §4.3 *)
type http_method = Http_get | Http_head | Http_post | Http_put | Http_delete 
                 | Http_connect | Http_options | Http_trace | Unknown_method | No_method

let get_request strm =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
      Some '\010' ->
        Stream.junk strm__;
        let s = strm__ in
        if len = 0 then [] else let str = get_buff len in str :: loop 0 s
    | Some '\013' -> Stream.junk strm__; loop len strm__
    | Some c -> Stream.junk strm__; loop (store len c) strm__
    | _ -> if len = 0 then [] else [get_buff len]
  in
  let request = loop 0 strm in
  let http_request = try List.hd request with _ -> "" in
  let meth, s, m = 
    try let i = String.index http_request ' ' in
      (match (String.uppercase_ascii @@ String.sub http_request 0 i) with
        | "GET" -> Http_get
        | "HEAD" -> Http_head
        | "POST" -> Http_post
        | "PUT" -> Http_put
        | "DELETE" -> Http_delete
        | "CONNECT" -> Http_connect
        | "OPTIONS" -> Http_options
        | "TRACE" -> Http_trace
        | _ -> Unknown_method)
        (* note : in HTTP, the first / is mandatory for the path. 
        It is intentionally omitted in path_and_query for further processing  *)
      , (String.sub http_request (i + 2) (String.length http_request - i - 2) )
      , (String.uppercase_ascii @@ String.sub http_request 0 i)
    with Not_found -> No_method, "",""
  in
  let (path_and_query, http_ver) = try let i = String.rindex s ' ' in
    String.sub s 0 i,
    String.sub s (i + 1) (String.length s - i - 1)
  with Not_found -> (s, "")
  in 
  meth, path_and_query, http_ver, request

let get_request_and_content strm =
  let meth, path_and_query, _, request = get_request strm in
  let contents =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  let path, query = try
    let i = String.index path_and_query '?' in
    String.sub path_and_query 0 i,
    String.sub path_and_query (i + 1) (String.length path_and_query - i - 1)
  with Not_found -> path_and_query, ""
  in
  meth, path, query, request, contents

let string_of_sockaddr =
  function
    Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, p) -> (Unix.string_of_inet_addr a) ^ ":" ^ (string_of_int p)

let sockaddr_of_string str =
  try
    let i = String.index str ':' in
    Unix.ADDR_INET (
      Unix.inet_addr_of_string (String.sub str 0 i), 
      int_of_string (String.sub str (i + 1) (String.length str - i - 1) )
    )
  with _ ->  Unix.ADDR_UNIX str

let check_stopping () =
  if Sys.file_exists !stop_server then
    begin
      flush stdout;
      Printf.eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
      Printf.eprintf "Remove that file to allow servers to run again.\n";
      flush stderr;
      exit 0
    end

let print_internal_error e addr path query backtrace =
  let state = !printing_state in
  if !printing_state <> Contents then begin
    http (if !cgi then Def.OK else Def.Internal_Server_Error);
    header "Content-type: text/html; charset=UTF-8";
  end;
  print_string ( Printf.sprintf 
                "<!DOCTYPE html>\n<html><head><title>Geneweb server error</title></head>\n<body>\n\
                  <h1>Unexpected Geneweb error, request not complete :</h1>\n\
                  <pre>- Raised with request %s?%s%s\n\
                  - Mode : %s, process id = %d\n\
                  - HTTP state (printing_state) : %s before exception\n\
                  - Exception : %s\n\
                  %s\n</pre><hr>\n\
                  <a href=\\>[Home page]</a>  \
                  <a href=\"javascript:window.history.go(-1)\">[Previous page]</a>\n\
                  </body>\n</html>\n"
                  path query 
                  ((if addr <> "" then " from " else "") ^ addr)
                  (if !cgi then "CGI script" else "HTTP server") (Unix.getpid ())
                  (match state with 
                    | Nothing -> "Nothing sent"
                    | Status -> "HTTP status set without content"
                    | Contents -> "Some HTTP data sent" 
                  )
                  (match e with 
                  | Sys_error msg -> "Sys_error - " ^ msg
                  | _ -> Printexc.to_string e
                  )
                  (if backtrace = "" then "" else ("- Backtrace :\n<hr>\n" ^ backtrace))
                ) 

#ifdef WINDOWS
let proceed_request addr request path query contents = 
  let addr_string = match addr with
    Unix.ADDR_UNIX a -> a
  | Unix.ADDR_INET (a, _) -> Unix.string_of_inet_addr a
  in
  let exec_script = Sys.argv.(0) in
  let postdata_len = String.length contents in
  let env = Array.append (Unix.environment ())
  [| "GATEWAY_INTERFACE=RELAY/HTTP" 
  ; "REMOTE_HOST="
  ; "REMOTE_ADDR=" ^ addr_string
  ; "REQUEST_METHOD=" ^ (if postdata_len > 0 then "POST" else "GET")
  ; "SCRIPT_NAME=" ^ exec_script
  ; "PATH_INFO=" ^ path
  ; "QUERY_STRING=" ^ query
  ; "HTTP_COOKIE=" ^ (Mutil.extract_param "content-length: " '\n' request)
  ; "CONTENT_TYPE=" ^ (Mutil.extract_param "content-type: " '\n' request)
  ; "CONTENT_LENGTH=" ^ (string_of_int postdata_len)
  ; "HTTP_ACCEPT_LANGUAGE=" ^ (Mutil.extract_param "accept_language: " '\n' request)
  ; "HTTP_REFERER=" ^ (Mutil.extract_param "referer: " '\n' request)
  ; "HTTP_USER_AGENT=" ^ (Mutil.extract_param "user-agent: " '\n' request)
  ; "HTTP_AUTHORIZATION=" ^ (Mutil.extract_param "authorization: " '\n' request)
  |]
  in
  let fd_in = 
    if postdata_len = 0 then
      Unix.stdin
    else
      let fd_out = Unix.openfile !in_fname [Unix.O_WRONLY; O_CREAT; O_TRUNC] 0o640 in
        if postdata_len <> Unix.write_substring fd_out contents 0 postdata_len then 
          failwith ("Wserver error, writing post data to file : " ^ !in_fname);
        Unix.close fd_out;
        Unix.openfile !in_fname [Unix.O_RDONLY] 0o640 
  in
  let fd_out = Unix.openfile !out_fname [Unix.O_WRONLY; O_CREAT; O_TRUNC] 0o640 in
  let pid = Unix.create_process_env exec_script Sys.argv env fd_in fd_out Unix.stderr in
  Unix.close fd_out;
  if postdata_len > 0  then Unix.close fd_in;
  ignore @@ Unix.waitpid [] pid;
  if not (print_http_filename !out_fname) then
    failwith ("Wserver error, creating response file : "  ^ !out_fname)
  else begin
    if postdata_len > 0 then Sys.remove !in_fname;
    Sys.remove !out_fname
  end
#endif

let treat_connection tmout callback addr fd =
  printing_state := Nothing;
  let (meth, path, query, request, contents) =
    let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
    get_request_and_content strm
  in 
  begin match meth with
  | No_method -> (* unlikely but possible if no data sent ; do nothing/ignore *)
      ()
  | Http_post
  | Http_get ->
      begin 
        try 
#ifdef WINDOWS
          let is_request = 
               (Filename.extension path) = "" &&
               (String.index_opt path '/' = None) && (String.index_opt path '\\') = None
          in
          if is_request && not !noproc then 
            proceed_request addr request path query contents
          else
#endif
            callback (addr, request) path (if meth = Http_get then query else contents)
        with 
        | Unix.Unix_error (_, "write", _) as e -> raise e
        | e -> 
          let sock_error = match Unix.getsockopt_error fd with
          | Some m -> Unix.error_message m
          | None -> "No socket error"
          in
          let backtrace =  Printexc.get_backtrace () in
          print_internal_error e ((string_of_sockaddr addr) ^ ", " ^ sock_error) path query backtrace
      end
  | _ -> 
      begin
        http Def.Method_Not_Allowed;
        header "Content-Type: text/html; charset=UTF-8";
        header "Connection: close";
        print_string "<html>\n\
                      <<head><title>Not allowed HTTP method</title></head>\n\
                      <body>Not allowed HTTP method</body>\n\
                      </html>\n"
      end
  end;
  if !printing_state = Status then 
    failwith ("Unexcepted HTTP printing state, connection from " ^ (string_of_sockaddr addr) ^ " without content sent :" )
  
(* elementary HTTP server, unix mode with forks *)
#ifdef UNIX

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  Unix.kill spid Sys.sigterm;
  let pid = Unix.fork () in
  if pid = 0 then
    if Unix.fork () = 0 then
      begin
        http Def.OK;
        header "Content-Type: text/html; charset=UTF-8";
        print_string "<head><title>Time out</title></head>\n";
        print_string "<body><h1>Time out</h1>\n";
        print_string ("Computation time > " ^ string_of_int tmout ^ "second(s)\n");
        print_string "</body>\n";
        wflush ();
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let rec list_remove x =
  function
    [] -> failwith "list_remove"
  | y :: l -> if x = y then l else y :: list_remove x l

let pids = ref []

let cleanup_sons () =
  List.iter begin fun p ->
    match fst (Unix.waitpid [ Unix.WNOHANG ] p) with
    | 0 -> ()
    | exception Unix.Unix_error (Unix.ECHILD, "waitpid", _)
    (* should not be needed anymore since [Unix.getpid () <> ppid] loop security *)
    | _ -> pids := list_remove p !pids
  end !pids

let wait_available max_clients s =
  match max_clients with
    Some m ->
      if List.length !pids >= m then
        (let (pid, _) = Unix.wait () in pids := list_remove pid !pids);
      if !pids <> [] then cleanup_sons ();
      let stop_verbose = ref false in
      while !pids <> [] && Unix.select [s] [] [] 15.0 = ([], [], []) do
        cleanup_sons ();
        if !pids <> [] && not !stop_verbose then
          begin
            stop_verbose := true;
            let tm = Unix.localtime (Unix.time ()) in
            Printf.eprintf
              "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n"
              tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
              (List.length !pids) (List.hd !pids);
            flush stderr;
            ()
          end
      done
  | None -> ()

let accept_connection tmout max_clients callback s =
  wait_available max_clients s;
  let (t, addr) = Unix.accept s in
  Unix.setsockopt t Unix.SO_KEEPALIVE true;
  match try Some (Unix.fork ()) with _ -> None with
  | Some 0 ->
    begin
      try
        if max_clients = None && Unix.fork () <> 0 then exit 0;
        Unix.close s;
        wserver_sock := t;
        if tmout > 0 then begin 
          let spid = Unix.fork () in
            if spid > 0 then begin
              ignore @@ Sys.signal Sys.sigalrm (Sys.Signal_handle (timeout tmout spid)) ;
              ignore @@ Unix.alarm tmout ;
              ignore @@ Unix.waitpid [] spid ;
              ignore @@ Sys.signal Sys.sigalrm Sys.Signal_default ;
              exit 0
            end
        end;
        treat_connection tmout callback addr t;
        wflush ();
        shutdown_noerr t;
        Unix.close t;
        exit 0
      with
      | Unix.Unix_error (Unix.ECONNRESET, _, _)
      | Unix.Unix_error (Unix.ECONNABORTED, _, _) -> Unix.close t; exit 0
      | e -> raise e
    end
  | Some id ->
      Unix.close t;
      if max_clients = None then let _ = Unix.waitpid [] id in ()
      else pids := id :: !pids
  | None -> 
      Unix.close t; Printf.eprintf "Fork failed\n"; flush stderr
#endif

(* elementary HTTP server, basic mode for windows *)
#ifdef WINDOWS
type conn_kind = Server | Connected_client | Closed_client
type conn_info = {
    addr : Unix.sockaddr;
    fd : Unix.file_descr;
    oc : out_channel;
    start_time : float;
    mutable kind : conn_kind }

let wserver_basic syslog tmout max_clients g s addr_server =
  let server = {
    addr = addr_server;
    start_time = Unix.time ();
    fd = s;
    oc = stdout;
    kind = Server } 
  in
  let cl = ref [server] in
  let fdl = ref [s] in
  let remove_from_poll fd =
    fdl:=List.filter (fun t -> t <> fd ) !fdl;
    cl:=List.filter (fun t -> t.fd <> fd ) !cl
  in 
  let max_conn = 
    match max_clients with
    | Some max -> if max = 0 then 10 else max * 5
    | None -> 10
  in 
  let conn_tmout = Float.of_int (if tmout < 30 then 30 else tmout) in
  let used_mem () = 
    let st = Gc.stat () in 
    (st.live_blocks * Sys.word_size) / 8 / 1024
  in 
  let err_nb = ref 0 in
  let mem_limit = ref (used_mem ()) in 
  while true do
    check_stopping ();
    match Unix.select !fdl [] [] 15.0 with 
    | ([], _, _) ->
      let n = List.length !fdl in
      if n > 0 then begin 
          List.iter (fun conn -> 
            let ttl =  Unix.time() -. conn.start_time in
            if (ttl >= conn_tmout) && (conn.kind = Connected_client) then begin
                shutdown_noerr conn.fd;
                Unix.close conn.fd;
                fdl:=List.filter (fun t -> t <> conn.fd ) !fdl;
                conn.kind <- Closed_client
              end else 
            if conn.kind = Connected_client then begin
                try let _ = Unix.getpeername conn.fd in ()
                with e -> 
                  fdl:=List.filter (fun t -> t <> conn.fd ) !fdl;
                  conn.kind <- Closed_client
                end else
            let mem = ref (used_mem ()) in 
              if !mem > !mem_limit then begin
                  syslog `LOG_INFO (Printf.sprintf "%d ko of memory used, invoke heap compaction" !mem);
                  Gc.compact (); 
                  mem := used_mem ();
                  mem_limit := !mem * 2;
              end
#ifdef DEBUG
              ;Printf.eprintf "Cnt[%d], Err[%d] - %d..%d ko used   \r%!" (List.length !fdl) !err_nb !mem !mem_limit
#endif
          ) !cl;
          cl:=List.filter (fun conn -> conn.kind <> Closed_client ) !cl;
          let n = List.length !cl in
          if n > max_conn then failwith ("Too many connection remaining : " ^ (string_of_int n))
        end
    | ( l, _, _) -> 
      let rec loop l i = 
        match l with
        | [] ->
          ()
        | fd :: lfd -> 
          if fd=s then begin (* accept new incoming connection *)
            let (client_fd, client_addr) = Unix.accept ~cloexec:true fd in 
            Unix.setsockopt client_fd Unix.SO_KEEPALIVE true;
            cl :=  { 
              addr = client_addr;
              fd = client_fd;
              oc = Unix.out_channel_of_descr client_fd;
              start_time = Unix.gettimeofday ();
              kind = Connected_client } :: !cl;
              fdl := client_fd :: !fdl
          end else begin (* treat incoming connection *)
            let conn = List.find ( fun t -> t.fd = fd ) !cl in
            begin try 
              wserver_sock := conn.fd;
              treat_connection tmout g conn.addr fd;
              wflush ()
            with 
            | Unix.Unix_error (Unix.ECONNRESET, "write", _) 
            | Unix.Unix_error (Unix.ECONNABORTED, "write", _) ->
                incr err_nb
            | e -> raise e
            end;
            shutdown_noerr conn.fd;
            Unix.close conn.fd;
            remove_from_poll conn.fd
          end;
          loop lfd (i+1)
      in
      loop l 0
  done
#endif
  
let create working_dir syslog addr_opt port tmout max_clients g =
#ifdef WINDOWS
  (* temporary files for process exchange *)
  in_fname := Filename.concat working_dir !in_fname;
  out_fname := Filename.concat working_dir !out_fname;
#endif 
  let addr =
    match addr_opt with
      Some addr ->
        begin try Unix.inet_addr_of_string addr with
          Failure _ -> (Unix.gethostbyname addr).Unix.h_addr_list.(0)
        end
    | None -> Unix.inet_addr_any
  in
  let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let a =  (Unix.ADDR_INET (addr, port)) in 
  Unix.bind s a;
  Unix.listen s 4;
  Unix.setsockopt s Unix.SO_KEEPALIVE true;
  let tm = Unix.localtime (Unix.time ()) in
  Printf.eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n%!"
    (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    port ;
#ifdef WINDOWS
  wserver_basic syslog tmout max_clients g s a
#else
  let _ = Unix.nice 1 in
  while true do
    check_stopping ();
    try accept_connection tmout max_clients g s with
    | Unix.Unix_error (Unix.ECONNRESET, "accept", _) as e ->
      syslog `LOG_INFO (Printexc.to_string e)
    | Sys_error msg as e when msg = "Broken pipe" ->
      syslog `LOG_INFO (Printexc.to_string e)
    | e -> raise e
  done
#endif
