(* Copyright (c) 1998-2007 INRIA *)

let connection_closed = ref false

let eprintf = Printf.eprintf

let stop_server = ref "STOP_SERVER"
let cgi = ref false

let wserver_sock = ref Unix.stdout
let wsocket () = !wserver_sock

let wserver_oc = ref stdout
let woc () = !wserver_oc

let wflush () = flush !wserver_oc

let printnl () =
  if not !cgi then output_byte !wserver_oc 13;
  output_byte !wserver_oc 10

type printing_status = Nothing | Status | Contents

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

  let http status =
    if !printing_state <> Nothing then failwith "HTTP Status already sent";
    (* ignore translation \n to \r\n under Windows *)
    set_binary_mode_out !wserver_oc true; 
    printing_state := Status;
    (* printing header cgi mode : see https://www.ietf.org/rfc/rfc3875.txt (CGI/1.1) :
                                 "Status:" status-code SP reason-phrase NL
       otherwise http mode      : see https://tools.ietf.org/html/rfc7231 (HTTP/1.1 )
                                 "HTTP/1.1" SP status-code SP reason-phrase NL
    *)
    if !cgi then Printf.fprintf !wserver_oc "Status:%s" (status_string status)
    else Printf.fprintf !wserver_oc "HTTP/1.1 %s" (status_string status);
    printnl ()

let header s =
  if !printing_state <> Status then
    if !printing_state = Nothing then http Def.OK
    else failwith "Cannot write HTTP headers: page contents already started";
    if !cgi then
      (* In CGI mode, it MUST NOT return any header fields that relate to client-side communication issues 
      and could affect the server's ability to send the response to the client.*)
      let f, v = try let i = String.rindex s ':' in
        (String.sub s 0 i),
        (String.sub s (i + 2) (String.length s - i - 2))
      with Not_found -> (s, "")
      in
      match String.lowercase_ascii f with
      | "cache-control"
      | "content-encoding"
      | "content-length"
      | "content-type" 
      | "content-disposition"
      | "location" ->  
          (*no space need in CGI mode *)
          output_string !wserver_oc (f ^ ":" ^ v);
          printnl ()
      | _ ->
          () (* ignore other HTTP field*)
    else
      (output_string !wserver_oc s; printnl ())

let printf fmt =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl ();
      printing_state := Contents
    end;
  Printf.fprintf !wserver_oc fmt

let print_string s =
  if !printing_state <> Contents then
    begin
      if !printing_state = Nothing then http Def.OK;
      printnl ();
      printing_state := Contents
    end ;
  output_string !wserver_oc s

let http_redirect_temporarily url =
  http Def.Found;
  header ("Location: " ^ url)

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

let timeout tmout spid _ =
  Unix.kill spid Sys.sigkill;
  Unix.kill spid Sys.sigterm;
  let pid = Unix.fork () in
  if pid = 0 then
    if Unix.fork () = 0 then
      begin
        http Def.OK;
        header "Content-type: text/html; charset=UTF-8";
        printf "<head><title>Time out</title></head>\n";
        printf "<body><h1>Time out</h1>\n";
        printf "Computation time > %d second(s)\n" tmout;
        printf "</body>\n";
        wflush ();
        exit 0
      end
    else exit 0;
  let _ = Unix.waitpid [] pid in (); exit 2

let get_request_and_content strm =
  let meth, path_and_query, _, request = get_request strm in
  let content =
    match Mutil.extract_param "content-length: " ' ' request with
    | "" -> ""
    | x -> String.init (int_of_string x) (fun _ -> Stream.next strm)
  in
  meth, path_and_query, request, content

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
      eprintf "\nServer stopped by presence of file %s.\n" !stop_server;
      eprintf "Remove that file to allow servers to run again.\n";
      flush stderr;
      exit 0
    end

let print_internal_error e addr path query =
  if !printing_state = Nothing then 
    http (if !cgi then Def.OK else Def.Internal_Server_Error);
  if !printing_state = Status then 
    begin
      header "Content-type: text/html; charset=UTF-8";
    end;
  printf "<html>\n<title>Geneweb server error</title>\n<body>\
          <h1>Unexpected Geneweb error, request not complete :</h1>\
          <pre>- Raised with request %s?%s from %s\n\
          - Mode : %s\n\
          - Exception : %s\n\
          - Backtrace :<hr>\n%s\n</pre><hr>\
          <a href=\\>Geneweb Home page</a\
          </body>\n</html>\n"
          path query 
          (if addr = "" then "not significant" else addr)
          (if !cgi then "CGI" else "Server")
          (Printexc.to_string e) 
          (Printexc.get_backtrace ())
          ;
  wflush ()

let treat_connection tmout callback addr fd =
  if Sys.unix then
    if tmout > 0 then
      begin let spid = Unix.fork () in
        if spid > 0 then
          begin
            ignore @@ Sys.signal Sys.sigalrm (Sys.Signal_handle (timeout tmout spid)) ;
            ignore @@ Unix.alarm tmout ;
            ignore @@ Unix.waitpid [] spid ;
            ignore @@ Sys.signal Sys.sigalrm Sys.Signal_default ;
            exit 0
          end
      end;
  let (meth, path_and_query, request, contents) =
    let strm = Stream.of_channel (Unix.in_channel_of_descr fd) in
    get_request_and_content strm
  in 
  let path, query = try
    let i = String.index path_and_query '?' in
    String.sub path_and_query 0 i,
    String.sub path_and_query (i + 1) (String.length path_and_query - i - 1)
  with Not_found -> path_and_query, ""
  in
  let query = 
    if meth = Http_post then
      if query = "" then contents else (query ^ "&" ^ contents) 
    else query
  in
  match meth with
  | No_method ->  (* unlikely but possible if no data sent ; do nothing/ignore *)
      ()
  | Http_post     (* application/x-www-form-urlencoded *)
  | Http_get ->
      begin 
        try callback (addr, request) path query 
        with e -> 
          print_internal_error e (string_of_sockaddr addr) path query
      end
  | _ -> 
      http Def.Method_Not_Allowed;
      header "Content-type: text/html; charset=UTF-8";
      header "Connection: close";
      printf "<html>\n\
              <title>Not allowed HTTP method</title>\n\
              <body>Not allowed HTTP method</body>\n\
              </html>\n";
      wflush ();
  ;
  if !printing_state = Status then 
    failwith ("Unexcepted HTTP printing state, connection from " ^ (string_of_sockaddr addr) ^ " without content sent :" );
  printing_state := Nothing

(* elementary HTTP server, unix mode with fork   *)
#ifdef UNIX
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
            eprintf
              "*** %02d/%02d/%4d %02d:%02d:%02d %d process(es) remaining after cleanup (%d)\n"
              tm.Unix.tm_mday (succ tm.Unix.tm_mon) (1900 + tm.Unix.tm_year)
              tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
              (List.length !pids) (List.hd !pids);
            flush stderr;
            ()
          end
      done
  | None -> ()

let skip_possible_remaining_chars fd =
  if not !connection_closed then
    let b = Bytes.create 3 in
    try
      let rec loop () =
        match Unix.select [fd] [] [] 5.0 with
        | [_], [], [] ->
          let len = Unix.read fd b 0 (Bytes.length b) in
          if len = Bytes.length b then loop ()
        | _ -> ()
      in
      loop ()
    with Unix.Unix_error (Unix.ECONNRESET, _, _) -> ()
  
 let close_connection () =
  if not !connection_closed then begin
    wflush () ;
    Unix.shutdown !wserver_sock Unix.SHUTDOWN_SEND ;
    skip_possible_remaining_chars !wserver_sock ;
    (* Closing the channel flushes the data and closes the underlying file descriptor *)
    close_out !wserver_oc ;
    connection_closed := true
  end

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
        wserver_oc := Unix.out_channel_of_descr t;
        treat_connection tmout callback addr t ;
        close_connection () ;
        exit 0
      with
      | Unix.Unix_error (Unix.ECONNRESET, "read", _) -> exit 0
      | e -> raise e
    end
  | Some id ->
      Unix.close t;
      if max_clients = None then let _ = Unix.waitpid [] id in ()
      else pids := id :: !pids
  | None -> 
      Unix.close t; eprintf "Fork failed\n"; flush stderr
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

let wserver_basic syslog tmout g s addr_server =
  let server = {
    addr = addr_server;
    start_time = Unix.time ();
    fd = s;
    oc = stdout;
    kind = Server } 
  in
  let cl = ref [server] in
  let fdl = ref [s] in
  let conn_tmout = Float.of_int tmout in
  let used_mem () = 
    let st = Gc.stat () in 
    (st.live_blocks * Sys.word_size) / 8 / 1024
  in 
  let systime () =
    let now = Unix.gettimeofday () in
    let tm = Unix.localtime (now) in
    let sd = Float.to_int @@ 1000000.0 *. (mod_float now 1.0)  in 
    Printf.sprintf "%02d:%02d:%02d.%06d" tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec sd
  in
  let shutdown fd = try Unix.shutdown fd Unix.SHUTDOWN_ALL with
  | Unix.Unix_error(Unix.ECONNRESET, "shutdown", "") -> ()
  | exc -> raise exc
  in
  let select () = try Unix.select !fdl [] [] 5.0 with 
  | Unix.Unix_error(Unix.ECONNRESET, "select", "") -> ([], [], []) 
(* debug ------------- to be remove after tracking *)
  | Unix.Unix_error( _ , "select", "") as e -> 
      eprintf "%s - wserver_basic.select exception %s (waitng 5 sec)\n%!" (systime()) (Printexc.to_string e);
      Unix.sleep 1; 
      ([], [], []) 
(* debug ----------------------------------------- *)
  | exc -> raise exc
  in
  let mem_limit = ref (used_mem ()) in 
  syslog `LOG_DEBUG (Printf.sprintf "Starting with %d ko of memory used" !mem_limit);
  while true do
    check_stopping ();
    match select() with 
    | ([], _, _) ->
      let n = List.length !fdl in
      if n > 0 then
        begin 
          List.iter (fun conn -> 
            let ttl =  Unix.time() -. conn.start_time in
            if (ttl >= conn_tmout) && (conn.kind = Connected_client) then 
              begin
                shutdown conn.fd;
                close_out_noerr conn.oc;
                Unix.close conn.fd;
                fdl:=List.filter (fun fd -> fd<>conn.fd ) !fdl;
                conn.kind <- Closed_client;
                syslog `LOG_DEBUG (Printf.sprintf "%s connection closed" (string_of_sockaddr conn.addr) );
              end
            else if conn.kind = Connected_client then 
                flush conn.oc
            else 
              let mem = ref (used_mem ()) in 
                if !mem > !mem_limit then
                  begin 
                    syslog `LOG_INFO (Printf.sprintf "%d ko of memory used, invoke heap compaction" !mem);
                    Gc.compact (); 
                    mem := used_mem ();
                    mem_limit := !mem * 2;
                  end;
                Printf.eprintf "- %d..%d ko used   \r%!" !mem !mem_limit
          ) !cl;
          cl:=List.filter (fun t -> t.kind <> Closed_client ) !cl;
          let n = List.length !cl in
          if n > 10 then failwith ("Too many connection remain : " ^ (string_of_int n))
        end
    | ( l, _, _) -> 
      let rec loop l i = 
        match l with
        | [] ->
          ()
        | fd :: lfd -> 
          if fd=s then
            begin (* accept new incoming connection *)
              let (client_fd, client_addr) = Unix.accept fd in 
              Unix.setsockopt client_fd Unix.SO_KEEPALIVE true;
              cl :=  { 
                addr = client_addr;
                fd = client_fd;
                oc = Unix.out_channel_of_descr client_fd;
                start_time = Unix.time ();
                kind = Connected_client } :: !cl;
              fdl := client_fd :: !fdl
            end
          else
            begin (* treat incoming connection *)
              let conn = List.find ( fun t -> t.fd = fd ) !cl in
              let remove_from_poll fd =
                fdl:=List.filter (fun t -> t <> fd ) !fdl;
                cl:=List.filter (fun t -> t.fd <> fd ) !cl
              in 
              wserver_sock := conn.fd;
              wserver_oc := conn.oc;
              (* If a Windows computer goes to sleep, it can put the TCP / IP connections
               in use to sleep without sending all data.
               The remote host will therefore cut the TCP / IP connection on timeout. 
               When waking up the server can then try to send data to a closed connection. 
               This can generate a many normal exception (to be ignored) like :
               - Sys_error("An existing connection was forcibly closed by the remote host") *)
              try
                treat_connection tmout g conn.addr fd;
                flush conn.oc;
                remove_from_poll conn.fd;
                shutdown conn.fd;
              with _ -> ();
              close_out_noerr conn.oc;
              Unix.close conn.fd;
            end
          ;
          loop lfd (i+1)
      in
      loop l 0
  done
#endif
  
let f syslog addr_opt port tmout max_clients g =
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

  let tm = Unix.localtime (Unix.time ()) in
  eprintf "Ready %4d-%02d-%02d %02d:%02d port %d...\n%!"
    (1900 + tm.Unix.tm_year)
    (succ tm.Unix.tm_mon) tm.Unix.tm_mday tm.Unix.tm_hour tm.Unix.tm_min
    port ;
#ifdef WINDOWS
  wserver_basic syslog tmout g s a
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
