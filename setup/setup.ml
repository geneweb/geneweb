(* camlp4r *)
(* $Id: setup.ml,v 1.8 1999-05-03 07:09:35 ddr Exp $ *)

value port = 2316;
value default_lang = "en";
value setup_dir = "setup";

value buff = ref (String.create 80);
value store len x =
  do if len >= String.length buff.val then
       buff.val := buff.val ^ String.create (String.length buff.val)
     else ();
     buff.val.[len] := x;
  return succ len
;
value get_buff len = String.sub buff.val 0 len;

value rec list_remove_assoc x =
  fun
  [ [(x1, y1) :: l] ->
      if x = x1 then l else [(x1, y1) :: list_remove_assoc x l]
  | [] -> [] ]
;

value header_no_page_title title =
  do Wserver.html "";
     Wserver.wprint "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
 \"http://www.w3.org/TR/REC-html40/loose.dtd\">\n";
     Wserver.wprint "<head>\n";
     Wserver.wprint "  <meta name=\"ROBOTS\" content=\"NONE\">\n";
     Wserver.wprint "  <title>";
     title True;
     Wserver.wprint "</title>\n";
     Wserver.wprint "</head>\n";
     Wserver.wprint "<body>\n";
  return ()
;

value trailer () =
  do Wserver.wprint "<p>\n";
     Wserver.wprint "
<hr><font size=-1><em>(c) Copyright INRIA 1999 -
GeneWeb %s</em></font>" Version.txt;
     Wserver.wprint "<br>";
     Wserver.wprint "</body>\n";
  return ()
;

value header title =
  do header_no_page_title title;
     Wserver.wprint "<h1>";
     title False;
     Wserver.wprint "</h1>\n";
  return ()
;

value strip_spaces str =
  let start = loop 0
    where rec loop i =
      if i == String.length str then i
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i ]
  in
  let stop = loop (String.length str - 1)
    where rec loop i =
      if i == -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start == 0 && stop == String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value code_varenv = Wserver.encode;
value decode_varenv = Wserver.decode;

value getenv env label =
  decode_varenv (List.assoc (decode_varenv label) env)
;

value p_getenv env label =
  try Some (getenv env label) with [ Not_found -> None ]
;

value s_getenv env label =
  try getenv env label with [ Not_found -> "" ]
;

value rec skip_spaces s i =
  if i < String.length s && s.[i] == ' ' then skip_spaces s (i + 1)
  else i
;

value create_env s =
  let rec get_assoc beg i =
    if i == String.length s then
      if i == beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] == ';' || s.[i] == '&' then
      let next_i = skip_spaces s (succ i) in
      [String.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] == '=' then
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)
;

value numbered_key k =
  if k = "" then None
  else
    match k.[String.length k - 1] with
    [ '1'..'9' as c -> Some (String.sub k 0 (String.length k - 1), c)
    | _ -> None ]
;

value parameters =
  loop "" where rec loop comm =
    fun
    [ [(k, s) :: env] ->
        let k = strip_spaces (decode_varenv k) in
        let s = strip_spaces (decode_varenv s) in
        if k = "" || s = "" then loop comm env
        else if k = "opt" then loop comm env
        else if k = "anon" then loop (comm ^ " " ^ s) env
        else
          match numbered_key k with
          [ Some (k, '1') ->
              let (s, env) =
                loop ("\"" ^ s ^ "\"") env where rec loop s =
                  fun
                  [ ([(k1, s1) :: env] as genv) ->
                      match numbered_key k1 with
                      [ Some (k1, _) when k1 = k ->
                          let s1 = strip_spaces (decode_varenv s1) in
                          let s =
                            if s1 = "" then s else s ^ " \"" ^ s1 ^ "\""
                          in
                          loop s env
                      | _ -> (s, genv) ]
                  | [] -> (s, []) ]
              in
              loop (comm ^ " -" ^ k ^ " " ^ s) env
          | Some _ -> loop comm env
          | None ->
              if s = "none" then loop comm env
              else if s = "on" then loop (comm ^ " -" ^ k) env
              else if s.[0] = '_' then loop (comm ^ " -" ^ k ^ s) env
              else if s.[String.length s - 1] = '_' then
                loop (comm ^ " -" ^ s ^ k) env
              else loop (comm ^ " -" ^ k ^ " " ^ s) env ]
    | [] -> comm ]
;

type config =
  { lang : string;
    comm : string;
    env : list (string * string) }
;

value rec list_replace k v =
  fun
  [ [] -> [(k, v)]
  | [(k1, v1) :: env] when k1 = k -> [(k1, v) :: env]
  | [kv :: env] -> [kv :: list_replace k v env] ]
;

value conf_with_env conf k v =
  {(conf) with env = list_replace k v conf.env}
;

value rec copy_from_stream conf print strm =
  try
    while True do
      match Stream.next strm with
      [ '$' ->
          let c = Stream.next strm in
          match c with
          [ '/' -> ifdef UNIX then print "/" else print "\\"
          | 'a' -> print (strip_spaces (s_getenv conf.env "anon"))
          | 'b' -> for_all_db conf print "." strm
          | 'c' -> print (Filename.concat "." conf.comm)
          | 'd' -> print conf.comm
          | 'e' ->
              do print "lang=";
                 print conf.lang;
                 List.iter
                   (fun (k, s) ->
                      if k = "opt" then ()
                      else
                        do print ";";
                           print k;
                           print "=";
                           print s;
                        return ())
                   conf.env;
              return ()
          | 'g' -> print_log conf print strm
          | 'h' ->
              do print "<input type=hidden name=lang value=";
                 print conf.lang;
                 print ">\n";
                 List.iter
                   (fun (k, s) ->
                      if k = "opt" then ()
                      else
                        do print "<input type=hidden name=";
                           print k;
                           print " value=\"";
                           print (decode_varenv s);
                           print "\">\n";
                        return ())
                   conf.env;
              return ()
          | 'i' -> print (strip_spaces (s_getenv conf.env "i"))
          | 'l' -> print conf.lang
          | 'o' -> print (strip_spaces (s_getenv conf.env "o"))
          | 'p' -> print (parameters conf.env)
          | 'u' -> print (Filename.dirname (Sys.getcwd ()))
          | 'v' ->
              let out = strip_spaces (s_getenv conf.env "o") in
              print_if_exists conf print (out ^ ".gwb") strm
          | 'w' -> print (Sys.getcwd ())
          | 'y' -> for_all_db conf print (s_getenv conf.env "anon") strm
          | 'z' -> print (string_of_int port)
          | '$' -> print "$"
          | c -> do print "BAD MACRO "; print (String.make 1 c); return () ]
      | c -> print (String.make 1 c) ];
    done
  with
  [ Stream.Failure -> () ]
and print_log conf print strm =
  match Stream.next strm with
  [ '{' ->
      let s =
        loop 0 where rec loop len =
          match Stream.next strm with
          [ '}' -> get_buff len
          | c -> loop (store len c) ]
      in
      let comm_log = "comm.log" in
      if Sys.file_exists comm_log then
        let ic = open_in comm_log in
        do if in_channel_length ic = 0 then
             copy_from_stream conf print (Stream.of_string s)
           else copy_from_stream conf print (Stream.of_channel ic);
           close_in ic;
        return ()
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> () ]
and print_if_exists conf print fname strm =
  match Stream.next strm with
  [ '{' ->
      let s =
        loop 0 where rec loop len =
          match Stream.next strm with
          [ '}' -> get_buff len
          | c -> loop (store len c) ]
      in
      if Sys.file_exists fname then
        copy_from_stream conf print (Stream.of_string s)
      else ()
  | _ -> () ]
and for_all_db conf print dir strm =
  match Stream.next strm with
  [ '{' ->
      let s_exist =
        loop 0 where rec loop len =
          match Stream.next strm with
          [ '|' -> get_buff len
          | c -> loop (store len c) ]
      in
      let s_empty =
        loop 0 where rec loop len =
          match Stream.next strm with
          [ '}' -> get_buff len
          | c -> loop (store len c) ]
      in
      let list = ref [] in
      let dh = Unix.opendir dir in
      do try
           while True do
             let e = Unix.readdir dh in
             if Filename.check_suffix e ".gwb" then
               list.val := [e :: list.val]
             else ();
           done
         with [ End_of_file -> () ];
         Unix.closedir dh;
         list.val := Sort.list \<= list.val;
         if list.val <> [] then
           List.iter
             (fun e ->
                let db = Filename.chop_suffix e ".gwb" in
                let conf = conf_with_env conf "anon" db in
                do copy_from_stream conf print (Stream.of_string s_exist);
                   print "\n";
                return ())
             list.val
          else
            do copy_from_stream conf print (Stream.of_string s_empty);
               print "\n";
            return ();
      return ()
  | _ -> () ]
;

value print_file conf fname =
  let dir = setup_dir in
  let fname = Filename.concat (Filename.concat dir conf.lang) fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do Wserver.html "";
         copy_from_stream conf (fun x -> Wserver.wprint "%s" x)
           (Stream.of_channel ic);
         close_in ic;
         trailer ();
      return ()
  | _ ->
      let title _ = Wserver.wprint "Error" in
      do header title;
         Wserver.wprint "<ul><li>\n";
         Wserver.wprint "Cannot access file \"%s\".\n" fname;
         Wserver.wprint "</ul>\n";
         trailer ();
      return raise Exit ]
;

value error str =
  do header (fun _ -> Wserver.wprint "Incorrect request");
     Wserver.wprint "<em>%s</em>\n" (String.capitalize str);
     trailer ();
  return ()
;

value good_name s =
  loop 0 where rec loop i =
    if i = String.length s then True
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '-' -> loop (i + 1)
      | _ -> False ]
;

value out_name_of_ged in_file =
  let f = Filename.basename in_file in
  if Filename.check_suffix f ".ged" then Filename.chop_suffix f ".ged"
  else if Filename.check_suffix f ".GED" then Filename.chop_suffix f ".GED"
  else f
;

value out_name_of_gw in_file =
  let f = Filename.basename in_file in
  if Filename.check_suffix f ".gw" then Filename.chop_suffix f ".gw"
  else if Filename.check_suffix f ".GW" then Filename.chop_suffix f ".GW"
  else f
;

value setup_gen conf =
  match p_getenv conf.env "v" with
  [ Some fname -> print_file conf fname
  | _ -> error "request needs \"v\" parameter" ]
;

value simple_check conf =
  let ged =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let out_file =
    if ged = "" then out_file
    else if out_file = "" then out_name_of_ged ged
    else out_file
  in
  let conf =
    {comm = if ged = "" then "gwc" else "ged2gwb";
     env = list_replace "o" out_file conf.env;
     lang = conf.lang}
  in
  if ged <> "" && not (Sys.file_exists ged) then print_file conf "unknown.html"
  else if out_file = "" then print_file conf "err_missing.html"
  else if not (good_name out_file) then print_file conf "err_name.html"
  else print_file conf "base_out.html"
;

value gwc_or_ged2gwb_check out_name_of_in_name conf =
  let in_file =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let out_file =
    if out_file = "" then out_name_of_in_name in_file else out_file
  in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_missing.html"
  else if not (Sys.file_exists in_file) then print_file conf "unknown.html"
  else if not (good_name out_file) then print_file conf "err_name.html"
  else print_file conf "base_out.html"
;

value gwc_check = gwc_or_ged2gwb_check out_name_of_gw;
value ged2gwb_check = gwc_or_ged2gwb_check out_name_of_ged;

value gwu_or_gwb2ged_check suffix conf =
  let in_file =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    if out_file = "" then in_file ^ suffix
    else if Filename.check_suffix out_file suffix then out_file
    else out_file ^ suffix
  in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_missing.html"
  else print_file conf "base_in.html"
;

value gwu_check = gwu_or_gwb2ged_check ".gw";
value gwb2ged_check = gwu_or_gwb2ged_check ".ged";

value consang_check conf =
  let in_f =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_f = "" then print_file conf "err_missing.html"
  else print_file conf "base_in.html"
;

value exec_f comm =
  let s = comm ^ " > " ^ "comm.log" in
  do Printf.eprintf "$ cd %s\n" (Sys.getcwd ());
     flush stderr;
     Printf.eprintf "$ %s\n" s;
     flush stderr;
  return
  Sys.command s
;

value recover conf =
  let init_dir =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let init_dir =
    if Sys.file_exists (Filename.concat init_dir ".") then init_dir
    else Filename.dirname init_dir
  in
  let conf = conf_with_env conf "anon" init_dir in
  let dest_dir = Sys.getcwd () in
  if init_dir = "" then print_file conf "err_missing.html"
  else if init_dir = dest_dir then print_file conf "err_same_dir.html"
  else if
    (ifdef UNIX then
       (Unix.stat (Filename.concat init_dir ".")).Unix.st_ino =
       (Unix.stat (Filename.concat dest_dir ".")).Unix.st_ino
     else False)
  then
    print_file conf "err_same_dir.html"
  else if not (Sys.file_exists init_dir) then
    print_file conf "err_no_such_directory.html"
  else
    let dh = Unix.opendir init_dir in
    let gwu_found =
      try
        loop () where rec loop () =
          let e = Unix.readdir dh in
          ifdef UNIX then
            if e = "gwu" then raise Exit else loop ()
          else
            if String.uncapitalize e = "gwu.exe" then raise Exit
            else loop ()
       with
       [ End_of_file -> False
       | Exit -> True ]
    in
    do Unix.closedir dh;
       if not gwu_found then print_file conf "err_not_gw.html"
       else print_file conf "recover_1.html";
    return ()
;

value recover_1 conf =
  let in_file =
    match p_getenv conf.env "i" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_not_appl.html"
  else print_file conf "recover_2.html"
;

value recover_2 conf =
  let init_dir =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_file =
    match p_getenv conf.env "i" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  let dir = Sys.getcwd () in
  do try
       do Printf.eprintf "$ cd %s\n" init_dir;
          flush stderr;
          Sys.chdir init_dir;
          let c =
            Filename.concat "." "gwu" ^ " " ^ in_file ^ " > " ^
            Filename.concat dir "tmp.gw"
          in
          do Printf.eprintf "$ %s\n" c;
             flush stderr;
             let _ = Sys.command c in ();
          return ();
       return ()
     with
     [ e -> do Sys.chdir dir; return raise e ];
     Printf.eprintf "$ cd %s\n" dir;
     flush stderr;
     Sys.chdir dir;
     let c =
       Filename.concat "." "gwc tmp.gw -o " ^ out_file ^
       " > " ^ "comm.log"
     in
     do Printf.eprintf "$ %s\n" c;
        flush stderr;
        let _ = Sys.command c in ();
     return ();
     Printf.eprintf "\n";
     flush stderr;
     print_file conf "created.html";
  return ()
;

ifdef WIN95 then
value infer_rc conf rc =
  if rc > 0 then rc
  else
    match p_getenv conf.env "o" with
    [ Some out_file ->
        if Sys.file_exists (out_file ^ ".gwb") then 0 else 2
    | _ -> 0 ]
;

value exec_command_out conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  let rc = ifdef WIN95 then infer_rc conf rc else rc in
  do Printf.eprintf "\n";
     flush stderr;
  return
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "base_out_err.html"
  else print_file conf "base_out_ok.html"
;

value exec_command_in conf ok_file =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  do Printf.eprintf "\n";
     flush stderr;
  return
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "base_in_err.html"
  else print_file conf ok_file
;

value has_gwb_directories dh =
  try
    loop () where rec loop () =
      let e = Unix.readdir dh in
      if Filename.check_suffix e ".gwb" then True
      else loop ()
  with
  [ End_of_file -> do Unix.closedir dh; return False ]
;

value setup_comm conf =
  fun
  [ "setup" -> setup_gen conf
  | "simple" -> simple_check conf
  | "recover" -> recover conf
  | "recover_1" -> recover_1 conf
  | "recover_2" -> recover_2 conf
  | "gwc" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwc_check conf
      | _ -> exec_command_out conf ]
  | "gwu" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwu_check conf
      | _ -> exec_command_in conf "gwu_ok.html" ]
  | "ged2gwb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb_check conf
      | _ -> exec_command_out conf ]
  | "gwb2ged" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwb2ged_check conf
      | _ -> exec_command_in conf "gwb2ged_ok.html" ]
  | "consang" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> consang_check conf
      | _ -> exec_command_in conf "consang_ok.html" ]
  | x -> error ("bad command: \"" ^ x ^ "\"") ]
;

value string_of_sockaddr =
  fun
  [ Unix.ADDR_UNIX s -> "ADDR_UNIX \"" ^ s ^ "\""
  | Unix.ADDR_INET a _ -> "ADDR_INET \"" ^ Unix.string_of_inet_addr a ^ "\"" ]
;

value setup (addr, req) str =
(*
  do Printf.eprintf "request from %s\n" (string_of_sockaddr addr);
     flush stderr;
  return
*)
  let (comm, env_str) =
    try
      let i = String.index str '?' in
      (String.sub str 0 i,
       String.sub str (i + 1) (String.length str - i - 1))
    with
    [ Not_found -> (str, "") ]
  in
  let env = create_env env_str in
  if env = [] && (comm = "" || String.length comm = 2) then
    let lang = if comm = "" then default_lang else String.uncapitalize comm in
    let conf = {lang = lang; comm = ""; env = env} in
    print_file conf "welcome.html"
  else
    let (lang, env) =
      match p_getenv env "lang" with
      [ Some x -> (x, list_remove_assoc "lang" env)
      | _ -> (default_lang, env) ]
    in
    let conf = {lang = lang; comm = comm; env = env} in
    setup_comm conf comm
;

value wrap_setup a b =
  try setup a b with
  [ Exit -> () ]
;

value copy_text lang fname =
  let dir = setup_dir in
  let fname = Filename.concat dir fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let conf = {lang = lang; comm = ""; env = []} in
      do copy_from_stream conf print_string (Stream.of_channel ic);
         flush stdout;
         close_in ic;
      return ()
  | _ ->
      do Printf.printf "\nCannot access file \"%s\".\n" fname;
         Printf.printf "Type \"Enter\" to exit\n? ";
         flush stdout;
         let _ = input_line stdin in ();
         exit 2;
      return () ]
;

value intro () =
  do Sys.chdir "gw";
     copy_text "" "intro.txt";
     let lang =
       let x = input_line stdin in
       if x = "" then default_lang else x
     in
     copy_text lang (Filename.concat lang "intro.txt");
  return ()
;

value main () =
  do ifdef UNIX then intro ()
     else
       try let _ = Sys.getenv "WSERVER" in () with [ Not_found -> intro () ];
     Wserver.f 2316 0 None (None, None) wrap_setup;
  return ()
;

main ();
