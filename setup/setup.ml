(* camlp4r *)
(* $Id: setup.ml,v 1.7 1999-05-01 19:03:12 ddr Exp $ *)

value port = 2316;
value default_lang = "en";
value setup_dir = ref "setup";

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

value parameters =
  List.fold_left
    (fun comm (k, s) ->
       let k = strip_spaces (decode_varenv k) in
       let s = strip_spaces (decode_varenv s) in
       match k with
       [ "opt" -> comm
       | "anon" -> comm ^ " " ^ s
       | _ ->
           match s with
           [ "" | "none" -> comm
           | "on" -> comm ^ " -" ^ k
           | _ ->
               if s.[0] = '_' then comm ^ " -" ^ k ^ s
               else if s.[String.length s - 1] = '_' then comm ^ " -" ^ s ^ k
               else comm ^ " -" ^ k ^ " " ^ s ] ])
    ""
;

type config =
  { lang : string;
    comm : string;
    env : list (string * string) }
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
          | 'g' -> print (Filename.concat "." "gwd")
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
      let dh = Unix.opendir dir in
      let list = ref [] in
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
                let conf = {(conf) with env = [("anon", db) :: conf.env]} in
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
  let dir = setup_dir.val in
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

value setup_gen conf =
  match p_getenv conf.env "m" with
  [ Some "H" ->
      match p_getenv conf.env "v" with
      [ Some fname -> print_file conf fname
      | _ -> error "request needs \"v\" parameter" ]
  | _ -> error "unknown mode" ]
;

value out_check conf =
  let out =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  if out = "" then print_file conf "missing.html"
  else if not (good_name out) then print_file conf "incorrect.html"
  else if Sys.file_exists (out ^ ".gwb") then print_file conf "conflict.html"
  else print_file conf "create.html"
;

value simple_check conf =
  let ged =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let conf =
    if ged = "" then {(conf) with comm = "gwc"}
     else {(conf) with comm = "ged2gwb"}
  in
  if ged <> "" && not (Sys.file_exists ged) then print_file conf "unknown.html"
  else if out = "" then print_file conf "missing.html"
  else if not (good_name out) then print_file conf "incorrect.html"
  else if Sys.file_exists (out ^ ".gwb") then print_file conf "conflict.html"
  else print_file conf "create.html"
;

value gwc_check conf =
  out_check conf
;

value ged2gwb_check conf =
  let ged =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if ged = "" then print_file conf "missing_ged.html"
  else if not (Sys.file_exists ged) then print_file conf "unknown.html"
  else out_check conf
;

value consang_check conf =
  let in_f =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_f = "" then print_file conf "missing_gwb.html"
  else print_file conf "input.html"
;

value exec_f comm =
  let s = comm ^ " > comm.log" in
  do Printf.eprintf "> cd %s\n" (Sys.getcwd ());
     flush stderr;
     Printf.eprintf "> %s\n" s;
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
  let dest_dir = Sys.getcwd () in
  if init_dir = "" then print_file conf "error_missing_dir.html"
  else if init_dir = dest_dir then print_file conf "error_same_dir.html"
  else if
    (Unix.stat (Filename.concat init_dir ".")).Unix.st_ino =
    (Unix.stat (Filename.concat dest_dir ".")).Unix.st_ino
  then
    print_file conf "error_same_dir.html"
  else if not (Sys.file_exists init_dir) then
    print_file conf "error_no_such_directory.html"
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
       if not gwu_found then print_file conf "error_not_a_gw_dir.html"
       else print_file conf "recover_1.html";
    return ()
;

value recover_1 conf =
  let in_base =
    match p_getenv conf.env "i" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_base =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_base = if out_base = "" then in_base else out_base in
  let conf = {(conf) with env = [("o", out_base) :: conf.env]} in
  if in_base = "" then print_file conf "not_applicable.html"
  else print_file conf "recover_2.html"
;

value recover_2 conf =
  let init_dir =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_base =
    match p_getenv conf.env "i" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_base =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let out_base = if out_base = "" then in_base else out_base in
  let conf = {(conf) with env = [("o", out_base) :: conf.env]} in
  let dir = Sys.getcwd () in
  do try
       do Printf.eprintf "> cd %s\n" init_dir;
          flush stderr;
          Sys.chdir init_dir;
          let c =
            Filename.concat "." "gwu" ^ " " ^ in_base ^ " > " ^
            Filename.concat dir "tmp.gw"
          in
          do Printf.eprintf "> %s\n" c;
             flush stderr;
             let _ = Sys.command c in ();
          return ();
       return ()
     with
     [ e -> do Sys.chdir dir; return raise e ];
     Printf.eprintf "> cd %s\n" dir;
     flush stderr;
     Sys.chdir dir;
     let c =
       Filename.concat "." "gwc tmp.gw -o " ^ out_base ^
       " > comm.log"
     in
     do Printf.eprintf "> %s\n" c;
        flush stderr;
        let _ = Sys.command c in ();
     return ();
     Printf.eprintf "\n";
     flush stderr;
     print_file conf "created.html";
  return ()
;

value exec_command_out conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  do Printf.eprintf "\n";
     flush stderr;
  return
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "error.html"
  else print_file conf "created.html"
;

value exec_command_in conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  do Printf.eprintf "\n";
     flush stderr;
  return
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "error.html"
  else print_file conf "input_ok.html"
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
  | "ged2gwb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb_check conf
      | _ -> exec_command_out conf ]
  | "consang" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> consang_check conf
      | _ -> exec_command_in conf ]
  | x -> error ("bad command: \"" ^ x ^ "\"") ]
;

value setup (addr, req) str =
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
    print_file conf "index.html"
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
  let dir = setup_dir.val in
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

value root_dir = ref "gw";

value intro () =
  do Sys.chdir root_dir.val;
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
