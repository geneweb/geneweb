(* camlp4r *)
(* $Id: setup.ml,v 1.6 1999-05-01 09:35:05 ddr Exp $ *)

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

(*
value apply s db =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else
      if s.[i] = '$' && s.[i+1] = 'x' then
        do Wserver.wprint "%s" db; return loop (i + 2)
      else
        do Wserver.wprint "%c" s.[i]; return loop (i + 1)
;
*)

value rec copy_from_stream conf print strm =
  try
    while True do
      match Stream.next strm with
      [ '$' ->
          let c = Stream.next strm in
          match c with
          [ 'a' -> print (strip_spaces (s_getenv conf.env "anon"))
          | 'b' -> for_all_db conf print strm
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
          | 'l' -> print conf.lang
          | 'o' -> print (strip_spaces (s_getenv conf.env "o"))
          | 'p' -> print (parameters conf.env)
          | 'w' -> print (Sys.getcwd ())
          | 'z' -> print (string_of_int port)
          | '$' -> print "$"
          | c -> do print "BAD MACRO "; print (String.make 1 c); return () ]
      | c -> print (String.make 1 c) ];
    done
  with
  [ Stream.Failure -> () ]
and for_all_db conf print strm =
  match Stream.next strm with
  [ '{' ->
      let s =
        loop 0 where rec loop len =
          match Stream.next strm with
          [ '}' -> get_buff len
          | c -> loop (store len c) ]
      in
      let dh = Unix.opendir "." in
      try
        while True do
          let e = Unix.readdir dh in
          if Filename.check_suffix e ".gwb" then
            let db = Filename.chop_suffix e ".gwb" in
            let conf = {(conf) with env = [("anon", db) :: conf.env]} in
            do copy_from_stream conf print (Stream.of_string s);
               print "\n";
            return ()
          else ();
        done
      with [ End_of_file -> Unix.closedir dh ]
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

value start_check conf =
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

value exec_command_out conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "error.html"
  else print_file conf "created.html"
;

value exec_command_in conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
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
  | "start" -> start_check conf
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
  | "list" ->
      let dh = Unix.opendir "." in
      if has_gwb_directories dh then print_file conf "list.html"
      else print_file conf "nolist.html"
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
