(* camlp4r *)
(* $Id: setup.ml,v 1.1 1999-04-30 11:45:48 ddr Exp $ *)

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
       if k = "opt" then comm
       else if k = "anon" then comm ^ " " ^ s
       else if s = "on" then comm ^ " -" ^ k
       else if s = "" then comm
       else comm ^ " -" ^ k ^ " " ^ s)
    ""
;

type config =
  { lang : string;
    comm : string;
    env : list (string * string) }
;

value apply s db =
  loop 0 where rec loop i =
    if i == String.length s then ()
    else
      if s.[i] = '$' && s.[i+1] = 'x' then
        do Wserver.wprint "%s" db; return loop (i + 2)
      else
        do Wserver.wprint "%c" s.[i]; return loop (i + 1)
;

value for_all_db conf ic =
  match input_char ic with
  [ '{' ->
      let s =
        loop 0 where rec loop len =
          let c = input_char ic in
          if c = '}' then get_buff len
          else loop (store len c)
      in
      let dh = Unix.opendir "." in
      try
        while True do
          let e = Unix.readdir dh in
          if Filename.check_suffix e ".gwb" then
            let db = Filename.chop_suffix e ".gwb" in
            do apply s db;
               Wserver.wprint "\n";
            return ()
          else ();
        done
      with [ End_of_file -> Unix.closedir dh ]
  | _ -> () ]
;

value copy_from_channel conf ic =
  try
    while True do
      match input_char ic with
      [ '$' ->
          let c = input_char ic in
          match c with
          [ 'a' -> Wserver.wprint "%s" (strip_spaces (getenv conf.env "anon"))
          | 'b' -> for_all_db conf ic
          | 'c' -> Wserver.wprint "%s" (Filename.concat "." conf.comm)
          | 'd' -> Wserver.wprint "%s" conf.comm
          | 'e' ->
              do Wserver.wprint "lang=%s" conf.lang;
                 List.iter
                   (fun (k, s) ->
                      if k = "opt" then ()
                      else Wserver.wprint ";%s=%s" k s)
                   conf.env;
              return ()
          | 'g' -> Wserver.wprint "%s" (Filename.concat "." "gwd")
          | 'l' -> Wserver.wprint "%s" conf.lang
          | 'o' -> Wserver.wprint "%s" (strip_spaces (getenv conf.env "o"))
          | 'p' -> Wserver.wprint "%s" (parameters conf.env)
          | 's' -> Wserver.wprint "setup?lang=%s;" conf.lang
          | 'w' -> Wserver.wprint "%s" (Sys.getcwd ())
          | c -> Wserver.wprint "$%c" c ]
    | c -> Wserver.wprint "%c" c ];
    done
  with
  [ End_of_file -> close_in ic ]
;

value print_file conf fname =
  let dir = setup_dir.val in
  let fname = Filename.concat (Filename.concat dir conf.lang) fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      do Wserver.html "";
         copy_from_channel conf ic;
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

value error () =
  do header (fun _ -> Wserver.wprint "Internal error");
     Wserver.wprint "Please report.\n";
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
      | _ -> error () ]
  | _ -> error () ]
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

value exec_f comm =
  let s = Filename.concat "." comm ^ " > comm.log" in
  do Printf.eprintf "> cd %s\n" (Sys.getcwd ());
     flush stderr;
     Printf.eprintf "> %s\n" s;
     flush stderr;
  return
  Sys.command s
;

value exec_command conf =
  let rc = exec_f (conf.comm ^ parameters conf.env) in
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "error.html"
  else print_file conf "created.html"
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
  | "gwc" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwc_check conf
      | _ -> exec_command conf ]
  | "ged2gwb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb_check conf
      | _ -> exec_command conf ]
  | "list" ->
      let dh = Unix.opendir "." in
      if has_gwb_directories dh then print_file conf "list.html"
      else print_file conf "nolist.html"
  | _ -> error () ]
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
  if env = [] then
    let lang = if comm = "" then default_lang else String.uncapitalize comm in
    if String.length lang = 2 then
      let conf = {lang = lang; comm = ""; env = env} in
      print_file conf "index.html"
    else error ()
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

value copy_text fname =
  let fname = Filename.concat setup_dir.val fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      try
        while True do
          let c = input_char ic in
          Printf.printf "%c" c;
        done
      with
      [ End_of_file -> do flush stdout; return close_in ic ]
  | _ ->
      do Printf.printf "\nCannot access file \"%s\".\n" fname;
         Printf.printf "Type \"Enter\" to exit\n? ";
         flush stdout;
         let _ = input_line stdin in ();
         exit 2;
      return () ]
;

value intro () =
  do copy_text "intro.txt";
     let lang =
       let x = input_line stdin in
       if x = "" then default_lang else x
     in
     copy_text (Filename.concat lang "intro.txt");
  return ()
;

value root_dir = ref "gw";

value main () =
  do Sys.chdir root_dir.val;
     intro ();
     Wserver.f 2318 0 None (None, None) wrap_setup;
  return ()
;

main ();
