(* camlp4r *)
(* $Id: setup.ml,v 1.18 1999-05-07 07:52:34 ddr Exp $ *)

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

value quote_escaped s =
  let rec need_code i =
    if i < String.length s then
      match s.[i] with
      [ '"' | '&' | '<' | '>' -> True
      | x -> need_code (succ i) ]
    else False
  in
  let rec compute_len i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> i1 + 6
        | '&' -> i1 + 5
        | '<' | '>' -> i1 + 4
        | _ -> succ i1 ]
      in
      compute_len (succ i) i1
    else i1
  in
  let rec copy_code_in s1 i i1 =
    if i < String.length s then
      let i1 =
        match s.[i] with
        [ '"' -> do String.blit "&#034;" 0 s1 i1 6; return i1 + 6
        | '&' -> do String.blit "&amp;" 0 s1 i1 5; return i1 + 5
        | '<' -> do String.blit "&lt;" 0 s1 i1 4; return i1 + 4
        | '>' -> do String.blit "&gt;" 0 s1 i1 4; return i1 + 4
        | c -> do s1.[i1] := c; return succ i1 ]
      in
      copy_code_in s1 (succ i) i1
    else s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (String.create len) 0 0
  else s
;

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
    env : list (string * string);
    request : list string }
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

value browser_with_type_file conf =
  let user_agent = Wserver.extract_param "user-agent: " '/' conf.request in
  String.lowercase user_agent <> "lynx"
;

value all_db dir =
  let list = ref [] in
  let dh = Unix.opendir dir in
  do try
       while True do
         let e = Unix.readdir dh in
         if Filename.check_suffix e ".gwb" then
           list.val := [Filename.chop_suffix e ".gwb" :: list.val]
         else ();
       done
     with [ End_of_file -> () ];
     Unix.closedir dh;
     list.val := Sort.list \<= list.val;
  return list.val
;

value parse_upto lim =
  loop 0 where rec loop len =
    parser
    [ [: `c when c = lim :] -> get_buff len
    | [: `c; s = loop (store len c) :] -> s ]
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
          | 'b' -> for_all conf print (all_db ".") strm
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
          | 'g' -> print_specific_file conf print "comm.log" strm
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
          | 'k' -> for_all conf print (fst (List.split conf.env)) strm
          | 'l' -> print conf.lang
          | 'n' -> print_if conf print (browser_with_type_file conf) strm
          | 'o' -> print (strip_spaces (s_getenv conf.env "o"))
          | 'p' -> print (parameters conf.env)
          | 'q' -> print Version.txt
          | 'r' -> print_specific_file conf print "gwd.arg" strm
          | 't' -> print_if conf print (ifdef UNIX then False else True) strm
          | 'u' -> print (Filename.dirname (Sys.getcwd ()))
          | 'v' ->
              let out = strip_spaces (s_getenv conf.env "o") in
              print_if conf print (Sys.file_exists (out ^ ".gwb")) strm
          | 'w' -> print (Sys.getcwd ())
          | 'y' -> for_all conf print (all_db (s_getenv conf.env "anon")) strm
          | 'z' -> print (string_of_int port)
          | '$' -> print "$"
          | 'A'..'Z' as c ->
              match p_getenv conf.env (String.make 1 c) with
              [ Some v ->
                  match strm with parser
                  [ [: `'{' :] ->
                      let s = parse_upto '}' strm in
                      do print "\""; print s; print "\"";
                         if v = s then print " selected" else ();
                      return ()
                  | [: `'[' :] ->
                      let s = parse_upto ']' strm in
                      do print "\""; print s; print "\"";
                         if v = s then print " checked" else ();
                      return ()
                  | [: :] -> print (strip_spaces v) ]
              | None -> print "BAD MACRO" ]
          | c -> do print "BAD MACRO "; print (String.make 1 c); return () ]
      | c -> print (String.make 1 c) ];
    done
  with
  [ Stream.Failure -> () ]
and print_specific_file conf print fname strm =
  match Stream.next strm with
  [ '{' ->
      let s = parse_upto '}' strm in
      if Sys.file_exists fname then
        let ic = open_in fname in
        do if in_channel_length ic = 0 then
             copy_from_stream conf print (Stream.of_string s)
           else copy_from_stream conf print (Stream.of_channel ic);
           close_in ic;
        return ()
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> () ]
and print_if conf print cond strm =
  match Stream.next strm with
  [ '{' ->
      let s = parse_upto '}' strm in
      if cond then
        copy_from_stream conf print (Stream.of_string s)
      else ()
  | _ -> () ]
and for_all conf print list strm =
  match Stream.next strm with
  [ '{' ->
      let s_exist = parse_upto '|' strm in
      let s_empty = parse_upto '}' strm in
      if list <> [] then
        List.iter
          (fun db ->
             let conf = conf_with_env conf "anon" db in
             do copy_from_stream conf print (Stream.of_string s_exist);
                print "\n";
             return ())
          list
       else
         do copy_from_stream conf print (Stream.of_string s_empty);
            print "\n";
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

value simple conf =
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
     lang = conf.lang;
     request = conf.request}
  in
  if ged <> "" && not (Sys.file_exists ged) then
    print_file conf "err_unknown.html"
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
  if in_file = "" || out_file = "" then print_file conf "err_missing.html"
  else if not (Sys.file_exists in_file) then
    print_file conf "err_unknown.html"
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
    else if Filename.check_suffix out_file (String.uppercase suffix) then
      out_file
    else out_file ^ suffix
  in
  let out_file = Filename.concat ".." out_file in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_missing.html"
  else print_file conf "base_in.html"
;

value gwu = gwu_or_gwb2ged_check ".gw";
value gwb2ged = gwu_or_gwb2ged_check ".ged";

value exec_f comm =
  let s = comm ^ " > " ^ "comm.log" in
  do Printf.eprintf "$ cd %s\n" (Sys.getcwd ());
     flush stderr;
     Printf.eprintf "$ %s\n" s;
     flush stderr;
  return
  Sys.command s
;

value gwb2ged_or_gwu_1 ok_file conf =
  let rc =
    exec_f (Filename.concat "." conf.comm ^ parameters conf.env)
  in
  do Printf.eprintf "\n";
     flush stderr;
  return
  if rc = 1 then print_file conf "warnings.html"
  else if rc > 1 then print_file conf "base_in_err.html"
  else
    let conf =
      conf_with_env conf "o" (Filename.basename (s_getenv conf.env "o"))
    in
    print_file conf ok_file
;

value gwb2ged_1 = gwb2ged_or_gwu_1 "gwb2ged_ok.html";
value gwu_1 = gwb2ged_or_gwu_1 "gwu_ok.html";

value consang_check conf =
  let in_f =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_f = "" then print_file conf "err_missing.html"
  else print_file conf "base_in.html"
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

value has_gwu dir =
  match try Some (Unix.opendir dir) with [ Unix.Unix_error _ _ _ -> None ] with
  [ Some dh ->
      let gwu_found =
	try
	  loop () where rec loop () =
	    let e = Unix.readdir dh in
	    ifdef UNIX then
	      match e with
	      [ "gwu" -> raise Exit
	      | _ -> loop () ]
	    else
	      match String.uncapitalize e with
	      [ "gwu.exe" -> raise Exit
	      | _ -> loop () ]
	 with
	 [ End_of_file -> False
	 | Exit -> True ]
      in
      do Unix.closedir dh; return gwu_found
  | None -> False ]
;

value recover conf =
  let init_dir =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let (init_dir, dir_has_gwu) =
    if has_gwu init_dir then (init_dir, True)
    else
      let dir = Filename.dirname init_dir in
      if has_gwu dir then (dir, True)
      else (init_dir, False)
  in
  let conf = conf_with_env conf "anon" init_dir in
  let dest_dir = Sys.getcwd () in
  if init_dir = "" then print_file conf "err_missing.html"
  else if init_dir = dest_dir then print_file conf "err_same_dir.html"
  else if not (Sys.file_exists init_dir) then
    print_file conf "err_no_such_dir.html"
  else if
    (ifdef UNIX then
       try
         (Unix.stat (Filename.concat init_dir ".")).Unix.st_ino =
         (Unix.stat (Filename.concat dest_dir ".")).Unix.st_ino
       with
       [ Unix.Unix_error _ _ _ -> False ]
     else False)
  then
    print_file conf "err_same_dir.html"
  else if not dir_has_gwu then print_file conf "err_not_gw.html"
  else print_file conf "recover_1.html"
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
  let by_gedcom =
    match p_getenv conf.env "ged" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_missing.html"
  else if not (good_name out_file) then print_file conf "err_name.html"
  else
    let (old_to_src, o_opt, tmp, src_to_new) =
      if not by_gedcom then ("gwu", " > ", "tmp.gw", "gwc")
      else ("gwb2ged", " -o ", "tmp.ged", "ged2gwb")
    in
    let conf =
      {(conf) with env =
       [("U", old_to_src); ("O", o_opt); ("T", tmp); ("C", src_to_new) ::
        conf.env]}
    in
    print_file conf "recover_2.html"
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
  let by_gedcom =
    match p_getenv conf.env "ged" with
    [ Some "on" -> True
    | _ -> False ]
  in
  let (old_to_src, o_opt, tmp, src_to_new) =
    if not by_gedcom then ("gwu", " > ", "tmp.gw", "gwc")
    else ("gwb2ged", " -o ", "tmp.ged", "ged2gwb")
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  let dir = Sys.getcwd () in
  do try
       do Printf.eprintf "$ cd %s\n" init_dir;
          flush stderr;
          Sys.chdir init_dir;
          let c =
            Filename.concat "." old_to_src ^ " " ^ in_file ^ o_opt ^
            Filename.concat dir tmp
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
       Filename.concat "." src_to_new ^ " " ^ tmp ^ " -o " ^ out_file ^
       " > " ^ "comm.log"
     in
     do Printf.eprintf "$ %s\n" c;
        flush stderr;
     return
     let rc = Sys.command c in
     let rc = ifdef WIN95 then infer_rc conf rc else rc in
     do Printf.eprintf "\n";
        flush stderr;
     return
     if rc = 1 then print_file conf "warnings.html"
     else if rc > 1 then print_file conf "recover_err.html"
     else print_file conf "base_out_ok.html";
  return ()
;

value rm_base dir =
  match try Some (Unix.opendir dir) with [ Unix.Unix_error _ _ _ -> None ] with
  [ Some dh ->
      let list = ref [] in
      do try
           while True do
             let file = Unix.readdir dh in
             if file = "." || file = ".." then ()
             else list.val := [file :: list.val];
           done
         with
         [ End_of_file -> () ];
         Unix.closedir dh;
         List.iter (fun file -> Unix.unlink (Filename.concat dir file))
           list.val;
         Unix.rmdir dir;
      return ()
  | _ -> () ]
;

value cleanup conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_base = "" then print_file conf "err_missing.html"
  else print_file conf "cleanup_1.html"
;

value cleanup_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_base_dir = in_base ^ ".gwb" in
  do Printf.eprintf "$ cd %s\n" (Sys.getcwd ()); flush stderr; return
  let c = Filename.concat "." "gwu" ^ " " ^ in_base ^ " -o tmp.gw" in
  do Printf.eprintf "$ %s\n" c; flush stderr; return
  let _ = Sys.command c in
  do ifdef UNIX  then Printf.eprintf "$ rm -rf old/%s\n" in_base_dir
     else
       do Printf.eprintf "$ del old\\%s*.*\n" in_base_dir;
          Printf.eprintf "$ rmdir old\\%s\n" in_base_dir;
       return ();
     flush stderr;
     rm_base (Filename.concat "old" in_base_dir);
     ifdef UNIX then Printf.eprintf "$ mv %s old/.\n" in_base_dir
     else Printf.eprintf "$ move %s old\\.\n" in_base_dir;
     flush stderr;
     Sys.rename in_base_dir (Filename.concat "old" in_base_dir);
  return
  let c =
    Filename.concat "." "gwc" ^ " tmp.gw -o " ^ in_base ^ " > comm.log "
  in
  do Printf.eprintf "$ %s\n" c;
     flush stderr;
     let rc = Sys.command c in
     let rc = ifdef WIN95 then infer_rc conf rc else rc in
     do Printf.eprintf "\n";
        flush stderr;
     return
     if rc = 1 then print_file conf "warnings.html"
     else if rc > 1 then print_file conf "cleanup_err.html"
     else print_file conf "cleanup_ok.html";
  return ()
;  

value rec check_new_names conf l1 l2 =
  match (l1, l2) with
  [ ([(k, v) :: l], [x :: m]) ->
      if k <> x then do print_file conf "err_outdated.html"; return raise Exit
      else if not (good_name v) then
        let conf = {(conf) with env = [("o", v) :: conf.env]} in
        do print_file conf "err_name.html"; return raise Exit
      else check_new_names conf l m
  | ([], []) -> ()
  | _ -> do print_file conf "err_outdated.html"; return raise Exit ]
;

value rec check_rename_conflict conf =
  fun
  [ [x :: l] ->
      if List.mem x l then
        let conf = {(conf) with env = [("o", x) :: conf.env]} in
        do print_file conf "err_conflict.html"; return raise Exit
      else check_rename_conflict conf l
  | [] -> () ]
;

value rename conf =
  let rename_list =
    List.map (fun (k, v) -> (k, strip_spaces (decode_varenv v))) conf.env
  in
  try
    do check_new_names conf rename_list (all_db ".");
       check_rename_conflict conf (snd (List.split rename_list));
       List.iter
         (fun (k, v) ->
            if k <> v then Sys.rename (k ^ ".gwb") ("_" ^ k ^ ".gwb")
            else ())
         rename_list;
       List.iter
         (fun (k, v) ->
            if k <> v then Sys.rename ("_" ^ k ^ ".gwb") (v ^ ".gwb")
            else ())
         rename_list;
       print_file conf "rename_ok.html";
    return ()
  with
  [ Exit -> () ]
;

value delete conf =
  print_file conf "delete_1.html"
;

value delete_1 conf =
  do List.iter
       (fun (k, v) -> if v = "del" then rm_base (k ^ ".gwb") else ())
       conf.env;
     print_file conf "delete_ok.html";
  return ()
;

value rec cut_at_equal s =
  try
    let i = String.index s '=' in
    (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  with
  [ Not_found -> (s, "") ]
;

value read_base_env bname =
  let fname = bname ^ ".gwf" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let env =
        loop [] where rec loop env =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some s ->
              if s = "" || s.[0] = '#' then loop env
              else loop [cut_at_equal s :: env]
          | None ->
              env ]
      in
      do close_in ic; return env
  | None -> [] ]
;

value read_gwd_arg () =
  let fname = "gwd.arg" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let list =
        loop [] where rec loop list =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some "" -> loop list
          | Some s -> loop [s :: list]
          | None -> list ]
      in
      do close_in ic; return
      loop [] (List.rev list) where rec loop env =
        fun
        [ [x :: l] ->
            if x.[0] = '-' then
              let x = String.sub x 1 (String.length x - 1) in
              match l with
              [ [y :: l] when y.[0] <> '-' -> loop [(x, y) :: env] l
              | _ -> loop [(x, "") :: env] l ]
            else loop env l
        | [] -> List.rev env ]
  | None -> [] ]
;

value file_contents fname =
  let len = ref 0 in
  try
    let ic = open_in fname in
    do while True do len.val := store len.val (input_char ic); done; return ""
  with
  [ Sys_error _ | End_of_file -> get_buff len.val ]
;

value gwf conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_base = "" then print_file conf "err_missing.html"
  else
    let benv = read_base_env in_base in
    let get v =
      try quote_escaped (List.assoc v benv) with [ Not_found -> "" ]
    in
    let trailer =
      quote_escaped
        (file_contents (Filename.concat "lang" (in_base ^ ".trl")))
    in
    let conf =
      {(conf) with env =
         [("B", get "body_prop"); ("L", get "default_lang");
          ("F", get "friend_passwd"); ("W", get "wizard_passwd");
          ("I", get "can_send_image"); ("J", get "wizard_just_friend");
          ("R", get "renamed"); ("T", trailer) :: conf.env]}
    in
    print_file conf "gwf_1.html"
;

value gwf_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  do let oc = open_out (in_base ^ ".gwf") in
     do Printf.fprintf oc "# File generated by \"setup\"\n\n";
        Printf.fprintf oc "body_prop=%s\n" (s_getenv conf.env "body_prop");
        Printf.fprintf oc "default_lang=%s\n"
          (s_getenv conf.env "default_lang");
        Printf.fprintf oc "friend_passwd=%s\n"
          (s_getenv conf.env "friend_passwd");
        Printf.fprintf oc "wizard_passwd=%s\n"
          (s_getenv conf.env "wizard_passwd");
        Printf.fprintf oc "can_send_image=%s\n"
          (s_getenv conf.env "can_send_image");
        Printf.fprintf oc "wizard_just_friend=%s\n"
          (s_getenv conf.env "wizard_just_friend");
        Printf.fprintf oc "renamed=%s\n"
          (s_getenv conf.env "renamed");
        close_out oc;
     return ();
     let trl = s_getenv conf.env "trailer" in
     let trl_file =  Filename.concat "lang" (in_base ^ ".trl") in
     do try
          if trl = "" then Sys.remove trl_file
          else
            let oc = open_out trl_file in
            do output_string oc trl; close_out oc; return ()
        with
        [ Sys_error _ -> () ];
     return ();
     print_file conf "gwf_ok.html";
  return ()
;

value gwd conf =
  let aenv = read_gwd_arg () in
  let get v = try List.assoc v aenv with [ Not_found -> "" ] in
  let conf =
    let nolock =
      try let _ = List.assoc "nolock" aenv in "on" with [ Not_found -> "" ]
    in
    {(conf) with env =
       [("H", get "hd"); ("B", get "bd");
        ("P", get "p"); ("W", get "wizard");
        ("F", get "friend"); ("L", get "lang");
        ("O", get "only"); ("A", get "auth");
        ("T", Filename.basename (get "log"));
        ("N", nolock);
        ("M", get "max_clients"); ("U", get "setuid");
        ("G", get "setgid") :: conf.env]}
  in
  print_file conf "gwd.html"
;

value gwd_1 conf =
  let oc = open_out "gwd.arg" in
  let print_param k =
    match p_getenv conf.env k with
    [ Some v when v <> "" -> Printf.fprintf oc "-%s\n%s\n" k v
    | _ -> () ]
  in
  do print_param "hd";
     print_param "bd";
     print_param "p";
     print_param "wizard";
     print_param "friend";
     match p_getenv conf.env "default_lang" with
     [ Some v when v <> "" -> Printf.fprintf oc "-lang\n%s\n" v
     | _ -> () ];
     print_param "only";
     print_param "auth";
     match p_getenv conf.env "log" with
     [ Some v when v <> "" ->
         Printf.fprintf oc "-log\n%s\n" (Filename.concat ".." v)
     | _ -> () ];
     match p_getenv conf.env "nolock" with
     [ Some "on" -> Printf.fprintf oc "-nolock\n"
     | _ -> () ];
     ifdef UNIX then
       do print_param "max_clients";
          print_param "setuid";
          print_param "setgid";
       return ()
     else ();
     close_out oc;
     print_file conf "gwd_ok.html";
  return ()
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
  | "simple" -> simple conf
  | "recover" -> recover conf
  | "recover_1" -> recover_1 conf
  | "recover_2" -> recover_2 conf
  | "cleanup" -> cleanup conf
  | "cleanup_1" -> cleanup_1 conf
  | "rename" -> rename conf
  | "delete" -> delete conf
  | "delete_1" -> delete_1 conf
  | "gwc" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwc_check conf
      | _ -> exec_command_out conf ]
  | "gwu" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwu conf
      | _ -> gwu_1 conf ]
  | "ged2gwb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb_check conf
      | _ -> exec_command_out conf ]
  | "gwb2ged" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwb2ged conf
      | _ -> gwb2ged_1 conf ]
  | "consang" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> consang_check conf
      | _ -> exec_command_in conf "consang_ok.html" ]
  | "gwf" -> gwf conf
  | "gwf_1" -> gwf_1 conf
  | "gwd" -> gwd conf
  | "gwd_1" -> gwd_1 conf
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
    let conf = {lang = lang; comm = ""; env = env; request = req} in
    print_file conf "welcome.html"
  else
    let (lang, env) =
      match p_getenv env "lang" with
      [ Some x -> (x, list_remove_assoc "lang" env)
      | _ -> (default_lang, env) ]
    in
    let conf = {lang = lang; comm = comm; env = env; request = req} in
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
      let conf = {lang = lang; comm = ""; env = []; request = []} in
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

value set_gwd_default_language_if_absent lang =
  let env = read_gwd_arg () in
  match try Some (open_out "gwd.arg") with [ Sys_error _ -> None ] with
  [ Some oc ->
      let lang_found = ref False in
      do List.iter
           (fun (k, v) ->
              do Printf.fprintf oc "-%s\n" k;
                 if k = "lang" then lang_found.val := True else ();
                 Printf.fprintf oc "%s\n" v;
              return ())
           env;
         if not lang_found.val then Printf.fprintf oc "-lang\n%s\n" lang
         else ();
         close_out oc;
      return ()
  | None -> () ]
;

value intro () =
  do Sys.chdir "gw";
     copy_text "" "intro.txt";
     let lang =
       let x = input_line stdin in
       if x = "" then default_lang else x
     in
     do set_gwd_default_language_if_absent lang;
        copy_text lang (Filename.concat lang "intro.txt");
     return ();
     Printf.printf "\n";
     flush stdout;
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
