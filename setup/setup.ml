(* camlp5r *)
(* $Id: setup.ml,v 5.8 2007-09-12 09:58:44 ddr Exp $ *)

open Printf;

value port = ref 2316;
value gwd_port = ref 2317;
value default_lang = ref "en";
value setup_dir = ref ".";
value bin_dir = ref "";
value base_dir = ref "";
value lang_param = ref "";
value only_file = ref "";
value bname = ref "";
value commnd = ref "";
value slashify s =
  String.init (String.length s) conv_char
    where conv_char i =
      match s.[i] with
      [ '\\' -> '/'
      | x -> x ]
;

value slashify_linux_dos s =
  String.init (String.length s) conv_char
    where conv_char i =
      match s.[i] with
      [ '/' -> if Sys.unix then '/' else '\\'
      | x -> x ]
;


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
        [ '"' -> do { String.blit "&#034;" 0 s1 i1 6; i1 + 6 }
        | '&' -> do { String.blit "&amp;" 0 s1 i1 5; i1 + 5 }
        | '<' -> do { String.blit "&lt;" 0 s1 i1 4; i1 + 4 }
        | '>' -> do { String.blit "&gt;" 0 s1 i1 4; i1 + 4 }
        | c -> do { Bytes.set s1 i1 c; succ i1 } ]
      in
      copy_code_in s1 (succ i) i1
    else Bytes.unsafe_to_string s1
  in
  if need_code 0 then
    let len = compute_len 0 0 in copy_code_in (Bytes.create len) 0 0
  else s
;

value rec list_remove_assoc x =
  fun
  [ [(x1, y1) :: l] ->
      if x = x1 then l else [(x1, y1) :: list_remove_assoc x l]
  | [] -> [] ]
;

value rec list_assoc_all x =
  fun
  [ [] -> []
  | [(a, b) :: l] ->
      if a = x then [b :: list_assoc_all x l] else list_assoc_all x l ]
;

type config =
  { lang : string;
    comm : string;
    env : list (string * string);
    request : list string;
    lexicon : Hashtbl.t string string }
;

value transl conf w =
  try Hashtbl.find conf.lexicon w with [ Not_found -> "[" ^ w ^ "]" ]
;

value charset conf =
  try Hashtbl.find conf.lexicon " !charset" with
  [ Not_found -> "utf-8" ]
;

value header_no_page_title conf title =
  do {
    Wserver.http HttpStatus.OK;
    Wserver.header "Content-type: text/html; charset=%s" (charset conf);
    Wserver.printf "\
<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
\"http://www.w3.org/TR/REC-html40/loose.dtd\">
";
    Wserver.printf "<head>\n";
    Wserver.printf "  <meta name=\"robots\" content=\"none\">\n";
    Wserver.printf "  <title>";
    title True;
    Wserver.printf "</title>\n";
    Wserver.printf "</head>\n";
    Wserver.printf "<body>\n"
  }
;

value abs_setup_dir () =
  if Filename.is_relative setup_dir.val then
    Filename.concat (Sys.getcwd ()) setup_dir.val
  else setup_dir.val
;

value trailer conf =
  do {
    Wserver.printf "\n<br />\n";
    Wserver.printf "<div id=\"footer\">\n" ;
    Wserver.printf "<hr />\n";
    Wserver.printf "<div>\n";
    Wserver.printf "<em>\n";
    Wserver.printf "<a href=\"https://github.com/geneweb/geneweb/\">
        <img src=\"images/logo_bas.png\" style = \"border: 0\" /></a>
        Version %s Copyright &copy 1998-2017\n</em>\n" Version.txt;
    Wserver.printf "</div>\n" ;
    Wserver.printf "</div>\n" ;
    (* finish the html page *)
    Wserver.printf "</body>\n";
    Wserver.printf "</html>\n";
  }
;

value header conf title =
  do {
    header_no_page_title conf title;
    Wserver.printf "<h1>";
    title False;
    Wserver.printf "</h1>\n";
  }
;

value strip_control_m s =
  loop 0 0 where rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = '\r' then loop (i + 1) len
    else loop (i + 1) (Buff.store len s.[i])
;

value strip_spaces str =
  let start =
    loop 0 where rec loop i =
      if i = String.length str then i
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i + 1)
        | _ -> i ]
  in
  let stop =
    loop (String.length str - 1) where rec loop i =
      if i = -1 then i + 1
      else
        match str.[i] with
        [ ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1 ]
  in
  if start = 0 && stop = String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)
;

value code_varenv = Wserver.encode;
value decode_varenv = Wserver.decode;

value getenv env label = decode_varenv (List.assoc (decode_varenv label) env);

value p_getenv env label =
  try Some (getenv env label) with [ Not_found -> None ]
;

value s_getenv env label = try getenv env label with [ Not_found -> "" ];

value rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i
;

value create_env s =
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [String.sub s beg (i - beg)]
    else if s.[i] = ';' || s.[i] = '&' then
      let next_i = skip_spaces s (succ i) in
      [String.sub s beg (i - beg) :: get_assoc next_i next_i]
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] = '=' then
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

value stringify s =
  try let _ = String.index s ' ' in "\"" ^ s ^ "\"" with [ Not_found -> s ]
;

value parameters =
  loop "" where rec loop comm =
    fun
    [ [(k, s) :: env] ->
        let k = strip_spaces (decode_varenv k) in
        let s = strip_spaces (decode_varenv s) in
        if k = "" || s = "" then loop comm env
        else if k = "opt" then loop comm env
        else if k = "anon" then loop (comm ^ " " ^ stringify s) env
        else
          match numbered_key k with
          [ Some (k, '1') ->
              let (s, env) =
                loop ("\"" ^ s ^ "\"") env where rec loop s =
                  fun
                  [ [(k1, s1) :: env] as genv ->
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
              else if s.[0] = '_' then loop (comm ^ " -" ^ k ^ stringify s) env
              else if s.[String.length s - 1] = '_' then
                loop (comm ^ " -" ^ s ^ k) env
              else loop (comm ^ " -" ^ k ^ " " ^ stringify s) env ]
    | [] -> comm ]
;

value parameters_1 =
  loop "" "" where rec loop comm bname =
    fun
    [ [(k, s) :: env] ->
        let k = strip_spaces (decode_varenv k) in
        let s = strip_spaces (decode_varenv s) in
        if k = "" || s = "" then loop comm bname env
        else if k = "opt" then loop comm bname env
        else if k = "gwd_p" && s <> "" then loop (comm ^ " -gwd_p " ^ stringify s ) bname env
        else if k = "anon" && s <> "" then loop (comm ^ " " ^ stringify s) (stringify s) env
        else if k = "a" then loop (comm ^ " -a") bname env
        else if k = "s" then loop (comm ^ " -s") bname env
        else if k = "d" && s <> "" then loop (comm ^ " -d " ^ stringify s ) bname env
        else if k = "i" && s <> "" then loop (comm ^ " -i " ^ stringify s) bname env
        else if k = "bf" then loop (comm ^ " -bf") bname env
        else if k = "del" && s <> "" then loop (comm ^ " -del " ^ stringify s) bname env
        else if k = "cnt" && s <> "" then loop (comm ^ " -cnt " ^ stringify s) bname env
        else if k = "exact" then loop (comm ^ " -exact") bname env
        else if k = "o1" && s <> "" then 
          let out = stringify s in
          (comm ^ " -o " ^ out ^ " > " ^ out)
        else if k = "o" && s <> "" then
          if s = "choice" then loop comm bname env
          else
            let out = stringify s in
            let out = if out = "/notes_d/connex.txt" then bname  ^ ".gwb" ^ out else out in
            let out = slashify_linux_dos out in
            (comm ^ " -o " ^ out ^ " > " ^ out)
        else loop comm bname env
    | [] -> comm ]
;

value parameters_2 =
  loop "" where rec loop comm =
    fun
    [ [(k, s) :: env] ->
        let k = strip_spaces (decode_varenv k) in
        let s = strip_spaces (decode_varenv s) in
        if k = "" || s = "" then loop comm env
        else if k = "opt" then loop comm env
        else if k = "anon1" then loop (comm ^ " " ^ stringify s) env
        else if k = "anon2" then loop (comm ^ " " ^ stringify s) env
        else if k = "a1" then loop (comm ^ " -1 " ^ stringify s) env
        else if k = "a2" then loop (comm ^ " " ^ stringify s) env
        else if k = "a3" then loop (comm ^ " " ^ stringify s) env
        else if k = "b1" then loop (comm ^ " -2 "  ^ stringify s) env
        else if k = "b2" then loop (comm ^ " " ^ stringify s) env
        else if k = "b3" then loop (comm ^ " " ^ stringify s) env
        else if k = "ad" then loop (comm ^ " -ad ") env
        else if k = "d" then loop (comm ^ " -d ") env
        else if k = "mem" then loop (comm ^ " -mem") env
        else if k = "o" then loop (comm ^ " -o " ^ stringify s ^ " > " ^ stringify s) env
        else loop comm env
    | [] -> comm ]
;

value rec list_replace k v =
  fun
  [ [] -> [(k, v)]
  | [(k1, v1) :: env] when k1 = k -> [(k1, v) :: env]
  | [kv :: env] -> [kv :: list_replace k v env] ]
;

value conf_with_env conf k v = {(conf) with env = list_replace k v conf.env};

value all_db dir =
  let list = ref [] in
  let dh = Unix.opendir dir in
  do {
    try
      while True do {
        let e = Unix.readdir dh in
        if Filename.check_suffix e ".gwb" then
          list.val := [Filename.chop_suffix e ".gwb" :: list.val]
        else ()
      }
    with
    [ End_of_file -> () ];
    Unix.closedir dh;
    list.val := List.sort compare list.val;
    list.val
  }
;

value selected env =
  List.fold_right (fun (k, v) env -> if v = "on_" then [k :: env] else env)
    env []
;

value parse_upto lim =
  loop 0 where rec loop len =
    parser
    [ [: `c when c = lim :] -> Buff.get len
    | [: `c; a = loop (Buff.store len c) :] -> a ]
;

value parse_upto_void lim =
  loop 0 where rec loop len =
    parser
    [ [: `c when c = lim :] -> ()
    | [: `c; a = loop (Buff.store len c) :] -> a ]
;

value is_directory x =
  try (Unix.lstat x).Unix.st_kind = Unix.S_DIR with
  [ Unix.Unix_error _ _ _ -> False ]
;

value server_string conf =
  let s = Wserver.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    String.sub s 0 i
  with
  [ Not_found -> "127.0.0.1" ]
;

value referer conf =
  Wserver.extract_param "referer: " '\r' conf.request
;

value only_file_name () =
  if only_file.val = "" then Filename.concat setup_dir.val "only.txt"
  else only_file.val
;

value macro conf =
  fun
  [ '/' -> if Sys.unix then "/" else "\\"
  | 'a' -> strip_spaces (s_getenv conf.env "anon")
  | 'c' -> stringify setup_dir.val
  | 'd' -> conf.comm
  | 'i' -> strip_spaces (s_getenv conf.env "i")
  | 'l' -> conf.lang
  | 'm' -> server_string conf
  | 'n' -> referer conf
  | 'o' -> strip_spaces (s_getenv conf.env "o")
  | 'O' -> Filename.remove_extension (Filename.basename (strip_spaces (s_getenv conf.env "o")))
  | 'p' -> parameters conf.env
  | 'q' -> Version.txt
  | 'u' -> Filename.dirname (abs_setup_dir ())
  | 'x' -> stringify bin_dir.val
  | 'w' -> slashify (Sys.getcwd ())
  | 'y' -> Filename.basename (only_file_name ())
  | '%' -> "%"
  | 'K' -> (* print the name of -o filename, prepend bname or -o1 filename *)
          let outfile1 = strip_spaces (s_getenv conf.env "o") in
          let bname = strip_spaces (s_getenv conf.env "anon") in
          let outfile2 = strip_spaces (s_getenv conf.env "o1") in
          let outfile = 
            if outfile2 <> "" then outfile2
            else if bname <> ""
              then slashify_linux_dos bname ^ ".gwb" ^ outfile1
              else outfile1
          in
          outfile
   | 'P' -> string_of_int gwd_port.val
   | 'Q' -> parameters_1 conf.env
   | 'R' -> parameters_2 conf.env
   | c -> "BAD MACRO 1 " ^ String.make 1 c ]
;

value get_variable strm =
  loop 0 where rec loop len =
    match strm with parser
    [ [: `';' :] -> Buff.get len
    | [: `c :] -> loop (Buff.store len c) ]
;

value get_binding strm =
  loop 0 where rec loop len =
    match strm with parser
    [ [: `'=' :] -> let k = Buff.get len in (k, get_variable strm)
    | [: `c :] -> loop (Buff.store len c) ]
;

value variables bname =
  let dir = Filename.concat setup_dir.val "setup" in
  let fname = Filename.concat (Filename.concat dir "lang") bname in
  let ic = open_in fname in
  let strm = Stream.of_channel ic in
  let (vlist, flist) =
    loop ([], []) where rec loop (vlist, flist) =
      match strm with parser
      [ [: `'%' :] ->
          let (vlist, flist) =
            match strm with parser
            [ [: `('E' | 'C') :] ->
                  let (v, _) = get_binding strm in
                  if not (List.mem v vlist) then ([v :: vlist], flist)
                  else (vlist, flist)
            | [: `'V' :] ->
                let v = get_variable strm in
                if not (List.mem v vlist) then ([v :: vlist], flist)
                else (vlist, flist)
            | [: `'F' :] ->
                let v = get_variable strm in
                if not (List.mem v flist) then (vlist, [v :: flist])
                else (vlist, flist)
            | [: :] -> (vlist, flist) ]
          in
          loop (vlist, flist)
      | [: `_ :] -> loop (vlist, flist)
      | [: :] -> (vlist, flist) ]
  in
  do {
    close_in ic;
    (List.rev vlist, flist)
  }
;

value nth_field s n =
  loop 0 0 where rec loop nth i =
    let j =
      try String.index_from s i '/' with [ Not_found -> String.length s ]
    in
    if nth = n then String.sub s i (j - i)
    else if j = String.length s then s
    else loop (nth + 1) (j + 1)
;

value translate_phrase lang lexicon s n =
  let n =
    match n with
    [ Some n -> n
    | None -> 0 ]
  in
  try
    let s = Hashtbl.find lexicon s in
    nth_field s n
  with
  [ Not_found -> "[" ^ nth_field s n ^ "]" ]
;

value file_contents fname =
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      loop 0 where rec loop len =
        match try Some (input_char ic) with [ End_of_file -> None ] with
        [ Some '\r' -> loop len
        | Some c -> loop (Buff.store len c)
        | None -> do { close_in ic; Buff.get len } ]
  | None -> "" ]
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
          | None -> env ]
      in
      do { close_in ic; env }
  | None -> [] ]
;

value rec copy_from_stream conf print strm =
  try
    while True do {
      match Stream.next strm with
      [ '[' ->
          match Stream.peek strm with
          [ Some '\n' ->
              let s = parse_upto ']' strm in
              let (s, alt) = Translate.inline conf.lang '%' (macro conf) s in
              let s = if alt then "[" ^ s ^ "]" else s in
              print s
          | _ ->
              let s =
                loop 0 where rec loop len =
                  match strm with parser
                  [ [: `']' :] -> Buff.get len
                  | [: `c :] -> loop (Buff.store len c)
                  | [: :] -> Buff.get len ]
              in
              let n =
                match strm with parser
                [ [: `('0'..'9' as c) :] -> Some (Char.code c - Char.code '0')
                | [: :] -> None ]
              in
              print (translate_phrase conf.lang conf.lexicon s n) ]
      | '%' ->
          let c = Stream.next strm in
          match c with
          [ 'b' -> for_all conf print (all_db ".") strm
          | 'e' ->
              do {
                print "lang=";
                print conf.lang;
                List.iter
                  (fun (k, s) ->
                     if k = "opt" then ()
                     else do { print ";"; print k; print "="; print s; () })
                  conf.env
              }
          | 'f' -> (* see r *)
                let in_file = get_variable strm in
                let s = 
                  (file_contents
                  (slashify_linux_dos (bin_dir.val ^ "/setup/" ^ in_file)))
                in
                let in_base = strip_spaces (s_getenv conf.env "anon") in
                let benv = read_base_env in_base in
                let conf =
                  {(conf) with
                    env =
                    List.map (fun (k, v) -> (k, v)) benv @ conf.env}
                in
                (* depending on when %f is called, conf may be sketchy *)
                (* conf will know bvars from basename.gwf and evars from url *)
                copy_from_stream conf print (Stream.of_string s)
          | 'g' -> print_specific_file conf print "comm.log" strm
          | 'h' ->
              do {
                print "<input type=hidden name=lang value=";
                print conf.lang;
                print ">\n";
                List.iter
                  (fun (k, s) ->
                     if k = "opt" then ()
                     else do {
                       print "<input type=hidden name=";
                       print k;
                       print " value=\"";
                       print (decode_varenv s);
                       print "\">\n";
                       ()
                     })
                  conf.env
              }
          | 'j' -> print_selector conf print
          | 'k' -> for_all conf print (fst (List.split conf.env)) strm
          | 'r' ->
              print_specific_file conf print
                (Filename.concat setup_dir.val "gwd.arg") strm
          | 's' -> for_all conf print (selected conf.env) strm
          | 't' ->
              print_if conf print (if Sys.unix then False else True) strm
          | 'v' ->
              let out = strip_spaces (s_getenv conf.env "o") in
              print_if conf print (Sys.file_exists (out ^ ".gwb")) strm
          | 'y' -> for_all conf print (all_db (s_getenv conf.env "anon")) strm
          | 'z' -> print (string_of_int port.val)
          | 'A'..'Z' | '0'..'9' as c ->
              match c with
              [ 'C' | 'E' ->
                  let (k, v) = get_binding strm in
                  match p_getenv conf.env k with
                  [ Some x ->
                      if x = v then
                        print (if c = 'C' then " checked" else " selected")
                      else ()
                  | None -> () ]
              | 'L' ->
                  let lang = get_variable strm in
                  let lang_def = transl conf " !languages" in
                  print (Translate.language_name lang lang_def)
              | 'V' | 'F' ->
                  let k = get_variable strm in
                  match p_getenv conf.env k with
                  [ Some v -> print v
                  | None -> () ]
              | 'G' -> print_specific_file_tail conf print "gwsetup.log" strm
              | 'H' -> do { (* print the content of -o filename, prepend bname *)
                  let outfile = strip_spaces (s_getenv conf.env "o") in
                  let bname = strip_spaces (s_getenv conf.env "anon") in
                  let outfile = if bname <> ""
                    then slashify_linux_dos bname ^ ".gwb" ^ outfile 
                    else outfile
                  in
                  print_specific_file conf print outfile strm;
                  }
              | 'I' -> 
                  (* %Ivar;value;{var = value part|false part} *)
                  (* var is a evar from url or a bvar from basename.gwf or setup.gwf *)
                  let k1 = get_variable strm in
                  let k2 = get_variable strm in
                  (* 
                    trying to interpret macros on k2 parameter
                    need to replace print function by something which accumulates a string
                  let k2 = parse_upto ';' strm in
                  let k2 = copy_from_stream conf print (Stream.of_string k2) in
                  *)
                  match p_getenv conf.env k1 with
                  [ Some v ->
                      print_if_else conf print (v = k2) strm
                  | None -> 
                      print_if_else conf print False strm ]
              | 'O' ->
                  let fname = Filename.remove_extension (Filename.basename (strip_spaces (s_getenv conf.env "o"))) in
                  let fname = slashify_linux_dos fname in
                  print fname
              | 'P' -> print (string_of_int gwd_port.val)
              | 'Q' -> print (parameters_1 conf.env) (* same as p *)
              | 'R' -> print (parameters_2 conf.env) (* same as p *)
              | _ ->
                  match p_getenv conf.env (String.make 1 c) with
                  [ Some v ->
                      match strm with parser
                      [ [: `'{' :] ->
                          let s = parse_upto '}' strm in
                          do {
                            print "\"";
                            print s;
                            print "\"";
                            if v = s then print " selected" else ()
                          }
                      | [: `'[' :] ->
                          let s = parse_upto ']' strm in
                          do {
                            print "\"";
                            print s;
                            print "\"";
                            if v = s then print " checked" else ()
                          }
                      | [: :] -> print (strip_spaces v) ]
                  | None -> print "BAD MACRO 2 " ] ]
          | c -> print (macro conf c) ]
      | c -> print (String.make 1 c) ]
    }
  with
  [ Stream.Failure -> () ]
and print_specific_file conf print fname strm =
  match Stream.next strm with
  [ '{' ->
      let s = parse_upto '}' strm in
      if Sys.file_exists fname then do {
        let ic = open_in fname in
        if in_channel_length ic = 0 then
          copy_from_stream conf print (Stream.of_string s)
        else copy_from_stream conf print (Stream.of_channel ic);
        close_in ic
      }
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> () ]
and print_specific_file_tail conf print fname strm =
  match Stream.next strm with
  [ '{' ->
      let s = parse_upto '}' strm in
      if Sys.file_exists fname then do {
        let ic = open_in fname in
        if in_channel_length ic = 0 then
          copy_from_stream conf print (Stream.of_string s)
        else copy_from_stream conf print (Stream.of_channel ic);
        close_in ic
      }
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> () ]
and print_selector conf print =
  let sel =
    try getenv conf.env "sel" with
    [ Not_found ->
        try Sys.getenv "HOME" with
        [ Not_found -> Sys.getcwd () ] ]
  in
  let list =
    let sel =
      if Sys.unix then sel
      else
        if String.length sel = 3 && sel.[1] = ':' && sel.[2] = '\\' then
          sel ^ "."
        else sel
    in
    try
      let dh = Unix.opendir sel in
      loop [] where rec loop list =
        match try Some (Unix.readdir dh) with [ End_of_file -> None ] with
        [ Some x ->
            let list =
              if x = ".." then [x :: list]
              else if String.length x > 0 && x.[0] = '.' then list
              else [x :: list]
            in
            loop list
        | None -> List.sort compare list ]
    with
    [ Unix.Unix_error _ _ _ -> [".."] ]
  in
  do {
    print "<pre>\n";
    print "     ";
    print "<input type=hidden name=anon value=\"";
    print sel;
    print "\">";
    print sel;
    let list =
      List.map
        (fun x ->
           let d =
             if x = ".." then
               if Sys.unix then Filename.dirname sel
               else
                 if sel.[String.length sel - 1] <> '\\' then
                   Filename.dirname sel ^ "\\"
                 else Filename.dirname sel
             else Filename.concat sel x
           in
           let x = if is_directory d then Filename.concat x "" else x in
           (d, x))
        list
    in
    let max_len =
      List.fold_left (fun max_len (_, x) -> max max_len (String.length x))
        0 list
    in
    let min_interv = 2 in
    let line_len = 72 in
    let n_by_line = max 1 ((line_len + min_interv) / (max_len + min_interv)) in
    let newline () = print "\n" in
    newline ();
    loop 1 list where rec loop i =
      fun
      [ [(d, x) :: list] ->
          do {
            print "<a class=\"j\" href=\"";
            print conf.comm;
            print "?lang=";
            print conf.lang;
            print ";";
            List.iter
              (fun (k, v) ->
                 if k = "sel" then ()
                 else if k = "body_prop" then ()
                 else do { print k; print "="; print v; print ";" })
              conf.env;
            print "sel=";
            print (code_varenv d);
            print "\">";
            print x;
            print "</a>";
            if i = n_by_line then do {
              newline ();
              loop 1 list;
            }
            else if list = [] then newline ()
            else do {
              print (String.make (max_len + 2 - String.length x) ' ');
              loop (i + 1) list
            }
          }
      | [] -> print "\n" ];
    print "</pre>\n";
  }
and print_if conf print cond strm =
  match Stream.next strm with
  [ '{' ->
      let s = parse_upto '}' strm in
      if cond then copy_from_stream conf print (Stream.of_string s) else ()
  | _ -> () ]
and print_if_else conf print cond strm =
  match Stream.next strm with
  [ '{' ->
      let s1 = parse_upto '|' strm in
      let s2 = parse_upto '}' strm in
      if cond then copy_from_stream conf print (Stream.of_string s1)
      else copy_from_stream conf print (Stream.of_string s2)
  | _ -> () ]
and for_all conf print list strm =
  match Stream.next strm with
  [ '{' ->
      let s_exist = parse_upto '|' strm in
      let s_empty = parse_upto '}' strm in
      let eol =
        match strm with parser [ [: `'\\' :] -> False | [: :] -> True ]
      in
      if list <> [] then
        List.iter
          (fun db ->
             let conf = conf_with_env conf "anon" db in
             do {
               copy_from_stream conf print (Stream.of_string s_exist);
               if eol then print "\n" else ()
             })
          list
      else do {
        copy_from_stream conf print (Stream.of_string s_empty);
        if eol then print "\n" else ()
      }
  | _ -> () ]
;

value print_file conf bname =
  let dir = Filename.concat setup_dir.val "setup" in
  let fname = Filename.concat (Filename.concat dir "lang") bname in
  let ic_opt =
    try Some (open_in fname) with
    [ Sys_error _ -> None ]
  in
  match ic_opt with
  [ Some ic ->
      do {
        Wserver.http HttpStatus.OK;
        Wserver.header "Content-type: text/html; charset=%s" (charset conf);
        copy_from_stream conf (fun x -> Wserver.printf "%s" x)
          (Stream.of_channel ic);
        close_in ic;
        trailer conf
      }
  | None ->
      let title _ = Wserver.printf "Error" in
      do {
        header conf title;
        Wserver.printf "<ul><li>\n";
        Wserver.printf "Cannot access file \"%s\".\n" fname;
        Wserver.printf "</ul>\n";
        trailer conf;
        raise Exit
      } ]
;

value error conf str =
  do {
    header conf (fun _ -> Wserver.printf "Incorrect request");
    Wserver.printf "<em>%s</em>\n" (String.capitalize_ascii str);
    trailer conf
  }
;

value exec_f comm =
  let s = comm ^ " > " ^ "comm.log" in
  do {
    eprintf "$ cd \"%s\"\n" (Sys.getcwd ());
    flush stderr;
    eprintf "$ %s\n" s;
    flush stderr;
    Sys.command s
  }
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

value basename s =
  loop (String.length s - 1) where rec loop i =
    if i < 0 then s
    else
      match s.[i] with
      [ 'a'..'z' | 'A'..'Z' | '0'..'9' | '_' | '.' -> loop (i - 1)
      | _ -> String.sub s (i + 1) (String.length s - i - 1) ]
;

value setup_gen conf =
  match p_getenv conf.env "v" with
  [ Some fname -> print_file conf (basename fname)
  | _ -> error conf "request needs \"v\" parameter" ]
;

value print_default_gwf_file conf =
  let gwf =
    [ "access_by_key=yes";
      "disable_forum=yes";
      "hide_private_names=no";
      "use_restrict=no";
      "show_consang=yes";
      "display_sosa=yes";
      "place_surname_link_to_ind=yes";
      "max_anc_level=8";
      "max_anc_tree=7";
      "max_desc_level=12";
      "max_desc_tree=4";
      "max_cousins=2000";
      "max_cousins_level=5";
      "latest_event=20";
      "template=*";
      "long_date=no";
      "counter=no";
      "full_siblings=yes";
      "hide_advanced_request=no";
      "perso_module_i=individu";
      "perso_module_p=parents";
      "perso_module_g=gr_parents";
      "perso_module_u=unions";
      "perso_module_f=fratrie";
      "perso_module_r=relations";
      "perso_module_c=chronologie";
      "perso_module_n=notes";
      "perso_module_s=sources";
      "perso_module_a=arbres";
      "perso_module_d=data_3col";
      "perso_module_l=ligne";
      "p_mod="
    ]
  in
  let bname = try List.assoc "o" conf.env with [ Not_found -> "" ] in
  let dir = Sys.getcwd () in
  let fname = Filename.concat dir (bname ^ ".gwf") in
  if Sys.file_exists fname then ()
  else do {
    let oc = open_out fname in
    List.iter (fun s -> fprintf oc "%s\n" s) gwf;
    close_out oc
  }
;

value simple conf =
  let ged =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let ged =
    if Filename.check_suffix (String.lowercase_ascii ged) ".ged" then ged
    else ""
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
  let env = [("f", "on") :: conf.env] in
  let env = list_replace "anon" ged env in
  let conf =
    {comm = if ged = "" then "gwc" else "ged2gwb";
     env = list_replace "o" out_file env; lang = conf.lang;
     request = conf.request; lexicon = conf.lexicon}
  in
  if ged <> "" && not (Sys.file_exists ged) then
    print_file conf "err_unkn.htm"
  else if out_file = "" then print_file conf "err_miss.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"
;

value simple2 conf =
  let ged =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let ged =
    if Filename.check_suffix (String.lowercase_ascii ged) ".ged" then ged
    else ""
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
  let env = [("f", "on") :: conf.env] in
  let env = list_replace "anon" ged env in
  let conf =
    {comm = if ged = "" then "gwc2" else "ged2gwb2";
     env = list_replace "o" out_file env; lang = conf.lang;
     request = conf.request; lexicon = conf.lexicon}
  in
  if ged <> "" && not (Sys.file_exists ged) then
    print_file conf "err_unkn.htm"
  else if out_file = "" then print_file conf "err_miss.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"
;

value gwc_or_ged2gwb out_name_of_in_name conf =
  let fname =
    match p_getenv conf.env "fname" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_file =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_file =
    if fname = "" then in_file
    else in_file ^ ( if Sys.unix then "/" else "\\" ) ^ fname
  in
  let conf = conf_with_env conf "anon" in_file in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let out_file =
    if out_file = "" then out_name_of_in_name in_file else out_file
  in
  (* clean up env *)
  let conf = conf_with_env conf "body_prop" "" in
  let conf = conf_with_env conf "fname" "" in
  
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" || out_file = "" then print_file conf "err_miss.htm"
  else if not (Sys.file_exists in_file) && not (String.contains fname '*')
    then print_file conf "err_unkn.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"
;

value gwc2_or_ged2gwb2 out_name_of_in_name conf =
  let fname =
    match p_getenv conf.env "fname" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_file =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_file =
    if fname = "" then in_file
    else in_file ^ ( if Sys.unix then "/" else "\\" ) ^ fname
  in
  let conf = conf_with_env conf "anon" in_file in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let out_file =
    if out_file = "" then out_name_of_in_name in_file else out_file
  in
  (* clean up env *)
  let conf = conf_with_env conf "body_prop" "" in
  let conf = conf_with_env conf "fname" "" in
  
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" || out_file = "" then print_file conf "err_miss.htm"
  else if not (Sys.file_exists in_file) && not (String.contains fname '*')
    then print_file conf "err_unkn.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"
;

value gwc_check conf =
  let conf = {(conf) with env = [("nofail", "on"); ("f", "on") :: conf.env]} in
  gwc_or_ged2gwb out_name_of_gw conf
;

value gwc2_check conf =
  let conf = {(conf) with env = [("nofail", "on"); ("f", "on") :: conf.env]} in
  gwc2_or_ged2gwb2 out_name_of_gw conf
;

value ged2gwb_check conf =
  let conf = {(conf) with env = [("f", "on") :: conf.env]} in
  gwc_or_ged2gwb out_name_of_ged conf
;

value ged2gwb2_check conf =
  let conf = {(conf) with env = [("f", "on") :: conf.env]} in
  gwc2_or_ged2gwb2 out_name_of_ged conf
;

(*ifdef WINDOWS then*)
value infer_rc conf rc =
  if rc > 0 then rc
  else
    match p_getenv conf.env "o" with
    [ Some out_file ->
        if Sys.file_exists (out_file ^ ".gwb") then 0 else 2
    | _ -> 0 ]
;

value gwc conf =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val "gwc") in
    exec_f (comm ^ parameters conf.env)
  in
  let rc = if Sys.unix then rc else infer_rc conf rc in
  do {
    let gwo = strip_spaces (s_getenv conf.env "anon") ^ "o" in
    try Sys.remove gwo with [ Sys_error _ -> () ];
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bso_err.htm"
    else do {
      print_default_gwf_file conf;
      print_file conf "bso_ok.htm"
    }
  }
;

value gwc2 conf =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val "gwc2") in
    exec_f (comm ^ parameters conf.env)
  in
  let rc = if Sys.unix then rc else infer_rc conf rc in
  do {
    let gwo = strip_spaces (s_getenv conf.env "anon") ^ "o" in
    try Sys.remove gwo with [ Sys_error _ -> () ];
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bso_err.htm"
    else do {
      print_default_gwf_file conf;
      print_file conf "bso_ok.htm"
    }
  }
;

value gwdiff_check conf =
  print_file conf "bsi.htm"
;

value gwdiff conf ok_file =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  let rc =
    let commnd = "cd " ^ (Sys.getcwd ()) ^ "; tput bel;" ^
        (stringify (Filename.concat bin_dir.val "gwdiff")) ^ " " ^
            parameters_2 conf.env in
    if uname = "Darwin" then
      let launch = "tell application \"Terminal\" to do script " in
      Sys.command ("osascript -e '" ^ launch ^ " \" " ^ commnd ^ " \"' " )
    else if uname = "Linux" then
      (* non testé ! *)
      Sys.command ("xterm -e \" " ^ commnd ^ " \" ")
    else if Sys.os_type = "Win32" then
      (* à compléter et tester ! *)
      let commnd = (stringify (Filename.concat bin_dir.val "gwdiff")) ^ " " ^
          parameters_2 conf.env in
      Sys.command (commnd)
    else do {
      eprintf "%s (%s) %s (%s)\n" 
        "Unknown Os_type" Sys.os_type "or wrong uname response" uname;
      2}
  in
  do {
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
;

value connex_check conf =
  print_file conf "bsi_connex.htm"
;

value connex conf ok_file =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  let rc =
    let commnd = "cd " ^ (Sys.getcwd ()) ^ "; tput bel;" ^
        (stringify (Filename.concat bin_dir.val "connex")) ^ " " ^
            parameters_1 conf.env in
    if uname = "Darwin" then
      let launch = "tell application \"Terminal\" to do script " in
      Sys.command ("osascript -e '" ^ launch ^ " \" " ^ commnd ^ " \"' " )
    else if uname = "Linux" then
      (* non testé ! *)
      Sys.command ("xterm -e \" " ^ commnd ^ " \" ")
    else if Sys.os_type = "Win32" then
      (* à compléter et tester ! *)
      let commnd = (stringify (Filename.concat bin_dir.val "connex")) ^ " " ^
          parameters_1 conf.env in
      Sys.command (commnd)
    else do {
      eprintf "%s (%s) %s (%s)\n" 
        "Unknown Os_type" Sys.os_type "or wrong uname response" uname;
      2}
  in
  do {
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
;

value gwu_or_gwb2ged_check suffix conf =
  let in_file =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let od =
    match p_getenv conf.env "od" with
    [ Some f -> Filename.basename (strip_spaces f)
    | None -> "" ]
  in
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> Filename.basename (strip_spaces f)
    | None -> "" ]
  in
  let odir =
    if od = "odir" then
      match p_getenv conf.env "odir" with
      [ Some f -> Filename.basename (strip_spaces f)
      | None -> "" ]
    else ""
  in
  let out_file =
    if out_file = "" || out_file = Filename.current_dir_name then
      in_file ^ suffix
    else if Filename.check_suffix out_file suffix then out_file
    else if Filename.check_suffix out_file (String.uppercase_ascii suffix) then
      out_file
    else out_file ^ suffix
  in
  let conf = conf_with_env conf "od" "" in
  let conf = conf_with_env conf "odir" odir in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_miss.htm"
  else print_file conf "bsi.htm"
;

value gwu = gwu_or_gwb2ged_check ".gw";
value gwb2ged = gwu_or_gwb2ged_check ".ged";

value gwb2ged_or_gwu_1 ok_file conf =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm"
    else
      let conf =
        conf_with_env conf "o" (Filename.basename (s_getenv conf.env "o"))
      in
      print_file conf ok_file
  }
;

value gwb2ged_1 = gwb2ged_or_gwu_1 "gw2gd_ok.htm";
value gwu_1 = gwb2ged_or_gwu_1 "gwu_ok.htm";

value consang_check conf =
  let in_f =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_f = "" then print_file conf "err_miss.htm"
  else print_file conf "bsi.htm"
;

value update_nldb_check conf =
  let in_f =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_f = "" then print_file conf "err_miss.htm"
  else print_file conf "bsi.htm"
;

value has_gwu dir =
  match
    try Some (Unix.opendir dir) with [ Unix.Unix_error _ _ _ -> None ]
  with
  [ Some dh ->
      let gwu_found =
        try
          loop () where rec loop () =
            let e = Unix.readdir dh in
            if Sys.unix then
              match e with
              [ "gwu" -> raise Exit
              | _ -> loop () ]
            else
              match String.lowercase_ascii e with
              [ "gwu.exe" -> raise Exit
              | _ -> loop () ]
        with
        [ End_of_file -> False
        | Exit -> True ]
      in
      do { Unix.closedir dh; gwu_found }
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
      let dir = init_dir in
      if has_gwu dir then (dir, True)
      else
        let dir = Filename.dirname init_dir in
        if has_gwu dir then (dir, True)
        else
          let dir = Filename.concat dir "gw" in
          if has_gwu dir then (dir, True) else (init_dir, False)
  in
  let conf = conf_with_env conf "anon" init_dir in
  let dest_dir = Sys.getcwd () in
  if init_dir = "" then print_file conf "err_miss.htm"
  else if init_dir = dest_dir then print_file conf "err_smdr.htm"
  else if not (Sys.file_exists init_dir) then print_file conf "err_ndir.htm"
  else if
    (if Sys.unix then
       try
         (Unix.stat (Filename.concat init_dir ".")).Unix.st_ino =
           (Unix.stat (Filename.concat dest_dir ".")).Unix.st_ino
       with
       [ Unix.Unix_error _ _ _ -> False ]
     else False)
  then
    print_file conf "err_smdr.htm"
  else if not dir_has_gwu then print_file conf "err_ngw.htm"
  else print_file conf "recover1.htm"
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
  if in_file = "" then print_file conf "err_miss.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else
    let (old_to_src, o_opt, tmp, src_to_new) =
      if not by_gedcom then ("gwu", " > ", "tmp.gw", "gwc")
      else ("gwb2ged", " -o ", "tmp.ged", "ged2gwb")
    in
    let conf =
      {(conf) with
        env =
          [("U", old_to_src); ("O", o_opt); ("T", tmp);
           ("src2new", src_to_new) :: conf.env]}
    in
    print_file conf "recover2.htm"
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
  let rc =
    try
      do {
        eprintf "$ cd \"%s\"\n" init_dir;
        flush stderr;
        Sys.chdir init_dir;
        let c =
          Filename.concat "." old_to_src ^ " " ^ in_file ^ o_opt ^
            stringify (Filename.concat dir tmp)
        in
        eprintf "$ %s\n" c;
        flush stderr;
        Sys.command c
      }
    with e ->
      do { Sys.chdir dir; raise e }
  in
  let rc =
    if rc = 0 then do {
      eprintf "$ cd \"%s\"\n" dir;
      flush stderr;
      Sys.chdir dir;
      let c =
        Filename.concat bin_dir.val src_to_new ^ " " ^ tmp ^ " -f -o " ^
          out_file ^ " > " ^ "comm.log"
      in
      eprintf "$ %s\n" c;
      flush stderr;
      let rc = Sys.command c in
      let rc = if Sys.unix then rc else infer_rc conf rc in
      eprintf "\n";
      flush stderr;
      rc
    }
    else rc
  in
  do {
    if rc > 1 then do { Sys.chdir dir; print_file conf "err_reco.htm" }
    else print_file conf "bso_ok.htm"
  }
;

value rmdir dir =
  (* Récupère tous les fichiers et dossier d'un dossier         *)
  (* et renvoie la liste des dossiers et la liste des fichiers. *)
  let read_files_folders fname =
    let list =
      List.map
        (fun file -> Filename.concat fname file)
        (Array.to_list (Sys.readdir fname))
    in
    List.partition Sys.is_directory list
  in
  (* Parcours récursif de tous les dossiers *)
  let rec loop l folders files =
    match l with
    [ [] -> (folders, files)
    | [x :: l] ->
        let (fd, fi) = read_files_folders x in
        let l = List.rev_append l fd in
        let folders = List.rev_append fd folders in
        let files = List.rev_append fi files in
        loop l folders files ]
  in
  (* Toute l'arborescence de dir *)
  let (folders, files) = loop [dir] [] [] in
  do {
    List.iter (fun f -> try Unix.unlink f with [ _ -> () ]) files;
    List.iter (fun f -> try Unix.rmdir f with [ _ -> () ]) folders;
    try Unix.rmdir dir with [ Unix.Unix_error _ _ _ -> () ]
  }
;

value rm_base dir = rmdir dir;

value cleanup conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let conf = {(conf) with comm = "."} in
  if in_base = "" then print_file conf "err_miss.htm"
  else print_file conf "cleanup1.htm"
;

value cleanup_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let in_base_dir = in_base ^ ".gwb" in
  do {
    eprintf "$ cd \"%s\"\n" (Sys.getcwd ());
    flush stderr;
    let c =
      Filename.concat bin_dir.val "gwu" ^ " " ^ in_base ^ " -o tmp.gw"
    in
    eprintf "$ %s\n" c;
    flush stderr;
    let _ = Sys.command c in
    eprintf "$ mkdir old\n";
    try Unix.mkdir "old" 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    if Sys.unix then eprintf "$ rm -rf old/%s\n" in_base_dir
    else do {
      eprintf "$ del old\\%s\\*.*\n" in_base_dir;
      eprintf "$ rmdir old\\%s\n" in_base_dir
    };
    flush stderr;
    rm_base (Filename.concat "old" in_base_dir);
    if Sys.unix then eprintf "$ mv %s old/.\n" in_base_dir
    else eprintf "$ move %s old\\.\n" in_base_dir;
    flush stderr;
    Sys.rename in_base_dir (Filename.concat "old" in_base_dir);
    let c =
      Filename.concat bin_dir.val "gwc" ^ " tmp.gw -nofail -o " ^ in_base ^
        " > comm.log 2>&1"
    in
    eprintf "$ %s\n" c;
    flush stderr;
    let rc = Sys.command c in
    let rc = if Sys.unix then rc else infer_rc conf rc in
    eprintf "\n";
    flush stderr;
    if rc > 1 then
      let conf = {(conf) with comm = "gwc"} in
      print_file conf "bsi_err.htm"
    else print_file conf "clean_ok.htm"
  }
;

value rec check_new_names conf l1 l2 =
  match (l1, l2) with
  [ ([(k, v) :: l], [x :: m]) ->
      if k <> x then do { print_file conf "err_outd.htm"; raise Exit }
      else if not (good_name v) then do {
        let conf = {(conf) with env = [("o", v) :: conf.env]} in
        print_file conf "err_name.htm";
        raise Exit
      }
      else check_new_names conf l m
  | ([], []) -> ()
  | _ -> do { print_file conf "err_outd.htm"; raise Exit } ]
;

value rec check_rename_conflict conf =
  fun
  [ [x :: l] ->
      if List.mem x l then do {
        let conf = {(conf) with env = [("o", x) :: conf.env]} in
        print_file conf "err_cnfl.htm";
        raise Exit
      }
      else check_rename_conflict conf l
  | [] -> () ]
;

value rename conf =
  let rename_list =
    List.map (fun (k, v) -> (k, strip_spaces (decode_varenv v))) conf.env
  in
  try
    do {
      check_new_names conf rename_list (all_db ".");
      check_rename_conflict conf (snd (List.split rename_list));
      List.iter
        (fun (k, v) ->
           if k <> v then Sys.rename (k ^ ".gwb") ("_" ^ k ^ ".gwb") else ())
        rename_list;
      List.iter
        (fun (k, v) ->
           if k <> v then Sys.rename ("_" ^ k ^ ".gwb") (v ^ ".gwb") else ())
        rename_list;
      print_file conf "ren_ok.htm"
    }
  with
  [ Exit -> () ]
;

value delete conf = print_file conf "delete_1.htm";

value delete_1 conf =
  do {
    List.iter (fun (k, v) -> if v = "del" then rm_base (k ^ ".gwb") else ())
      conf.env;
    print_file conf "del_ok.htm"
  }
;

value merge conf =
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let conf = {(conf) with comm = "."} in
  let bases = selected conf.env in
  if out_file = "" || List.length bases < 2 then
    print_file conf "err_miss.htm"
  else if not (good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "merge_1.htm"
;

value merge_1 conf =
  let out_file =
    match p_getenv conf.env "o" with
    [ Some f -> strip_spaces f
    | _ -> "" ]
  in
  let bases = selected conf.env in
  let dir = Sys.getcwd () in
  do {
    eprintf "$ cd \"%s\"\n" dir;
    flush stderr;
    Sys.chdir dir;
    let rc =
      loop bases where rec loop =
        fun
        [ [] -> 0
        | [b :: bases] ->
            let c =
              Filename.concat bin_dir.val "gwu" ^ " " ^ b ^ " -o " ^ b ^
                ".gw"
            in
            do {
              eprintf "$ %s\n" c;
              flush stderr;
              let r = Sys.command c in
              if r = 0 then loop bases else r
            } ]
    in
    let rc =
      if rc <> 0 then rc
      else do {
        let c =
          Filename.concat bin_dir.val "gwc" ^
            List.fold_left
              (fun s b ->
                 if s = "" then " " ^ b ^ ".gw" else s ^ " -sep " ^ b ^ ".gw")
              "" bases ^
            " -f -o " ^ out_file ^ " > comm.log 2>&1"
        in
        eprintf "$ %s\n" c;
        flush stderr;
        Sys.command c
      }
    in
    if rc > 1 then print_file conf "bso_err.htm"
    else print_file conf "bso_ok.htm"
  }
;

value read_gwd_arg () =
  let fname = Filename.concat setup_dir.val "gwd.arg" in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let list =
        loop [] where rec loop list =
          match try Some (input_line ic) with [ End_of_file -> None ] with
          [ Some "" -> loop list
          | Some s -> loop [s :: list]
          | None -> list ]
      in
      do {
        close_in ic;
        let rec loop env =
          fun
          [ [x :: l] ->
              if x.[0] = '-' then
                let x = String.sub x 1 (String.length x - 1) in
                match l with
                [ [y :: l] when y.[0] <> '-' -> loop [(x, y) :: env] l
                | _ -> loop [(x, "") :: env] l ]
              else loop env l
          | [] -> List.rev env ]
        in
        loop [] (List.rev list)
      }
  | None -> [] ]
;

value gwf conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  if in_base = "" then print_file conf "err_miss.htm"
  else
    let benv = read_base_env in_base in
    let trailer =
      quote_escaped
        (file_contents (Filename.concat "lang" (in_base ^ ".trl")))
    in
    let conf =
      {(conf) with
        env =
          List.map (fun (k, v) -> (k, v)) benv @
          [("trailer", trailer) :: conf.env]}
    in
    print_file conf "gwf_1.htm"
;

value gwf_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with
    [ Some f -> strip_spaces f
    | None -> "" ]
  in
  let benv = read_base_env in_base in
  let (vars, files) = variables "gwf_1.htm" in
  do {
    let oc = open_out (in_base ^ ".gwf") in
    let body_prop =
      match p_getenv conf.env "proposed_body_prop" with
      [ Some "" | None -> s_getenv conf.env "body_prop"
      | Some x -> x ]
    in
    fprintf oc "# File generated by \"setup\"\n\n";
    List.iter
      (fun k ->
         match k with
         [ "body_prop" ->
             if body_prop = "" then ()
             else fprintf oc "body_prop=%s\n" body_prop
         | _ -> fprintf oc "%s=%s\n" k (s_getenv conf.env k) ])
      vars;
    List.iter
      (fun (k, v) ->
         if List.mem k vars then ()
         else fprintf oc "%s=%s\n" k v)
      benv;

    close_out oc;
    let trl = strip_spaces (strip_control_m (s_getenv conf.env "trailer")) in
    let trl_file = Filename.concat "lang" (in_base ^ ".trl") in
    try Unix.mkdir "lang" 0o755 with [ Unix.Unix_error _ _ _ -> () ];
    try
      if trl = "" then Sys.remove trl_file
      else do {
        let oc = open_out trl_file in
        output_string oc trl;
        output_string oc "\n";
        close_out oc
      }
    with
    [ Sys_error _ -> () ];
    print_file conf "gwf_ok.htm"
  }
;

value gwd conf =
  let aenv = read_gwd_arg () in
  let get v = try List.assoc v aenv with [ Not_found -> "" ] in
  let conf =
    {(conf) with
      env =
        [("default_lang", get "lang"); ("only", get "only");
         ("log", Filename.basename (get "log")) ::
         conf.env]}
  in
  print_file conf "gwd.htm"
;

value gwd_1 conf =
  let oc = open_out (Filename.concat setup_dir.val "gwd.arg") in
  let print_param k =
    match p_getenv conf.env k with
    [ Some v when v <> "" -> fprintf oc "-%s\n%s\n" k v
    | _ -> () ]
  in
  do {
    match p_getenv conf.env "setup_link" with
    [ Some v -> fprintf oc "-setup_link\n"
    | _ -> () ];
    print_param "only";
    match p_getenv conf.env "default_lang" with
    [ Some v when v <> "" -> fprintf oc "-lang\n%s\n" v
    | _ -> () ];
    print_param "log";
    close_out oc;
    print_file conf "gwd_ok.htm"
  }
;

value ged2gwb conf =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ " -fne '\"\"'" ^ parameters conf.env)
  in
  let rc = if Sys.unix then rc else infer_rc conf rc in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bso_err.htm"
    else do {
      print_default_gwf_file conf;
      print_file conf "bso_ok.htm"
    }
  }
;

value ged2gwb2 conf =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ " -fne '\"\"'" ^ parameters conf.env)
  in
  let rc = if Sys.unix then rc else infer_rc conf rc in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bso_err.htm"
    else do {
      print_default_gwf_file conf;
      print_file conf "bso_ok.htm"
    }
  }
;

value consang conf ok_file =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
;

value update_nldb conf ok_file =
  let rc =
    let comm = stringify (Filename.concat bin_dir.val conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  do {
    eprintf "\n";
    flush stderr;
    if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file
  }
;

value separate_slashed_filename s =
  loop 0 where rec loop i =
    match try Some (String.index_from s i '/') with [ Not_found -> None ] with
    [ Some j ->
        if j > i then [String.sub s i (j - i) :: loop (j + 1)]
        else loop (j + 1)
    | None ->
        if i >= String.length s then []
        else [String.sub s i (String.length s - i)] ]
;

value start_with s x =
  let slen = String.length s in
  let xlen = String.length x in
  slen >= xlen && String.sub s 0 xlen = x
;

value end_with s x =
  let slen = String.length s in
  let xlen = String.length x in
  slen >= xlen && String.sub s (slen - xlen) xlen = x
;

value print_typed_file conf typ fname =
  let ic_opt =
    try Some (open_in_bin fname) with
    [ Sys_error _ -> None ]
  in
  match ic_opt with
  [ Some ic ->
      do {
        Wserver.http HttpStatus.OK;
        Wserver.header "Content-type: %s" typ;
        Wserver.header "Content-length: %d" (in_channel_length ic);
        try
          while True do {
            let c = input_char ic in
            Wserver.printf "%c" c
          }
        with [ End_of_file -> () ];
        close_in ic;
      }
  | None ->
      let title _ = Wserver.printf "Error" in
      do {
        header conf title;
        Wserver.printf "<ul><li>\n";
        Wserver.printf "Cannot access file \"%s\".\n" fname;
        Wserver.printf "</ul>\n";
        trailer conf;
        raise Exit
      } ]
;

value raw_file conf s =
  let fname =
    List.fold_left Filename.concat setup_dir.val
      (separate_slashed_filename s)
  in
  let typ =
    if end_with s ".png" then "image/png"
    else if end_with s ".jpg" then "image/jpeg"
    else if end_with s ".gif" then "image/gif"
    else if end_with s ".css" then "text/css"
    else "text/html"
  in
  print_typed_file conf typ fname
;

value has_gwb_directories dh =
  try
    let rec loop () =
      let e = Unix.readdir dh in
      if Filename.check_suffix e ".gwb" then True else loop ()
    in
    loop ()
  with
  [ End_of_file -> do { Unix.closedir dh; False } ]
;

value setup_comm_ok conf =
  fun
  [ "gwsetup" -> setup_gen conf
  | "simple" -> simple conf
  | "simple2" -> simple2 conf
  | "recover" -> recover conf
  | "recover_1" -> recover_1 conf
  | "recover_2" -> recover_2 conf
  | "cleanup" -> cleanup conf
  | "cleanup_1" -> cleanup_1 conf
  | "rename" -> rename conf
  | "delete" -> delete conf
  | "delete_1" -> delete_1 conf
  | "merge" -> merge conf
  | "merge_1" -> merge_1 conf
  | "gwc" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwc_check conf
      | _ -> gwc conf ]
  | "gwc2" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwc2_check conf
      | _ -> gwc2 conf ]
  | "gwu" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwu conf
      | _ -> gwu_1 conf ]
  | "ged2gwb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb_check conf
      | _ -> ged2gwb conf ]
  | "ged2gwb2" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> ged2gwb2_check conf
      | _ -> ged2gwb2 conf ]
  | "gwb2ged" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> gwb2ged conf
      | _ -> gwb2ged_1 conf ]
  | "consang" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> consang_check conf
      | _ -> consang conf "consg_ok.htm" ]
  | "update_nldb" ->
      match p_getenv conf.env "opt" with
      [ Some "check" -> update_nldb_check conf
      | _ -> update_nldb conf "update_nldb_ok.htm" ]
  | "gwf" -> gwf conf
  | "gwf_1" -> gwf_1 conf
  | "gwd" -> gwd conf
  | "gwd_1" -> gwd_1 conf
  | "connex" -> 
       match p_getenv conf.env "opt" with
      [ Some "check" -> connex_check conf
      | _ -> connex conf "connex_ok.htm" ]
  | "gwdiff" -> 
       match p_getenv conf.env "opt" with
      [ Some "check" -> gwdiff_check conf
      | _ -> gwdiff conf "gwdiff_ok.htm" ]
  | x ->
      if start_with x "doc/" || start_with x "images/" || start_with x "css/"
      then raw_file conf x
      else error conf ("bad command: \"" ^ x ^ "\"") ]
;

value setup_comm conf comm =
  match p_getenv conf.env "cancel" with
  [ Some _ ->
      setup_gen {(conf) with env = [("lang", conf.lang); ("v", "main.htm")]}
  | None -> setup_comm_ok conf comm ]
;

value string_of_sockaddr =
  fun
  [ Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET a _ -> Unix.string_of_inet_addr a ]
;

value local_addr = "127.0.0.1";

value only_addr () =
  let fname = only_file_name () in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let v = try input_line ic with [ End_of_file -> local_addr ] in
      do { close_in ic; v }
  | None -> local_addr ]
;

value lindex s c =
  pos 0 where rec pos i =
    if i = String.length s then None
    else if s.[i] = c then Some i
    else pos (i + 1)
;

value input_lexicon lang =
  let t = Hashtbl.create 501 in
  try
    let ic =
      open_in
        (List.fold_right Filename.concat [setup_dir.val; "setup"; "lang"]
           "lexicon.txt")
    in
    let derived_lang =
      match lindex lang '-' with
      [ Some i -> String.sub lang 0 i
      | _ -> "" ]
    in
    try
      do {
        try
          while True do {
            let k =
              find_key (input_line ic) where rec find_key line =
                if String.length line < 4 then find_key (input_line ic)
                else if String.sub line 0 4 <> "    " then
                  find_key (input_line ic)
                else line
            in
            let k = String.sub k 4 (String.length k - 4) in
            let rec loop line =
              match lindex line ':' with
              [ Some i ->
                  let line_lang = String.sub line 0 i in
                  do {
                    if line_lang = lang ||
                       line_lang = derived_lang && not (Hashtbl.mem t k) then
                      let v =
                        if i + 1 = String.length line then ""
                        else
                          String.sub line (i + 2) (String.length line - i - 2)
                      in
                      Hashtbl.add t k v
                    else ();
                    loop (input_line ic)
                  }
              | None -> () ]
            in
            loop (input_line ic)
          }
        with
        [ End_of_file -> () ];
        close_in ic;
        t
      }
    with e ->
      do { close_in ic; raise e }
  with
  [ Sys_error _ -> t ]
;

value setup (addr, req) comm env_str =
  let conf =
    let env = create_env env_str in
    if env = [] && (comm = "" || String.length comm = 2) then
      let lang =
        if comm = "" then default_lang.val else String.lowercase_ascii comm
      in
      let lexicon = input_lexicon lang in
      {lang = lang; comm = ""; env = env; request = req; lexicon = lexicon}
    else
      let (lang, env) =
        match p_getenv env "lang" with
        [ Some x -> (x, list_remove_assoc "lang" env)
        | _ -> (default_lang.val, env) ]
      in
      let lexicon = input_lexicon lang in
      {lang = lang; comm = comm; env = env; request = req; lexicon = lexicon}
  in
  let saddr = string_of_sockaddr addr in
  let s = only_addr () in
  if s <> saddr then do {
    let conf = {(conf) with env = [("anon", saddr); ("o", s)]} in
    eprintf "Invalid request from \"%s\"; only \"%s\" accepted.\n"
      saddr s;
    flush stderr;
    print_file conf "err_acc.htm"
  }
  else if conf.comm = "" then print_file conf "welcome.htm"
  else setup_comm conf comm
;

value wrap_setup a b c =
  do {
    if Sys.unix then ()
    else do {
      (* another process have been launched, therefore we lost variables;
         and we cannot parse the arg list again, because of possible spaces
         in arguments which may appear as separators *)
      try default_lang.val := Sys.getenv "GWLANG" with [ Not_found -> () ];
      try setup_dir.val := Sys.getenv "GWGD" with [ Not_found -> () ];
      try bin_dir.val := Sys.getenv "GWGD" with [ Not_found -> () ]
    };
    try setup a b c with [ Exit -> () ]
  }
;

value copy_text lang fname =
  let dir = Filename.concat setup_dir.val "setup" in
  let fname = Filename.concat dir fname in
  match try Some (open_in fname) with [ Sys_error _ -> None ] with
  [ Some ic ->
      let conf =
        {lang = lang; comm = ""; env = []; request = [];
         lexicon = Hashtbl.create 1}
      in
      do {
        copy_from_stream conf print_string (Stream.of_channel ic);
        flush stdout;
        close_in ic
      }
  | _ ->
      do {
        printf "\nCannot access file \"%s\".\n" fname;
        printf "Type \"Enter\" to exit\n? ";
        flush stdout;
        let _ = input_line stdin in ();
        exit 2
      } ]
;

value set_gwd_default_language_if_absent lang =
  let env = read_gwd_arg () in
  let fname = Filename.concat setup_dir.val "gwd.arg" in
  match try Some (open_out fname) with [ Sys_error _ -> None ] with
  [ Some oc ->
      let lang_found = ref False in
      do {
        List.iter
          (fun (k, v) ->
             do {
               fprintf oc "-%s\n" k;
               if k = "lang" then lang_found.val := True else ();
               if v <> "" then fprintf oc "%s\n" v else ();
             })
          env;
        if not lang_found.val then fprintf oc "-lang\n%s\n" lang
        else ();
        close_out oc
      }
  | None -> () ]
;

value daemon = ref False;

value usage =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:";
value speclist =
  [("-bd", Arg.String (fun x -> base_dir.val := x),
    "<dir>: Directory where the databases are installed.");
   ("-gwd_p", Arg.Int (fun x -> gwd_port.val := x),
    "<number>: Specify the port number of gwd (default = " ^
      string_of_int gwd_port.val ^ "); > 1024 for normal users.");
   ("-lang", Arg.String (fun x -> lang_param.val := x),
    "<string>: default lang");
   ("-daemon", Arg.Set daemon, ": Unix daemon mode.");
   ("-p", Arg.Int (fun x -> port.val := x),
    "<number>: Select a port number (default = " ^
      string_of_int port.val ^ "); > 1024 for normal users.");
   ("-only", Arg.String (fun s -> only_file.val := s),
    "<file>: File containing the only authorized address");
   ("-gd", Arg.String (fun x -> setup_dir.val := x),
    "<string>: gwsetup directory");
   ("-bindir", Arg.String (fun x -> bin_dir.val := x),
    "<string>: binary directory (default = value of option -gd)") ]
;
value anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s));

value null_reopen flags fd =
  if Sys.unix then do {
    let fd2 = Unix.openfile "/dev/null" flags 0 in
    Unix.dup2 fd2 fd;
    Unix.close fd2
  }
  else ()
;

value setup_available_languages = ["de"; "en"; "es"; "fr"; "it"; "lv"; "sv"];

value intro () =
  let (default_gwd_lang, default_setup_lang) =
    if Sys.unix then
      let s = try Sys.getenv "LANG" with [ Not_found -> "" ] in
      if List.mem s Version.available_languages then
        (s, if List.mem s setup_available_languages then s else "en")
      else
        let s = try Sys.getenv "LC_CTYPE" with [ Not_found -> "" ] in
        if String.length s >= 2 then
          let s = String.sub s 0 2 in
          if List.mem s Version.available_languages then
            (s, if List.mem s setup_available_languages then s else "en")
          else (default_lang.val, default_lang.val)
        else (default_lang.val, default_lang.val)
    else (default_lang.val, default_lang.val)
  in
  do {
    Argl.parse speclist anonfun usage;
    if bin_dir.val = "" then bin_dir.val := setup_dir.val else ();
    default_lang.val := default_setup_lang;
    let (gwd_lang, setup_lang) =
      if daemon.val then
        if Sys.unix then do {
          let setup_lang =
            if String.length lang_param.val < 2 then default_setup_lang
            else lang_param.val
          in
          printf "To start, open location http://localhost:%d/\n"
            port.val;
          flush stdout;
          if Unix.fork () = 0 then do {
            Unix.close Unix.stdin;
            null_reopen [Unix.O_WRONLY] Unix.stdout
          }
          else exit 0;
          (default_gwd_lang, setup_lang)
        }
        else (default_gwd_lang, default_setup_lang)
      else do {
        let (gwd_lang, setup_lang) =
          if String.length lang_param.val < 2 then do {
            copy_text "" "intro.txt";
            let x = String.lowercase_ascii (input_line stdin) in
            if String.length x < 2 then
              (default_gwd_lang, default_setup_lang)
            else let x = String.sub x 0 2 in (x, x)
          }
          else (lang_param.val, lang_param.val)
        in
        copy_text setup_lang (Filename.concat "lang" "intro.txt");
        (gwd_lang, setup_lang)
      }
    in
    set_gwd_default_language_if_absent gwd_lang;
    default_lang.val := setup_lang;
    if Sys.unix then ()
    else do {
      Unix.putenv "GWLANG" setup_lang; Unix.putenv "GWGD" setup_dir.val
    };
    printf "\n";
    flush stdout
  }
;

value main () =
  do {
    if Sys.unix then intro ()
    else
      try let _ = Sys.getenv "WSERVER" in () with [ Not_found -> intro () ];
    Wserver.f None port.val 0 None wrap_setup
  }
;

main ();
