open Geneweb

let port = ref 2316
let gwd_port = ref 2317
let default_lang = ref "en"
let setup_dir = ref "."
let bin_dir = ref ""
let base_dir = ref ""
let lang_param = ref ""
let only_file = ref ""
let bname = ref ""
let commnd = ref ""

let printer_conf =
  {
    Config.empty with
    output_conf =
      {
        status = Wserver.http;
        header = Wserver.header;
        body = Wserver.print_string;
        flush = Wserver.wflush;
      };
  }

let slashify s = String.map (function '\\' -> '/' | c -> c) s

let slashify_linux_dos s =
  if Sys.unix then s else String.map (function '/' -> '\\' | c -> c) s

let decode s = Mutil.decode (Adef.encoded s)
let encode s = (Mutil.encode s :> string)

let rec list_remove_assoc x = function
  | (x1, y1) :: l -> if x = x1 then l else (x1, y1) :: list_remove_assoc x l
  | [] -> []

let rec list_assoc_all x = function
  | [] -> []
  | (a, b) :: l -> if a = x then b :: list_assoc_all x l else list_assoc_all x l

type config = {
  lang : string;
  comm : string;
  env : (string * string) list;
  request : string list;
  lexicon : (string, string) Hashtbl.t;
}

let transl conf w =
  try Hashtbl.find conf.lexicon w with Not_found -> "[" ^ w ^ "]"

let charset conf =
  try Hashtbl.find conf.lexicon "!charset" with Not_found -> "utf-8"

let header_no_page_title conf title =
  Output.status printer_conf Def.OK;
  Output.header printer_conf "Content-type: text/html; charset=%s"
    (charset conf);
  Output.print_sstring printer_conf
    "<!DOCTYPE HTML PUBLIC \"-//W3C//DTD HTML 4.0 Transitional//EN\" \
     \"http://www.w3.org/TR/REC-html40/loose.dtd\"><head><meta name=\"robots\" \
     content=\"none\"><title>";
  title true;
  Output.print_sstring printer_conf "</title></head><body>"

let abs_setup_dir () =
  if Filename.is_relative !setup_dir then
    Filename.concat (Sys.getcwd ()) !setup_dir
  else !setup_dir

let trailer _conf =
  Output.print_sstring printer_conf {|<br><div id="footer"><hr><div><em>|};
  Output.print_sstring printer_conf
    {|<a href="https://github.com/geneweb/geneweb/">|};
  Output.print_sstring printer_conf
    {|<img src="images/logo_bas.png" style="border:0"></a>|};
  Output.print_sstring printer_conf {| Version |};
  Output.print_sstring printer_conf Version.ver;
  Output.print_sstring printer_conf
    " Copyright &copy; 1998-2021</em></div></div></body></html>"

let header conf title =
  header_no_page_title conf title;
  Output.print_sstring printer_conf "<h1>";
  title false;
  Output.print_sstring printer_conf "</h1>"

let strip_control_m s =
  let rec loop i len =
    if i = String.length s then Buff.get len
    else if s.[i] = '\r' then loop (i + 1) len
    else loop (i + 1) (Buff.store len s.[i])
  in
  loop 0 0

let strip_spaces str =
  let start =
    let rec loop i =
      if i = String.length str then i
      else
        match str.[i] with ' ' | '\r' | '\n' | '\t' -> loop (i + 1) | _ -> i
    in
    loop 0
  in
  let stop =
    let rec loop i =
      if i = -1 then i + 1
      else
        match str.[i] with
        | ' ' | '\r' | '\n' | '\t' -> loop (i - 1)
        | _ -> i + 1
    in
    loop (String.length str - 1)
  in
  if start = 0 && stop = String.length str then str
  else if start > stop then ""
  else String.sub str start (stop - start)

let getenv env label = decode (List.assoc (decode label) env)
let p_getenv env label = try Some (getenv env label) with Not_found -> None
let s_getenv env label = try getenv env label with Not_found -> ""

let rec skip_spaces s i =
  if i < String.length s && s.[i] = ' ' then skip_spaces s (i + 1) else i

let create_env s =
  let s = (s : Adef.encoded_string :> string) in
  let rec get_assoc beg i =
    if i = String.length s then
      if i = beg then [] else [ String.sub s beg (i - beg) ]
    else if s.[i] = ';' || s.[i] = '&' then
      let next_i = skip_spaces s (succ i) in
      String.sub s beg (i - beg) :: get_assoc next_i next_i
    else get_assoc beg (succ i)
  in
  let rec separate i s =
    if i = String.length s then (s, "")
    else if s.[i] = '=' then
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
    else separate (succ i) s
  in
  List.map (separate 0) (get_assoc 0 0)

let numbered_key k =
  if k = "" then None
  else
    match k.[String.length k - 1] with
    | '1' .. '9' as c -> Some (String.sub k 0 (String.length k - 1), c)
    | _ -> None

let stringify s =
  try
    let _ = String.index s ' ' in
    "\"" ^ s ^ "\""
  with Not_found -> s

let parameters =
  let rec loop comm = function
    | (k, s) :: env -> (
        let k = strip_spaces (decode k) in
        let s = strip_spaces (decode s) in
        if k = "" || s = "" then loop comm env
        else if k = "opt" then loop comm env
        else if k = "anon" then loop (comm ^ " " ^ stringify s) env
        else
          match numbered_key k with
          | Some (k, '1') ->
              let s, env =
                let rec loop s = function
                  | (k1, s1) :: env as genv -> (
                      match numbered_key k1 with
                      | Some (k1, _) when k1 = k ->
                          let s1 = strip_spaces (decode s1) in
                          let s =
                            if s1 = "" then s else s ^ " \"" ^ s1 ^ "\""
                          in
                          loop s env
                      | _ -> (s, genv))
                  | [] -> (s, [])
                in
                loop ("\"" ^ s ^ "\"") env
              in
              loop (comm ^ " -" ^ k ^ " " ^ s) env
          | Some _ -> loop comm env
          | None ->
              if s = "none" then loop comm env
              else if s = "on" then loop (comm ^ " -" ^ k) env
              else if s.[0] = '_' then loop (comm ^ " -" ^ k ^ stringify s) env
              else if s.[String.length s - 1] = '_' then
                loop (comm ^ " -" ^ s ^ k) env
              else loop (comm ^ " -" ^ k ^ " " ^ stringify s) env)
    | [] -> comm
  in
  loop ""

let parameters_1 =
  let rec loop comm bname = function
    | (k, s) :: env ->
        let k = strip_spaces (decode k) in
        let s = strip_spaces (decode s) in
        if k = "" || s = "" then loop comm bname env
        else if k = "opt" then loop comm bname env
        else if k = "gwd_p" && s <> "" then
          loop (comm ^ " -gwd_p " ^ stringify s) bname env
        else if k = "anon" && s <> "" then
          loop (comm ^ " " ^ stringify s) (stringify s) env
        else if k = "a" then loop (comm ^ " -a") bname env
        else if k = "s" then loop (comm ^ " -s") bname env
        else if k = "d" && s <> "" then
          loop (comm ^ " -d " ^ stringify s) bname env
        else if k = "i" && s <> "" then
          loop (comm ^ " -i " ^ stringify s) bname env
        else if k = "bf" then loop (comm ^ " -bf") bname env
        else if k = "del" && s <> "" then
          loop (comm ^ " -del " ^ stringify s) bname env
        else if k = "cnt" && s <> "" then
          loop (comm ^ " -cnt " ^ stringify s) bname env
        else if k = "exact" then loop (comm ^ " -exact") bname env
        else if k = "o1" && s <> "" then
          let out = stringify s in
          comm ^ " -o " ^ out ^ " > " ^ out
        else if k = "o" && s <> "" then
          if s = "choice" then loop comm bname env
          else
            let out = stringify s in
            let out =
              if out = "/notes_d/connex.txt" then bname ^ ".gwb" ^ out else out
            in
            let out = slashify_linux_dos out in
            comm ^ " -o " ^ out ^ " > " ^ out
        else loop comm bname env
    | [] -> comm
  in
  loop "" ""

let parameters_2 =
  let rec loop comm = function
    | (k, s) :: env ->
        let k = strip_spaces (decode k) in
        let s = strip_spaces (decode s) in
        if k = "" || s = "" then loop comm env
        else if k = "opt" then loop comm env
        else if k = "anon1" then loop (comm ^ " " ^ stringify s) env
        else if k = "anon2" then loop (comm ^ " " ^ stringify s) env
        else if k = "a1" then loop (comm ^ " -1 " ^ stringify s) env
        else if k = "a2" then loop (comm ^ " " ^ stringify s) env
        else if k = "a3" then loop (comm ^ " " ^ stringify s) env
        else if k = "b1" then loop (comm ^ " -2 " ^ stringify s) env
        else if k = "b2" then loop (comm ^ " " ^ stringify s) env
        else if k = "b3" then loop (comm ^ " " ^ stringify s) env
        else if k = "ad" then loop (comm ^ " -ad ") env
        else if k = "d" then loop (comm ^ " -d ") env
        else if k = "mem" then loop (comm ^ " -mem") env
        else if k = "o" then
          loop (comm ^ " -o " ^ stringify s ^ " > " ^ stringify s) env
        else loop comm env
    | [] -> comm
  in
  loop ""

let parameters_3 =
  let rec loop comm = function
    | (k, s) :: env ->
        let k = strip_spaces (decode k) in
        if k = "" || k = "opt" then loop comm env
        else if k = "anon" && s <> "" then loop (comm ^ " " ^ stringify s) env
        else if k = "bd" && s <> "" then
          loop (comm ^ " -" ^ stringify k ^ " " ^ stringify s) env
        else loop (comm ^ " -" ^ stringify k) env
    | [] -> comm
  in
  loop ""

let rec list_replace k v = function
  | [] -> [ (k, v) ]
  | (k1, _) :: env when k1 = k -> (k1, v) :: env
  | kv :: env -> kv :: list_replace k v env

let conf_with_env conf k v = { conf with env = list_replace k v conf.env }

let all_db dir =
  let list = ref [] in
  let dh = Unix.opendir dir in
  (try
     while true do
       let e = Unix.readdir dh in
       if Filename.check_suffix e ".gwb" then
         list := Filename.chop_suffix e ".gwb" :: !list
     done
   with End_of_file -> ());
  Unix.closedir dh;
  list := List.sort compare !list;
  !list

let selected env =
  List.fold_right (fun (k, v) env -> if v = "on_" then k :: env else env) env []

let parse_upto lim =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some c when c = lim ->
        Stream.junk strm__;
        Buff.get len
    | Some c -> (
        Stream.junk strm__;
        try loop (Buff.store len c) strm__
        with Stream.Failure -> raise (Stream.Error ""))
    | _ -> raise Stream.Failure
  in
  loop 0

let parse_upto_void lim =
  let rec loop len (strm__ : _ Stream.t) =
    match Stream.peek strm__ with
    | Some c when c = lim ->
        Stream.junk strm__;
        ()
    | Some c -> (
        Stream.junk strm__;
        try loop (Buff.store len c) strm__
        with Stream.Failure -> raise (Stream.Error ""))
    | _ -> raise Stream.Failure
  in
  loop 0

let is_directory x =
  try (Unix.lstat x).Unix.st_kind = Unix.S_DIR
  with Unix.Unix_error (_, _, _) -> false

let server_string conf =
  let s = Mutil.extract_param "host: " '\r' conf.request in
  try
    let i = String.rindex s ':' in
    String.sub s 0 i
  with Not_found -> "127.0.0.1"

let referer conf = Mutil.extract_param "referer: " '\r' conf.request

let only_file_name =
  lazy
    (if !only_file = "" then Filename.concat !setup_dir "only.txt"
     else Filename.concat !setup_dir !only_file)

(* this set of macros are used within translations, hence the repeat of some *)
(* like %l, %L, %P, ... and they may be different! %G  *)
let macro conf = function
  | '/' -> if Sys.unix then "/" else "\\"
  | 'a' -> strip_spaces (s_getenv conf.env "anon")
  | 'c' -> stringify !setup_dir
  | 'd' -> conf.comm
  | 'i' -> strip_spaces (s_getenv conf.env "i")
  | 'l' -> conf.lang
  | 'm' -> server_string conf
  | 'n' -> referer conf
  | 'o' -> strip_spaces (s_getenv conf.env "o")
  | 'O' ->
      Filename.remove_extension
        (Filename.basename (strip_spaces (s_getenv conf.env "o")))
  | 'p' -> parameters conf.env
  | 'q' -> Version.ver
  | 'u' -> Filename.dirname (abs_setup_dir ())
  | 'x' -> stringify !bin_dir
  | 'v' -> strip_spaces (s_getenv conf.env "odir")
  | 'w' -> slashify (Sys.getcwd ())
  | 'y' -> Filename.basename (Lazy.force only_file_name)
  | 'z' -> string_of_int !port
  | 'D' -> transl conf "!doc"
  | 'G' -> transl conf "!geneweb"
  | 'L' ->
      let lang = conf.lang in
      let lang_def = transl conf "!languages" in
      Translate.language_name ~sep:'|' lang lang_def
  | 'P' -> string_of_int !gwd_port
  | 'Q' -> parameters_1 conf.env
  | 'R' -> parameters_2 conf.env
  | '%' -> "%"
  | c -> "BAD MACRO " ^ String.make 1 c

let get_variable (strm : _ Stream.t) =
  let rec loop len =
    match Stream.peek strm with
    | Some ';' ->
        Stream.junk strm;
        Buff.get len
    | Some c ->
        Stream.junk strm;
        loop (Buff.store len c)
    | _ -> raise Stream.Failure
  in
  loop 0

let get_binding strm =
  let rec loop len =
    match Stream.peek strm with
    | Some '=' ->
        Stream.junk strm;
        let k = Buff.get len in
        (k, get_variable strm)
    | Some c ->
        Stream.junk strm;
        loop (Buff.store len c)
    | _ -> raise Stream.Failure
  in
  loop 0

let variables bname =
  let dir = Filename.concat !setup_dir "setup" in
  let fname = Filename.concat (Filename.concat dir "lang") bname in
  let ic = open_in fname in
  let strm = Stream.of_channel ic in
  let vlist, flist =
    let rec loop (vlist, flist) =
      match Stream.peek strm with
      | Some '%' ->
          Stream.junk strm;
          let vlist, flist =
            let (strm : _ Stream.t) = strm in
            match Stream.peek strm with
            | Some ('E' | 'C') ->
                Stream.junk strm;
                let v, _ = get_binding strm in
                if not (List.mem v vlist) then (v :: vlist, flist)
                else (vlist, flist)
            | Some 'V' ->
                Stream.junk strm;
                let v = get_variable strm in
                if not (List.mem v vlist) then (v :: vlist, flist)
                else (vlist, flist)
            | Some 'F' ->
                Stream.junk strm;
                let v = get_variable strm in
                if not (List.mem v flist) then (vlist, v :: flist)
                else (vlist, flist)
            | _ -> (vlist, flist)
          in
          loop (vlist, flist)
      | Some _ ->
          Stream.junk strm;
          loop (vlist, flist)
      | _ -> (vlist, flist)
    in
    loop ([], [])
  in
  close_in ic;
  (List.rev vlist, flist)

let nth_field s n =
  let rec loop nth i =
    let j = try String.index_from s i '|' with Not_found -> String.length s in
    if nth = n then String.sub s i (j - i)
    else if j = String.length s then s
    else loop (nth + 1) (j + 1)
  in
  loop 0 0

let translate_phrase lexicon s n =
  let n = match n with Some n -> n | None -> 0 in
  try
    let s = Hashtbl.find lexicon s in
    nth_field s n
  with Not_found -> "[" ^ nth_field s n ^ "]"

let file_contents fname =
  try
    let ic = open_in fname in
    let rec loop len =
      match try Some (input_char ic) with End_of_file -> None with
      | Some '\r' -> loop len
      | Some c -> loop (Buff.store len c)
      | None ->
          close_in ic;
          Buff.get len
    in
    loop 0
  with Sys_error _ -> ""

let cut_at_equal s =
  match String.index_opt s '=' with
  | Some i ->
      (String.sub s 0 i, String.sub s (succ i) (String.length s - succ i))
  | None -> (s, "")

let loc_read_base_env bname =
  let fname = !GWPARAM.config bname in
  match try Some (open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let rec loop env =
        match try Some (input_line ic) with End_of_file -> None with
        | None ->
            close_in ic;
            env
        | Some s ->
            let s =
              if String.length s >= 1 && Char.code s.[String.length s - 1] = 13
              then String.sub s 0 (String.length s - 1)
              else s
            in
            if s = "" || s.[0] = '#' then loop env
            else loop (cut_at_equal s :: env)
      in
      loop []
  | None -> []

let rec split_string acc s =
  if String.length s < 80 then acc ^ s
  else
    match String.index_from_opt s 70 ' ' with
    | Some i when String.length s > i + 3 ->
        split_string
          (acc ^ String.sub s 0 i ^ "\n")
          (String.sub s (i + 1) (String.length s - i - 1))
    | _ -> acc ^ s

let rec copy_from_stream conf print strm =
  try
    while true do
      match Stream.next strm with
      | '[' -> (
          match Stream.peek strm with
          | Some '\n' ->
              let s = parse_upto ']' strm in
              print
                "Translations must be on a single line: [string to translate]";
              print s
          | _ ->
              let s =
                let rec loop len =
                  match Stream.peek strm with
                  | Some ']' ->
                      Stream.junk strm;
                      Buff.get len
                  | Some c ->
                      Stream.junk strm;
                      loop (Buff.store len c)
                  | _ -> Buff.get len
                in
                loop 0
              in
              let n =
                match Stream.peek strm with
                | Some ('0' .. '9' as c) ->
                    Stream.junk strm;
                    Char.code c - Char.code '0'
                | _ -> 0
              in
              (* translate before macro processing *)
              let s = nth_field (transl conf s) n in
              (* FIXME must be more efficient way of doing this (with buffers?) *)
              let s =
                let rec loop acc s =
                  if String.length s = 0 then acc
                  else if s.[0] = '%' then
                    loop
                      (acc ^ macro conf s.[1])
                      (String.sub s 2 (String.length s - 2))
                  else
                    loop
                      (acc ^ String.sub s 0 1)
                      (String.sub s 1 (String.length s - 1))
                in
                loop "" s
              in
              print (split_string "" s))
      | '%' -> (
          let c = Stream.next strm in
          match c with
          | '(' ->
              let rec loop () =
                let _s = parse_upto '%' strm in
                let c = Stream.next strm in
                if c = ')' then () else loop ()
              in
              loop ()
          | 'b' -> for_all conf print (all_db ".") strm
          | 'e' ->
              print "lang=";
              print conf.lang;
              List.iter
                (fun (k, s) ->
                  if k = "opt" then ()
                  else (
                    print ";";
                    print k;
                    print "=";
                    print s;
                    ()))
                conf.env
          | 'f' ->
              (* see r *)
              let in_file = get_variable strm in
              let s =
                file_contents
                  (slashify_linux_dos (!bin_dir ^ "/setup/" ^ in_file))
              in
              let in_base = strip_spaces (s_getenv conf.env "anon") in
              GWPARAM.test_reorg in_base;
              let benv = loc_read_base_env in_base in
              let conf = { conf with env = benv @ conf.env } in
              (* depending on when %f is called, conf may be sketchy *)
              (* conf will know bvars from basename.gwf and evars from url *)
              copy_from_stream conf print (Stream.of_string s)
          | 'g' -> print_specific_file conf print "comm.log" strm
          | 'h' ->
              print "<input type=hidden name=lang value=";
              print conf.lang;
              print ">\n";
              List.iter
                (fun (k, s) ->
                  if k <> "opt" then (
                    print "<input type=hidden name=";
                    print k;
                    print " value=\"";
                    print (decode s);
                    print "\">\n"))
                conf.env
          | 'j' -> print_selector conf print
          | 'k' -> for_all conf print (fst (List.split conf.env)) strm
          | 'l' -> print conf.lang
          | 'r' ->
              print_specific_file conf print
                (Filename.concat !setup_dir "gwd.arg")
                strm
          | 's' -> for_all conf print (selected conf.env) strm
          | 't' -> print_if conf print (not Sys.unix) strm
          | 'v' ->
              let out = strip_spaces (s_getenv conf.env "o") in
              let bd = strip_spaces (s_getenv conf.env "bd") in
              let base = Filename.concat bd out in
              print_if conf print (Sys.file_exists (base ^ ".gwb")) strm
          | 'y' -> for_all conf print (all_db (s_getenv conf.env "anon")) strm
          | 'z' -> print (string_of_int !port)
          | ('A' .. 'Z' | '0' .. '9') as c -> (
              match c with
              | 'C' | 'E' -> (
                  let k, v = get_binding strm in
                  match p_getenv conf.env k with
                  | Some x ->
                      if x = v then
                        print (if c = 'C' then " checked" else " selected")
                  | None -> ())
              | 'D' -> print (transl conf "!doc")
              (* | 'F' see 'V' *)
              | 'G' -> print_specific_file_tail conf print "gwsetup.log" strm
              | 'H' ->
                  (* print the content of -o filename, prepend bname *)
                  let outfile = strip_spaces (s_getenv conf.env "o") in
                  let bname = strip_spaces (s_getenv conf.env "anon") in
                  let outfile =
                    if bname <> "" then
                      slashify_linux_dos bname ^ ".gwb" ^ outfile
                    else outfile
                  in
                  print_specific_file conf print outfile strm
              | 'I' ->
                  (* %Ivar;value;{var = value part|false part} *)
                  (* var is a evar from url or a bvar from basename.gwf or setup.gwf *)
                  let k1 = get_variable strm in
                  let k2 = get_variable strm in
                  print_if_else conf print (p_getenv conf.env k1 = Some k2) strm
              | 'K' ->
                  (* print the name of -o filename, prepend bname or -o1 filename *)
                  let outfile1 = strip_spaces (s_getenv conf.env "o") in
                  let bname = strip_spaces (s_getenv conf.env "anon") in
                  let outfile2 = strip_spaces (s_getenv conf.env "o1") in
                  let outfile =
                    if outfile2 <> "" then outfile2
                    else if bname <> "" then
                      slashify_linux_dos bname ^ ".gwb" ^ outfile1
                    else outfile1
                  in
                  print outfile
              | 'L' ->
                  let lang = get_variable strm in
                  let lang_def = transl conf "!languages" in
                  print (Translate.language_name ~sep:'|' lang lang_def)
              | 'O' ->
                  let fname =
                    Filename.remove_extension
                      (Filename.basename (strip_spaces (s_getenv conf.env "o")))
                  in
                  let fname = slashify_linux_dos fname in
                  print fname
              | 'P' -> print (string_of_int !gwd_port)
              | 'Q' -> print (parameters_1 conf.env) (* same as p *)
              | 'R' -> print (parameters_2 conf.env) (* same as p *)
              | 'V' | 'F' -> (
                  let k = get_variable strm in
                  match p_getenv conf.env k with
                  | Some v -> print v
                  | None -> ())
              | 'S' -> print (parameters_3 conf.env)
              | _ -> (
                  match p_getenv conf.env (String.make 1 c) with
                  | Some v -> (
                      match Stream.peek strm with
                      | Some '{' ->
                          Stream.junk strm;
                          let s = parse_upto '}' strm in
                          print "\"";
                          print s;
                          print "\"";
                          if v = s then print " selected"
                      | Some '[' ->
                          Stream.junk strm;
                          let s = parse_upto ']' strm in
                          print "\"";
                          print s;
                          print "\"";
                          if v = s then print " checked"
                      | _ -> print (strip_spaces v))
                  | None -> print ("BAD MACRO 2" ^ String.make 1 c)))
          | c -> print (macro conf c))
      | c -> print (String.make 1 c)
    done
  with Stream.Failure -> ()

and print_specific_file conf print fname strm =
  match Stream.next strm with
  | '{' ->
      let s = parse_upto '}' strm in
      if Sys.file_exists fname then (
        let ic = open_in fname in
        if in_channel_length ic = 0 then
          copy_from_stream conf print (Stream.of_string s)
        else copy_from_stream conf print (Stream.of_channel ic);
        close_in ic)
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> ()

and print_specific_file_tail conf print fname strm =
  match Stream.next strm with
  | '{' ->
      (* TODO implement the "tail" part *)
      let s = parse_upto '}' strm in
      if Sys.file_exists fname then (
        let ic = open_in fname in
        if in_channel_length ic = 0 then
          copy_from_stream conf print (Stream.of_string s)
        else copy_from_stream conf print (Stream.of_channel ic);
        close_in ic)
      else copy_from_stream conf print (Stream.of_string s)
  | _ -> ()

and print_selector conf print =
  let sel =
    try getenv conf.env "sel"
    with Not_found -> (
      try Sys.getenv "HOME" with Not_found -> Sys.getcwd ())
  in
  let list =
    let sel =
      if
        (not Sys.unix)
        && String.length sel = 3
        && sel.[1] = ':'
        && sel.[2] = '\\'
      then sel ^ "."
      else sel
    in
    try
      let dh = Unix.opendir sel in
      let rec loop list =
        match try Some (Unix.readdir dh) with End_of_file -> None with
        | Some x ->
            let list =
              if x = ".." then x :: list
              else if String.length x > 0 && x.[0] = '.' then list
              else x :: list
            in
            loop list
        | None -> List.sort compare list
      in
      loop []
    with Unix.Unix_error (_, _, _) -> [ ".." ]
  in
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
            else if sel.[String.length sel - 1] <> '\\' then
              Filename.dirname sel ^ "\\"
            else Filename.dirname sel
          else Filename.concat sel x
        in
        let x = if is_directory d then Filename.concat x "" else x in
        (d, x))
      list
  in
  let max_len =
    List.fold_left (fun max_len (_, x) -> max max_len (String.length x)) 0 list
  in
  let min_interv = 2 in
  let line_len = 72 in
  let n_by_line = max 1 ((line_len + min_interv) / (max_len + min_interv)) in
  let newline () = print "\n" in
  newline ();
  (let rec loop i = function
     | (d, x) :: list ->
         print "<a class=\"j\" href=\"";
         print conf.comm;
         print "?lang=";
         print conf.lang;
         print ";";
         List.iter
           (fun (k, v) ->
             if k <> "sel" && k <> "body_prop" then (
               print k;
               print "=";
               print v;
               print ";"))
           conf.env;
         print "sel=";
         print (encode d);
         print "\">";
         print x;
         print "</a>";
         if i = n_by_line then (
           newline ();
           loop 1 list)
         else if list = [] then newline ()
         else (
           print (String.make (max_len + 2 - String.length x) ' ');
           loop (i + 1) list)
     | [] -> print "\n"
   in
   loop 1 list);
  print "</pre>\n"

and print_if conf print cond strm =
  match Stream.next strm with
  | '{' ->
      let s = parse_upto '}' strm in
      if cond then copy_from_stream conf print (Stream.of_string s)
  | _ -> ()

and print_if_else conf print cond strm =
  match Stream.next strm with
  | '{' ->
      let s1 = parse_upto '|' strm in
      let s2 = parse_upto '}' strm in
      if cond then copy_from_stream conf print (Stream.of_string s1)
      else copy_from_stream conf print (Stream.of_string s2)
  | _ -> ()

and for_all conf print list strm =
  match Stream.next strm with
  | '{' ->
      let s_exist = parse_upto '|' strm in
      let s_empty = parse_upto '}' strm in
      let eol =
        match Stream.peek strm with
        | Some '\\' ->
            Stream.junk strm;
            false
        | _ -> true
      in
      if list <> [] then
        List.iter
          (fun db ->
            let conf = conf_with_env conf "anon" db in
            copy_from_stream conf print (Stream.of_string s_exist);
            if eol then print "\n")
          list
      else (
        copy_from_stream conf print (Stream.of_string s_empty);
        if eol then print "\n")
  | _ -> ()

let print_file conf bname =
  let dir = Filename.concat !setup_dir "setup" in
  let fname = Filename.concat (Filename.concat dir "lang") bname in
  let ic_opt = try Some (open_in fname) with Sys_error _ -> None in
  match ic_opt with
  | Some ic ->
      Output.status printer_conf Def.OK;
      Output.header printer_conf "Content-type: text/html; charset=%s"
        (charset conf);
      copy_from_stream conf
        (Output.print_sstring printer_conf)
        (Stream.of_channel ic);
      close_in ic;
      trailer conf
  | None ->
      let title _ = Output.print_sstring printer_conf "Error" in
      header conf title;
      Output.print_sstring printer_conf "<ul><li>\n";
      Output.printf printer_conf "Cannot access file \"%s\".\n" fname;
      Output.print_sstring printer_conf "</ul>\n";
      trailer conf;
      raise Exit

let error conf str =
  header conf (fun _ -> Output.print_sstring printer_conf "Incorrect request");
  Output.printf printer_conf "<em>%s</em>\n" (String.capitalize_ascii str);
  trailer conf

let exec_f comm =
  let s = comm ^ " > " ^ "comm.log" in
  Printf.eprintf "$ cd \"%s\"\n" (Sys.getcwd ());
  flush stderr;
  Printf.eprintf "$ %s\n" s;
  flush stderr;
  Sys.command s

let out_name_of_ged in_file =
  let f = Filename.basename in_file in
  if Filename.check_suffix f ".ged" then Filename.chop_suffix f ".ged"
  else if Filename.check_suffix f ".GED" then Filename.chop_suffix f ".GED"
  else f

let out_name_of_gw in_file =
  let f = Filename.basename in_file in
  if Filename.check_suffix f ".gw" then Filename.chop_suffix f ".gw"
  else if Filename.check_suffix f ".GW" then Filename.chop_suffix f ".GW"
  else f

let basename s =
  let rec loop i =
    if i < 0 then s
    else
      match s.[i] with
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '.' -> loop (i - 1)
      | _ -> String.sub s (i + 1) (String.length s - i - 1)
  in
  loop (String.length s - 1)

let setup_gen conf =
  match p_getenv conf.env "v" with
  | Some fname -> print_file conf (basename fname)
  | _ -> error conf "request needs \"v\" parameter"

let simple conf =
  let ged =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let ged =
    if Filename.check_suffix (String.lowercase_ascii ged) ".ged" then ged
    else ""
  in
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | _ -> ""
  in
  let out_file =
    if ged = "" then out_file
    else if out_file = "" then out_name_of_ged ged
    else out_file
  in
  let env = ("f", "on") :: conf.env in
  let env = list_replace "anon" ged env in
  let conf =
    {
      comm = (if ged = "" then "gwc" else "ged2gwb");
      env = list_replace "o" out_file env;
      lang = conf.lang;
      request = conf.request;
      lexicon = conf.lexicon;
    }
  in
  if ged <> "" && not (Sys.file_exists ged) then print_file conf "err_unkn.htm"
  else if out_file = "" then print_file conf "err_miss.htm"
  else if not (Mutil.good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"

let gwc_or_ged2gwb out_name_of_in_name conf =
  let fname =
    match p_getenv conf.env "fname" with Some f -> strip_spaces f | None -> ""
  in
  let in_file =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let in_file =
    if fname = "" then in_file
    else in_file ^ (if Sys.unix then "/" else "\\") ^ fname
  in
  let conf = conf_with_env conf "anon" in_file in
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | _ -> ""
  in
  let out_file =
    if out_file = "" then out_name_of_in_name in_file else out_file
  in
  (* clean up env *)
  let conf = conf_with_env conf "body_prop" "" in
  let conf = conf_with_env conf "fname" "" in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" || out_file = "" then print_file conf "err_miss.htm"
  else if (not (Sys.file_exists in_file)) && not (String.contains fname '*')
  then print_file conf "err_unkn.htm"
  else if not (Mutil.good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "bso.htm"

let gwc_check conf =
  let conf = { conf with env = ("nofail", "on") :: ("f", "on") :: conf.env } in
  gwc_or_ged2gwb out_name_of_gw conf

let ged2gwb_check conf =
  let conf = { conf with env = ("f", "on") :: conf.env } in
  gwc_or_ged2gwb out_name_of_ged conf

let infer_rc conf rc =
  if not Sys.unix then
    if rc > 0 then rc
    else
      match p_getenv conf.env "o" with
      | Some out_file -> if Sys.file_exists (out_file ^ ".gwb") then 0 else 2
      | _ -> 0
  else rc

let gwc conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir "gwc") in
    exec_f (comm ^ parameters conf.env)
  in
  let rc = if not Sys.unix then infer_rc conf rc else rc in
  let gwo = strip_spaces (s_getenv conf.env "anon") ^ "o" in
  (try Sys.remove gwo with Sys_error _ -> ());
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bso_err.htm" else print_file conf "bso_ok.htm"

let gwdiff_check conf = print_file conf "bsi_diff.htm"

let gwdiff ok_file conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ parameters_2 conf.env)
  in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm"
  else
    let conf =
      conf_with_env conf "o" (Filename.basename (s_getenv conf.env "o"))
    in
    print_file conf ok_file

let gwfixbase_check conf = print_file conf "bsi_fix.htm"

let gwfixbase ok_file conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file

let cache_files_check conf =
  let in_base =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  if in_base = "" then print_file conf "err_miss.htm";
  print_file conf "bsi_cache_files.htm"

let cache_files ok_file conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir "cache_files") ^ " " in
    exec_f (comm ^ parameters_3 conf.env ^ " > comm.log")
  in
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file

let connex_check conf = print_file conf "bsi_connex.htm"

let connex ok_file conf =
  let ic = Unix.open_process_in "uname" in
  let uname = input_line ic in
  let () = close_in ic in
  let rc =
    let commnd =
      "cd " ^ Sys.getcwd () ^ "; tput bel;"
      ^ stringify (Filename.concat !bin_dir "connex")
      ^ " " ^ parameters_1 conf.env
    in
    if uname = "Darwin" then
      let launch = "tell application \"Terminal\" to do script " in
      Sys.command ("osascript -e '" ^ launch ^ " \" " ^ commnd ^ " \"' ")
    else if uname = "Linux" then
      (* non testé ! *)
      Sys.command ("xterm -e \" " ^ commnd ^ " \" ")
    else if Sys.win32 then
      (* à compléter et tester ! *)
      let commnd =
        stringify (Filename.concat !bin_dir "connex")
        ^ " " ^ parameters_1 conf.env
      in
      Sys.command commnd
    else (
      Printf.eprintf "%s (%s) %s (%s)\n" "Unknown Os_type" Sys.os_type
        "or wrong uname response" uname;
      2)
  in
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file

let gwu_or_gwb2ged_check suffix conf =
  let in_file =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let od =
    match p_getenv conf.env "od" with
    | Some f -> Filename.basename (strip_spaces f)
    | None -> ""
  in
  let out_file =
    match p_getenv conf.env "o" with
    | Some f -> Filename.basename (strip_spaces f)
    | None -> ""
  in
  let odir =
    if od = "odir" then
      match p_getenv conf.env "odir" with
      | Some f -> Filename.basename (strip_spaces f)
      | None -> ""
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

let gwu = gwu_or_gwb2ged_check ".gw"
let gwb2ged = gwu_or_gwb2ged_check ".ged"

let gwb2ged_or_gwu_1 ok_file conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm"
  else
    let conf =
      conf_with_env conf "o" (Filename.basename (s_getenv conf.env "o"))
    in
    print_file conf ok_file

let gwb2ged_1 = gwb2ged_or_gwu_1 "gw2gd_ok.htm"
let gwu_1 = gwb2ged_or_gwu_1 "gwu_ok.htm"

let consang_check conf =
  let in_f =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  if in_f = "" then print_file conf "err_miss.htm"
  else print_file conf "bsi.htm"

let update_nldb_check conf =
  let in_f =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  if in_f = "" then print_file conf "err_miss.htm"
  else print_file conf "bsi.htm"

let has_gwu dir =
  try
    if Sys.unix then Array.mem "gwu" (Sys.readdir dir)
    else
      Array.exists
        (fun s -> String.lowercase_ascii s = "gwu.exe")
        (Sys.readdir dir)
  with _ -> false

let recover conf =
  let init_dir =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let init_dir, dir_has_gwu =
    if has_gwu init_dir then (init_dir, true)
    else
      let dir = init_dir in
      if has_gwu dir then (dir, true)
      else
        let dir = Filename.dirname init_dir in
        if has_gwu dir then (dir, true)
        else
          let dir = Filename.concat dir "gw" in
          if has_gwu dir then (dir, true) else (init_dir, false)
  in
  let conf = conf_with_env conf "anon" init_dir in
  let dest_dir = Sys.getcwd () in
  if init_dir = "" then print_file conf "err_miss.htm"
  else if init_dir = dest_dir then print_file conf "err_smdr.htm"
  else if not (Sys.file_exists init_dir) then print_file conf "err_ndir.htm"
  else if
    Sys.unix
    &&
    try
      (Unix.stat (Filename.concat init_dir ".")).Unix.st_ino
      = (Unix.stat (Filename.concat dest_dir ".")).Unix.st_ino
    with Unix.Unix_error (_, _, _) -> false
  then print_file conf "err_smdr.htm"
  else if not dir_has_gwu then print_file conf "err_ngw.htm"
  else print_file conf "recover1.htm"

let recover_1 conf =
  let in_file =
    match p_getenv conf.env "i" with Some f -> strip_spaces f | None -> ""
  in
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | None -> ""
  in
  let by_gedcom =
    match p_getenv conf.env "ged" with Some "on" -> true | _ -> false
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  if in_file = "" then print_file conf "err_miss.htm"
  else if not (Mutil.good_name out_file) then print_file conf "err_name.htm"
  else
    let old_to_src, o_opt, tmp, src_to_new =
      if not by_gedcom then ("gwu", " > ", "tmp.gw", "gwc")
      else ("gwb2ged", " -o ", "tmp.ged", "ged2gwb")
    in
    let conf =
      {
        conf with
        env =
          ("U", old_to_src) :: ("O", o_opt) :: ("T", tmp)
          :: ("src2new", src_to_new) :: conf.env;
      }
    in
    print_file conf "recover2.htm"

let recover_2 conf =
  let init_dir =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let in_file =
    match p_getenv conf.env "i" with Some f -> strip_spaces f | None -> ""
  in
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | None -> ""
  in
  let by_gedcom =
    match p_getenv conf.env "ged" with Some "on" -> true | _ -> false
  in
  let old_to_src, o_opt, tmp, src_to_new =
    if not by_gedcom then ("gwu", " > ", "tmp.gw", "gwc")
    else ("gwb2ged", " -o ", "tmp.ged", "ged2gwb")
  in
  let out_file = if out_file = "" then in_file else out_file in
  let conf = conf_with_env conf "o" out_file in
  let dir = Sys.getcwd () in
  let rc =
    try
      Printf.eprintf "$ cd \"%s\"\n" init_dir;
      flush stderr;
      Sys.chdir init_dir;
      let c =
        Filename.concat "." old_to_src
        ^ " " ^ in_file ^ o_opt
        ^ stringify (Filename.concat dir tmp)
      in
      Printf.eprintf "$ %s\n" c;
      flush stderr;
      Sys.command c
    with e ->
      Sys.chdir dir;
      raise e
  in
  let rc =
    if rc = 0 then (
      Printf.eprintf "$ cd \"%s\"\n" dir;
      flush stderr;
      Sys.chdir dir;
      let c =
        Filename.concat !bin_dir src_to_new
        ^ " " ^ tmp ^ " -f -o " ^ out_file ^ " > " ^ "comm.log"
      in
      Printf.eprintf "$ %s\n" c;
      flush stderr;
      let rc = Sys.command c in
      let rc = if not Sys.unix then infer_rc conf rc else rc in
      Printf.eprintf "\n";
      flush stderr;
      rc)
    else rc
  in
  if rc > 1 then (
    Sys.chdir dir;
    print_file conf "err_reco.htm")
  else print_file conf "bso_ok.htm"

let cleanup conf =
  let in_base =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let conf = { conf with comm = "." } in
  if in_base = "" then print_file conf "err_miss.htm"
  else print_file conf "cleanup1.htm"

let cleanup_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let in_base_dir = in_base ^ ".gwb" in
  Printf.eprintf "$ cd \"%s\"\n" (Sys.getcwd ());
  flush stderr;
  let c = Filename.concat !bin_dir "gwu" ^ " " ^ in_base ^ " -o tmp.gw" in
  Printf.eprintf "$ %s\n" c;
  flush stderr;
  let _ = Sys.command c in
  Printf.eprintf "$ mkdir old\n";
  (try Unix.mkdir "old" 0o755 with Unix.Unix_error (_, _, _) -> ());

  if Sys.unix then Printf.eprintf "$ rm -rf old/%s\n" in_base_dir
  else (
    Printf.eprintf "$ del old\\%s\\*.*\n" in_base_dir;
    Printf.eprintf "$ rmdir old\\%s\n" in_base_dir);

  flush stderr;
  Mutil.rm_rf (Filename.concat "old" in_base_dir);

  if Sys.unix then Printf.eprintf "$ mv %s old/.\n" in_base_dir
  else Printf.eprintf "$ move %s old\\.\n" in_base_dir;

  flush stderr;
  Sys.rename in_base_dir (Filename.concat "old" in_base_dir);
  let c =
    Filename.concat !bin_dir "gwc"
    ^ " tmp.gw -nofail -o " ^ in_base ^ " > comm.log 2>&1"
  in
  Printf.eprintf "$ %s\n" c;
  flush stderr;
  let rc = Sys.command c in

  let rc = if not Sys.unix then infer_rc conf rc else rc in

  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then
    let conf = { conf with comm = "gwc" } in
    print_file conf "bsi_err.htm"
  else print_file conf "clean_ok.htm"

let rec check_new_names conf l1 l2 =
  match (l1, l2) with
  | (k, v) :: l, x :: m ->
      if k <> x then (
        print_file conf "err_outd.htm";
        raise Exit)
      else if not (Mutil.good_name v) then (
        let conf = { conf with env = ("o", v) :: conf.env } in
        print_file conf "err_name.htm";
        raise Exit)
      else check_new_names conf l m
  | [], [] -> ()
  | _ ->
      print_file conf "err_outd.htm";
      raise Exit

let rec check_rename_conflict conf = function
  | x :: l ->
      if List.mem x l then (
        let conf = { conf with env = ("o", x) :: conf.env } in
        print_file conf "err_cnfl.htm";
        raise Exit)
      else check_rename_conflict conf l
  | [] -> ()

let rename conf =
  let rename_list =
    List.map (fun (k, v) -> (k, strip_spaces (decode v))) conf.env
  in
  try
    check_new_names conf rename_list (all_db ".");
    check_rename_conflict conf (snd (List.split rename_list));
    List.iter
      (fun (k, v) -> if k <> v then Sys.rename (k ^ ".gwb") ("_" ^ k ^ ".gwb"))
      rename_list;
    List.iter
      (fun (k, v) -> if k <> v then Sys.rename ("_" ^ k ^ ".gwb") (v ^ ".gwb"))
      rename_list;
    print_file conf "ren_ok.htm"
  with Exit -> ()

let delete conf = print_file conf "delete_1.htm"

let delete_1 conf =
  List.iter (fun (k, v) -> if v = "del" then Mutil.rm_rf (k ^ ".gwb")) conf.env;
  print_file conf "del_ok.htm"

let merge conf =
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | _ -> ""
  in
  let conf = { conf with comm = "." } in
  let bases = selected conf.env in
  if out_file = "" || List.length bases < 2 then print_file conf "err_miss.htm"
  else if not (Mutil.good_name out_file) then print_file conf "err_name.htm"
  else print_file conf "merge_1.htm"

let merge_1 conf =
  let out_file =
    match p_getenv conf.env "o" with Some f -> strip_spaces f | _ -> ""
  in
  let bases = selected conf.env in
  let dir = Sys.getcwd () in
  Printf.eprintf "$ cd \"%s\"\n" dir;
  flush stderr;
  Sys.chdir dir;
  let rc =
    let rec loop = function
      | [] -> 0
      | b :: bases ->
          let c =
            Filename.concat !bin_dir "gwu" ^ " " ^ b ^ " -o " ^ b ^ ".gw"
          in
          Printf.eprintf "$ %s\n" c;
          flush stderr;
          let r = Sys.command c in
          if r = 0 then loop bases else r
    in
    loop bases
  in
  let rc =
    if rc <> 0 then rc
    else
      let c =
        Filename.concat !bin_dir "gwc"
        ^ List.fold_left
            (fun s b ->
              if s = "" then " " ^ b ^ ".gw" else s ^ " -sep " ^ b ^ ".gw")
            "" bases
        ^ " -f -o " ^ out_file ^ " > comm.log 2>&1"
      in
      Printf.eprintf "$ %s\n" c;
      flush stderr;
      Sys.command c
  in
  if rc > 1 then print_file conf "bso_err.htm" else print_file conf "bso_ok.htm"

let read_gwd_arg () =
  let fname = Filename.concat !setup_dir "gwd.arg" in
  match try Some (open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let list =
        let rec loop list =
          match try Some (input_line ic) with End_of_file -> None with
          | Some "" -> loop list
          | Some s -> loop (s :: list)
          | None -> list
        in
        loop []
      in
      close_in ic;
      let rec loop env = function
        | x :: l ->
            if x.[0] = '-' then
              let x = String.sub x 1 (String.length x - 1) in
              match l with
              | y :: l when y.[0] <> '-' -> loop ((x, y) :: env) l
              | _ -> loop ((x, "") :: env) l
            else loop env l
        | [] -> List.rev env
      in
      loop [] (List.rev list)
  | None -> []

let gwf conf =
  let in_base =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  if in_base = "" then print_file conf "err_miss.htm"
  else
    let benv = loc_read_base_env in_base in
    let trailer =
      if !GWPARAM.reorg then
        Filename.concat (!GWPARAM.lang_d in_base "") (in_base ^ ".trl")
      else
        Filename.concat "lang" (in_base ^ ".trl")
        |> file_contents |> Util.escape_html
        |> fun s -> (s :> string)
    in
    let conf = { conf with env = benv @ (("trailer", trailer) :: conf.env) } in
    print_file conf "gwf_1.htm"

let gwf_1 conf =
  let in_base =
    match p_getenv conf.env "anon" with Some f -> strip_spaces f | None -> ""
  in
  let reorg = match p_getenv conf.env "reorg" with Some s -> s | _ -> "" in
  if reorg = "on" then GWPARAM.reorg := true;
  GWPARAM.test_reorg in_base;
  let benv = loc_read_base_env in_base in
  let vars, _ = variables "gwf_1.htm" in
  let oc =
    open_out
      (if !GWPARAM.reorg then
         Filename.concat (!GWPARAM.bpath in_base) in_base ^ ".gwf"
       else in_base ^ ".gwf")
  in
  let body_prop =
    match p_getenv conf.env "proposed_body_prop" with
    | Some "" | None -> s_getenv conf.env "body_prop"
    | Some x -> x
  in
  Printf.fprintf oc "# File generated by \"setup\"\n\n";
  List.iter
    (fun k ->
      match k with
      | "body_prop" ->
          if body_prop = "" then ()
          else Printf.fprintf oc "body_prop=%s\n" body_prop
      | _ -> Printf.fprintf oc "%s=%s\n" k (s_getenv conf.env k))
    vars;
  List.iter
    (fun (k, v) ->
      if List.mem k vars then () else Printf.fprintf oc "%s=%s\n" k v)
    benv;
  close_out oc;
  let trl = strip_spaces (strip_control_m (s_getenv conf.env "trailer")) in

  let trl_dir = !GWPARAM.etc_d in_base in
  let trl_file = Filename.concat trl_dir "trl.txt" in
  try Unix.mkdir trl_dir 0o755
  with Unix.Unix_error (_, _, _) ->
    ();
    (try
       if trl = "" then Sys.remove trl_file
       else
         let oc = open_out trl_file in
         output_string oc trl;
         output_string oc "\n";
         close_out oc
     with Sys_error _ -> ());
    print_file conf "gwf_ok.htm"

let gwd conf =
  let aenv = read_gwd_arg () in
  let get v = try List.assoc v aenv with Not_found -> "" in
  let conf =
    {
      conf with
      env =
        ("default_lang", get "lang")
        :: ("only", get "only")
        :: ("log", Filename.basename (get "log"))
        :: conf.env;
    }
  in
  print_file conf "gwd.htm"

let gwd_1 conf =
  let oc = open_out (Filename.concat !setup_dir "gwd.arg") in
  let print_param k =
    match p_getenv conf.env k with
    | Some v when v <> "" -> Printf.fprintf oc "-%s\n%s\n" k v
    | _ -> ()
  in
  if p_getenv conf.env "setup_link" <> None then
    Printf.fprintf oc "-setup_link\n";
  print_param "only";
  (match p_getenv conf.env "default_lang" with
  | Some v when v <> "" -> Printf.fprintf oc "-lang\n%s\n" v
  | _ -> ());
  print_param "log";
  close_out oc;
  print_file conf "gwd_ok.htm"

let ged2gwb conf =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ " -fne '\"\"'" ^ parameters conf.env)
  in
  let rc = if not Sys.unix then infer_rc conf rc else rc in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bso_err.htm"
  else
    let bname = try List.assoc "o" conf.env with Not_found -> "" in
    Util.print_default_gwf_file bname;
    print_file conf "bso_ok.htm"

let consang conf ok_file =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file

let update_nldb conf ok_file =
  let rc =
    let comm = stringify (Filename.concat !bin_dir conf.comm) in
    exec_f (comm ^ parameters conf.env)
  in
  Printf.eprintf "\n";
  flush stderr;
  if rc > 1 then print_file conf "bsi_err.htm" else print_file conf ok_file

let separate_slashed_filename s =
  let rec loop i =
    match try Some (String.index_from s i '/') with Not_found -> None with
    | Some j ->
        if j > i then String.sub s i (j - i) :: loop (j + 1) else loop (j + 1)
    | None ->
        if i >= String.length s then []
        else [ String.sub s i (String.length s - i) ]
  in
  loop 0

let end_with s x =
  let slen = String.length s in
  let xlen = String.length x in
  slen >= xlen && String.sub s (slen - xlen) xlen = x

let print_typed_file conf typ fname =
  let ic_opt = try Some (open_in_bin fname) with Sys_error _ -> None in
  match ic_opt with
  | Some ic ->
      Output.status printer_conf Def.OK;
      Output.header printer_conf "Content-type: %s" typ;
      Output.header printer_conf "Content-length: %d" (in_channel_length ic);
      (try
         while true do
           let c = input_char ic in
           Output.printf printer_conf "%c" c
         done
       with End_of_file -> ());
      close_in ic
  | None ->
      let title _ = Output.print_sstring printer_conf "Error" in
      header conf title;
      Output.print_sstring printer_conf "<ul><li>";
      Output.print_sstring printer_conf "Cannot access file \"";
      Output.print_string printer_conf (Util.escape_html fname);
      Output.print_sstring printer_conf "\".</ul>";
      trailer conf;
      raise Exit

let raw_file conf s =
  let fname =
    List.fold_left Filename.concat !setup_dir (separate_slashed_filename s)
  in
  let typ =
    if end_with s ".png" then "image/png"
    else if end_with s ".jpg" then "image/jpeg"
    else if end_with s ".gif" then "image/gif"
    else if end_with s ".css" then "text/css"
    else "text/html"
  in
  print_typed_file conf typ fname

let has_gwb_directories dh =
  try
    let rec loop () =
      let e = Unix.readdir dh in
      if Filename.check_suffix e ".gwb" then true else loop ()
    in
    loop ()
  with End_of_file ->
    Unix.closedir dh;
    false

let setup_comm_ok conf = function
  | "gwsetup" -> setup_gen conf
  | "simple" -> simple conf
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
  | "gwc" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> gwc_check conf
      | _ -> gwc conf)
  | "gwu" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> gwu conf
      | _ -> gwu_1 conf)
  | "ged2gwb" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> ged2gwb_check conf
      | _ -> ged2gwb conf)
  | "gwb2ged" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> gwb2ged conf
      | _ -> gwb2ged_1 conf)
  | "consang" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> consang_check conf
      | _ -> consang conf "consg_ok.htm")
  | "update_nldb" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> update_nldb_check conf
      | _ -> update_nldb conf "update_nldb_ok.htm")
  | "gwf" -> gwf conf
  | "gwf_1" -> gwf_1 conf
  | "gwd" -> gwd conf
  | "gwd_1" -> gwd_1 conf
  | "cache_files" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> cache_files_check conf
      | _ -> cache_files "cache_files_ok.htm" conf)
  | "connex" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> connex_check conf
      | _ -> connex "connex_ok.htm" conf)
  | "gwdiff" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> gwdiff_check conf
      | _ -> gwdiff "gwdiff_ok.htm" conf)
  | "gwfixbase" -> (
      match p_getenv conf.env "opt" with
      | Some "check" -> gwfixbase_check conf
      | _ -> gwfixbase "gwfix_ok.htm" conf)
  | x ->
      if
        Mutil.start_with "doc/" 0 x
        || Mutil.start_with "images/" 0 x
        || Mutil.start_with "css/" 0 x
      then raw_file conf x
      else error conf ("bad command: \"" ^ x ^ "\"")

let setup_comm conf comm =
  match p_getenv conf.env "cancel" with
  | Some _ ->
      setup_gen { conf with env = [ ("lang", conf.lang); ("v", "main.htm") ] }
  | None -> setup_comm_ok conf comm

let string_of_sockaddr = function
  | Unix.ADDR_UNIX s -> s
  | Unix.ADDR_INET (a, _) ->
      let str = Unix.string_of_inet_addr a in
      if str = "::ffff:127.0.0.1" then "::1"
      else if String.length str > 7 && String.sub str 0 7 = "::ffff:" then
        String.sub str 7 (String.length str - 7)
      else str

let only_addr () =
  let local_addr =
    if Unix.string_of_inet_addr Unix.inet6_addr_any = "::" then
      Unix.string_of_inet_addr Unix.inet_addr_loopback
    else Unix.string_of_inet_addr Unix.inet6_addr_loopback
  in
  let fname = Lazy.force only_file_name in
  match try Some (open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let v = try input_line ic with End_of_file -> local_addr in
      close_in ic;
      v
  | None -> local_addr

let lindex s c =
  let rec pos i =
    if i = String.length s then None
    else if s.[i] = c then Some i
    else pos (i + 1)
  in
  pos 0

let input_lexicon lang =
  let t = Hashtbl.create 501 in
  try
    let ic =
      List.fold_right Filename.concat
        [ !setup_dir; "setup"; "lang" ]
        "lexicon.txt"
      |> open_in
    in
    let derived_lang =
      match lindex lang '-' with Some i -> String.sub lang 0 i | _ -> ""
    in
    try
      (try
         while true do
           let k =
             let rec find_key line =
               if String.length line < 4 then find_key (input_line ic)
               else if String.sub line 0 4 <> "    " then
                 find_key (input_line ic)
               else line
             in
             find_key (input_line ic)
           in
           let k = String.sub k 4 (String.length k - 4) in
           let rec loop line =
             match lindex line ':' with
             | Some i ->
                 let line_lang = String.sub line 0 i in
                 (if
                    line_lang = lang
                    || (line_lang = derived_lang && not (Hashtbl.mem t k))
                  then
                    let v =
                      if i + 1 = String.length line then ""
                      else String.sub line (i + 2) (String.length line - i - 2)
                    in
                    Hashtbl.add t k v);
                 loop (input_line ic)
             | None -> ()
           in
           loop (input_line ic)
         done
       with End_of_file -> ());
      close_in ic;
      t
    with e ->
      close_in ic;
      raise e
  with Sys_error _ -> t

let setup (addr, req) comm (env_str : Adef.encoded_string) =
  let conf =
    let env = create_env env_str in
    if env = [] && (comm = "" || String.length comm = 2) then
      let lang =
        if comm = "" then !default_lang else String.lowercase_ascii comm
      in
      let lexicon = input_lexicon lang in
      { lang; comm = ""; env; request = req; lexicon }
    else
      let lang, env =
        match p_getenv env "lang" with
        | Some x -> (x, list_remove_assoc "lang" env)
        | _ -> (!default_lang, env)
      in
      let lexicon = input_lexicon lang in
      { lang; comm; env; request = req; lexicon }
  in
  let saddr = string_of_sockaddr addr in
  let s = only_addr () in
  if s <> saddr then (
    let conf = { conf with env = [ ("anon", saddr); ("o", s) ] } in
    Printf.eprintf "Invalid request from \"%s\"; only \"%s\" accepted.\n" saddr
      s;
    flush stderr;
    print_file conf "err_acc.htm")
  else if conf.comm = "" then print_file conf "welcome.htm"
  else setup_comm conf comm

let wrap_setup a b (c : Adef.encoded_string) =
  if not Sys.unix then (
    (* another process have been launched, therefore we lost variables;
       and we cannot parse the arg list again, because of possible spaces
       in arguments which may appear as separators *)
    (try default_lang := Sys.getenv "GWLANG" with Not_found -> ());
    (try setup_dir := Sys.getenv "GWGD" with Not_found -> ());
    try bin_dir := Sys.getenv "GWGD" with Not_found -> ());
  try setup a b c with Exit -> ()

let copy_text lang fname =
  let dir = Filename.concat !setup_dir "setup" in
  let fname = Filename.concat dir fname in
  match try Some (open_in fname) with Sys_error _ -> None with
  | Some ic ->
      let lexicon = input_lexicon lang in
      let conf = { lang; comm = ""; env = []; request = []; lexicon } in
      copy_from_stream conf print_string (Stream.of_channel ic);
      flush stdout;
      close_in ic
  | _ ->
      Printf.printf "\nCannot access file \"%s\".\n" fname;
      Printf.printf "Type \"Enter\" to exit\n? ";
      flush stdout;
      let _ = input_line stdin in
      ();
      exit 2

let set_gwd_default_language_if_absent lang =
  let env = read_gwd_arg () in
  let fname = Filename.concat !setup_dir "gwd.arg" in
  match try Some (open_out fname) with Sys_error _ -> None with
  | Some oc ->
      let lang_found = ref false in
      List.iter
        (fun (k, v) ->
          Printf.fprintf oc "-%s\n" k;
          if k = "lang" then lang_found := true;
          if v <> "" then Printf.fprintf oc "%s\n" v)
        env;
      if not !lang_found then Printf.fprintf oc "-lang\n%s\n" lang;
      close_out oc
  | None -> ()

let daemon = ref false

let usage =
  "Usage: " ^ Filename.basename Sys.argv.(0) ^ " [options] where options are:"

let speclist =
  [
    ( "-bd",
      Arg.String (fun x -> base_dir := x),
      "<dir> Directory where the databases are installed." );
    ( "-gwd_p",
      Arg.Int (fun x -> gwd_port := x),
      "<number> Specify the port number of gwd (default = "
      ^ string_of_int !gwd_port ^ "); > 1024 for normal users." );
    ("-lang", Arg.String (fun x -> lang_param := x), "<string> default lang");
    ("-daemon", Arg.Set daemon, " Unix daemon mode.");
    ( "-p",
      Arg.Int (fun x -> port := x),
      "<number> Select a port number (default = " ^ string_of_int !port
      ^ "); > 1024 for normal users." );
    ( "-only",
      Arg.String (fun s -> only_file := s),
      "<file> File containing the only authorized address" );
    ("-gd", Arg.String (fun x -> setup_dir := x), "<string> gwsetup directory");
    ( "-bindir",
      Arg.String (fun x -> bin_dir := x),
      "<string> binary directory (default = value of option -gd)" );
  ]
  |> List.sort compare |> Arg.align

let anonfun s = raise (Arg.Bad ("don't know what to do with " ^ s))

let null_reopen flags fd =
  if Sys.unix then (
    let fd2 = Unix.openfile "/dev/null" flags 0 in
    Unix.dup2 fd2 fd;
    Unix.close fd2)

let setup_available_languages = [ "de"; "en"; "es"; "fr"; "it"; "lv"; "sv" ]

let intro () =
  let default_gwd_lang, default_setup_lang =
    if Sys.unix then
      let s = try Sys.getenv "LANG" with Not_found -> "" in
      if List.mem s Version.available_languages then
        (s, if List.mem s setup_available_languages then s else "en")
      else
        let s = try Sys.getenv "LC_CTYPE" with Not_found -> "" in
        if String.length s >= 2 then
          let s = String.sub s 0 2 in
          if List.mem s Version.available_languages then
            (s, if List.mem s setup_available_languages then s else "en")
          else (!default_lang, !default_lang)
        else (!default_lang, !default_lang)
    else (!default_lang, !default_lang)
  in
  Secure.set_base_dir ".";
  Arg.parse speclist anonfun usage;
  if !bin_dir = "" then bin_dir := !setup_dir;
  default_lang := default_setup_lang;
  let gwd_lang, setup_lang =
    if !daemon then
      if Sys.unix then (
        let setup_lang =
          if String.length !lang_param < 2 then default_setup_lang
          else !lang_param
        in
        Printf.printf "To start, open location http://localhost:%d/\n" !port;
        flush stdout;
        if Unix.fork () = 0 then (
          Unix.close Unix.stdin;
          null_reopen [ Unix.O_WRONLY ] Unix.stdout)
        else exit 0;
        (default_gwd_lang, setup_lang))
      else (default_gwd_lang, default_setup_lang)
    else
      let gwd_lang, setup_lang =
        if String.length !lang_param < 2 then (
          copy_text "" "intro.txt";
          let x = String.lowercase_ascii (input_line stdin) in
          if String.length x < 2 then (default_gwd_lang, default_setup_lang)
          else
            let x = String.sub x 0 2 in
            (x, x))
        else (!lang_param, !lang_param)
      in
      copy_text setup_lang (Filename.concat "lang" "intro.txt");
      (gwd_lang, setup_lang)
  in
  set_gwd_default_language_if_absent gwd_lang;
  default_lang := setup_lang;
  if not Sys.unix then (
    Unix.putenv "GWLANG" setup_lang;
    Unix.putenv "GWGD" !setup_dir);
  Printf.printf "\n";
  flush stdout

let () =
  if Sys.unix then intro ()
  else if Sys.getenv_opt "WSERVER" = None then intro ();
  Wserver.start ~port:!port ~max_pending_requests:150 ~n_workers:1 wrap_setup
